#include "llvm/Transforms/Instrumentation/GlobalsIntrospection.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/MemoryBufferRef.h"
#include "llvm/Support/RandomNumberGenerator.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/IRReader/IRReader.h"

#ifdef __linux__
  #include <unistd.h>
#else

#endif

using namespace llvm;

namespace {
struct GlobalsIntrospectionPass : public ModulePass {
  static char ID;
  GlobalsIntrospectionPass() : ModulePass(ID) {
    initializeGlobalsIntrospectionPassPass(*PassRegistry::getPassRegistry());
  }

  bool runOnModule(Module &M) override {

    std::string staticLibPath = getStaticLibPath();

    if(staticLibPath.empty()) {
      WithColor::warning(errs()).resetColor() << "could not find introspection library, so metadata will not be included\n";
      return false;
    }

    std::unique_ptr<Module> staticLib = getStaticLib(M.getContext(), staticLibPath);

    Function* registratorFunction = copyRegistratorFunction(staticLib.get(), M);

    if(registratorFunction == nullptr) {
      WithColor::warning(errs()).resetColor() << "registrator function was not succesfully copied\n";
      return true;
    }

    registratorFunction->setLinkage(GlobalValue::LinkageTypes::LinkOnceAnyLinkage);

    // Generate a random identifier for this module, to make sure that
    // modules with equal filenames but different paths don't give global variable
    // name clashes
    char random_id[11] = {0};

    getRandomAsciiIdentifier(random_id, 10);

    Twine modUniqueName = Twine(M.getName()).concat(Twine(random_id));

    size_t numEntries;
    GlobalVariable* moduleEntries = createModuleEntries(M, modUniqueName, staticLib.get(), numEntries);
    GlobalVariable* moduleHeader = createModuleHeader(M, modUniqueName, staticLib.get(), moduleEntries, numEntries);

    GlobalVariable* registerFile = createStringConst(M, "register_file", PEEKFS_REGISTER_FILE);
    GlobalVariable* unregisterFile = createStringConst(M, "unregister_file", PEEKFS_UNREGISTER_FILE);


    Function* peekfsCtor = getPeekfsInitFunction(M, Twine("__peekfs_init.").concat(modUniqueName), registerFile, moduleHeader, registratorFunction);
    Function* peekfsDtor = getPeekfsInitFunction(M, Twine("__peekfs_exit.").concat(modUniqueName), unregisterFile, moduleHeader, registratorFunction);
    appendToGlobalCtors(M, peekfsCtor, 0);
    appendToGlobalDtors(M, peekfsDtor, 0);
    M.dump();

    return true;
  }

  private:

  Function* getPeekfsInitFunction(Module &M, Twine name, GlobalVariable* initFile, GlobalVariable* modHeader, Function* registratorFunction) {
    Function* introspectionConstructor = Function::createWithDefaultAttr(
      FunctionType::get(Type::getVoidTy(M.getContext()), false),
      GlobalValue::LinkageTypes::ExternalLinkage,
      M.getDataLayout().getProgramAddressSpace(),
      name,
      &M
    );

    introspectionConstructor->addFnAttr(Attribute::NoUnwind);
    introspectionConstructor->addFnAttr(Attribute::OptimizeNone);
    introspectionConstructor->addFnAttr(Attribute::NoInline);

    IRBuilder<> *Builder = new IRBuilder<>(BasicBlock::Create(M.getContext(), "entry", introspectionConstructor));
    std::vector<Value *> args;
    args.push_back(initFile);
    args.push_back(modHeader);

    Builder->CreateCall(registratorFunction, args);
    Builder->CreateRetVoid();

    return introspectionConstructor;
  }

  Function* copyRegistratorFunction(Module* staticLib, Module &M) {
    ValueToValueMapTy VMap;
    std::vector<Function*> toCopy;

    // First, copy all the declarations to the new module
    for(auto &staticLibFunc : staticLib->getFunctionList()) {
      toCopy.push_back(&staticLibFunc);

      std::vector<Type *> argTypes;

      for (const Argument &I : staticLibFunc.args()) {
          argTypes.push_back(I.getType());
      }

      FunctionType *newFunctionType = FunctionType::get(
          staticLibFunc.getFunctionType()->getReturnType(),
          argTypes,
          staticLibFunc.getFunctionType()->isVarArg()
        );

      Function *newFunction = Function::Create(
          newFunctionType,
          staticLibFunc.getLinkage(),
          staticLibFunc.getAddressSpace(),
          staticLibFunc.getName(),
          &M
        );

      VMap[&staticLibFunc] = newFunction;
    }

    for(Function* oldFunc : toCopy) {
      Value* newFuncVal = VMap[oldFunc];
      Function* newFunc = static_cast<Function*>(newFuncVal);

      Function::arg_iterator DestI = newFunc->arg_begin();
      for (const Argument &I : oldFunc->args()) {
        DestI->setName(I.getName()); // Copy the name over...
        VMap[&I] = &*DestI++;        // Add mapping to VMap
      }

      SmallVector<ReturnInst *, 8> Returns; // Ignore returns cloned.
      CloneFunctionInto(newFunc, oldFunc, VMap, CloneFunctionChangeType::DifferentModule,
                        Returns);
    }

    return M.getFunction("__peekfs_module_registrator");
  }

  /**
   * Returns the compiled static library module, from which the global introspection
   * struct types are taken.
   *
   * @param ctx The LLVMContext struct used for compilation
   * @param path The path to the static library
   *
   * @return A unique_ptr to the compiled module, or nullptr on failure
   */
  std::unique_ptr<Module> getStaticLib(LLVMContext &ctx, std::string &path) {
    SMDiagnostic err;

    std::unique_ptr<llvm::Module> staticLib = parseIRFile(path, err, ctx);

    if(!staticLib) {
      errs() << "Error parsing static lib\n";
      err.print("staticlib", errs());

      return nullptr;
    } else {
      return staticLib;
    }
  }

  /**
   * Returns the constant initializers used to statically initialize each global introspection
   * entry for this module.
   *
   * @param M The module to generate initializers for
   * @param staticLibEntry The entry global in the static library, for templating
   *
   * @return A std::vector containing initializers for each global entry
   */
  std::vector<Constant *> getGlobalEntryConstants(Module &M, GlobalVariable* staticLibEntry) {
    std::vector<Constant *> constantMap = std::vector<Constant *>();

    for(auto &global : M.getGlobalList()) {
      if(global.getName().startswith(".str") || global.getName().startswith("llvm.")) {
        continue;
      }

      std::vector<Constant *> entryInitializers = {
        createStringConst(M, "__introspection_entry_name", global.getName()),
        ConstantInt::get(Type::getIntNTy(M.getContext(), GET_TYPE_BITS(size_t)), APInt(GET_TYPE_BITS(size_t), M.getDataLayout().getTypeAllocSize(global.getType()), false)),
        ConstantInt::get(Type::getIntNTy(M.getContext(), GET_TYPE_BITS(int)), APInt(GET_TYPE_BITS(int), 0, false)),
        &global
      };

      Constant* entryInitializer = ConstantStruct::get((StructType*)staticLibEntry->getValueType(), ArrayRef<Constant*>(entryInitializers));

      constantMap.push_back(entryInitializer);
    }

    return constantMap;
  }

  /**
   * Creates the global array of introspection entry structs and adds their static initializers.
   *
   * @param M The module to create the array for
   * @param modUniqueName A unique name for this compilation module. Must be globally unique in the
   *                      sense that the final linked program must not contain module name clashes
   * @param staticLib The static library, used for templating
   * @param numEntries A reference where the total amount of entries will be placed by this function
   *
   * @return The created global array
   */
  GlobalVariable *createModuleEntries(Module &M, Twine &modUniqueName, Module *staticLib, size_t &numEntries) {
    GlobalVariable* staticLibGlobalEntryVar = staticLib->getNamedGlobal("entries");

    std::vector<Constant *> entryConstants = getGlobalEntryConstants(M, staticLibGlobalEntryVar);

    GlobalVariable* globalEntry = new GlobalVariable(M,
      ArrayType::get(staticLibGlobalEntryVar->getValueType(), entryConstants.size()),
      true,
      GlobalValue::LinkageTypes::ExternalLinkage,
      nullptr,
      Twine("__introspection_mod_entries.").concat(modUniqueName)
    );

    globalEntry->setAlignment(MaybeAlign(staticLibGlobalEntryVar->getAlignment()));
    globalEntry->setInitializer(ConstantArray::get((ArrayType*) globalEntry->getValueType(), entryConstants));
    globalEntry->setSection(GLOBALS_INTROSPECTION_SECTION_NAME);

    numEntries = entryConstants.size();

    return globalEntry;
  }

  /**
   * Creates the module introspection header for this module. The header contains magic bytes for easy memory searching,
   * and metadata like the module name, number of introspection entries and the pointer to the entries array.
   *
   * @param M The module to create the header for
   * @param modUniqueName A unique name for this compilation module. Must be globally unique in the
   *                      sense that the final linked program must not contain module name clashes
   * @param staticLib The static library, used for templating
   * @param moduleEntries The global referencing the array of entries
   * @param numEntries The amount of entries in the global array
   */
  GlobalVariable *createModuleHeader(Module &M, Twine &modUniqueName, Module *staticLib, GlobalVariable* moduleEntries, size_t numEntries) {
    GlobalVariable* staticLibModEntryVar = staticLib->getNamedGlobal("module");

    GlobalVariable* moduleEntry = new GlobalVariable(M,
      staticLibModEntryVar->getValueType(),
      true,
      GlobalValue::LinkageTypes::ExternalLinkage,
      nullptr,
      Twine("__introspection_mod_hdr.").concat(modUniqueName)
    );

    moduleEntry->setAlignment(MaybeAlign(staticLibModEntryVar->getAlignment()));

    const std::string magic_bytes_str = "_ISDATA_MOD_HDR_";
    const std::vector<char> magic_bytes(magic_bytes_str.begin(), magic_bytes_str.end());

    Constant *magic_init = ConstantDataArray::get(M.getContext(), ArrayRef<char>(magic_bytes));

    std::vector<Constant *> initializers = {
      magic_init,
      createStringConst(M, "__introspection_mod_name", M.getName()),
      ConstantInt::get(Type::getIntNTy(M.getContext(), GET_TYPE_BITS(size_t)), APInt(GET_TYPE_BITS(size_t), numEntries, false)),
      moduleEntries,
    };

    moduleEntry->setInitializer(ConstantStruct::get((StructType*)moduleEntry->getValueType(), ArrayRef<Constant*>(initializers)));
    moduleEntry->setSection(GLOBALS_INTROSPECTION_SECTION_NAME);

    return moduleEntry;
  }

  GlobalVariable *createStringConst(Module &module, const Twine &name, StringRef str) {
    Constant* strData = ConstantDataArray::getString(module.getContext(), str);

    return new GlobalVariable(module,
      strData->getType(),
      true,
      GlobalValue::LinkageTypes::PrivateLinkage,
      strData,
      Twine(".str.isdata.").concat(name)
    );
  }

  /**
   * Creates a random ASCII identifier in buf of length len
   *
   * @param buf Buffer to place identifier in
   * @param len The length of the idenfitier
   */
  void getRandomAsciiIdentifier(char* buf, size_t len) {
    getRandomBytes(buf, len);

    for(size_t offset = 0; offset < len; offset++) {
      buf[offset] = (buf[offset] % (0x7a - 0x61)) + 0x61;
    }
  }

  /**
   * Searches for the path to the static introspection library, and returns it if found.
   * Otherwise, returns an empty std::string
   *
   * @return The path if found, empty string otherwise
   */
  std::string getStaticLibPath() {
    #ifdef __linux__
      char buf[1024] = {0};

      const ssize_t retval = readlink("/proc/self/exe", buf, 1023);

      if(retval == -1 || retval == 1023) {
        return std::string();
      }

      SmallString<1024> smallstr = SmallString<1024>(buf);

      sys::path::remove_filename(smallstr);

      return std::string(smallstr.c_str()).append("/../lib/libintrospection.bc");
    #else
      return std::string();
    #endif
  }
};
}  // end of anonymous namespace

char GlobalsIntrospectionPass::ID = 0;

ModulePass *llvm::createGlobalsIntrospectionPass() {
    return new GlobalsIntrospectionPass();
}

INITIALIZE_PASS_BEGIN(GlobalsIntrospectionPass, GLOBALS_INTROSPECTION_PASS_ARG,
                      GLOBALS_INTROSPECTION_PASS_NAME,
                      false, false)
// INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass) // Or whatever your Pass dependencies
INITIALIZE_PASS_END(GlobalsIntrospectionPass, GLOBALS_INTROSPECTION_PASS_ARG,
                    GLOBALS_INTROSPECTION_PASS_NAME,
                    false, false)
