#include <unordered_map>

#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/Transforms/Instrumentation/GlobalsIntrospection.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DebugInfoMetadata.h"
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
#include "../../../static/introspection/isdata-headers/isdata_meta.h"

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
    resetClassMembers();

    std::string staticLibPath = getStaticLibPath();

    if(staticLibPath.empty()) {
      WithColor::warning(errs()).resetColor() << "could not find introspection library, so metadata will not be included\n";
      resetClassMembers();
      return false;
    }

    std::unique_ptr<Module> staticLib = getStaticLib(M.getContext(), staticLibPath);

    // Generate a random identifier for this module, to make sure that
    // modules with equal filenames but different paths don't give global variable
    // name clashes
    char random_id[11] = {0};

    getRandomAsciiIdentifier(random_id, 10);

    targetModule = &M;

    modUniqueName = Twine(M.getName()).concat(Twine(random_id)).str();

    if((staticLibGlobalEntryVar = staticLib->getNamedGlobal("entries")) == nullptr) {
      WithColor::warning(errs()).resetColor() << "could not find entries variable, so introspection metadata will not be included\n";
      resetClassMembers();
      return false;
    }

    if((staticLibStructDefEntry = staticLib->getNamedGlobal("structdef")) == nullptr) {
      WithColor::warning(errs()).resetColor() << "could not find structdef variable, so introspection metadata will not be included\n";
      resetClassMembers();
      return false;
    }

    if((staticLibStructFieldEntry = staticLib->getNamedGlobal("structfield")) == nullptr) {
      WithColor::warning(errs()).resetColor() << "could not find structfield variable, so introspection metadata will not be included\n";
      resetClassMembers();
      return false;
    }

    if((staticLibModEntryVar = staticLib->getNamedGlobal("module")) == nullptr) {
      WithColor::warning(errs()).resetColor() << "could not find module variable, so introspection metadata will not be included\n";
      resetClassMembers();
      return false;
    }

    finder.processModule(M);

    size_t numEntries;
    GlobalVariable* moduleEntries = createModuleEntries(staticLib.get(), numEntries);

    if(numEntries == 0) {
      resetClassMembers();
      return false; // No globals to introspect!
    }

    Function* registratorFunction = copyRegistratorFunction(staticLib.get());

    if(registratorFunction == nullptr) {
      WithColor::warning(errs()).resetColor() << "registrator function was not succesfully copied\n";
      resetClassMembers();
      return true;
    }

    registratorFunction->setLinkage(GlobalValue::LinkageTypes::LinkOnceAnyLinkage);

    GlobalVariable* moduleHeader = createModuleHeader(moduleEntries, numEntries);

    GlobalVariable* registerFile = createStringConst("register_file", PEEKFS_REGISTER_FILE);
    GlobalVariable* unregisterFile = createStringConst("unregister_file", PEEKFS_UNREGISTER_FILE);


    Function* peekfsCtor = getPeekfsInitFunction(Twine("__peekfs_init.").concat(modUniqueName), registerFile, moduleHeader, registratorFunction);
    Function* peekfsDtor = getPeekfsInitFunction(Twine("__peekfs_exit.").concat(modUniqueName), unregisterFile, moduleHeader, registratorFunction);

    appendToGlobalCtors(M, peekfsCtor, 0);
    appendToGlobalDtors(M, peekfsDtor, 0);

    resetClassMembers();

    return true;
  }

  private:
  Module* targetModule;
  DebugInfoFinder finder;
  std::unordered_map<StructType*, std::pair<GlobalVariable*, bool>> structDefMap;
  std::string modUniqueName;
  GlobalVariable* staticLibGlobalEntryVar;
  GlobalVariable* staticLibStructDefEntry;
  GlobalVariable* staticLibStructFieldEntry;
  GlobalVariable* staticLibModEntryVar;

  void resetClassMembers() {
    structDefMap = std::unordered_map<StructType*, std::pair<GlobalVariable*, bool>>();
    modUniqueName = std::string();
    staticLibGlobalEntryVar = nullptr;
    staticLibStructDefEntry = nullptr;
    staticLibStructFieldEntry = nullptr;
    staticLibModEntryVar = nullptr;
    targetModule = nullptr;
  }

  LLVMContext& getCtx() {
    return targetModule->getContext();
  }

  Function* getPeekfsInitFunction(Twine name, GlobalVariable* initFile, GlobalVariable* modHeader, Function* registratorFunction) {
    Function* introspectionConstructor = Function::createWithDefaultAttr(
      FunctionType::get(Type::getVoidTy(getCtx()), false),
      GlobalValue::LinkageTypes::ExternalLinkage,
      targetModule->getDataLayout().getProgramAddressSpace(),
      name,
      targetModule
    );

    introspectionConstructor->addFnAttr(Attribute::NoUnwind);
    introspectionConstructor->addFnAttr(Attribute::OptimizeNone);
    introspectionConstructor->addFnAttr(Attribute::NoInline);

    IRBuilder<> *Builder = new IRBuilder<>(BasicBlock::Create(getCtx(), "entry", introspectionConstructor));
    std::vector<Value *> args;
    args.push_back(initFile);
    args.push_back(modHeader);

    Builder->CreateCall(registratorFunction, args);
    Builder->CreateRetVoid();

    return introspectionConstructor;
  }

  Function* copyRegistratorFunction(Module* staticLib) {
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
          targetModule
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

    return targetModule->getFunction("__peekfs_module_registrator");
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

  GlobalVariable* getStructDefForType(StructType* structType, DICompositeType* structDbgInfo) {
    std::pair<GlobalVariable*, bool> cachedStructDefPair = structDefMap[structType];
    GlobalVariable* cachedStructDef = cachedStructDefPair.first;

    if(cachedStructDef) {
      return cachedStructDef;
    }

    bool haveDebugInfo = false;
    std::vector<DIDerivedType*> dbgMembers = std::vector<DIDerivedType*>();

    if(structDbgInfo) {
      if(getMembersFromComposite(structDbgInfo, dbgMembers, structType->elements().size())) {
        haveDebugInfo = true;
      }
    }

    const StructLayout* layout = targetModule->getDataLayout().getStructLayout(structType);

    std::string structName;

    if(haveDebugInfo && !structDbgInfo->getName().empty()) {
      structName = structDbgInfo->getName().str();
    } else if(!structType->isLiteral()) {
      structName = structType->getName().str();
    } else {
      char structUniqueID[10] = {0};
      getRandomAsciiIdentifier(structUniqueID, 10);
      structName = std::string(structUniqueID, 10);
    }

    std::vector<Constant*> structFieldInitializers = std::vector<Constant *>();
    uint64_t elementNum = 0;
    for(Type* structElement : structType->elements()) {
      DIDerivedType* memberDbg = haveDebugInfo ? dbgMembers[elementNum] : nullptr;

      std::string fieldDisplayName;

      if(haveDebugInfo && !memberDbg->getName().empty()) {
        fieldDisplayName = memberDbg->getName().str();
      } else {
        fieldDisplayName = Twine("@unknown_field_").concat(Twine(elementNum)).str();
      }

      Type* elemType = structElement;
      Twine fieldMetaName = Twine(structName).concat(Twine('.')).concat(fieldDisplayName);
      uint8_t flags = 0;
      uint64_t numElems = 1;
      uint64_t offset = layout->getElementOffsetInBits(elementNum);
      Constant* sizeOrDefInit;

      if(structElement->isArrayTy()) {
        numElems = structElement->getArrayNumElements();
        elemType = structElement->getArrayElementType();
      }

      if(elemType->isStructTy()) {
        flags |= ISDATA_SFFLAG_STRUCT;
        DICompositeType* nestedStructDbg = nullptr;

        if(haveDebugInfo && DICompositeType::classof(memberDbg->getBaseType())) {
          DICompositeType* maybeDbg = (DICompositeType*) memberDbg->getBaseType();

          if(numElems > 1) {
            if(maybeDbg->getBaseType() && DICompositeType::classof(maybeDbg->getBaseType())) {
              nestedStructDbg = (DICompositeType*) maybeDbg->getBaseType();
            }
          } else {
            nestedStructDbg = maybeDbg;
          }
        }

        sizeOrDefInit = ConstantExpr::getPtrToInt(getStructDefForType((StructType*) elemType, nestedStructDbg), Type::getInt64Ty(getCtx()));
      } else {
        sizeOrDefInit = ConstantInt::get(Type::getInt64Ty(getCtx()), APInt(64, targetModule->getDataLayout().getTypeAllocSize(elemType).getFixedSize(), false));
      }

      std::vector<Constant *> isdataStructFieldFields = {
        ConstantInt::get(Type::getInt8Ty(getCtx()), APInt(8, flags, false)),
        ConstantInt::get(Type::getInt16Ty(getCtx()), APInt(16, fieldDisplayName.length() + 1, false)),
        createStringConst(fieldMetaName.concat(Twine(".name")), fieldDisplayName),
        ConstantInt::get(Type::getInt64Ty(getCtx()), APInt(64, offset, false)),
        sizeOrDefInit,
        ConstantInt::get(Type::getInt64Ty(getCtx()), APInt(64, numElems, false)),
      };

      Constant* isdataStructField = ConstantStruct::get((StructType*)staticLibStructFieldEntry->getValueType(), ArrayRef<Constant*>(isdataStructFieldFields));

      structFieldInitializers.push_back(isdataStructField);
      elementNum++;
    }

    GlobalVariable* structFields = new GlobalVariable(*targetModule,
        ArrayType::get(staticLibStructFieldEntry->getValueType(), structFieldInitializers.size()),
        true,
        GlobalValue::LinkageTypes::ExternalLinkage,
        nullptr,
        Twine(structName).concat(Twine(".structfields.")).concat(modUniqueName)
      );

    structFields->setAlignment(MaybeAlign(staticLibStructFieldEntry->getAlignment()));
    structFields->setInitializer(ConstantArray::get((ArrayType*) structFields->getValueType(), structFieldInitializers));
    structFields->setSection(GLOBALS_INTROSPECTION_SECTION_NAME);

    GlobalVariable* structDef = new GlobalVariable(*targetModule,
      staticLibStructDefEntry->getValueType(),
      true,
      GlobalValue::LinkageTypes::ExternalLinkage,
      nullptr,
      Twine(structName).concat(Twine(".structdef.")).concat(modUniqueName)
    );

    structDef->setAlignment(MaybeAlign(staticLibStructDefEntry->getAlignment()));
    structDef->setSection(GLOBALS_INTROSPECTION_SECTION_NAME);

    uint8_t flags = 0;

    if(structType->isPacked()) {
      flags |= ISDATA_SDFLAG_PACKED;
    }

    std::vector<Constant *> initializers = {
      ConstantInt::get(Type::getInt8Ty(getCtx()), APInt(8, flags, false)),
      ConstantInt::get(Type::getInt64Ty(getCtx()), APInt(64, layout->getSizeInBytes(), false)),
      ConstantInt::get(Type::getInt64Ty(getCtx()), APInt(64, structFieldInitializers.size(), false)),
      structFields
    };

    Constant* structDefInit = ConstantStruct::get((StructType*)structDef->getValueType(), initializers);

    structDef->setInitializer(structDefInit);

    structDefMap[structType] = std::pair<GlobalVariable*, bool>(structDef, haveDebugInfo);

    return structDef;
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
  std::vector<Constant *> getGlobalEntryConstants() {
    std::vector<Constant *> constantMap = std::vector<Constant *>();

    for(auto &global : targetModule->getGlobalList()) {
      if(global.getName().startswith("llvm.")) {
        continue;
      }

      if(global.getName().startswith(".str")) {
        continue;
      }

      if(global.hasSection() && global.getSection().startswith(GLOBALS_INTROSPECTION_SECTION_NAME)) {
        continue;
      }

      uint32_t flags = 0;
      uint64_t numElems = 1;
      Type* elemType = global.getValueType();

      if(elemType->isArrayTy()) {
        numElems = elemType->getArrayNumElements();
        elemType = elemType->getArrayElementType();
      }

      if(elemType->isPointerTy()) {
        flags |= ISDATA_EFLAG_PTR;
      }

      Constant* sizeOrDefInit;
      if(elemType->isStructTy()) {
        flags |= ISDATA_EFLAG_STRUCT;

        DICompositeType* structDbgInfo = nullptr;
        SmallVector<DIGlobalVariableExpression *, 1> gvExpressions;

        global.getDebugInfo(gvExpressions);

        for(DIGlobalVariableExpression* gvExpr : gvExpressions) {
          DICompositeType* debugInfoFromExpr = getStructDbgInfoFromGV(gvExpr, numElems > 1);

          if(debugInfoFromExpr) {
            structDbgInfo = debugInfoFromExpr;
            break;
          }
        }

        sizeOrDefInit = ConstantExpr::getPtrToInt(getStructDefForType((StructType*) elemType, structDbgInfo), Type::getInt64Ty(getCtx()));
      } else {
        sizeOrDefInit = ConstantInt::get(Type::getInt64Ty(getCtx()), APInt(64, targetModule->getDataLayout().getTypeAllocSize(elemType).getFixedSize(), false));
      }

      std::vector<Constant *> entryInitializers = {
        ConstantInt::get(Type::getInt16Ty(getCtx()), APInt(16, global.getName().size() + 1, false)),
        ConstantInt::get(Type::getInt32Ty(getCtx()), APInt(32, flags, false)),
        createStringConst("__introspection_entry_name", global.getName()),
        &global,
        sizeOrDefInit,
        ConstantInt::get(Type::getInt64Ty(getCtx()), APInt(64, numElems, false))
      };

      Constant* entryInitializer = ConstantStruct::get((StructType*)staticLibGlobalEntryVar->getValueType(), ArrayRef<Constant*>(entryInitializers));

      constantMap.push_back(entryInitializer);
    }

    return constantMap;
  }

  DICompositeType* getStructDbgInfoFromGV(DIGlobalVariableExpression* gvExpr, bool isArray) {
    bool hasType = gvExpr->getVariable() &&
                   gvExpr->getVariable()->getType() &&
                   DICompositeType::classof(gvExpr->getVariable()->getType());

    if(hasType) {
      DICompositeType* maybeDbg = (DICompositeType*) gvExpr->getVariable()->getType();

      if(isArray) {
        if(maybeDbg->getBaseType() && DICompositeType::classof(maybeDbg->getBaseType())) {
          return (DICompositeType*) maybeDbg->getBaseType();
        }
      } else {
        return maybeDbg;
      }
    }

    return nullptr;
  }

  bool getMembersFromComposite(DICompositeType* comp, std::vector<DIDerivedType*> &members, unsigned int expected_length) {
    if(comp->getElements().size() != expected_length) {
      return false;
    }

    for(DINode* member : comp->getElements()) {
      if(!DIDerivedType::classof(member)) {
        return false;
      }

      DIDerivedType* asDerived = (DIDerivedType*) member;

      if(asDerived->getBaseType() == nullptr) {
        return false;
      }

      members.push_back(asDerived);
    }

    return true;
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
  GlobalVariable *createModuleEntries(Module *staticLib, size_t &numEntries) {
    std::vector<Constant *> entryConstants = getGlobalEntryConstants();
    numEntries = entryConstants.size();

    if(numEntries > 0) {
      GlobalVariable* globalEntry = new GlobalVariable(*targetModule,
        ArrayType::get(staticLibGlobalEntryVar->getValueType(), entryConstants.size()),
        true,
        GlobalValue::LinkageTypes::ExternalLinkage,
        nullptr,
        Twine("__introspection_mod_entries.").concat(modUniqueName)
      );

      globalEntry->setAlignment(MaybeAlign(staticLibGlobalEntryVar->getAlignment()));
      globalEntry->setInitializer(ConstantArray::get((ArrayType*) globalEntry->getValueType(), entryConstants));
      globalEntry->setSection(GLOBALS_INTROSPECTION_SECTION_NAME);

      return globalEntry;
    } else {
      return nullptr;
    }
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
  GlobalVariable *createModuleHeader(GlobalVariable* moduleEntries, size_t numEntries) {
    GlobalVariable* moduleEntry = new GlobalVariable(*targetModule,
      staticLibModEntryVar->getValueType(),
      true,
      GlobalValue::LinkageTypes::ExternalLinkage,
      nullptr,
      Twine("__introspection_mod_hdr.").concat(modUniqueName)
    );

    moduleEntry->setAlignment(MaybeAlign(staticLibModEntryVar->getAlignment()));

    DEFINE_ISDATA_MAGIC_BYTES(magic_bytes_raw);
    const std::vector<uint8_t> magic_bytes_vec(magic_bytes_raw, magic_bytes_raw + ISDATA_MAGIC_BYTES_LEN);

    assert(magic_bytes_vec.size() == ISDATA_MAGIC_BYTES_LEN);

    Constant *magic_init = ConstantDataArray::get(getCtx(), ArrayRef<uint8_t>(magic_bytes_vec));

    std::vector<Constant *> initializers = {
      magic_init,
      ConstantInt::get(Type::getInt16Ty(getCtx()), APInt(16, ISDATA_VERSION, false)),
      ConstantInt::get(Type::getInt16Ty(getCtx()), APInt(16, targetModule->getName().size() + 1, false)),
      createStringConst("__introspection_mod_name", targetModule->getName()),
      ConstantInt::get(Type::getInt64Ty(getCtx()), APInt(64, numEntries, false)),
      moduleEntries,
    };

    Constant* moduleInit = ConstantStruct::get((StructType*)moduleEntry->getValueType(), ArrayRef<Constant*>(initializers));

    moduleEntry->setInitializer(moduleInit);
    moduleEntry->setSection(GLOBALS_INTROSPECTION_SECTION_NAME);

    return moduleEntry;
  }

  GlobalVariable *createStringConst(const Twine &name, StringRef str) {
    Constant* strData = ConstantDataArray::getString(getCtx(), str);

    GlobalVariable* toRet = new GlobalVariable(*targetModule,
      strData->getType(),
      true,
      GlobalValue::LinkageTypes::PrivateLinkage,
      strData,
      Twine(".str.isdata.").concat(name)
    );

    toRet->setSection(GLOBALS_INTROSPECTION_STR_SECTION_NAME);

    return toRet;
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
