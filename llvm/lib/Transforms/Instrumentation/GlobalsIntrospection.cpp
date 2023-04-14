#include "llvm/Transforms/Instrumentation/GlobalsIntrospection.h"
#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/MemoryBufferRef.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/IRReader/IRReader.h"

using namespace llvm;

namespace {
struct GlobalsIntrospectionPass : public ModulePass {
  static char ID;
  GlobalsIntrospectionPass() : ModulePass(ID) {
    initializeGlobalsIntrospectionPassPass(*PassRegistry::getPassRegistry());
  }

  bool runOnModule(Module &M) override {
    errs() << "Hello: ";
    errs().write_escaped(M.getName()) << '\n';

    std::unique_ptr<Module> staticLib = getStaticLib(M.getContext());

    size_t numEntries;

    GlobalVariable* moduleEntries = createModuleEntries(M, staticLib.get(), numEntries);
    createModuleHeader(M, staticLib.get(), moduleEntries, numEntries);

    return false;
  }

  private:

  std::unique_ptr<Module> getStaticLib(LLVMContext &ctx) {
    SMDiagnostic err;
    std::unique_ptr<llvm::Module> staticLib = parseIRFile("/home/cloone8/code/llvm-project/llvm/static/introspection/globals.bc", err, ctx);

    if(!staticLib) {
      errs() << "Error parsing static lib\n";
      err.print("staticlib", errs());

      return nullptr;
    } else {
      return staticLib;
    }
  }

  std::vector<Constant *> getGlobalEntryConstants(Module &M, GlobalVariable* staticLibEntry) {
    std::vector<Constant *> constantMap = std::vector<Constant *>();

    for(auto &global : M.getGlobalList()) {

      if(global.getName().startswith(".str")) {
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

  GlobalVariable *createModuleEntries(Module &M, Module *staticLib, size_t &numEntries) {
    GlobalVariable* staticLibGlobalEntryVar = staticLib->getNamedGlobal("entries");

    std::vector<Constant *> entryConstants = getGlobalEntryConstants(M, staticLibGlobalEntryVar);

    GlobalVariable* globalEntry = new GlobalVariable(M,
      ArrayType::get(staticLibGlobalEntryVar->getValueType(), entryConstants.size()),
      true,
      GlobalValue::LinkageTypes::ExternalLinkage,
      nullptr,
      "__introspection_mod_entries"
    );

    globalEntry->setAlignment(MaybeAlign(staticLibGlobalEntryVar->getAlignment()));
    globalEntry->setInitializer(ConstantArray::get((ArrayType*) globalEntry->getValueType(), entryConstants));
    globalEntry->setSection(GLOBALS_INTROSPECTION_SECTION_NAME);

    numEntries = entryConstants.size();

    return globalEntry;
  }

  GlobalVariable *createModuleHeader(Module &M, Module *staticLib, GlobalVariable* moduleEntries, size_t numEntries) {
    GlobalVariable* staticLibModEntryVar = staticLib->getNamedGlobal("module");

    GlobalVariable* moduleEntry = new GlobalVariable(M,
      staticLibModEntryVar->getValueType(),
      true,
      GlobalValue::LinkageTypes::ExternalLinkage,
      nullptr,
      "__introspection_mod_hdr"
    );

    moduleEntry->setAlignment(MaybeAlign(staticLibModEntryVar->getAlignment()));

    std::vector<Constant *> initializers = {
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
      Twine(".str.").concat(name)
    );
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
