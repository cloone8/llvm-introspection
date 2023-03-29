#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

#include "llvm/IR/LegacyPassManager.h"

using namespace llvm;

namespace {
struct Introspection : public ModulePass {
  static char ID;
  Introspection() : ModulePass(ID) {}

  bool runOnModule(Module &M) override {
    errs() << "Hello: ";
    errs().write_escaped(M.getName()) << '\n';
    return false;
  }
};
}

char Introspection::ID = 0;
static RegisterPass<Introspection> X("introspection", "Introspection Pass",
                             false /* Only looks at CFG */,
                             false /* Analysis Pass */);
