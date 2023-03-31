#ifndef LLVM_TRANSFORMS_INSTRUMENTATION_GLOBALS_INTROSPECTION_H
#define LLVM_TRANSFORMS_INSTRUMENTATION_GLOBALS_INTROSPECTION_H

#define GLOBALS_INTROSPECTION_PASS_ARG ("globals-introspection")
#define GLOBALS_INTROSPECTION_PASS_NAME ("Pass that adds global introspection data")

namespace llvm {
    class ModulePass;

    ModulePass *createGlobalsIntrospectionPass();
}

#endif
