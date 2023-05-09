#ifndef LLVM_TRANSFORMS_INSTRUMENTATION_GLOBALS_INTROSPECTION_H
#define LLVM_TRANSFORMS_INSTRUMENTATION_GLOBALS_INTROSPECTION_H

#define GET_TYPE_BITS(t) (sizeof(t) * 8)

#define GLOBALS_INTROSPECTION_PASS_ARG ("globals-introspection")
#define GLOBALS_INTROSPECTION_PASS_NAME ("Pass that adds global introspection data")
#define GLOBALS_INTROSPECTION_SECTION_NAME (".isdata")
#define PEEKFS_REGISTER_FILE ("/proc/peek/register")
#define PEEKFS_UNREGISTER_FILE ("/proc/peek/unregister")

namespace llvm {
    class ModulePass;

    ModulePass *createGlobalsIntrospectionPass();
}

#endif
