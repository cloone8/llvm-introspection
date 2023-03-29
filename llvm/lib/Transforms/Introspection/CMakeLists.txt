add_llvm_library( LLVMIntrospection MODULE
  Introspection.cpp

  PLUGIN_TOOL
  opt
  )
