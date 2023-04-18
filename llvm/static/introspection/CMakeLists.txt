if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_custom_command(
        OUTPUT introspection.bc
        COMMAND ${CMAKE_C_COMPILER} -emit-llvm -c ${CMAKE_CURRENT_SOURCE_DIR}/globals.c
                        -o ${CMAKE_BINARY_DIR}/lib/libintrospection.bc
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/globals.c
        VERBATIM
    )

    add_custom_target(libintrospection ALL DEPENDS introspection.bc)

    install(FILES ${CMAKE_BINARY_DIR}/lib/libintrospection.bc DESTINATION lib)
else()
    message(WARNING "Will not compile static introspection library, as the compiler used is not clang and is thus not able to produce LLVM bitcode")
endif()
