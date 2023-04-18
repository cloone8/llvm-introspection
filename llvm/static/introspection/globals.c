#include <stdlib.h>

struct __introspection_global_entry_t {
    char* name;
    size_t size;
    int as_str;
    void* addr;
};

struct __introspection_module_entry_t {
    char magic[16];
    char* name;
    size_t num_entries;
    struct __introspection_global_entry_t* entries;
};

struct __introspection_global_entry_t entries = {
    .name = NULL,
    .size = 0,
    .as_str = 0,
    .addr = NULL
};

struct __introspection_module_entry_t module = {
    .magic = {'_', 'I', 'S', 'D', 'A', 'T', 'A', '_', 'M', 'O', 'D', '_', 'H', 'D', 'R', '_'},
    .name = NULL,
    .num_entries = 0,
    .entries = &entries
};
