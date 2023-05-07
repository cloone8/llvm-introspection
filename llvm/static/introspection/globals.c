#include <stdlib.h>

#ifdef __linux__
    #include <fcntl.h>
    #include <unistd.h>
#endif

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

void __peekfs_module_registrator(const char* path, const void* hdr)  {
#ifdef __linux__
    int fd = open(path, O_WRONLY);

    if(fd == -1) {
        return;
    }

    write(fd, &hdr, sizeof(void*));

    close(fd);
#endif
}
