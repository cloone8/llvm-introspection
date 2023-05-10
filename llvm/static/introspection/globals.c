#include <stdlib.h>
#include <stdint.h>

#ifdef __linux__
    #include <fcntl.h>
    #include <unistd.h>
#endif

struct isdata_entry {
    uint16_t name_len;
    char* name;
    uint64_t size;
    uint32_t flags;
    void* addr;
};

struct isdata_module {
    uint8_t magic[16];
    uint16_t version;
    uint16_t name_len;
    char* name;
    uint64_t num_entries;
    struct isdata_entry* entries;
};

struct isdata_entry entries = {
    .name_len = 0,
    .name = NULL,
    .size = 0,
    .flags = 0,
    .addr = NULL
};

struct isdata_module module = {
    .magic = {'_', 'I', 'S', 'D', 'A', 'T', 'A', '_', 'M', 'O', 'D', '_', 'H', 'D', 'R', '_'},
    .version = 0,
    .name_len = 0,
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
