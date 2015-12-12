#ifndef ASYNC_H
#define ASYNC_H

#include <stdint.h>

#define YIELD(cmd, data, state) \
    do { \
        return (uint64_t)(cmd) << 48 | (uint64_t)(data) << 32 | (uint64_t)(uint32_t)(state); \
    } while (0)

#define EXIT(data) \
    do { \
        return ((uint64_t)(uint32_t)(data)) << 32; \
    } while (0)

enum {
    CMD_YIELD,
    CMD_SLEEP,
};

#endif
