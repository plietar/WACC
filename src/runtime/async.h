#ifndef ASYNC_H
#define ASYNC_H

#include <stdint.h>

#define YIELD(cmd, state) \
    do { \
        return (uint64_t)(uint32_t)(cmd) << 32 | (uint64_t)(uint32_t)(state); \
    } while (0)

#define EXIT(data) \
    do { \
        return ((uint64_t)(uint32_t)(data)) << 32; \
    } while (0)

typedef struct wacc_sock wacc_sock;

typedef struct yield_cmd {
    enum {
        CMD_YIELD,
        CMD_SLEEP,
        CMD_POLL_READ,
        CMD_POLL_WRITE
    } type;

    union {
        uint64_t wakeup_time;
        wacc_sock *sock;
    };
} yield_cmd;

#endif
