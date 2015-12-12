#include "async.h"

#include <stdlib.h>

uint64_t wacc_yield(uint32_t state) {
    if (state == 0) {
        yield_cmd *cmd = malloc(sizeof *cmd);
        cmd->type = CMD_YIELD;
        cmd->wakeup_time = 0;
        YIELD(cmd, 1);
    } else {
        EXIT(0);
    }
}

uint64_t wacc_sleep_ms(uint32_t state, uint32_t delay) {
    if (state == 0) {
        yield_cmd *cmd = malloc(sizeof *cmd);
        cmd->type = CMD_SLEEP;
        cmd->wakeup_time = delay;
        YIELD(cmd, 1);
    } else {
        EXIT(0);
    }
}
