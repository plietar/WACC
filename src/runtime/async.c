#include "async.h"

uint64_t wacc_yield(uint32_t state) {
    if (state == 0) {
        YIELD(CMD_YIELD, 0, 1);
    } else {
        EXIT(0);
    }
}

uint64_t wacc_sleep_ms(uint32_t state, uint32_t delay) {
    if (state == 0) {
        YIELD(CMD_SLEEP, delay, 1);
    } else {
        EXIT(0);
    }
}
