#ifndef WACC_H
#define WACC_H

#include <stdint.h>

typedef struct {
    uint32_t length;
    char data[];
} wacc_string;

#endif
