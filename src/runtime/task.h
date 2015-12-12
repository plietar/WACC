#ifndef TASK_H
#define TASK_H

#include <stdint.h>

typedef struct yield_cmd yield_cmd;

typedef uint64_t (*task_entry)(uint32_t state, uint32_t argument);

typedef struct wacc_task {
    task_entry entry;
    const char *name;
    uint32_t state;
    uint32_t argument;

    struct wacc_task *next;
    struct wacc_task *prev;
} wacc_task;

wacc_task *task_create(const char *name, task_entry entry, uint32_t argument);
void task_destroy(wacc_task *task);
yield_cmd *task_execute(wacc_task *task);

#endif
