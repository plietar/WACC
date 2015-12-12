#ifndef TASK_H
#define TASK_H

#include <stdint.h>

typedef uint64_t (*task_entry)(uint32_t state, uint32_t argument);

typedef struct wacc_task {
    task_entry entry;
    const char *name;
    uint32_t state;
    uint32_t argument;
    uint64_t wakeup_time;

    struct wacc_task *next;
    struct wacc_task *prev;
} wacc_task;

wacc_task *task_create(const char *name, task_entry entry, uint32_t argument);
void task_destroy(wacc_task *task);
uint32_t task_execute(wacc_task *task);

#endif
