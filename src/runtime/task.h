#ifndef TASK_H
#define TASK_H

#include "list.h"

#include <stdint.h>

typedef struct yield_cmd yield_cmd;

typedef uint64_t (*task_entry)(uint32_t state, uint32_t argument);

typedef struct wacc_task {
    task_entry entry;
    uint32_t state;
    uint32_t argument;

    list_elem current_list;
    list_elem all_list;
} wacc_task;

wacc_task *task_create(task_entry entry, uint32_t argument);
void task_destroy(wacc_task *task);
yield_cmd *task_execute(wacc_task *task);

#endif
