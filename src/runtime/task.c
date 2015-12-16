#include "task.h"
#include "async.h"

#include <stdlib.h>

wacc_task *task_create(const char *name, task_entry entry, uint32_t argument) {
    wacc_task *task = calloc(1, sizeof(wacc_task));
    task->name = name;
    task->entry = entry;
    task->state = 0;
    task->argument = argument;
    return task;
}

void task_destroy(wacc_task *task) {
    free(task);
}

yield_cmd *task_execute(wacc_task *task) {
    uint32_t argument = task->argument;
    if (task->state == 0) {
        task->argument = 0;
    }

    uint64_t ret = task->entry(task->state, argument);
    task->state = ret & 0xFFFFFFFF;
    return (yield_cmd *)(uint32_t)((ret >> 32) & 0xFFFFFFFF);
}

