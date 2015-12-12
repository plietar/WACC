#include "list.h"
#include "task.h"

#include <stddef.h>

void list_insert(wacc_task **list, wacc_task *task) {
    task->next = *list;
    task->prev = NULL;

    if (*list != NULL) {
        (*list)->prev = task;
    }

    *list = task;
}

void list_remove(wacc_task **list, wacc_task *task) {
    if (task->next != NULL) {
        task->next->prev = task->prev;
    }

    if (task->prev != NULL) {
        task->prev->next = task->next;
    }

    if (*list == task) {
        *list = task->next;
    }

    task->next = NULL;
    task->prev = NULL;
}

