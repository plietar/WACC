#ifndef LIST_H
#define LIST_H

#include <stdbool.h>

typedef struct wacc_task wacc_task;

bool list_empty(wacc_task *list);
void list_insert(wacc_task **list, wacc_task *task);
void list_remove(wacc_task **list, wacc_task *task);

#endif

