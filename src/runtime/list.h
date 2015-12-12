#ifndef LIST_H
#define LIST_H

typedef struct wacc_task wacc_task;

void list_insert(wacc_task **list, wacc_task *task);
void list_remove(wacc_task **list, wacc_task *task);

#endif

