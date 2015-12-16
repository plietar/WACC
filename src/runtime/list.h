#ifndef LIST_H
#define LIST_H

#include <stdbool.h>
#include <stddef.h>

typedef struct list_elem {
    struct list_elem *prev;
    struct list_elem *next;
} list_elem;

typedef list_elem *list_head;

bool list_empty(list_head *list);
void list_insert(list_head *list, list_elem *elem);
void list_remove(list_head *list, list_elem *elem);

#define list_get(ptr, type, member) \
    (type *)((void*)ptr - offsetof(type, member))

#endif

