#include "list.h"

bool list_empty(list_head *list) {
    return (*list) == NULL;
}

void list_insert(list_head *list, list_elem *elem) {
    elem->next = *list;
    elem->prev = NULL;

    if (*list != NULL) {
        (*list)->prev = elem;
    }

    *list = elem;
}

void list_remove(list_head *list, list_elem *elem) {
    if (elem->next != NULL) {
        elem->next->prev = elem->prev;
    }

    if (elem->prev != NULL) {
        elem->prev->next = elem->next;
    }

    if (*list == elem) {
        *list = elem->next;
    }

    elem->next = NULL;
    elem->prev = NULL;
}

