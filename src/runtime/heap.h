#ifndef HEAP_H
#define HEAP_H

#include <stdbool.h>
#include <stdint.h>

typedef struct wacc_task wacc_task;

typedef struct {
    uint64_t key;
    wacc_task *task;
} heap_entry;

typedef struct {
    uint32_t size;
    uint32_t capacity;

    heap_entry *entries;
} heap;

heap *heap_create();
void heap_insert(heap *h, uint64_t key, wacc_task *task);
bool heap_peek(heap *h, uint64_t *keyp, wacc_task **taskp);
bool heap_pop(heap *h, uint64_t *keyp, wacc_task **taskp);
void heap_destroy(heap *h);

#endif
