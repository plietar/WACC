#include "heap.h"

#include <stdlib.h>
#include <stdio.h>

#define HEAP_DEFAULT_CAPACITY 16

#define LEFT_CHILD(i)   (((i) * 2) + 1)
#define RIGHT_CHILD(i)  (((i) * 2) + 2)
#define PARENT_ENTRY(i) (((i) - 1) / 2)

heap *heap_create() {
    heap *h = malloc(sizeof *h);
    h->size = 0;
    h->capacity = HEAP_DEFAULT_CAPACITY;
    h->entries = malloc(sizeof(heap_entry) * h->capacity);

    return h;
}

static void heap_swap(heap *h, uint32_t left, uint32_t right) {
    uint32_t key = h->entries[left].key;
    wacc_task *task = h->entries[left].task;

    h->entries[left].key = h->entries[right].key;
    h->entries[left].task = h->entries[right].task;

    h->entries[right].key = key;
    h->entries[right].task = task;
}

void heap_insert(heap *h, uint64_t key, wacc_task *task) {
    if (h->size == h->capacity) {
        h->capacity *= 2;
        h->entries = realloc(h->entries, sizeof(heap_entry) * h->capacity);
    }

    h->size += 1;

    uint32_t idx = h->size - 1;
    h->entries[idx].key = key;
    h->entries[idx].task = task;

    while (idx > 0 && h->entries[PARENT_ENTRY(idx)].key > h->entries[idx].key) {
        heap_swap(h, idx, PARENT_ENTRY(idx));
        idx = PARENT_ENTRY(idx);
    }
}

bool heap_peek(heap *h, uint64_t *keyp, wacc_task **taskp) {
    uint64_t key = 0;
    wacc_task *task = NULL;
    bool ret = false;

    if (h->size > 0) {
        key = h->entries[0].key;
        task = h->entries[0].task;
        ret = true;
    }

    if (keyp != NULL) {
        *keyp = key;
    }

    if (taskp != NULL) {
        *taskp = task;
    }

    return ret;
}

bool heap_pop(heap *h, uint64_t *keyp, wacc_task **taskp) {
    uint64_t key = 0;
    wacc_task *task = NULL;
    bool ret = false;

    if (h->size > 0) {
        key = h->entries[0].key;
        task = h->entries[0].task;
        ret = true;

        h->size -= 1;

        if (h->size > 0) {
            heap_swap(h, 0, h->size);

            uint32_t idx = 0;
            while (LEFT_CHILD(idx) < h->size) {
                uint32_t left = LEFT_CHILD(idx);
                uint32_t right = RIGHT_CHILD(idx);

                uint32_t child;
                if (right < h->size && h->entries[right].key < h->entries[left].key) {
                    child = right;
                } else {
                    child = left;
                }

                if (h->entries[child].key < h->entries[idx].key) {
                    heap_swap(h, child, idx);
                    idx = child;
                } else {
                    break;
                }
            }
        }
    }

    if (keyp != NULL) {
        *keyp = key;
    }

    if (taskp != NULL) {
        *taskp = task;
    }

    return ret;
}

void heap_destroy(heap *h);
