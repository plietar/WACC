#define _POSIX_C_SOURCE 199309L

#include <stdint.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>

extern uint64_t wacc_main(uint32_t, uint32_t);

typedef uint64_t (*generator)(uint32_t, uint32_t);

struct coroutine {
    generator entry;
    const char *name;
    uint32_t state;
    uint32_t argument;
    uint64_t wakeup_time;

    struct coroutine *next;
    struct coroutine *prev;
};

struct wacc_string {
    uint32_t length;
    char data[];
};

enum {
    CMD_YIELD,
    CMD_SLEEP,
};

struct coroutine *ready_tasks = NULL;
struct coroutine *sleep_tasks = NULL;

void list_insert(struct coroutine **list, struct coroutine *task) {
    task->next = *list;
    task->prev = NULL;

    if (*list != NULL) {
        (*list)->prev = task;
    }

    *list = task;
}

void list_remove(struct coroutine **list, struct coroutine *task) {
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

void start_task(const char *name, generator entry, uint32_t argument) {
    struct coroutine *task = malloc(sizeof(struct coroutine));
    task->name = name;
    task->entry = entry;
    task->state = 0;
    task->argument = argument;
    task->wakeup_time = 0;

    list_insert(&ready_tasks, task);
}

void free_task(struct coroutine *task) {
    free(task);
}

#define YIELD(cmd, data, state) \
    do { \
        return ((uint64_t)(cmd) << 48 | (uint64_t)(data) << 32 | (state)); \
    } while (0)

#define EXIT(data) \
    do { \
        return ((uint64_t)(data) << 32); \
    } while (0)

uint64_t millis() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

uint64_t wacc_yield(uint32_t state) {
    if (state == 0) {
        YIELD(CMD_YIELD, 0, 1);
    } else {
        EXIT(0);
    }
}

void wacc_fire(generator entry, struct wacc_string *name, uint32_t argument) {
    start_task(name->data, entry, argument);
}

uint64_t wacc_sleep_ms(uint32_t state, uint32_t delay) {
    if (state == 0) {
        YIELD(CMD_SLEEP, delay, 1);
    } else {
        EXIT(0);
    }
}

int main() {
    start_task("main", wacc_main, 0);

    while (1) {
        struct coroutine *task = ready_tasks;

        while (task != NULL) {
            uint32_t argument = task->argument;
            if (task->state == 0) {
                task->argument = 0;
            }

            uint64_t ret = task->entry(task->state, argument);

            uint16_t cmd   = (ret >> 48) & 0xFFFF;
            uint16_t data  = (ret >> 32) & 0xFFFF;
            uint32_t state = ret & 0xFFFFFFFF;

            struct coroutine *next = task->next;

            if (state == 0) {
                free_task(task);
                list_remove(&ready_tasks, task);
            } else {
                task->state = state;

                switch (cmd) {
                    case CMD_YIELD:
                        break;

                    case CMD_SLEEP:
                        task->wakeup_time = millis() + data;
                        list_remove(&ready_tasks, task);
                        list_insert(&sleep_tasks, task);
                        break;
                }
            }

            task = next;
        }

        task = sleep_tasks;
        while (task != NULL) {
            struct coroutine *next = task->next;
            if (task->wakeup_time < millis()) {
                list_remove(&sleep_tasks, task);
                list_insert(&ready_tasks, task);
            }
            task = next;
        }
    }
}

