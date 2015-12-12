#include "list.h"
#include "task.h"
#include "wacc.h"
#include "async.h"

#include <stdint.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>

extern uint64_t wacc_main(uint32_t, uint32_t);

wacc_task *ready_tasks = NULL;
wacc_task *sleep_tasks = NULL;

uint64_t millis() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

void start_task(const char *name, task_entry entry, uint32_t argument) {
    wacc_task *task = task_create(name, entry, argument);
    list_insert(&ready_tasks, task);
}

void wacc_fire(task_entry entry, wacc_string *name, uint32_t argument) {
    start_task(name->data, entry, argument);
}

int main() {
    start_task("main", wacc_main, 0);

    while (1) {
        for (wacc_task *task = ready_tasks; task != NULL; ) {
            uint32_t ret  = task_execute(task);
            uint16_t cmd  = (ret >> 16) & 0xFFFF;
            uint16_t data = ret & 0xFFFF;

            wacc_task *next = task->next;

            if (task->state == 0) {
                list_remove(&ready_tasks, task);
                task_destroy(task);
            } else {
                switch (cmd) {
                    case CMD_YIELD:
                        break;

                    case CMD_SLEEP:
                        task->wakeup_time = millis() + data;
                        list_remove(&ready_tasks, task);
                        list_insert(&sleep_tasks, task);
                        break;

                    // TODO: use epoll to wait for the fd to be ready
                    case CMD_POLL_READ:
                        break;

                    case CMD_POLL_WRITE:
                        break;
                }
            }

            task = next;
        }

        for (wacc_task *task = sleep_tasks; task != NULL;) {
            wacc_task *next = task->next;

            if (task->wakeup_time < millis()) {
                list_remove(&sleep_tasks, task);
                list_insert(&ready_tasks, task);
            }

            task = next;
        }
    }
}

