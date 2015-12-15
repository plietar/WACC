#include "list.h"
#include "task.h"
#include "wacc.h"
#include "async.h"
#include "network.h"
#include "heap.h"

#include <stdint.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <sys/epoll.h>
#include <signal.h>
#include <malloc.h>

extern uint64_t wacc_main(uint32_t, uint32_t);

void GCInit();
void GCFree();

uint32_t task_count = 0;
wacc_task *ready_tasks = NULL;
heap *sleep_tasks;
int epoll_fd;

uint64_t millis() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

void start_task(const char *name, task_entry entry, uint32_t argument) {
    wacc_task *task = task_create(name, entry, argument);
    list_insert(&ready_tasks, task);

    task_count += 1;
}

void wacc_fire(task_entry entry, wacc_string *name, uint32_t argument) {
    start_task(name->data, entry, argument);
}

void task_wakeup(wacc_task *task, wacc_task **task_list) {
    list_remove(task_list, task);
    list_insert(&ready_tasks, task);
}

void register_socket(wacc_sock *sock) {
    struct epoll_event ev;
    ev.events = 0;
    ev.data.ptr = sock;

    int ret = epoll_ctl(epoll_fd, EPOLL_CTL_ADD, sock->fd, &ev);
    if (ret < 0) {
        perror("epoll_ctl ADD");
        exit(1);
    }
}

void update_socket(wacc_sock *sock) {
    struct epoll_event ev;
    ev.events = 0;
    if (sock->recv_list != NULL) {
        ev.events |= EPOLLIN;
    }
    if (sock->send_list != NULL) {
        ev.events |= EPOLLOUT;
    }
    ev.data.ptr = sock;

    int ret = epoll_ctl(epoll_fd, EPOLL_CTL_MOD, sock->fd, &ev);
    if (ret < 0) {
        perror("epoll_ctl MOD");
        exit(1);
    }
}

void throw_double_free() {
    printf("DoubleFreeError: pair freed twice\n");
}

int main() {
    GCInit();
    mallopt(M_CHECK_ACTION, 2);
    signal(SIGABRT, throw_double_free);

    epoll_fd = epoll_create(1);
    sleep_tasks = heap_create();

    start_task("main", wacc_main, 0);

    for (;;) {
        for (wacc_task *task = ready_tasks; task != NULL; ) {
            yield_cmd *cmd = task_execute(task);

            wacc_task *next = task->next;

            if (task->state == 0) {
                list_remove(&ready_tasks, task);
                task_destroy(task);
                task_count -= 1;
            } else {
                switch (cmd->type) {
                    case CMD_YIELD:
                        break;

                    case CMD_SLEEP:
                        list_remove(&ready_tasks, task);
                        heap_insert(sleep_tasks, cmd->wakeup_time, task);
                        break;

                    case CMD_WAIT:
                        list_remove(&ready_tasks, task);
                        list_insert(cmd->wait_list, task);
                        break;

                    case CMD_POLL_READ:
                        list_remove(&ready_tasks, task);
                        list_insert(&cmd->sock->recv_list, task);
                        update_socket(cmd->sock);
                        break;

                    case CMD_POLL_WRITE:
                        list_remove(&ready_tasks, task);
                        list_insert(&cmd->sock->send_list, task);
                        update_socket(cmd->sock);
                        break;
                }
            }

            task = next;
        }

        if (task_count == 0) {
            break;
        }

        int timeout = 0;
        if (list_empty(ready_tasks)) {
            uint64_t next_wakeup;
            if (heap_peek(sleep_tasks, &next_wakeup, NULL)) {
                uint64_t now = millis();
                if (next_wakeup > now) {
                    timeout = next_wakeup - now;
                } else {
                    timeout = 0;
                }
            } else {
                timeout = -1;
            }
        }

        struct epoll_event evs[8];
        int nev = epoll_wait(epoll_fd, evs, 8, timeout);
        for (int i = 0; i < nev; i++) {
            wacc_sock *sock = evs[i].data.ptr;

            if (evs[i].events & EPOLLIN) {
                for (wacc_task *task = sock->recv_list; task != NULL;) {
                    wacc_task *next = task->next;
                    task_wakeup(task, &sock->recv_list);

                    task = next;
                }
            }

            if (evs[i].events & EPOLLOUT) {
                for (wacc_task *task = sock->send_list; task != NULL;) {
                    wacc_task *next = task->next;
                    task_wakeup(task, &sock->send_list);

                    task = next;
                }
            }
        }

        uint64_t now = millis();
        uint64_t next_wakeup;
        while (heap_peek(sleep_tasks, &next_wakeup, NULL) && next_wakeup <= now) {
            wacc_task *task;
            heap_pop(sleep_tasks, NULL, &task);
            list_insert(&ready_tasks, task);
        }
    }
    GCFree();
}

