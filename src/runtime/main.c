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


uint32_t task_count = 0;
list_head ready_tasks = NULL;
list_head all_tasks = NULL;
heap *sleep_tasks;
int epoll_fd;

uint64_t millis() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

void start_task(task_entry entry, uint32_t argument) {
    wacc_task *task = task_create(entry, argument);
    list_insert(&all_tasks, &task->all_list);
    list_insert(&ready_tasks, &task->current_list);

    task_count += 1;
}

void kill_task(wacc_task *task) {
    list_remove(&all_tasks, &task->all_list);
    task_destroy(task);
    task_count -= 1;
}

void wacc_fire(task_entry entry, uint32_t argument) {
    start_task(entry, argument);
}

void task_pause(wacc_task *task, list_head *new_list) {
    list_remove(&ready_tasks, &task->current_list);
    list_insert(new_list, &task->current_list);
}

void task_wakeup(wacc_task *task, list_head *old_list) {
    list_remove(old_list, &task->current_list);
    list_insert(&ready_tasks, &task->current_list);
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
    mallopt(M_CHECK_ACTION, 2);
    signal(SIGABRT, throw_double_free);

    epoll_fd = epoll_create(1);
    sleep_tasks = heap_create();

    start_task(wacc_main, 0);

    for (;;) {
        for (list_elem *elem = ready_tasks; elem != NULL; ) {
            wacc_task *task = list_get(elem, wacc_task, current_list);
            list_elem *next = elem->next;

            yield_cmd *cmd = task_execute(task);

            if (task->state == 0) {
                list_remove(&ready_tasks, elem);
                kill_task(task);
            } else {
                switch (cmd->type) {
                    case CMD_YIELD:
                        break;

                    case CMD_SLEEP:
                        list_remove(&ready_tasks, elem);
                        heap_insert(sleep_tasks, cmd->wakeup_time, task);
                        break;

                    case CMD_WAIT:
                        task_pause(task, cmd->wait_list);
                        break;

                    case CMD_POLL_READ:
                        task_pause(task, &cmd->sock->recv_list);
                        update_socket(cmd->sock);
                        break;

                    case CMD_POLL_WRITE:
                        task_pause(task, &cmd->sock->send_list);
                        update_socket(cmd->sock);
                        break;
                }
            }

            elem = next;
        }

        if (task_count == 0) {
            break;
        }

        int timeout = 0;
        if (list_empty(&ready_tasks)) {
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
                for (list_elem *elem = sock->recv_list; elem != NULL; ) {
                    wacc_task *task = list_get(elem, wacc_task, current_list);
                    list_elem *next = elem->next;

                    task_wakeup(task, &sock->recv_list);

                    elem = next;
                }
            }

            if (evs[i].events & EPOLLOUT) {
                for (list_elem *elem = sock->send_list; elem != NULL; ) {
                    wacc_task *task = list_get(elem, wacc_task, current_list);
                    list_elem *next = elem->next;

                    task_wakeup(task, &sock->send_list);

                    elem = next;
                }
            }
        }

        uint64_t now = millis();
        uint64_t next_wakeup;
        while (heap_peek(sleep_tasks, &next_wakeup, NULL) && next_wakeup <= now) {
            wacc_task *task;
            heap_pop(sleep_tasks, NULL, &task);
            list_insert(&ready_tasks, &task->current_list);
        }
    }
}

