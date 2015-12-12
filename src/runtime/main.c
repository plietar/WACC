#include "list.h"
#include "task.h"
#include "wacc.h"
#include "async.h"
#include "network.h"

#include <stdint.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <sys/epoll.h>

extern uint64_t wacc_main(uint32_t, uint32_t);

wacc_task *ready_tasks = NULL;
wacc_task *sleep_tasks = NULL;
int epoll_fd;

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

int main() {
    start_task("main", wacc_main, 0);

    epoll_fd = epoll_create(1);

    while (1) {
        for (wacc_task *task = ready_tasks; task != NULL; ) {
            yield_cmd *cmd = task_execute(task);

            wacc_task *next = task->next;

            if (task->state == 0) {
                list_remove(&ready_tasks, task);
                task_destroy(task);
            } else {
                switch (cmd->type) {
                    case CMD_YIELD:
                        break;

                    case CMD_SLEEP:
                        task->wakeup_time = millis() + cmd->wakeup_time;
                        list_remove(&ready_tasks, task);
                        list_insert(&sleep_tasks, task);
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

        struct epoll_event evs[8];
        int nev = epoll_wait(epoll_fd, evs, 8, 100);
        for (int i = 0; i < nev; i++) {
            wacc_sock *sock = evs[i].data.ptr;

            if (evs[i].events & EPOLLIN) {
                for (wacc_task *task = sock->recv_list; task != NULL;) {
                    wacc_task *next = task->next;

                    list_remove(&sock->recv_list, task);
                    list_insert(&ready_tasks, task);

                    task = next;
                }
            }

            if (evs[i].events & EPOLLOUT) {
                for (wacc_task *task = sock->send_list; task != NULL;) {
                    wacc_task *next = task->next;

                    list_remove(&sock->send_list, task);
                    list_insert(&ready_tasks, task);

                    task = next;
                }
            }
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

