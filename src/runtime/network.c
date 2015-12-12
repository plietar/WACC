#include "network.h"
#include "task.h"
#include "wacc.h"
#include "async.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <sys/epoll.h>

void register_socket(wacc_sock *sock);

static wacc_sock *create_socket(int fd) {
    wacc_sock *sock = malloc(sizeof *sock);
    sock->fd = fd;
    sock->recv_list = NULL;
    sock->send_list = NULL;

    register_socket(sock);
    return sock;
}
wacc_sock *wacc_socket() {
    int fd = socket(AF_INET, SOCK_STREAM | SOCK_NONBLOCK, 0);
    if (fd < 0) {
        perror("socket");
        exit(1);
    }

    int flag = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &flag, sizeof(int));

    return create_socket(fd);
}

void wacc_bind(wacc_sock *sock, uint32_t port) {
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = INADDR_ANY;

    int ret = bind(sock->fd, (struct sockaddr *)&addr, sizeof(addr));
    if (ret < 0) {
        perror("bind");
        exit(1);
    }
}

void wacc_listen(wacc_sock *sock) {
    int ret = listen(sock->fd, 5);
    if (ret < 0) {
        perror("bind");
        exit(1);
    }
}

uint64_t wacc_accept(uint32_t _state, wacc_sock *sock) {
    if (_state != 0) {
        sock = (wacc_sock*)_state;
    }

    for (;;) {
        int ret = accept4(sock->fd, NULL, NULL, SOCK_NONBLOCK);
        if (ret < 0) {
            if (errno == EINTR) {
                continue;
            } else if (errno == EAGAIN) {
                yield_cmd *cmd = malloc(sizeof *cmd);
                cmd->type = CMD_POLL_READ;
                cmd->sock = sock;
                YIELD(cmd, sock);
            } else {
                perror("accept");
                exit(1);
            }
        }

        EXIT(create_socket(ret));
    }
}

struct recv_state {
    wacc_sock *sock;
    wacc_string *buffer;
};

uint64_t wacc_recv(uint32_t _state, wacc_sock *sock) {
    struct recv_state *state;
    if (_state == 0) {
        state = malloc(sizeof *state);
        state->sock = sock;
        state->buffer = malloc(sizeof(wacc_string) + 128);
        state->buffer->length = 0;
    } else {
        state = (struct recv_state*)_state;
    }

    for (;;) {
        char c;
        int ret = recv(state->sock->fd, (void*)&c, 1, 0);

        if (ret < 0) {
            if (errno == EINTR) {
                continue;
            } else if (errno == EAGAIN) {
                yield_cmd *cmd = malloc(sizeof *cmd);
                cmd->type = CMD_POLL_READ;
                cmd->sock = sock;
                YIELD(cmd, state);
            } else {
                perror("recv");
                exit(1);
            }
        } else if (ret == 0) {
            wacc_string *ret = state->buffer;
            free(state);
            EXIT(ret);
        } else {
            state->buffer->data[state->buffer->length] = c;
            state->buffer->length ++;

            if (c == '\n') {
                wacc_string *ret = state->buffer;
                free(state);
                EXIT(ret);
            }
        }
    }
}

struct send_state {
    wacc_sock *sock;
    wacc_string *buffer;
    size_t total;
};

uint64_t wacc_send(uint32_t _state, wacc_sock *sock, wacc_string *buffer) {
    struct send_state *state;
    if (_state == 0) {
        state = malloc(sizeof *state);
        state->sock = sock;
        state->buffer = buffer;
        state->total = 0;
    } else {
        state = (struct send_state*)_state;
    }

    for (;;) {
        ssize_t ret = send(state->sock->fd,
                           state->buffer->data   + state->total,
                           state->buffer->length - state->total,
                           0);

        if (ret < 0) {
            if (errno == EINTR) {
                continue;
            } else if (errno == EAGAIN) {
                yield_cmd *cmd = malloc(sizeof *cmd);
                cmd->type = CMD_POLL_WRITE;
                cmd->sock = sock;
                YIELD(cmd, state);
            } else {
                perror("recv");
                exit(1);
            }
        } else {
            state->total += ret;
            if (state->total < state->buffer->length) {
                yield_cmd *cmd = malloc(sizeof *cmd);
                cmd->type = CMD_POLL_WRITE;
                cmd->sock = sock;
                YIELD(cmd, state);
            } else {
                EXIT(0);
            }
        }
    }
}

