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

uint32_t wacc_socket() {
    return socket(AF_INET, SOCK_STREAM, 0);
}

void wacc_bind(uint32_t fd, uint32_t port) {
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = INADDR_ANY;

    int ret = bind(fd, (struct sockaddr *)&addr, sizeof(addr));
    if (ret < 0) {
        perror("bind");
        exit(1);
    }
}

void wacc_listen(uint32_t fd) {
    int ret = listen(fd, 5);
    if (ret < 0) {
        perror("bind");
        exit(1);
    }
}

uint64_t wacc_accept(uint32_t state, uint32_t fd) {
    int ret = accept(fd, NULL, NULL);

    if (ret < 0) {
        perror("accept");
        exit(1);
    }

    EXIT(ret);
}

struct recv_state {
    uint32_t fd;
    wacc_string *buffer;
};

uint64_t wacc_recv(uint32_t s, uint32_t fd) {
    struct recv_state *state;
    if (s == 0) {
        state = malloc(sizeof(struct recv_state));
        state->fd = fd;
        state->buffer = malloc(sizeof(wacc_string) + 128);
        state->buffer->length = 0;
    } else {
        state = (struct recv_state*)s;
    }

    char c;
    int ret = recv(state->fd, (void*)&c, 1, 0);

    if (ret < 0) {
        perror("recv");
        exit(1);
    } else if (ret == 0 || c == '\n') {
        wacc_string *ret = state->buffer;
        free(state);
        EXIT(ret);
    } else {
        state->buffer->data[state->buffer->length] = c;
        state->buffer->length ++;

        YIELD(CMD_YIELD, 0, state);
    }
}

