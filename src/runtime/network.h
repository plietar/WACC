#ifndef NETWORK_H
#define NETWORK_H

#include <stdint.h>

typedef struct wacc_task wacc_task;
typedef struct wacc_string wacc_string;

typedef struct wacc_sock {
    int fd;

    wacc_task *recv_list;
    wacc_task *send_list;
} wacc_sock;

wacc_sock *wacc_socket();
void wacc_bind(wacc_sock *sock, uint32_t port);
void wacc_listen(wacc_sock *sock);
uint64_t wacc_accept(uint32_t _state, wacc_sock *sock);
uint64_t wacc_recv(uint32_t _state, wacc_sock *sock);
uint64_t wacc_send(uint32_t _state, wacc_sock *sock, wacc_string *buffer);

#endif

