#ifndef NETWORK_H
#define NETWORK_H

#include <stdint.h>

typedef struct list_elem *list_head;
typedef struct wacc_string wacc_string;

typedef struct wacc_sock {
    int fd;

    list_head recv_list;
    list_head send_list;
} wacc_sock;

wacc_sock *wacc_socket();
void wacc_bind(wacc_sock *sock, uint32_t port);
void wacc_listen(wacc_sock *sock);
uint64_t wacc_accept(uint32_t _state, wacc_sock *sock);
uint64_t wacc_recv(uint32_t _state, wacc_sock *sock);
uint64_t wacc_send(uint32_t _state, wacc_sock *sock, wacc_string *buffer);

#endif

