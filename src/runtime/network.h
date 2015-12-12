#ifndef NETWORK_H
#define NETWORK_H

#include <stdint.h>

uint32_t wacc_socket();
void wacc_bind(uint32_t fd, uint32_t port);
void wacc_listen(uint32_t fd);
uint64_t wacc_accept(uint32_t state, uint32_t fd);
uint64_t wacc_recv(uint32_t s, uint32_t fd);

#endif

