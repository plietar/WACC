#ifndef CHANNEL_H
#define CHANNEL_H

#include <stdint.h>
#include <stdbool.h>

typedef struct list_elem *list_head;

typedef struct wacc_channel {
    uint32_t buffer;
    bool full;

    list_head recv_list;
    list_head send_list;
} wacc_channel;

wacc_channel *wacc_channel_create();
uint64_t wacc_channel_receive(uint32_t _state, wacc_channel *ch);
uint64_t wacc_channel_send(uint32_t _state, wacc_channel *_ch, uint32_t _value);

#endif
