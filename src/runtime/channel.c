#include "channel.h"
#include "task.h"
#include "async.h"

#include <stdlib.h>

void task_wakeup(wacc_task *task, wacc_task **task_list);

wacc_channel *wacc_channel_create() {
    wacc_channel *ch = malloc(sizeof *ch);
    ch->recv_list = NULL;
    ch->send_list = NULL;
    ch->full = false;

    return ch;
}

uint64_t wacc_channel_receive(uint32_t _state, wacc_channel *ch) {
    if (_state != 0) {
        ch = (wacc_channel *)_state;
    }

    if (ch->full) {
        ch->full = false;

        if (ch->send_list != NULL) {
            task_wakeup(ch->send_list, &ch->send_list);
        }

        EXIT(ch->buffer);
    } else {
        yield_cmd *cmd = malloc(sizeof *cmd);
        cmd->type = CMD_WAIT;
        cmd->wait_list = &ch->recv_list;
        YIELD(cmd, ch);
    }
}

struct channel_send_state {
    wacc_channel *ch;
    uint32_t value;
};

uint64_t wacc_channel_send(uint32_t _state, wacc_channel *_ch, uint32_t _value) {
    struct channel_send_state *state;
    if (_state == 0) {
        state = malloc(sizeof *state);
        state->ch = _ch;
        state->value = _value;
    } else {
        state = (struct channel_send_state*)_state;
    }

    if (!state->ch->full) {
        state->ch->full = true;
        state->ch->buffer = state->value;

        if (state->ch->recv_list != NULL) {
            task_wakeup(state->ch->recv_list, &state->ch->recv_list);
        }

        EXIT(0);
    } else {
        yield_cmd *cmd = malloc(sizeof *cmd);
        cmd->type = CMD_WAIT;
        cmd->wait_list = &state->ch->send_list;
        YIELD(cmd, state);
    }
}

