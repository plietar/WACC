#include <stdint.h>

struct generator {
    uint32_t state;
    uint32_t value;
};

void print_generator(struct generator *g) {
    printf("%x %x\n", g->state, g->value);
}

struct generator wacc_main(uint32_t state) __value_in_regs;

struct generator wacc_sleep(uint32_t state) __value_in_regs {
    if (state == 0) {
        state = 1;
    } else {
        state += 1;
    }

    return struct generator {
        .state = state,
        .value = 0xdeadbeef
    };
}

int main() {
    struct generator g = wacc_main(0);
    print_generator(&g);
}

