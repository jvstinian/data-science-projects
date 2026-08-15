#ifndef INC_RL_RANDOM_H
#define INC_RL_RANDOM_H

#include <stdlib.h>

/* Generate a random number from [0, 1] assuming the uniform distribution */
float rand_float();

enum SeedResetTag {
    SET_DEFAULT,
    NO_SET,
    SET_SEED
};

struct SeedReset {
    enum SeedResetTag tag;
    unsigned int seed;  /* Only used if tag == SET_SEED */
};

void crand_reset(struct SeedReset reset);

#endif
