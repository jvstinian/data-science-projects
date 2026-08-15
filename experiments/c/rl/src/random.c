#include <reinforcementlearning/random.h>
#include <time.h>

float rand_float() {
    return (float)rand() / (float)RAND_MAX;
};

void crand_reset(struct SeedReset seed_reset) {
    switch (seed_reset.tag) {
        case SET_DEFAULT:
            srand(time(NULL));
            break;
        case NO_SET:
            break;
        case SET_SEED:
            srand(seed_reset.seed);
            break;
    }
}
