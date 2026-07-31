#include <reinforcementlearning/math.h>

#if defined(__STDC__) && !defined(__STDC_VERSION__)
    float fmaxf(float x, float y) {
        return (x > y) ? x : y;
    }
    float fabsf(float x) {
        return (float) fabs((double) x);
    }
    float floorf(float arg) {
        return (float) floor((double) arg);
    }
#endif

