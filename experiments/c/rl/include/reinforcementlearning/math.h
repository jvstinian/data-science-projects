#ifndef INC_RL_MATH_H
#define INC_RL_MATH_H

#include <math.h> /* floor, fabs */

#if defined(__STDC__) && !defined(__STDC_VERSION__)
    /* Defining as static to avoid name collisions */
    float fmaxf(float x, float y);
    float fabsf(float x);
    float floorf(float arg);
#endif

#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif
/* Using macro definition specified in GNU documentation
 * https://ftp.gnu.org/old-gnu/Manuals/glibc-2.2.3/html_chapter/libc_19.html
 * Looks like the next digit is 9 so it should probably be rounded to
 * 3.14...328.
 * Alternatively: M_PI acos(-1.0) */

#endif
