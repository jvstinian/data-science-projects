#include <statistics.h>
#include <mtwister/mtwister.h>
#include <stdio.h>
#include <stdlib.h>

double mean(double* data, size_t size) {
    size_t i;
    double ret;
    for (i = 0; i < size; i++) {
        ret += data[i];
    }
    return ret / (double) size;
}

int main() {
    double data[] = {1.0, 2.0, 3.0, 4.0, 5.0};
    double res = mean(data, 5);
    printf("Mean: %f\n", res);
 
    size_t i;
    MTRand rng = seedRand(123);
    double data2[10000];
    for (i = 0; i < 10000; i++) {
        double urand = genRand(&rng);
        /*
        printf("Random number %lu: %f\n", i + 1, urand);
        */
        data2[i] = urand;
    }
    printf("Mean of uniformly randomly sampled data: %f\n", mean(data2, 10000));
    return 0;
}

