#include <stdlib.h>
#include <math.h>

float rand_float() {
    return (float)rand() / (float)RAND_MAX;
};

unsigned int poisson(float lambda) {
    float lower_limit, product;
    int k;

    /* Algo found in:
     * http://en.wikipedia.org/wiki/Poisson_distribution
     * #Generating_Poisson-distributed_random_variables
     * referring to:
     * Knuth (whom else ?!), The Art of Computer Programming,
     * Volume 2, Seminumerical algorithms, 3.4.1. Numerical Distributions,
     * F. Important integer-valued distributions. */
    lower_limit = exp(-lambda);
    k = -1;
    product = 1.0;
    while (1) {
      k += 1;
      product *= rand_float();
      if (product <= lower_limit) break;
    }
    return (unsigned int) k;
}
