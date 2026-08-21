/* This was translated from the earlier Haskell implementation */
#include <math.h> /* INFINITY */

struct ParetoDistParams {
    double scale;
    double shape;
};

static double compl_cumulative(struct ParetoDistParams params, double x) {
    return pow(params.scale / x, params.shape);
}

double cumulative(struct ParetoDistParams params, double x) {
    return 1.0 - compl_cumulative(params, x);
}

double mean(struct ParetoDistParams params) {
    if (params.shape <= 1.0) {
        return INFINITY;
    }
    return params.scale * params.shape / (params.shape - 1.0);
}

/*
double density(struct ParetoDistParams params, double x) {
    if (x < params.scale) {
        return 0.0;
    }
    return params.shape * pow(params.scale, params.shape) / pow(x, params.shape + 1.0);
}

double log_density(struct ParetoDistParams params, double x) {
    if (x < params.scale) {
        return -INFINITY;
    }
    return log(params.shape) + params.shape * log(params.scale) - (params.shape + 1.0) * log(x);
}
*/
