#ifndef INC_RL_ALGS_ET_CONFIG_H
#define INC_RL_ALGS_ET_CONFIG_H

struct ETConfig {
    float alpha;
    float gamma;
    float lambda;
};

struct ETSARSAConfig {
    struct SARSAConfig td_config;
    float lambda;
};

#endif
