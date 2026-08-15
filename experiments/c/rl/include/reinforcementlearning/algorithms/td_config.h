#ifndef INC_RL_ALGS_TD_CONFIG_H
#define INC_RL_ALGS_TD_CONFIG_H

struct TDConfig {
    float alpha;
    float gamma;
};

struct SARSAConfig {
    float alpha;
    float gamma;
    float initial_epsilon;
    float minimum_epsilon;
    unsigned int episodes_to_minimum_epsilon;
};

#endif
