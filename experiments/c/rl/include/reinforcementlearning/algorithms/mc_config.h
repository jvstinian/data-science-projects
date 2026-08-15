#ifndef INC_RL_ALGS_MC_CONFIG_H
#define INC_RL_ALGS_MC_CONFIG_H

enum MCVisit {
    FIRST_VISIT,
    EVERY_VISIT
};

struct MCConfig {
    unsigned int num_episodes;
    unsigned int max_episode_steps;
    enum MCVisit visit_type;
    float discount_factor; /* = 1.0 */
};

struct MCEpsilonSoftConfig {
    unsigned int num_episodes;
    unsigned int max_episode_steps;
    /* enum MCVisit visit_type; */
    float discount_factor;
    float epsilon;
};

#endif
