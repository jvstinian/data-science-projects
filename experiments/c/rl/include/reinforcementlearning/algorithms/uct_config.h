#ifndef INC_RL_ALGS_UCT_CONFIG_H
#define INC_RL_ALGS_UCT_CONFIG_H

struct UCTParams {
    float exploration_const;
};

struct UCTParams get_selection_uct_params(struct UCTParams params);

#endif
