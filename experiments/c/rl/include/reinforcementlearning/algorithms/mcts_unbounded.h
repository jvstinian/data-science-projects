#ifndef INC_RL_ALGS_MCTS_UNBOUNDED_H
#define INC_RL_ALGS_MCTS_UNBOUNDED_H

#include <reinforcementlearning/envs/linewalk.h>
#include <reinforcementlearning/algorithms/uct_config.h>

enum TreeKind {
    Node,
    Leaf
};

struct Tree;

struct ActionReward {
    enum LineWalkAction action;
    float reward;
};

struct Node {
    unsigned int num_visits;
    struct LineWalkState node_state;
    unsigned int num_actions;
    struct ActionReward* ars;
    unsigned int actions_tried;
    struct Tree* trees;
};

struct Leaf {
    unsigned int num_visits;
    struct LineWalkState terminal_state;
};

union TreeData {
    struct Node node;
    struct Leaf leaf;
};

struct Tree {
    enum TreeKind tag;
    union TreeData data;
};
    
void uct_update(struct UCTParams params, struct Tree* t);
struct LineWalkState uct_get_state(const struct Tree* tree);
int uct_search(unsigned int n, struct UCTParams params, struct Tree* tree, enum LineWalkAction* action_out);
int uct_take_action(struct Tree* tree, enum LineWalkAction action);
struct Tree* mcts_tree_new(struct LineWalkConfig config);
void mcts_tree_free(struct Tree* tree);

int uct_main();

#endif
