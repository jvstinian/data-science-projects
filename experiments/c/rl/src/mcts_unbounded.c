#include <reinforcementlearning/algorithms/mcts_unbounded.h>
#include <stdlib.h> /* malloc */
#include <math.h> /* sqrt */
#include <float.h> /* FLT_MAX */
#include <string.h> /* memcpy */
#include <stdio.h> /* printf */

struct UCTParams get_selection_uct_params(struct UCTParams params) {
    struct UCTParams ret = params;
    ret.exploration_const = 0.0;
    return ret;
}

static struct Node initialize_node(struct LineWalkState s, unsigned int num_visits) {
    unsigned int a;
    struct Node ret;
    ret.num_visits = num_visits;
    ret.node_state = s;
    struct LineWalkActionList* action_list = linewalk_experimental_get_valid_actions(s);
    /* TODO: In general we will need to handle errors */
    ret.num_actions = linewalk_action_list_length(action_list);
    linewalk_action_list_shuffle(action_list);
    ret.ars = malloc(ret.num_actions * sizeof(struct ActionReward));
    for (a = 0; a < ret.num_actions; a++) {
        ret.ars[a].action = linewalk_action_list_get(action_list, (size_t) a);
        ret.ars[a].reward = 0.0;
    }
    /* We allocate for the trees but do not initialize */
    ret.trees = malloc(ret.num_actions * sizeof(struct Tree));
    ret.actions_tried = 0;
    linewalk_action_list_destroy(action_list);
    return ret;
}

void tree_init(struct Tree* tree, struct LineWalkState s) {
    if (linewalk_is_terminal(s)) {
        tree->tag = Leaf;
        tree->data.leaf = (struct Leaf) { 1, s };
    } else {
        tree->tag = Node;
        tree->data.node = initialize_node(s, 1);
    }
}

struct Tree* mcts_tree_new(struct LineWalkConfig config) {
    struct LineWalkState s = linewalk_initial_state(config);

    struct Tree* t = malloc(sizeof(struct Tree));
    if (linewalk_is_terminal(s)) {
        t->tag = Leaf;
        t->data.leaf = (struct Leaf) { 0, s };
    } else {
        t->tag = Node;
        t->data.node = initialize_node(s, 0);
    }
    return t;
}

void mcts_tree_deinit(struct Tree* tree) {
    unsigned int i;

    switch (tree->tag) {
        case Node:
            /* Iterate over the trees, de-initializing each of them */
            for(i = 0; i < tree->data.node.actions_tried; i++) {
                mcts_tree_deinit(&tree->data.node.trees[i]);
            }
            free(tree->data.node.ars);
            free(tree->data.node.trees);
            break;
        case Leaf:
        default:
            break;
    }
}

void node_deinit_preserve_action_idx(struct Node* node, unsigned int action_idx, struct Tree* child_tree) {
    /* printf("Deinitializing node but preserving action index %u\n", action_idx); */
    unsigned int i;
    /* Iterate over the trees, performing a soft copy for the target index and
     * de-initializing the other trees */
    for(i = 0; i < node->actions_tried; i++) {
        if (i == action_idx) {
            memcpy(child_tree, &node->trees[i], sizeof(struct Tree));
        } else {
            mcts_tree_deinit(&node->trees[i]);
        }
    }
    free(node->ars);
    free(node->trees);
}

void mcts_tree_free(struct Tree* tree) {
    mcts_tree_deinit(tree);
    free(tree);
}

struct LineWalkState default_policy_terminal_state(struct LineWalkState s) {
    enum LineWalkAction a;
    struct LineWalkState next_state;
    if (linewalk_is_terminal(s)) {
        return s;
    } else {
        a = linewalk_mctsenv_get_random_action(s);
        next_state = linewalk_act(s, a);
        return default_policy_terminal_state(next_state);
    }
}

unsigned int tree_visits(struct Tree tree) {
    switch (tree.tag) {
        case Node:
            return tree.data.node.num_visits;
        case Leaf:
            return tree.data.leaf.num_visits;
        default:
            return 0; /* Should be unreachable */
    }
}

float uct_objective(struct UCTParams params, unsigned int parentvisits, struct Tree tree, float totalreward) {
    float pvs = (float) parentvisits;
    float cvs = (float) tree_visits(tree);
    return (totalreward / cvs) + params.exploration_const * sqrt(2.0 * log(pvs) / cvs);
}

unsigned int best_child_index(struct UCTParams params, struct Node node) {
    unsigned int best_index = 0, i;
    float val;
    float max_val = -FLT_MAX;
    for(i=0; i < node.actions_tried; i++) {
        val = uct_objective(params, node.num_visits, node.trees[i], node.ars[i].reward);
        if (val > max_val) {
            max_val = val;
            best_index = i;
        }
    }
    return best_index;
}

struct LineWalkState uct_update_with_reward(struct UCTParams params, struct Node* node) {
    unsigned int action_idx;
    enum LineWalkAction a;
    struct LineWalkState child_state, terminal_state;
    enum LineWalkPlayer player;
    float inc_reward;

    if (node->actions_tried < node->num_actions) {
        /* Do we need to specify this is an expansion operation? */
        action_idx = node->actions_tried;
        a = node->ars[action_idx].action;

        child_state = linewalk_act(node->node_state, a);
        terminal_state = default_policy_terminal_state(child_state);
        player = linewalk_get_player(node->node_state);
        inc_reward = linewalk_reward(player, terminal_state);

        tree_init(&node->trees[action_idx], child_state);
        node->ars[action_idx].reward = inc_reward;
        node->actions_tried++;

        node->num_visits++;
        return terminal_state;
    } else {
        action_idx = best_child_index(params, *node);
        a = node->ars[action_idx].action;
        switch (node->trees[action_idx].tag) {
            case Node:
                /* Recursively call the function and obtain a terminal state */
                terminal_state = uct_update_with_reward(params, &node->trees[action_idx].data.node);
                player = linewalk_get_player(node->node_state);
                inc_reward = linewalk_reward(player, terminal_state);
                /* Increment reward */
                node->ars[action_idx].reward += inc_reward;
                /* Increment number of visits current node */
                node->num_visits++;
                return terminal_state;
            case Leaf:
                terminal_state = node->trees[action_idx].data.leaf.terminal_state;
                player = linewalk_get_player(node->node_state);
                inc_reward = linewalk_reward(player, terminal_state);
                /* Increment number of visits to terminal child */
                node->trees[action_idx].data.leaf.num_visits++;
                /* Increment reward */
                node->ars[action_idx].reward += inc_reward;
                /* Increment number of visits current node */
                node->num_visits++;
                return terminal_state;
            default:
                return node->node_state; /* Unreachable */
        }
    }
}

struct LineWalkState uct_get_state(const struct Tree* tree) {
    switch (tree->tag) {
        case Node:
            return tree->data.node.node_state;
        case Leaf:
            return tree->data.leaf.terminal_state;
        default: /* should be unreachable */
            return tree->data.leaf.terminal_state;
    }
}

void uct_update(struct UCTParams params, struct Tree* t) {
    switch (t->tag) {
        case Node:
            uct_update_with_reward(params, &t->data.node);
            break;
        case Leaf:
        default:
            /* do nothing */
            break;
    }
}

int uct_search(unsigned int n, struct UCTParams params, struct Tree* tree, enum LineWalkAction* action_out) {
    struct UCTParams exploit_params;
    unsigned int action_idx;
    struct Tree tree_copy;

    while (n > 0) {
        uct_update(params, tree);
        n--;
    }

    exploit_params = get_selection_uct_params(params);

    switch (tree->tag) {
        case Node:
            action_idx = best_child_index(exploit_params, tree->data.node);
            *action_out = tree->data.node.ars[action_idx].action;
            node_deinit_preserve_action_idx(&tree->data.node, action_idx, &tree_copy);
            memcpy(tree, &tree_copy, sizeof(struct Tree));
            return 0;
        case Leaf:
        default:
            fprintf(stderr, "uct_take_action: tree already at a terminal state\n");
            return 1;
    }
}

int uct_take_action(struct Tree* tree, enum LineWalkAction action) {
    unsigned int action_idx;
    struct Tree tree_copy;

    switch (tree->tag) {
        case Node:
            for (action_idx = 0; action_idx < tree->data.node.actions_tried; action_idx++) {
                if (tree->data.node.ars[action_idx].action == action) {
                    break;
                }
            }
            if (action_idx >= tree->data.node.actions_tried) {
                fprintf(stderr, "uct_take_action: action %d has not been explored\n", action);
                return 1;
            }
            node_deinit_preserve_action_idx(&tree->data.node, action_idx, &tree_copy);
            memcpy(tree, &tree_copy, sizeof(struct Tree));
            return 0;
        case Leaf:
        default:
            fprintf(stderr, "uct_take_action: tree already at a terminal state\n");
            return 1;
    }
}

int uct_main() {
    struct UCTParams uctparams = { sqrt (2.0) };
    struct LineWalkConfig config = (struct LineWalkConfig) { 5 };
    enum LineWalkAction a;
    struct Tree* tree = mcts_tree_new(config);
    uct_update(uctparams, tree);
    uct_search(50, uctparams, tree, &a);
    printf("Took action %d\n", a);
    printf("Now in state %u\n", tree->data.node.node_state.position);
    printf("Number of visits: %u\n", tree_visits(*tree));
    /* IN PROGRESS: There's probably a memory leak with a dangling pointer here.
     *              While deinit is run on tree, tree itself is not freed
     *              prior to the reassignment in the following. */
    uct_search(0, uctparams, tree, &a);
    printf("Took action %d\n", a);
    printf("Now in state %u\n", tree->data.node.node_state.position);
    printf("Number of visits: %u\n", tree_visits(*tree));
    mcts_tree_free(tree);
    return 0;
};

