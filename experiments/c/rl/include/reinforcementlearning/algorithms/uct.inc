#ifndef ENVIRONMENT_PREFIX
#error "ENVIRONMENT_PREFIX must be defined in uct.inc"
#endif

#ifndef ENVIRONMENT_STRUCT_PREFIX
#error "ENVIRONMENT_STRUCT_PREFIX must be defined in uct.inc"
#endif

#ifndef CONFIG_TYPE
#error "CONFIG_TYPE is not defined in uct.inc"
#endif

#ifndef STATE_TYPE
#error "STATE_TYPE is not defined in uct.inc"
#endif

#ifndef ACTION_TYPE
#error "ACTION_TYPE is not defined in uct.inc"
#endif


#define UCT_FUNC_NAME(name) UCT_CONCAT1(ENVIRONMENT_PREFIX, name)

#define UCT_CONCAT1(prefix, name) UCT_CONCAT2(prefix, name)
#define UCT_CONCAT2(prefix, name) prefix##_##name

#define UCT_STRUCT_CONCAT1(prefix, name) UCT_STRUCT_CONCAT2(prefix, name)
#define UCT_STRUCT_CONCAT2(prefix, name) prefix##name

#define UCT_TREE_TYPE struct UCT_STRUCT_CONCAT1(ENVIRONMENT_STRUCT_PREFIX, Tree)


#ifdef UCT_DECLS_ONLY


#include <reinforcementlearning/algorithms/uct_config.h>

/* Declare tree type */
UCT_TREE_TYPE;

UCT_TREE_TYPE* UCT_FUNC_NAME(mcts_tree_new) (CONFIG_TYPE config);
STATE_TYPE UCT_FUNC_NAME(uct_get_state) (const UCT_TREE_TYPE* tree);
int UCT_FUNC_NAME(uct_search) (unsigned int n, struct UCTParams params, UCT_TREE_TYPE* tree, ACTION_TYPE* action_out);
int UCT_FUNC_NAME(uct_take_action) (UCT_TREE_TYPE* tree, ACTION_TYPE action);
void UCT_FUNC_NAME(mcts_tree_free) (UCT_TREE_TYPE* tree);


#else


/* The following macros are only needed for the implementation */
#ifndef PLAYER_TYPE
#error "PLAYER_TYPE is not defined in uct.inc"
#endif

#ifndef INITIAL_STATE_METHOD
#error "INITIAL_STATE_METHOD is not defined in uct.inc"
#endif

#ifndef STEP_METHOD
#error "STEP_METHOD is not defined in uct.inc"
#endif

#ifndef GET_PLAYER_METHOD
#error "GET_PLAYER_METHOD is not defined in uct.inc"
#endif

#ifndef RANDOM_ACTION_METHOD
#error "RANDOM_ACTION_METHOD is not defined in uct.inc"
#endif

#ifndef IS_TERMINAL_METHOD
#error "IS_TERMINAL_METHODis not defined in uct.inc"
#endif

#ifndef REWARD_METHOD
#error "REWARD_METHOD not defined in uct.inc"
#endif

#ifndef ACTION_LIST_TYPE
#error "ACTION_LIST_TYPE not defined in uct.inc"
#endif

#ifndef GET_VALID_ACTIONS_METHOD
#error "GET_VALID_ACTIONS_METHOD not defined in uct.inc"
#endif

#ifndef ACTION_LIST_GET_METHOD
#error "ACTION_LIST_GET_METHOD not defined in uct.inc"
#endif

#ifndef ACTION_LIST_LENGTH_METHOD
#error "ACTION_LIST_LENGTH_METHOD not defined in uct.inc"
#endif

#ifndef ACTION_LIST_SHUFFLE_METHOD
#error "ACTION_LIST_SHUFFLE_METHOD not defined in uct.inc"
#endif

#ifndef ACTION_LIST_DESTROY_METHOD
#error "ACTION_LIST_DESTROY_METHOD not defined in uct.inc"
#endif

#ifndef ACTION_EQ_METHOD
#error "ACTION_EQ_METHOD not defined in uct.inc"
#endif

/* Template header approach */
#include <stdlib.h> /* malloc */
#include <string.h> /* memcpy */
#include <math.h> /* sqrt */
#include <float.h> /* FLT_MAX */

/* TODO: Possibly move to a uct_params.h header file 
struct UCTParams get_selection_uct_params(struct UCTParams params) {
    struct UCTParams ret = params;
    ret.exploration_const = 0.0;
    return ret;
}
*/

enum TreeKind {
    Node,
    Leaf
};

struct ActionReward {
    ACTION_TYPE action;
    float reward;
};

struct Node {
    unsigned int num_visits;
    STATE_TYPE node_state;
    unsigned int num_actions;
    struct ActionReward* ars;
    unsigned int actions_tried;
    UCT_TREE_TYPE* trees;
};

struct Leaf {
    unsigned int num_visits;
    STATE_TYPE terminal_state;
};

union TreeData {
    struct Node node;
    struct Leaf leaf;
};

UCT_TREE_TYPE {
    enum TreeKind tag;
    union TreeData data;
};

static int initialize_node(STATE_TYPE s, unsigned int num_visits, struct Node* node_out) {
    unsigned int a;
    node_out->num_visits = num_visits;
    node_out->node_state = s;
    ACTION_LIST_TYPE* action_list = GET_VALID_ACTIONS_METHOD(s);
    if (action_list == NULL) {
        fprintf(stderr, "initialize_node: failed to get valid actions for state\n");
        return 1;
    }
    node_out->num_actions = ACTION_LIST_LENGTH_METHOD(action_list);
    ACTION_LIST_SHUFFLE_METHOD(action_list);
    node_out->ars = malloc(node_out->num_actions * sizeof(struct ActionReward));
    if (node_out->ars == NULL) {
        fprintf(stderr, "initialize_node: failed to allocate memory for action rewards\n");
        ACTION_LIST_DESTROY_METHOD(action_list);
        return 1;
    }
    for (a = 0; a < node_out->num_actions; a++) {
        node_out->ars[a].action = ACTION_LIST_GET_METHOD(action_list, a);
        node_out->ars[a].reward = 0.0;
    }
    /* We allocate for the trees but do not initialize */
    node_out->trees = malloc(node_out->num_actions * sizeof(UCT_TREE_TYPE));
    if (node_out->trees == NULL) {
        fprintf(stderr, "initialize_node: failed to allocate memory for child trees\n");
        free(node_out->ars);
        ACTION_LIST_DESTROY_METHOD(action_list);
        return 1;
    }
    node_out->actions_tried = 0;
    ACTION_LIST_DESTROY_METHOD(action_list);
    return 0;
}

static int tree_init(UCT_TREE_TYPE* tree, STATE_TYPE s) {
    if (IS_TERMINAL_METHOD(s)) {
        tree->tag = Leaf;
        tree->data.leaf = (struct Leaf) { 1, s };
    } else {
        tree->tag = Node;
        if (initialize_node(s, 1, &tree->data.node)) {
            fprintf(stderr, "tree_init: failed to initialize node\n");
            return 1;
        }
    }
    return 0;
}

UCT_TREE_TYPE* UCT_FUNC_NAME(mcts_tree_new) (CONFIG_TYPE config) {
    STATE_TYPE s = INITIAL_STATE_METHOD(config);

    UCT_TREE_TYPE* t = malloc(sizeof(UCT_TREE_TYPE));
    if (t == NULL) {
        fprintf(stderr, "mcts_tree_new: failed to allocate memory for tree\n");
        return NULL;
    }
    if (IS_TERMINAL_METHOD(s)) {
        t->tag = Leaf;
        t->data.leaf = (struct Leaf) { 0, s };
    } else {
        t->tag = Node;
        if (initialize_node(s, 0, &t->data.node)) {
            fprintf(stderr, "mcts_tree_new: failed to initialize node\n");
            free(t);
            return NULL;
        }
    }
    return t;
}

static void mcts_tree_deinit(UCT_TREE_TYPE* tree) {
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

static void node_deinit_preserve_action_idx(struct Node* node, unsigned int action_idx, UCT_TREE_TYPE* child_tree) {
    unsigned int i;
    /* Iterate over the trees, performing a soft copy for the target index and
     * de-initializing the other trees */
    for(i = 0; i < node->actions_tried; i++) {
        if (i == action_idx) {
            memcpy(child_tree, &node->trees[i], sizeof(UCT_TREE_TYPE));
        } else {
            mcts_tree_deinit(&node->trees[i]);
        }
    }
    free(node->ars);
    free(node->trees);
}

void UCT_FUNC_NAME(mcts_tree_free) (UCT_TREE_TYPE* tree) {
    mcts_tree_deinit(tree);
    free(tree);
}

static STATE_TYPE default_policy_terminal_state(STATE_TYPE s) {
    ACTION_TYPE a;
    STATE_TYPE next_state;
    if (IS_TERMINAL_METHOD(s)) {
        return s;
    } else {
        a = RANDOM_ACTION_METHOD(s);
        next_state = STEP_METHOD(s, a);
        return default_policy_terminal_state(next_state);
    }
}

static unsigned int tree_visits(UCT_TREE_TYPE tree) {
    switch (tree.tag) {
        case Node:
            return tree.data.node.num_visits;
        case Leaf:
            return tree.data.leaf.num_visits;
        default:
            return 0; /* Should be unreachable */
    }
}

static float uct_objective(struct UCTParams params, unsigned int parentvisits, UCT_TREE_TYPE tree, float totalreward) {
    float pvs = (float) parentvisits;
    float cvs = (float) tree_visits(tree);
    return (totalreward / cvs) + params.exploration_const * sqrt(2.0 * log(pvs) / cvs);
}

static unsigned int best_child_index(struct UCTParams params, struct Node node) {
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

static int uct_update_with_reward(struct UCTParams params, struct Node* node, STATE_TYPE* terminal_state_out) {
    unsigned int action_idx;
    ACTION_TYPE a;
    STATE_TYPE child_state, terminal_state;
    PLAYER_TYPE player;
    float inc_reward;

    if (node->actions_tried < node->num_actions) {
        /* Do we need to specify this is an expansion operation? */
        action_idx = node->actions_tried;
        a = node->ars[action_idx].action;

        child_state = STEP_METHOD(node->node_state, a);
        terminal_state = default_policy_terminal_state(child_state);
        player = GET_PLAYER_METHOD(node->node_state);
        inc_reward = REWARD_METHOD(player, terminal_state);

        if (tree_init(&node->trees[action_idx], child_state)) {
            fprintf(stderr, "uct_update_with_reward: failed to initialize child tree\n");
            /* We set the terminal state even though it likely can't be used by the caller */
            *terminal_state_out = terminal_state;
            return 1;
        }
        node->ars[action_idx].reward = inc_reward;
        node->actions_tried++;

        node->num_visits++;
        *terminal_state_out = terminal_state;
        return 0;
    } else {
        action_idx = best_child_index(params, *node);
        a = node->ars[action_idx].action;
        switch (node->trees[action_idx].tag) {
            case Node:
                /* Recursively call the function and obtain a terminal state */
                /* TODO
                terminal_state = uct_update_with_reward(params, &node->trees[action_idx].data.node);
                */
                if (uct_update_with_reward(params, &node->trees[action_idx].data.node, terminal_state_out)) {
                    /* We don't print a message as the error has already been
                     * printed by the recursive call */
                    return 1;
                }
                terminal_state = *terminal_state_out; /* TODO: Probably not a good way to do things */
                player = GET_PLAYER_METHOD(node->node_state);
                inc_reward = REWARD_METHOD(player, terminal_state);
                /* Increment reward */
                node->ars[action_idx].reward += inc_reward;
                /* Increment number of visits current node */
                node->num_visits++;
                /* TODO: terminal_state_out already set */
                return 0;
            case Leaf:
                terminal_state = node->trees[action_idx].data.leaf.terminal_state;
                player = GET_PLAYER_METHOD(node->node_state);
                inc_reward = REWARD_METHOD(player, terminal_state);
                /* Increment number of visits to terminal child */
                node->trees[action_idx].data.leaf.num_visits++;
                /* Increment reward */
                node->ars[action_idx].reward += inc_reward;
                /* Increment number of visits current node */
                node->num_visits++;
                *terminal_state_out = terminal_state;
                return 0;
            default:
                fprintf(stderr, "uct_update_with_reward: unexpected tree tag\n");
                return 1; /*node->node_state;*/ /* Unreachable */
        }
    }
}

STATE_TYPE UCT_FUNC_NAME(uct_get_state) (const UCT_TREE_TYPE* tree) {
    switch (tree->tag) {
        case Node:
            return tree->data.node.node_state;
        case Leaf:
            return tree->data.leaf.terminal_state;
        default: /* should be unreachable */
            return tree->data.leaf.terminal_state;
    }
}

static int uct_update(struct UCTParams params, UCT_TREE_TYPE* t) {
    STATE_TYPE unused_terminal_state;
    switch (t->tag) {
        case Node:
            return uct_update_with_reward(params, &t->data.node, &unused_terminal_state);
        case Leaf:
        default:
            /* do nothing */
            fprintf(stderr, "uct_update: tree already at a terminal state\n");
            return 1;
    }
}

int UCT_FUNC_NAME(uct_search) (unsigned int n, struct UCTParams params, UCT_TREE_TYPE* tree, ACTION_TYPE* action_out) {
    struct UCTParams exploit_params;
    unsigned int action_idx;
    UCT_TREE_TYPE tree_copy;

    while (n > 0) {
        if (uct_update(params, tree)) {
            fprintf(stderr, "uct_search: failed to update tree\n");
            return 1;
        }
        n--;
    }

    exploit_params = get_selection_uct_params(params);

    switch (tree->tag) {
        case Node:
            action_idx = best_child_index(exploit_params, tree->data.node);
            *action_out = tree->data.node.ars[action_idx].action;
            node_deinit_preserve_action_idx(&tree->data.node, action_idx, &tree_copy);
            memcpy(tree, &tree_copy, sizeof(UCT_TREE_TYPE));
            return 0;
        case Leaf:
        default:
            fprintf(stderr, "uct_take_action: tree already at a terminal state\n");
            return 1;
    }
}

int UCT_FUNC_NAME(uct_take_action) (UCT_TREE_TYPE* tree, ACTION_TYPE action) {
    unsigned int action_idx;
    UCT_TREE_TYPE tree_copy;

    switch (tree->tag) {
        case Node:
            for (action_idx = 0; action_idx < tree->data.node.actions_tried; action_idx++) {
                if (ACTION_EQ_METHOD(tree->data.node.ars[action_idx].action, action)) {
                    break;
                }
            }
            if (action_idx >= tree->data.node.actions_tried) {
                fprintf(stderr, "uct_take_action: action has not been explored\n");
                return 1;
            }
            node_deinit_preserve_action_idx(&tree->data.node, action_idx, &tree_copy);
            memcpy(tree, &tree_copy, sizeof(UCT_TREE_TYPE));
            return 0;
        case Leaf:
        default:
            fprintf(stderr, "uct_take_action: tree already at a terminal state\n");
            return 1;
    }
}

#undef ACTION_EQ_METHOD
#undef ACTION_LIST_DESTROY_METHOD
#undef ACTION_LIST_SHUFFLE_METHOD
#undef ACTION_LIST_LENGTH_METHOD
#undef ACTION_LIST_GET_METHOD
#undef GET_VALID_ACTIONS_METHOD
#undef ACTION_LIST_TYPE
#undef REWARD_METHOD
#undef IS_TERMINAL_METHOD
#undef RANDOM_ACTION_METHOD
#undef GET_PLAYER_METHOD
#undef STEP_METHOD
#undef INITIAL_STATE_METHOD
#undef PLAYER_TYPE

#endif  /* UCT_DECLS_ONLY */

#undef ACTION_TYPE
#undef STATE_TYPE
#undef CONFIG_TYPE
#undef ENVIRONMENT_PREFIX
#undef ENVIRONMENT_STRUCT_PREFIX
#undef UCT_FUNC_NAME
#undef UCT_FUNC_NAME_CONCAT1
#undef UCT_FUNC_NAME_CONCAT
#undef UCT_STRUCT_CONCAT1
#undef UCT_STRUCT_CONCAT2
#undef UCT_TREE_TYPE
#ifdef UCT_DECLS_ONLY
#undef UCT_DECLS_ONLY
#endif

