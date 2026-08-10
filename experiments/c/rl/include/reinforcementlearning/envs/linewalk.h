#ifndef INC_RL_ENVS_LINEWALK_H
#define INC_RL_ENVS_LINEWALK_H

#include <stddef.h>
#include <reinforcementlearning/algorithms/result_types.h>
#include <reinforcementlearning/bool.h>

typedef struct LineWalkConfig {
    int N; /* Number of positions in the line */
} LineWalkConfig;

enum StateKind {
    ACTIVE,
    TERMINAL
};

enum LineWalkAction {
    MOVE_LEFT,
    MOVE_RIGHT
};

enum LineWalkPlayer {
    PLAYER1
};

typedef struct LineWalkState {
    LineWalkConfig config;
    enum StateKind kind;
    unsigned short int position;
    int reward;
} LineWalkState;

/* TODO: LineWalk action list
#define ENVIRONMENT_PREFIX linewalk
#define ENVIRONMENT_STRUCT_PREFIX LineWalk
#define ACTION_TYPE enum LineWalkAction
#define AA_DECLS_ONLY
#include <reinforcementlearning/action_array_template.inc>
*/

/* We use a custom action list for LineWalk, since the number of actions is always 2. */
struct LineWalkActionList;
size_t linewalk_action_list_length (struct LineWalkActionList* lp);
enum LineWalkAction linewalk_action_list_get (struct LineWalkActionList* lp, size_t i);
void linewalk_action_list_shuffle (struct LineWalkActionList* lp);
void linewalk_action_list_destroy (struct LineWalkActionList* lp);

/* MCTS Interface */
LineWalkState linewalk_initial_state(LineWalkConfig config);
Boolean linewalk_is_terminal (LineWalkState state);
enum LineWalkPlayer linewalk_get_player(LineWalkState state);
LineWalkState linewalk_act(LineWalkState state, enum LineWalkAction action);
float linewalk_reward(enum LineWalkPlayer player, LineWalkState state);
/* TODO: Remove the following */
unsigned int linewalk_get_available_actions (LineWalkState state, enum LineWalkAction *available_actions, unsigned int* num_actions);
struct LineWalkActionList* linewalk_experimental_get_valid_actions(struct LineWalkState s);
enum LineWalkAction linewalk_mctsenv_get_random_action(LineWalkState state);
void linewalk_print_state(LineWalkState state);

#define ENVIRONMENT_PREFIX linewalk
#define CONFIG_TYPE LineWalkConfig
#define MURA_DECLS_ONLY
#include <reinforcementlearning/algorithms/mctsenv_uniform_random_actions.inc>

#define ENVIRONMENT_PREFIX linewalk
#define ENVIRONMENT_STRUCT_PREFIX LineWalk
#define CONFIG_TYPE struct LineWalkConfig
#define STATE_TYPE struct LineWalkState
#define ACTION_TYPE enum LineWalkAction
#define UCT_DECLS_ONLY
#include <reinforcementlearning/algorithms/uct.inc>


/* RL Interface */
struct LineWalkObservation {
    unsigned short int position;
};

/* declaration */
struct LineWalkEnvironment;

struct LineWalkStepReturn {
    struct LineWalkObservation observation;
    float reward;
    Boolean terminated;
};
 
struct LineWalkEnvironment* linewalk_make(struct LineWalkConfig config);
int linewalk_init(struct LineWalkConfig config, struct LineWalkEnvironment* env);
/* TODO: Add support for a seed reset */
struct LineWalkObservation linewalk_reset(struct LineWalkEnvironment* env);
struct LineWalkStepReturn linewalk_step(struct LineWalkEnvironment* env, enum LineWalkAction action);
void linewalk_deinit(struct LineWalkEnvironment* env);
void linewalk_close(struct LineWalkEnvironment* env);


enum LineWalkAction linewalk_get_random_action(struct LineWalkEnvironment* env);
struct SimulationSummary linewalk_uniform_random_actions(struct LineWalkConfig config, Boolean verbose);

#endif
