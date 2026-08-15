#ifndef INC_RL_ENVS_TTT_H
#define INC_RL_ENVS_TTT_H

#include <reinforcementlearning/bool.h>

struct TTTConfig {
    int unused;
};

enum TTTPlayer {
    PlayerX,
    PlayerO
};

struct TTTAction {
    unsigned short row;
    unsigned short col;
};

enum TTTMark {
    X,
    O,
    TTT_No_Mark
};

enum TTTGameStatus {
    X_Move,
    O_Move,
    Draw,
    X_Wins,
    O_Wins
};

struct TTTState {
    enum TTTMark board[3][3];
    enum TTTGameStatus status;
};

struct TTTValidActions {
    unsigned short int num_actions;
    struct TTTAction actions[9];
};

struct TTTState initial_state();
Boolean is_terminal(struct TTTState s);
enum TTTPlayer get_player(struct TTTState s);
struct TTTState step(struct TTTState s, struct TTTAction a);
float reward(enum TTTPlayer p, struct TTTState s);
struct TTTValidActions get_valid_actions(struct TTTState s);
void print_state(struct TTTState s);

struct TTTAction ttt_mctsenv_get_random_action(struct TTTState state);

#define ENVIRONMENT_PREFIX ttt
#define CONFIG_TYPE struct TTTConfig
#define MURA_DECLS_ONLY
#include <reinforcementlearning/algorithms/mctsenv_uniform_random_actions.inc>

/* The following defines
struct TTTActionList;
struct TTTActionList* ttt_action_list_create (size_t cpty);
int ttt_action_list_realloc (struct TTTActionList** lpp, size_t new_capacity);
int ttt_action_list_push (struct TTTActionList** lpp, ACTION_TYPE val);
size_t ttt_action_list_length (struct TTTActionList* lp);
struct TTTAction ttt_action_list_get (struct TTTActionList* lp, size_t i);
void ttt_action_list_shuffle (struct TTTActionList* lp);
void ttt_action_list_destroy (struct TTTActionList* lp);
*/
#define ENVIRONMENT_PREFIX ttt
#define ENVIRONMENT_STRUCT_PREFIX TTT
#define ACTION_TYPE struct TTTAction
#define AA_DECLS_ONLY
#include <reinforcementlearning/action_array_template.inc>

struct TTTActionList* ttt_experimental_get_valid_actions(struct TTTState s);

#define ENVIRONMENT_PREFIX ttt
#define ENVIRONMENT_STRUCT_PREFIX TTT
#define CONFIG_TYPE struct TTTConfig
#define STATE_TYPE struct TTTState
#define ACTION_TYPE struct TTTAction
#define UCT_DECLS_ONLY
#include <reinforcementlearning/algorithms/uct.inc>

#endif
