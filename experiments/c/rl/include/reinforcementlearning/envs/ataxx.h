#ifndef INC_RL_ENVS_ATAXX_H
#define INC_RL_ENVS_ATAXX_H

#include <reinforcementlearning/bool.h>
#include <stddef.h> /* size_t */

#define BOARD_WIDTH 7
#define MAX_PLAYER_COUNT 4

enum AtaxxPlayer {
    Ataxx_Red,
    Ataxx_Blue,
    Ataxx_White,
    Ataxx_Black
};

struct AtaxxCellIndices {
    unsigned short row;
    unsigned short col;
};

struct AtaxxAction {
    struct AtaxxCellIndices source;
    struct AtaxxCellIndices target;
};

enum AtaxxMark {
    Ataxx_Mark_Red,
    Ataxx_Mark_Blue,
    Ataxx_Mark_White,
    Ataxx_Mark_Black,
    Ataxx_Mark_X,
    Ataxx_No_Mark
};

enum AtaxxGameStatus {
    Ataxx_Active,
    Ataxx_Finished
};

enum AtaxxPlayerCount {
    Two_Player,
    Four_Player
};

struct AtaxxConfig {
    enum AtaxxPlayerCount player_count;
};

struct AtaxxState {
    Boolean player_indicators[MAX_PLAYER_COUNT];
    enum AtaxxMark board[BOARD_WIDTH][BOARD_WIDTH];
    enum AtaxxGameStatus status;
    unsigned short int scores[MAX_PLAYER_COUNT];
    enum AtaxxPlayer current_player;
};

struct AtaxxState ataxx_initial_state(struct AtaxxConfig config);
Boolean ataxx_is_terminal (struct AtaxxState state);
enum AtaxxPlayer ataxx_get_player(struct AtaxxState state);
struct AtaxxState ataxx_act(struct AtaxxState state, struct AtaxxAction action);
float ataxx_reward(enum AtaxxPlayer player, struct AtaxxState state);

void ataxx_print_state(struct AtaxxState state);
struct AtaxxAction ataxx_mctsenv_get_random_action(struct AtaxxState state);

#define ENVIRONMENT_PREFIX ataxx
#define CONFIG_TYPE struct AtaxxConfig
#define MURA_DECLS_ONLY
#include <reinforcementlearning/algorithms/mctsenv_uniform_random_actions.inc>

/* The following defines
struct AtaxxActionList;
struct AtaxxActionList* ataxx_action_list_create (size_t cpty);
int ataxx_action_list_realloc (struct AtaxxActionList** lpp, size_t new_capacity);
int ataxx_action_list_push (struct AtaxxActionList** lpp, struct AtaxxAction val);
size_t ataxx_action_list_length (struct AtaxxActionList* lp);
struct AtaxxAction ataxx_action_list_get (struct AtaxxActionList* lp, size_t i);
void ataxx_action_list_shuffle (struct AtaxxActionList* lp);
void ataxx_action_list_destroy (struct AtaxxActionList* lp);
*/
#define ENVIRONMENT_PREFIX ataxx
#define ENVIRONMENT_STRUCT_PREFIX Ataxx
#define ACTION_TYPE struct AtaxxAction
#define AA_DECLS_ONLY
#include <reinforcementlearning/action_array_template.inc>

struct AtaxxActionList* ataxx_experimental_get_valid_actions(struct AtaxxState state);

#define ENVIRONMENT_PREFIX ataxx
#define ENVIRONMENT_STRUCT_PREFIX Ataxx
#define CONFIG_TYPE struct AtaxxConfig
#define STATE_TYPE struct AtaxxState
#define ACTION_TYPE struct AtaxxAction
#define UCT_DECLS_ONLY
#include <reinforcementlearning/algorithms/uct.inc>

int ataxx_example_main();
/* TODO: Eventually remove or adapt the following */
int ataxx_uct_example();

#endif
