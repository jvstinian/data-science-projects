#ifndef INC_RL_ENVS_HEX_H
#define INC_RL_ENVS_HEX_H

#include <reinforcementlearning/bool.h>
#include <stddef.h> /* size_t */

#define BOARD_WIDTH 7
#define BOARD_SIZE (BOARD_WIDTH * BOARD_WIDTH)

struct HexConfig {
    int unused;  /* Unused parameter required for struct definition */
};

enum HexPlayer {
    Player1,
    Player2
};

enum HexStoneColor {
    Red,
    Blue
};

struct HexAction {
    unsigned short row;
    unsigned short col;
};

enum HexMark {
    Red_Stone,
    Blue_Stone,
    No_Mark
};

enum HexGameStatus {
    Active,
    Player1_Wins,
    Player2_Wins
};

struct HexState {
    enum HexStoneColor player_colors[2];
    enum HexMark board[BOARD_WIDTH][BOARD_WIDTH];
    enum HexGameStatus status;
    enum HexPlayer current_player;
};

struct HexState hex_initial_state();
Boolean hex_is_terminal(struct HexState s);
enum HexPlayer hex_get_player(struct HexState s);
struct HexState hex_act(struct HexState s, struct HexAction a);
float hex_reward(enum HexPlayer player, struct HexState s);
void hex_print_state (struct HexState s);
struct HexAction hex_mctsenv_get_random_action(struct HexState state);

/* The following defines
struct HexActionList;
struct HexActionList* hex_action_list_create (size_t cpty);
int hex_action_list_realloc (struct HexActionList** lpp, size_t new_capacity);
int hex_action_list_push (struct HexActionList** lpp, struct HexAction val);
size_t hex_action_list_length (struct HexActionList* lp);
struct HexAction hex_action_list_get (struct HexActionList* lp, size_t i);
void hex_action_list_shuffle (struct HexActionList* lp);
void hex_action_list_destroy (struct HexActionList* lp);
*/
#define ENVIRONMENT_PREFIX hex
#define ENVIRONMENT_STRUCT_PREFIX Hex
#define ACTION_TYPE struct HexAction
#define AA_DECLS_ONLY
#include <reinforcementlearning/action_array_template.inc>

struct HexActionList* hex_experimental_get_valid_actions(struct HexState state);

#define ENVIRONMENT_PREFIX hex
#define ENVIRONMENT_STRUCT_PREFIX Hex
#define CONFIG_TYPE struct HexConfig
#define STATE_TYPE struct HexState
#define ACTION_TYPE struct HexAction
#define UCT_DECLS_ONLY
#include <reinforcementlearning/algorithms/uct.inc>


/* NOTE: The following method was not needed after all */
Boolean neighboring_hexagons(unsigned short b1, unsigned short r1, unsigned short b2, unsigned short r2);

int hex_example_main();

/* TODO: Eventually remove or adapt the following */
int hex_uct_example();

#endif
