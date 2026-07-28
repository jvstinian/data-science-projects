#ifndef INC_RL_ENVS_ATAXX_H
#define INC_RL_ENVS_ATAXX_H

#include <reinforcementlearning/bool.h>
#include <stddef.h> /* size_t */

#define BOARD_WIDTH 7
#define MAX_PLAYER_COUNT 4

enum AtaxxPlayer {
    Red,
    Blue,
    White,
    Black
};

/*
    subtype Axis_Label is Integer range 1 .. Board_Width;
*/
struct AtaxxCellIndices {
    unsigned short row;
    unsigned short col;
};

struct AtaxxAction {
    struct AtaxxCellIndices source;
    struct AtaxxCellIndices target;
};

/*
struct AtaxxValidActions;
    type Valid_Actions_Type is array (Natural range <>) of Action_Type;
*/
struct AtaxxValidActionsList;
typedef struct AtaxxValidActionsList AtaxxValidActionsList;
AtaxxValidActionsList* ataxx_actions_list_create(size_t cpty);
int ataxx_actions_list_realloc(AtaxxValidActionsList** lpp, size_t new_capacity);
int ataxx_actions_list_push(AtaxxValidActionsList** lpp, struct AtaxxAction val);
size_t ataxx_actions_list_length(AtaxxValidActionsList* lp);
struct AtaxxAction ataxx_actions_list_get(AtaxxValidActionsList* lp, size_t i);
void ataxx_actions_list_destroy(AtaxxValidActionsList* lp);

enum AtaxxMark {
    Mark_Red,
    Mark_Blue,
    Mark_White,
    Mark_Black,
    Mark_X,
    No_Mark
};

enum AtaxxGameStatus {
    Active,
    Finished
};
/*
    type Game_Score_Type is array (Player_Type) of Reward_Type;
    type Board_Type is array (Axis_Label, Axis_Label) of Mark;
*/

enum AtaxxPlayerCount {
    Two_Player,
    Four_Player
};
/*
    type Player_Indicator_Type is array (Player_Type) of Boolean;
*/

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

struct AtaxxState initial_state(struct AtaxxConfig config);
Boolean is_terminal (struct AtaxxState state);
enum AtaxxPlayer get_player(struct AtaxxState state);
struct AtaxxState step(struct AtaxxState state, struct AtaxxAction action);
float reward(enum AtaxxPlayer player, struct AtaxxState state);
AtaxxValidActionsList* get_valid_actions (struct AtaxxState state);

void print_state(struct AtaxxState state);

int ataxx_example_main();

#endif
