#ifndef INC_RL_ENVS_HEX_H
#define INC_RL_ENVS_HEX_H

#include <reinforcementlearning/bool.h>

#define BOARD_WIDTH 7
#define BOARD_SIZE (BOARD_WIDTH * BOARD_WIDTH)

enum HexPlayer {
    Player1,
    Player2
};

const char* player_names[2] = {"Player 1", "Player 2"};

enum HexStoneColor {
    Red,
    Blue
};

struct HexAction {
    unsigned short row;
    unsigned short col;
};

struct HexValidActions {
    unsigned short int num_actions;
    struct HexAction actions[BOARD_SIZE];
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
struct HexValidActions hex_get_valid_actions(struct HexState s);
void hex_print_state (struct HexState s);

/* NOTE: The following method was not needed after all */
Boolean neighboring_hexagons(unsigned short b1, unsigned short r1, unsigned short b2, unsigned short r2);

int hex_example_main();

#endif
