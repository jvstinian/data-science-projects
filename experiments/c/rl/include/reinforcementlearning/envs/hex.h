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

/*
   subtype Blue_Label is Integer range 1 .. Board_Width;
   subtype Red_Label is Character range 'A' .. Character'Val(Character'Pos('A') + Board_Width - 1);
*/

struct HexState initial_state();
Boolean is_terminal(struct HexState s);
enum HexPlayer get_player(struct HexState s);
struct HexState step(struct HexState s, struct HexAction a);
float reward(enum HexPlayer player, struct HexState s);
struct HexValidActions get_valid_actions(struct HexState s);
void print_state (struct HexState s);
/*
private
   function Get_Number_Of_Stones (State : State_Type) return Natural;
   function Check_Win (Board : Board_Type; Stone : Stone_Color_Type) return Boolean;
   function Check_Blue_Win (Board : Board_Type) return Boolean;
   function Check_Red_Win (Board : Board_Type) return Boolean;
   
*/
/* NOTE: The following method was not needed after all */
Boolean neighboring_hexagons(unsigned short b1, unsigned short r1, unsigned short b2, unsigned short r2);

int hex_example_main();

#endif
