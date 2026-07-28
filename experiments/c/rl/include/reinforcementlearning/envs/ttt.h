#ifndef INC_RL_ENVS_TTT_H
#define INC_RL_ENVS_TTT_H

#include <reinforcementlearning/bool.h>

enum TTTPlayer {
    PlayerX,
    PlayerO
};

/*
   type Row_Label is new Integer range 0 .. 2;
   -- We use a custom discrete type for column labels
   type Col_Label is (A, B, C);
*/

struct TTTAction {
    unsigned short row;
    unsigned short col;
};

enum TTTMark {
    X,
    O,
    No_Mark
};

enum TTTGameStatus {
    X_Move,
    O_Move,
    Draw,
    X_Wins,
    O_Wins
};

/*
struct Board {
    enum TTTMark position[3][3];
};
*/

struct TTTState {
    /*struct Board board; */
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

#endif
