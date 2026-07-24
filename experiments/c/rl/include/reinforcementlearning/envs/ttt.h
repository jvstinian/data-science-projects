
typedef enum Boolean {
    FALSE,
    TRUE
} Boolean;

enum TTTPlayer {
    PlayerX,
    PlayerO
};

/*
   type Row_Label is new Integer range 0 .. 2;
   -- We use a custom discrete type for column labels
   type Col_Label is (A, B, C);
*/

struct Action {
    unsigned short row;
    unsigned short col;
};

enum Mark {
    X,
    O,
    No_Mark
};

enum GameStatus {
    X_Move,
    O_Move,
    Draw,
    X_Wins,
    O_Wins
};

/*
struct Board {
    enum Mark position[3][3];
};
*/

struct State {
    /*struct Board board; */
    enum Mark board[3][3];
    enum GameStatus status;
};

struct ValidActions {
    unsigned short int num_actions;
    struct Action actions[9];
};

struct State initial_state();
Boolean is_terminal(struct State s);
enum TTTPlayer get_player(struct State s);
struct State step(struct State s, struct Action a);
float reward(enum TTTPlayer p, struct State s);
struct ValidActions get_valid_actions(struct State s);
void print_state(struct State s);
