#include <stddef.h> /* size_t */

typedef enum Boolean {
    FALSE,
    TRUE
} Boolean;

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
struct CellIndices {
    unsigned short row;
    unsigned short col;
};

struct AtaxxAction {
    struct CellIndices source;
    struct CellIndices target;
};

/*
struct ValidActions;
    type Valid_Actions_Type is array (Natural range <>) of Action_Type;
*/
struct ValidActionsList;
typedef struct ValidActionsList ValidActionsList;
ValidActionsList* ataxx_actions_list_create(size_t cpty);
int ataxx_actions_list_realloc(ValidActionsList** lpp, size_t new_capacity);
int ataxx_actions_list_push(ValidActionsList** lpp, struct AtaxxAction val);
size_t ataxx_actions_list_length(ValidActionsList* lp);
struct AtaxxAction ataxx_actions_list_get(ValidActionsList* lp, size_t i);
void ataxx_actions_list_destroy(ValidActionsList* lp);

enum AtaxxMark {
    Mark_Red,
    Mark_Blue,
    Mark_White,
    Mark_Black,
    Mark_X,
    No_Mark
};

enum GameStatus {
    Active,
    Finished
};
/*
    type Game_Score_Type is array (Player_Type) of Reward_Type;
    type Board_Type is array (Axis_Label, Axis_Label) of Mark;
*/

enum PlayerCount {
    Two_Player,
    Four_Player
};
/*
    type Player_Indicator_Type is array (Player_Type) of Boolean;
*/

struct AtaxxConfig {
    enum PlayerCount player_count;
};

struct AtaxxState {
    Boolean player_indicators[MAX_PLAYER_COUNT];
    enum AtaxxMark board[BOARD_WIDTH][BOARD_WIDTH];
    enum GameStatus status;
    unsigned short int scores[MAX_PLAYER_COUNT];
    enum AtaxxPlayer current_player;
};

struct AtaxxState initial_state(struct AtaxxConfig config);
Boolean is_terminal (struct AtaxxState state);
enum AtaxxPlayer get_player(struct AtaxxState state);
struct AtaxxState step(struct AtaxxState state, struct AtaxxAction action);
float reward(enum AtaxxPlayer player, struct AtaxxState state);
ValidActionsList* get_valid_actions (struct AtaxxState state);

void print_state(struct AtaxxState state);

int main();

