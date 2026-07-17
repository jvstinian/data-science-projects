typedef enum Boolean {
    FALSE,
    TRUE
} Boolean;

#define BOARD_WIDTH 7
#define BOARD_SIZE (BOARD_WIDTH * BOARD_WIDTH)

enum HexPlayer {
    Player1,
    Player2
};

const char* player_names[2] = {"Player 1", "Player 2"};

enum StoneColor {
    Red,
    Blue
};

struct Action {
    unsigned short row;
    unsigned short col;
};

struct ValidActions {
    unsigned short int num_actions;
    struct Action actions[BOARD_SIZE];
};
   
enum Mark {
    Red_Stone,
    Blue_Stone,
    No_Mark
};

enum GameStatus {
    Active,
    Player1_Wins,
    Player2_Wins
};

struct State {
    enum StoneColor player_colors[2];
    enum Mark board[BOARD_WIDTH][BOARD_WIDTH];
    enum GameStatus status;
    enum HexPlayer current_player;
};

/*
   subtype Blue_Label is Integer range 1 .. Board_Width;
   subtype Red_Label is Character range 'A' .. Character'Val(Character'Pos('A') + Board_Width - 1);
*/

struct State initial_state();
Boolean is_terminal(struct State s);
enum HexPlayer get_player(struct State s);
struct State step(struct State s, struct Action a);
float reward(enum HexPlayer player, struct State s);
struct ValidActions get_valid_actions(struct State s);
void print_state (struct State s);
/*
private
   function Get_Number_Of_Stones (State : State_Type) return Natural;
   function Check_Win (Board : Board_Type; Stone : Stone_Color_Type) return Boolean;
   function Check_Blue_Win (Board : Board_Type) return Boolean;
   function Check_Red_Win (Board : Board_Type) return Boolean;
   
*/
/* NOTE: The following method was not needed after all */
Boolean neighboring_hexagons(unsigned short b1, unsigned short r1, unsigned short b2, unsigned short r2);

int main();
