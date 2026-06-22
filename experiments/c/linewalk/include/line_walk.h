typedef struct LineWalkConfig {
    int N; /* Number of positions in the line */
} LineWalkConfig;

typedef enum StateKind {
    ACTIVE,
    TERMINAL
} StateKind;

typedef enum Action {
    MOVE_LEFT,
    MOVE_RIGHT
} Action;

typedef enum Player {
    PLAYER1
} Player;

typedef enum Boolean {
    FALSE,
    TRUE
} Boolean;

typedef struct LineWalkState {
    LineWalkConfig config;
    StateKind kind;
    unsigned short int position;
    int reward;
} LineWalkState;

LineWalkState initial_state(LineWalkConfig config);
Boolean is_terminal (LineWalkState state);
Player get_player(LineWalkState state);
LineWalkState step(LineWalkState state, Action action);
float reward(Player player, LineWalkState state);
unsigned int get_available_actions (LineWalkState state, Action *available_actions, unsigned int* num_actions);
void print_state(LineWalkState state);

void take_random_action(const LineWalkConfig* config, unsigned int max_steps);

#define CONFIG_TYPE LineWalkConfig
#include "random_action_h.inc"
#undef CONFIG_TYPE

int main();

