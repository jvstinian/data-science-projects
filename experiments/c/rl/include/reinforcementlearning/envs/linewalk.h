#include <stddef.h>

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

/* MCTS Interface */
LineWalkState initial_state(LineWalkConfig config);
Boolean is_terminal (LineWalkState state);
Player get_player(LineWalkState state);
LineWalkState step(LineWalkState state, Action action);
float reward(Player player, LineWalkState state);
unsigned int get_available_actions (LineWalkState state, Action *available_actions, unsigned int* num_actions);
enum Action linewalk_mctsenv_get_random_action(LineWalkState state);
void print_state(LineWalkState state);

/* RL Interface */
struct LineWalkObservation {
    unsigned short int position;
};

/* LineWalkEnvironment declaration */
struct LineWalkEnvironment;

struct LineWalkStepReturn {
    struct LineWalkObservation observation;
    float reward;
    Boolean terminated;
};
 
struct LineWalkEnvironment* linewalk_make(struct LineWalkConfig config);
int linewalk_init(struct LineWalkConfig config, struct LineWalkEnvironment* env);
/* TODO: Add support for a seed reset */
struct LineWalkObservation linewalk_reset(struct LineWalkEnvironment* env);
struct LineWalkStepReturn linewalk_step(struct LineWalkEnvironment* env, enum Action action);
void linewalk_deinit(struct LineWalkEnvironment* env);
void linewalk_close(struct LineWalkEnvironment* env);

void linewalk_mctsenv_uniform_random_actions(const LineWalkConfig* config, unsigned int max_steps);


struct SimulationSummary {
    size_t num_steps;
    float total_reward;
};

enum Action linewalk_get_random_action(struct LineWalkEnvironment* env);
struct SimulationSummary linewalk_uniform_random_actions(struct LineWalkConfig config, Boolean verbose);

#define CONFIG_TYPE LineWalkConfig
#include "reinforcementlearning/algorithms/random_action_h.inc"
#undef CONFIG_TYPE

int main();

