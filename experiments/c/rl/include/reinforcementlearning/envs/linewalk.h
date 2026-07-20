#include <stddef.h>

typedef struct LineWalkConfig {
    int N; /* Number of positions in the line */
} LineWalkConfig;

typedef enum StateKind {
    ACTIVE,
    TERMINAL
} StateKind;

enum LineWalkAction {
    MOVE_LEFT,
    MOVE_RIGHT
};

enum LineWalkPlayer {
    PLAYER1
};

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
enum LineWalkPlayer get_player(LineWalkState state);
LineWalkState step(LineWalkState state, enum LineWalkAction action);
float reward(enum LineWalkPlayer player, LineWalkState state);
unsigned int get_available_actions (LineWalkState state, enum LineWalkAction *available_actions, unsigned int* num_actions);
enum LineWalkAction linewalk_mctsenv_get_random_action(LineWalkState state);
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
struct LineWalkStepReturn linewalk_step(struct LineWalkEnvironment* env, enum LineWalkAction action);
void linewalk_deinit(struct LineWalkEnvironment* env);
void linewalk_close(struct LineWalkEnvironment* env);

void linewalk_mctsenv_uniform_random_actions(const LineWalkConfig* config, unsigned int max_steps);


struct SimulationSummary {
    size_t num_steps;
    float total_reward;
};

enum LineWalkAction linewalk_get_random_action(struct LineWalkEnvironment* env);
struct SimulationSummary linewalk_uniform_random_actions(struct LineWalkConfig config, Boolean verbose);

#define CONFIG_TYPE LineWalkConfig
#include "reinforcementlearning/algorithms/random_action_h.inc"
#undef CONFIG_TYPE

/* TODO: Remove when ready 
int main();
*/

