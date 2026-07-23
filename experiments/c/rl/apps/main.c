#include <stdio.h>
#include "reinforcementlearning/envs/linewalk.h"
#include "reinforcementlearning/envs/frozenlake.h"

enum Environments {
    LINEWALK,
    FROZENLAKE
};

enum Algorithms {
    RL_RANDOM_ACTIONS
};

struct RunConfig {
    enum Environments env_name;
    enum Algorithms algorithm_name;
};

int linewalk_example() {
    LineWalkConfig config = { .N = 5 }; /* Example configuration with 5 positions */
    LineWalkState state = initial_state(config);
    
    /* Example usage
     * Print initial state */
    printf("Initial position: %d\n", state.position);
    
    /* Step right */
    state.position += 1;
    printf("Position after moving right: %d\n", state.position);
    print_state(state);
    
    /* Step left */
    state.position -= 1;
    printf("Position after moving left: %d\n", state.position);
    print_state(state);

    enum LineWalkAction available_actions[2];
    unsigned int num_actions = 0;
    get_available_actions(state, available_actions, &num_actions);
    printf("Number of available actions: %u\n", num_actions);
    printf("Available actions: ");
    size_t i;
    for (i = 0; i < num_actions; i++) {
        if (available_actions[i] == MOVE_LEFT) {
            printf("MOVE_LEFT, ");
        } else if (available_actions[i] == MOVE_RIGHT) {
            printf("MOVE_RIGHT, ");
        }
    }
  
    linewalk_mctsenv_uniform_random_actions(&config, 20);

    struct SimulationSummary simsum = linewalk_uniform_random_actions(config, TRUE);
    printf("Simulation Summary: Steps = %lu, Total Reward = %5.2f\n", simsum.num_steps, simsum.total_reward);
 
    return 0;
}

int frozenlake_mc_policy_evaluation_example() {
    unsigned int s;
    struct FrozenlakeConfig config = { MAP_4X4, FALSE };
    enum FrozenlakeAction dpolicy[16];
    dpolicy[0] = DOWN;
    dpolicy[1] = RIGHT;
    dpolicy[2] = DOWN;
    dpolicy[3] = LEFT;
    dpolicy[4] = DOWN;
    dpolicy[5] = LEFT;
    dpolicy[6] = DOWN;
    dpolicy[7] = LEFT;
    dpolicy[8] = RIGHT;
    dpolicy[9] = DOWN;
    dpolicy[10] = DOWN;
    dpolicy[11] = LEFT;
    dpolicy[12] = LEFT;
    dpolicy[13] = RIGHT;
    dpolicy[14] = RIGHT;
    dpolicy[15] = LEFT;
    struct MCConfig mc_config = { 100, 50, FIRST_VISIT, 0.9 };
    float svalue_func[16]; /* TODO: Avoiding allocation */
    int status = frozenlake_mc_policy_evaluation(config, dpolicy, mc_config, svalue_func);
    for (s = 0; s < 16; s++) {
        printf("%d: %d, %.4f\n", s, dpolicy[s], svalue_func[s]);
    }
    return status;
}

int main() {
    /*
    return linewalk_example();
    */
    return frozenlake_mc_policy_evaluation_example();
}

