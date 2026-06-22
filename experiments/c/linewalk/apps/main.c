#include <stdio.h>
#include "line_walk.h"


int main() {
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

    Action available_actions[2];
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
   
    take_random_action2(&config, 20);
 
    return 0;
}

