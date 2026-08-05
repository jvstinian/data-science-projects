#define _GNU_SOURCE /* getopt */
/* #define _POSIX_C_SOURCE 2 getopt */
#include <stdio.h>
#include <unistd.h>
#include <ctype.h> /* tolower */
#include "reinforcementlearning/envs/linewalk.h"
#include "reinforcementlearning/envs/frozenlake.h"
#include "reinforcementlearning/envs/carrental.h"
#include "reinforcementlearning/algorithms/mcts_unbounded.h"

enum Environment {
    ENV_LINEWALK,
    ENV_FROZENLAKE,
    ENV_CARRENTAL,
    ENV_UNKNOWN
};

const char *env_names[4] = {
    "linewalk",
    "frozenlake",
    "carrental",
    "unknown"
};

enum Algorithm {
    ALG_RL_RANDOM_ACTIONS,
    ALG_DP_POLICY_ITERATION,
    ALG_UNKNOWN
};

const char *alg_names[3] = {
    "rl_random_actions",
    "dp_policy_iteration",
    "unknown"
};

struct RunConfig {
    enum Environment env;
    enum Algorithm alg;
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

int process_environment_value(char *argv, struct RunConfig *run_config) {
    char env_value[16];
    size_t i;
    strncpy(env_value, argv, sizeof(env_value) - 1);
    env_value[sizeof(env_value) - 1] = '\0'; /* Ensure null-termination */
    for (i = 0; i < sizeof(env_value); i++) {
        env_value[i] = (char) tolower((int) env_value[i]);
    }
    if (strcmp(env_value, "linewalk") == 0) {
        run_config->env = ENV_LINEWALK;
    } else if (strcmp(env_value, "frozenlake") == 0) {
        run_config->env = ENV_FROZENLAKE;
    } else if (strcmp(env_value, "carrental") == 0) {
        run_config->env = ENV_CARRENTAL;
    } else {
        run_config->env = ENV_UNKNOWN;
        fprintf(stderr, "Unknown environment: %s\n", argv);
        return 1;
    }
    return 0;
}

int process_algorithm_value(char *argv, struct RunConfig *run_config) {
    char alg_value[24];
    size_t i;
    strncpy(alg_value, argv, sizeof(alg_value) - 1);
    alg_value[sizeof(alg_value) - 1] = '\0'; /* Ensure null-termination */
    for (i = 0; i < sizeof(alg_value); i++) {
        alg_value[i] = (char) tolower((int) alg_value[i]);
    }
    if (strcmp(alg_value, "rl_random_actions") == 0) {
        run_config->alg = ALG_RL_RANDOM_ACTIONS;
    } else if (strcmp(alg_value, "dp_policy_iteration") == 0) {
        run_config->alg = ALG_DP_POLICY_ITERATION;
    } else {
        run_config->alg = ALG_UNKNOWN;
        fprintf(stderr, "Unknown algorithm: %s\n", argv);
        return 1;
    }
    return 0;
}

void print_help() {
    printf("Usage: program [-h] [-e environment] [-a algorithm]\n");
    printf("Options:\n");
    printf("  -h               Show this help message\n");
    printf("  -e environment   Specify the environment.  Possible values:\n");
    printf("                     linewalk, frozenlake, carrental\n");
    printf("  -a algorithm     Specify the algorithm.  Possible values:\n");
    printf("                     rl_random_actions, dp_policy_iteration\n");
}

Boolean supported_env_alg[ENV_UNKNOWN][ALG_UNKNOWN] = {
    {  TRUE, FALSE },  /* ENV_LINEWALK */
    { FALSE,  TRUE },   /* ENV_FROZENLAKE */
    { FALSE,  TRUE }   /* ENV_CARRENTAL */
};

int process_arguments(int argc, char *argv[], struct RunConfig *run_config) {
    int opt;
    int i;
    (void)run_config; /* Suppress unused variable warning */
    
    while ((opt = getopt(argc, argv, "he:a:")) != -1) {
        switch (opt) {
            case 'h':
                print_help();
                exit(0);
            case 'e':
                /* printf("Environment specified: %s\n", optarg); */
                if (process_environment_value(optarg, run_config)) {
                    return 1; /* Error processing environment value */
                }
                break;
            case 'a':
                /* printf("Algorithms specified: %s\n", optarg); */
                if (process_algorithm_value(optarg, run_config)) {
                    return 1; /* Error processing algorithm value */
                }
                break;
            case '?': /* Unknown option or missing required argument */
                fprintf(stderr, "Unknown option or missing argument: -%c\n", optopt);
                return 1;
        }
    }

    if (optind < argc) {
        fprintf(stderr, "Non-option arguments:\n");
        for (i = optind; i < argc; i++) {
            printf("  %s\n", argv[i]);
        }
        return 1;
    }
    
    /* Additional validation */
    if (run_config->env == ENV_UNKNOWN && run_config->alg == ALG_UNKNOWN) {
        fprintf(stderr, "Environment and algorithm must be specified.\n");
        print_help();
        return 1;
    } else if (run_config->env == ENV_UNKNOWN) {
        fprintf(stderr, "The algorithm %s only supports the following environments:\n", alg_names[run_config->alg]);
        for (i = 0; i < (int)ENV_UNKNOWN; i++) {
            if (supported_env_alg[i][run_config->alg]) {
                fprintf(stderr, "  %s\n", env_names[i]);
            }
        }
        return 2;
    } else if (run_config->alg == ALG_UNKNOWN) {
        fprintf(stderr, "The environment %s only supports the following algorithms:\n", env_names[run_config->env]);
        for (i = 0; i < (int)ALG_UNKNOWN; i++) {
            if (supported_env_alg[run_config->env][i]) {
                fprintf(stderr, "  %s\n", alg_names[i]);
            }
        }
        return 2;
    } else if (!supported_env_alg[run_config->env][run_config->alg]) {
        fprintf(stderr, "The specified environment and algorithm combination is not supported.\n");
        return 3;
    }
    return 0;
}

int main(int argc, char *argv[]) {
    struct RunConfig run_config = { ENV_UNKNOWN, ALG_UNKNOWN };
    /*
    return linewalk_example();
    return frozenlake_mc_policy_evaluation_example();
    */
    if (process_arguments(argc, argv, &run_config) != 0) {
        return 1;
    }
    carrental_dp_example();
    frozenlake_example_main();
    return uct_main();
}

