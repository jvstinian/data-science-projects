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
    ALG_DP_VALUE_ITERATION,
    ALG_MCTS_UCT,
    ALG_UNKNOWN
};

const char *alg_names[5] = {
    "rl_random_actions",
    "dp_policy_iteration",
    "dp_value_iteration",
    "mcts_uct",
    "unknown"
};

struct RunConfig {
    enum Environment env;
    enum Algorithm alg;
};

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
    } else if (strcmp(alg_value, "dp_value_iteration") == 0) {
        run_config->alg = ALG_DP_VALUE_ITERATION;
    } else if (strcmp(alg_value, "mcts_uct") == 0) {
        run_config->alg = ALG_MCTS_UCT;
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
    {  TRUE, FALSE, FALSE,  TRUE },  /* ENV_LINEWALK */
    { FALSE,  TRUE, FALSE, FALSE },  /* ENV_FROZENLAKE */
    { FALSE,  TRUE, FALSE, FALSE }   /* ENV_CARRENTAL */
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
    if (process_arguments(argc, argv, &run_config) != 0) {
        return 1;
    }
    switch (run_config.env) {
        case ENV_LINEWALK:
            switch (run_config.alg) {
                case ALG_MCTS_UCT:
                    return uct_main();
                case ALG_RL_RANDOM_ACTIONS:
                case ALG_DP_POLICY_ITERATION:
                case ALG_DP_VALUE_ITERATION:
                case ALG_UNKNOWN:  /* Should be unreachable */
                default:
                    fprintf(stderr, "Unsupported algorithm for Linewalk environment.\n");
                    return 1;
            }
        case ENV_CARRENTAL:
            switch (run_config.alg) {
                case ALG_DP_POLICY_ITERATION:
                    return carrental_dp_example();
                case ALG_RL_RANDOM_ACTIONS:
                case ALG_MCTS_UCT:
                case ALG_DP_VALUE_ITERATION:
                case ALG_UNKNOWN:  /* Should be unreachable */
                default:
                    fprintf(stderr, "Unsupported algorithm for Carrental environment.\n");
                    return 1;
            }
        case ENV_FROZENLAKE:
        case ENV_UNKNOWN:  /* Should be unreachable */
        default:
            fprintf(stderr, "No example for environment %s and algorithm %s\n", 
                    env_names[run_config.env], alg_names[run_config.alg]
            );
            return 1;
    }
}

