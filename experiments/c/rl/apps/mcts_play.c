#define _GNU_SOURCE /* getopt */
#include <stdio.h>
#include <stdlib.h> /* exit */
#include <unistd.h>
#include <string.h> /* strncpy */
#include <ctype.h> /* tolower */
#include <reinforcementlearning/envs/ttt.h>
#include <reinforcementlearning/envs/ataxx.h>
#include <reinforcementlearning/envs/hex.h>

enum Environment {
    ENV_TTT,
    ENV_ATAXX,
    ENV_HEX,
    ENV_UNKNOWN
};

const char *env_names[3] = {
    "ttt",
    "ataxx",
    "hex"
};

struct TTTRunConfig {
    enum TTTPlayer player;
};

struct AtaxxRunConfig {
    enum AtaxxPlayer player;
};

struct HexRunConfig {
    enum HexPlayer player;
};

void print_help() {
    printf("Usage: mcts-play ENVIRONMENT [-h] [ENVIRONMENT_ARGS]\n");
    printf("Options:\n");
    printf("  -h               Show this help message\n");
    printf("\n");
    printf("ENVIRONMENT must be one of ttt, ataxx, hex\n");
    printf("ENVIRONMENT_ARGS depend on the value of ENVIRONMENT\n");
}

int process_environment_value(const char *argv, enum Environment* env) {
    char env_value[6];
    size_t i;
    strncpy(env_value, argv, sizeof(env_value) - 1);
    env_value[sizeof(env_value) - 1] = '\0'; /* Ensure null-termination */
    for (i = 0; i < sizeof(env_value); i++) {
        env_value[i] = (char) tolower((int) env_value[i]);
    }
    if (strcmp(env_value, "ttt") == 0) {
        *env = ENV_TTT;
    } else if (strcmp(env_value, "ataxx") == 0) {
        *env = ENV_ATAXX;
    } else if (strcmp(env_value, "hex") == 0) {
        *env = ENV_HEX;
    } else {
        *env = ENV_UNKNOWN;
        fprintf(stderr, "Error: unknown environment %s\n", argv);
        print_help();
        return 1;
    }
    return 0;
}

int process_ttt_player_value(const char *argv, enum TTTPlayer* player) {
    if (((char) tolower((int) argv[0])) == 'x') {
        *player = PlayerX;
    } else if (((char) tolower((int) argv[0])) == 'o') {
        *player = PlayerO;
    } else {
        fprintf(stderr, "Error: invalid tic tac toe player %s\n", argv);
        /* TODO print_help(); */
        return 1;
    }
    return 0;
}

int process_ttt_arguments(int argc, char *argv[], struct TTTRunConfig* run_config) {
    int opt;
    /* int i; */
    /* (void)run_config; Suppress unused variable warning */
    
    while ((opt = getopt(argc, argv, "hp:")) != -1) {
        switch (opt) {
            case 'h':
                print_help();
                exit(0);
            case 'p':
                process_ttt_player_value(optarg, &run_config->player);
                /*
                fprintf(stderr, "player specified: %s\n", optarg);
                */
                /* Error processing environment value */
                /*
                if (process_environment_value(optarg, run_config)) {
                    return 1; 
                }
                */
                break;
            case '?': /* Unknown option or missing required argument */
                fprintf(stderr, "Unknown option or missing argument: -%c\n", optopt);
                return 1;
        }
    }

    /*
    if (optind < argc) {
        fprintf(stderr, "Non-option arguments:\n");
        for (i = optind; i < argc; i++) {
            printf("  %s\n", argv[i]);
        }
        return 1;
    }
    */
    return 0;
}

#include <math.h>
int ttt_uct_example2(struct TTTRunConfig run_config) {
    struct UCTParams uctparams = { sqrt (2.0) };
    struct TTTConfig config; /* No configuration needed for tic-tac-toe */
    struct TTTAction a;
    float reward_est;
    struct TTTState s;
    enum TTTPlayer p;
    struct TTTTree* tree = ttt_mcts_tree_new(config);
    unsigned int p_row, p_col;

    /* TODO: Taking a bad initial action 
    if (ttt_uct_take_action(tree, (struct TTTAction) {1, 1})) {
        fprintf(stderr, "ttt_uct_example: failed to take initial action\n");
    }
    if (ttt_uct_take_action(tree, (struct TTTAction) {1, 0})) {
        fprintf(stderr, "ttt_uct_example: failed to take initial action\n");
    }
    */

    s = ttt_uct_get_state(tree);
    p = get_player(s);
    print_state(s);
    while (!is_terminal(s)) {
        if (p == run_config.player) {
            printf("Enter action for player in the following form: row col\n");
            while (1) {
                if (scanf("%u %u", &p_row, &p_col) != 2) {
                    fprintf(stderr, "Error reading input. Please enter two integers.\n");
                    continue;
                }
                if (p_row > 2 || p_col > 2) {
                    fprintf(stderr, "Invalid action. Row and column must be between 0 and 2.\n");
                    continue;
                } else {
                    a = (struct TTTAction) {p_row, p_col};
                    break;
                }
            }
            ttt_uct_take_action(tree, a);
        } else {
            if (ttt_uct_search(10000, uctparams, tree, &a, &reward_est)) {
                /* Encountered an error during search */
                break;
            }
            /* TODO
            printf("Number of visits after search: %u\n", tree_visits(*tree));
            */
            printf("Player %d Took action (%u, %u) with reward estimate %f\n", p, a.row, a.col, reward_est);
        }
        s = ttt_uct_get_state(tree);
        p = get_player(s);
        print_state(s);
    }
    ttt_mcts_tree_free(tree);
    return 0;
};


int main(int argc, char *argv[]) {
    enum Environment env = ENV_UNKNOWN;
    if (argc < 2) {
        fprintf(stderr, "Error: ENVIRONMENT argument is required.\n");
        print_help();
        return 1;
    }
    if (process_environment_value(argv[1], &env)) {
        return 1; /* Error processing environment value */
    }
    switch (env) {
        case ENV_TTT:
            struct TTTRunConfig ttt_config;
            process_ttt_arguments(argc, argv, &ttt_config);
            return ttt_uct_example2(ttt_config);
        case ENV_ATAXX:
            return ataxx_uct_example();
        case ENV_HEX:
            return hex_uct_example();
        case ENV_UNKNOWN:
        default:
            /* Should be unreachable */
            return 1;
    }
}
