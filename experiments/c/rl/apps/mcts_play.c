#define _GNU_SOURCE /* getopt */
#include <reinforcementlearning/envs/ttt.h>
#include <reinforcementlearning/envs/ataxx.h>
#include <reinforcementlearning/envs/hex.h>
#include <stdio.h>
#include <stdlib.h> /* exit */
#include <unistd.h>
#include <string.h> /* strncpy */
#include <ctype.h> /* tolower */
#include <limits.h> /* UINT_MAX */
#include <math.h> /* sqrt */

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
    unsigned int uct_search_size;
};

struct AtaxxRunConfig {
    enum AtaxxPlayer player;
    unsigned int uct_search_size;
};

struct HexRunConfig {
    enum HexPlayer player;
    unsigned int uct_search_size;
};

void print_help() {
    printf("Usage: mcts-play ENVIRONMENT [-h] [ENVIRONMENT_ARGS]\n");
    printf("Options:\n");
    printf("  -h               Show this help message\n");
    printf("\n");
    printf("ENVIRONMENT must be one of ttt, ataxx, hex\n");
    printf("ENVIRONMENT_ARGS depend on the value of ENVIRONMENT\n");
}

void print_ttt_help() {
    printf("Usage: mcts-play ttt [-h] [ENVIRONMENT_ARGS]\n");
    printf("Options:\n");
    printf("  -h               Show this help message\n");
    printf("  -p               The player to play as, valid values\n");
    printf("                     are x and o\n");
    printf("  -s               Number of UCT search iterations to perform\n");
    printf("                     per move\n");
    printf("\n");
}

void print_ataxx_help() {
    printf("Usage: mcts-play ataxx [-h] [ENVIRONMENT_ARGS]\n");
    printf("Options:\n");
    printf("  -h               Show this help message\n");
    printf("  -p               The player to play as, valid values\n");
    printf("                     are red and blue for a 2 player game,\n");
    printf("                     and additionally white and black for a\n");
    printf("                     a 4 player game\n");
    printf("  -s               Number of UCT search iterations to perform\n");
    printf("                     per move\n");
    printf("\n");
}

void print_hex_help() {
    printf("Usage: mcts-play hex [-h] [ENVIRONMENT_ARGS]\n");
    printf("Options:\n");
    printf("  -h               Show this help message\n");
    printf("  -p               The player to play as, valid values\n");
    printf("                     are 1 and 2\n");
    printf("  -s               Number of UCT search iterations to perform\n");
    printf("                     per move\n");
    printf("\n");
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

int process_uct_search_size(const char *argv, unsigned int* n) {
    unsigned long val = strtoul(argv, NULL, 10);

    if (val > UINT_MAX) {
        fprintf(stderr, "process_uct_search_size: value %lu exceeds maximum unsigned int\n", val);
        return 1;
    }
    *n = (unsigned int) val;
    return 0;
}

int process_ttt_player_value(const char *argv, enum TTTPlayer* player) {
    if (((char) tolower((int) argv[0])) == 'x') {
        *player = PlayerX;
    } else if (((char) tolower((int) argv[0])) == 'o') {
        *player = PlayerO;
    } else {
        fprintf(stderr, "Error: invalid tic tac toe player %s\n", argv);
        return 1;
    }
    return 0;
}

int process_ttt_arguments(int argc, char *argv[], struct TTTRunConfig* run_config) {
    int opt;
    
    while ((opt = getopt(argc, argv, "hp:s:")) != -1) {
        switch (opt) {
            case 'h':
                print_ttt_help();
                exit(0);
            case 'p':
                if (process_ttt_player_value(optarg, &run_config->player)) {
                    print_ttt_help();
                    exit(1);
                }
                break;
            case 's':
                if (process_uct_search_size(optarg, &run_config->uct_search_size)) {
                    print_ttt_help();
                    exit(1);
                }
                break;
            case '?': /* Unknown option or missing required argument */
                fprintf(stderr, "Unknown option or missing argument: -%c\n", optopt);
                return 1;
        }
    }

    return 0;
}

int process_ataxx_player_value(const char *argv, enum AtaxxPlayer* player) {
    char player_value[6];
    size_t i;
    strncpy(player_value, argv, sizeof(player_value) - 1);
    player_value[sizeof(player_value) - 1] = '\0'; /* Ensure null-termination */
    for (i = 0; i < sizeof(player_value); i++) {
        player_value[i] = (char) tolower((int) player_value[i]);
    }
    if (strcmp(player_value, "red") == 0) {
        *player = Ataxx_Red;
    } else if (strcmp(player_value, "blue") == 0) {
        *player = Ataxx_Blue;
    } else if (strcmp(player_value, "white") == 0) {
        *player = Ataxx_White;
    } else if (strcmp(player_value, "black") == 0) {
        *player = Ataxx_Black;
    } else {
        fprintf(stderr, "Error: invalid ataxx player %s\n", argv);
        return 1;
    }
    return 0;
}

int process_ataxx_arguments(int argc, char *argv[], struct AtaxxRunConfig* run_config) {
    int opt;
    
    while ((opt = getopt(argc, argv, "hp:s:")) != -1) {
        switch (opt) {
            case 'h':
                print_ataxx_help();
                exit(0);
            case 'p':
                if (process_ataxx_player_value(optarg, &run_config->player)) {
                    print_ataxx_help();
                    return 1; 
                }
                break;
            case 's':
                if (process_uct_search_size(optarg, &run_config->uct_search_size)) {
                    print_ttt_help();
                    exit(1);
                }
                break;
            case '?': /* Unknown option or missing required argument */
                fprintf(stderr, "Unknown option or missing argument: -%c\n", optopt);
                return 1;
        }
    }

    return 0;
}

int process_hex_player_value(const char *argv, enum HexPlayer* player) {
    if (((char) tolower((int) argv[0])) == '1') {
        *player = Player1;
    } else if (((char) tolower((int) argv[0])) == '2') {
        *player = Player2;
    } else {
        fprintf(stderr, "Error: invalid hex player %s\n", argv);
        return 1;
    }
    return 0;
}

int process_hex_arguments(int argc, char *argv[], struct HexRunConfig* run_config) {
    int opt;
    
    while ((opt = getopt(argc, argv, "hp:s:")) != -1) {
        switch (opt) {
            case 'h':
                print_hex_help();
                exit(0);
            case 'p':
                if (process_hex_player_value(optarg, &run_config->player)) {
                    print_hex_help();
                    return 1; 
                }
                break;
            case 's':
                if (process_uct_search_size(optarg, &run_config->uct_search_size)) {
                    print_ttt_help();
                    exit(1);
                }
                break;
            case '?': /* Unknown option or missing required argument */
                fprintf(stderr, "Unknown option or missing argument: -%c\n", optopt);
                return 1;
        }
    }

    return 0;
}

int ttt_uct_example(struct TTTRunConfig run_config) {
    struct UCTParams uctparams = { sqrt (2.0) };
    struct TTTConfig config; /* No configuration needed for tic-tac-toe */
    struct TTTAction a;
    float reward_est;
    struct TTTState s;
    enum TTTPlayer p;
    struct TTTTree* tree = ttt_mcts_tree_new(config);
    unsigned int p_row, p_col;

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
            if (ttt_uct_search(run_config.uct_search_size, uctparams, tree, &a, &reward_est)) {
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

int ataxx_uct_example(struct AtaxxRunConfig run_config) {
    struct UCTParams uctparams = { sqrt (2.0) };
    struct AtaxxConfig config = { Two_Player };
    struct AtaxxAction a;
    float reward_est;
    struct AtaxxState s;
    enum AtaxxPlayer p;
    unsigned int p_srow, p_scol, p_drow, p_dcol;
    char omit;
    struct AtaxxTree* tree = ataxx_mcts_tree_new(config);

    s = ataxx_uct_get_state(tree);
    p = ataxx_get_player(s);
    ataxx_print_state(s);
    while (!ataxx_is_terminal(s)) {
        if (p == run_config.player) {
            printf("Enter action for player in the following form: source_row source_col dest_row dest_col\n");
            while (1) {
                if (scanf("%u %u %u %u", &p_srow, &p_scol, &p_drow, &p_dcol) != 4) {
                    fprintf(stderr, "Error reading input. Please enter four integers.\n");
                    while ((omit = getchar()) != '\n' && omit != EOF); 
                    continue;
                }
                if (p_srow > BOARD_WIDTH || p_scol > BOARD_WIDTH || p_drow > BOARD_WIDTH || p_dcol > BOARD_WIDTH) {
                    fprintf(stderr, "Invalid action. Row and column must be between 0 and %u.\n", BOARD_WIDTH - 1);
                    continue;
                } else {
                    a = (struct AtaxxAction) { 
                        (struct AtaxxCellIndices) {p_srow, p_scol},
                        (struct AtaxxCellIndices) {p_drow, p_dcol}
                    };
                    break;
                }
            }
            ataxx_uct_take_action(tree, a);
        } else {
            if (ataxx_uct_search(run_config.uct_search_size, uctparams, tree, &a, &reward_est)) {
                /* Encountered an error during search */
                break;
            }
            /* TODO
            printf("Number of visits after search: %u\n", tree_visits(*tree));
            */
            printf("Player %d Took action (%u, %u) -> (%u, %u) with reward estimate %f\n",
                    p,
                    a.source.row, a.source.col,
                    a.target.row, a.target.col,
                    reward_est
            );
        }
        s = ataxx_uct_get_state(tree);
        p = ataxx_get_player(s);
        ataxx_print_state(s);
    }
    ataxx_mcts_tree_free(tree);
    return 0;
};

int hex_uct_example(struct HexRunConfig run_config) {
    struct UCTParams uctparams = { sqrt (2.0) };
    struct HexConfig config;
    struct HexAction a;
    float reward_est;
    struct HexState s;
    enum HexPlayer p;
    unsigned int p_row, p_col;
    struct HexTree* tree = hex_mcts_tree_new(config);

    s = hex_uct_get_state(tree);
    p = hex_get_player(s);
    hex_print_state(s);
    while (!hex_is_terminal(s)) {
        if (p == run_config.player) {
            printf("Enter action for player in the following form: row col\n");
            while (1) {
                if (scanf("%u %u", &p_row, &p_col) != 2) {
                    fprintf(stderr, "Error reading input. Please enter two integers.\n");
                    continue;
                }
                if (p_row > BOARD_WIDTH || p_col > BOARD_WIDTH) {
                    fprintf(stderr, "Invalid action. Row and column must be between 0 and %u.\n", BOARD_WIDTH - 1);
                    continue;
                } else {
                    a = (struct HexAction) { p_row, p_col };
                    break;
                }
            }
            hex_uct_take_action(tree, a);
        } else {
            if (hex_uct_search(run_config.uct_search_size, uctparams, tree, &a, &reward_est)) {
                /* Encountered an error during search */
                break;
            }
            /* TODO
            printf("Number of visits after search: %u\n", tree_visits(*tree));
            */
            printf("Player %d took action (%u, %u) with reward estimate %f\n",
                    p,
                    a.row, a.col,
                    reward_est
            );
        }
        s = hex_uct_get_state(tree);
        p = hex_get_player(s);
        hex_print_state(s);
    }
    hex_mcts_tree_free(tree);
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
            struct TTTRunConfig ttt_config = { PlayerX, 1000 }; /* Default values */
            process_ttt_arguments(argc, argv, &ttt_config);
            return ttt_uct_example(ttt_config);
        case ENV_ATAXX:
            struct AtaxxRunConfig ataxx_config = { Ataxx_Red, 10000 }; /* Default values */
            process_ataxx_arguments(argc, argv, &ataxx_config);
            return ataxx_uct_example(ataxx_config);
        case ENV_HEX:
            struct HexRunConfig hex_config = { Player1, 10000 }; /* Default values */
            process_hex_arguments(argc, argv, &hex_config);
            return hex_uct_example(hex_config);
        case ENV_UNKNOWN:
        default:
            /* Should be unreachable */
            return 1;
    }
}
