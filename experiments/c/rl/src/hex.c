#include <reinforcementlearning/envs/hex.h>
#include <stdio.h>
#include <string.h> /* memset, memcpy */
#include <stdlib.h> /* rand */
#include <assert.h>

static const char* player_names[2] = {"Player 1", "Player 2"};
static const char* color_names[2] = {"red", "blue"};

struct HexState hex_initial_state() {
    /* enum HexMark board[BOARD_WIDTH][BOARD_WIDTH] = {{No_Mark}}; */
    unsigned int b, r;

    struct HexState ret;

    ret.player_colors[0] = Red;
    ret.player_colors[1] = Blue;

    for (b = 0; b < BOARD_WIDTH; b++) {
        for (r = 0; r < BOARD_WIDTH; r++) {
            ret.board[b][r] = No_Mark;
        }
    }

    ret.status = Active;
    ret.current_player = Player1;

    return ret;
}

Boolean hex_is_terminal(struct HexState s) {
    if (s.status != Active) {
        return TRUE;  /* Game is already finished */
    }
    return FALSE;
}

enum HexPlayer hex_get_player(struct HexState s) {
    return s.current_player;
}

static unsigned short get_number_of_stones(struct HexState s) {
    unsigned short count = 0;
    unsigned short b, r;
    for (b = 0; b  < BOARD_WIDTH; b++) {
        for (r = 0; r < BOARD_WIDTH; r++) {
            switch (s.board[b][r]) {
                case Red_Stone:
                case Blue_Stone:
                    count += 1;
                    break;
                case No_Mark:
                    break;
            }
        }
    }
    return count;
}

struct HexPosition {
    unsigned short row;
    unsigned short col;
};

static Boolean expand_to_neighbors(struct HexPosition pos, const enum HexMark (*board)[BOARD_WIDTH], Boolean (*reachable)[BOARD_WIDTH]) {
    struct HexPosition neighbors[6];
    unsigned short num_neighbors = 0;
    enum HexMark mark = board[pos.row][pos.col];
    unsigned short b = pos.row;
    unsigned short r = pos.col;
    unsigned short b1, r1;
    unsigned short n;

    /* For a hexagon (I1, J1), the neighboring hexagons with the blue
     * label I2 where I2 is the successor of I1 are (I2, J1) and (I2, J2),
     * where J2 is the predecessor of J1 (if it exists).
     * Reversing this relationship, when iterating over the
     * red labels for the next blue label, we look back to the
     * at the connection value for the previous blue label and
     * the same and successor red labels. */
    if (b > 0) {
        neighbors[num_neighbors++] = (struct HexPosition) {b - 1, r};  /* Up Left */
    }
    if (b < (BOARD_WIDTH - 1)) {
        neighbors[num_neighbors++] = (struct HexPosition) {b + 1, r};  /* Down Right */
    }
    if (r > 0) {
        neighbors[num_neighbors++] = (struct HexPosition) {b, r - 1};  /* Left */
    }
    if (r < (BOARD_WIDTH - 1)) {
        neighbors[num_neighbors++] = (struct HexPosition) {b, r + 1};  /* Right */
    }
    if ((b > 0) && (r < (BOARD_WIDTH - 1))) {
        neighbors[num_neighbors++] = (struct HexPosition) {b - 1, r + 1};  /* Up Right */
    }
    if ((b < (BOARD_WIDTH - 1)) && (r > 0)) {
        neighbors[num_neighbors++] = (struct HexPosition) {b + 1, r - 1};  /* Down Left */
    }

    for (n = 0; n < num_neighbors; n++) {
        b1 = neighbors[n].row;
        r1 = neighbors[n].col;
        if (!reachable[b1][r1] && (board[b1][r1] == mark)) {
            /* The neighboring hexagon has the same mark has not been visited yet */
            reachable[b1][r1] = TRUE;
            if ((mark == Red_Stone) && (b1 == (BOARD_WIDTH - 1))) {
                return TRUE;  /* Reached the last row, so Red wins */
            } else if ((mark == Blue_Stone) && (r1 == (BOARD_WIDTH - 1))) {
                return TRUE;  /* Reached the last row, so Blue wins */
            } else if (expand_to_neighbors(neighbors[n], board, reachable)) {
                return TRUE;
            }
        }
    }
    return FALSE;
}

static Boolean check_red_win(const enum HexMark (*board)[BOARD_WIDTH]) {
    Boolean reachable[BOARD_WIDTH][BOARD_WIDTH];
    struct HexPosition pos;
    unsigned short r;

    /* Initialize */
    memset(reachable, 0, BOARD_WIDTH * BOARD_WIDTH * sizeof(Boolean));

    for (r = 0; r < BOARD_WIDTH; r++) {
        if (board[0][r] == Red_Stone) {
            reachable[0][r] = TRUE;
            pos = (struct HexPosition) {0, r};
            if (expand_to_neighbors(pos, board, reachable)) {
                return TRUE;
            }
        }
    }
    return FALSE;
}

static Boolean check_blue_win(const enum HexMark (*board)[BOARD_WIDTH]) {
    Boolean reachable[BOARD_WIDTH][BOARD_WIDTH];
    struct HexPosition pos;
    unsigned short b;

    /* Initialize */
    memset(reachable, 0, BOARD_WIDTH * BOARD_WIDTH * sizeof(Boolean));

    for (b = 0; b < BOARD_WIDTH; b++) {
        if (board[b][0] == Blue_Stone) {
            reachable[b][0] = TRUE;
            pos = (struct HexPosition) {b, 0};
            if (expand_to_neighbors(pos, board, reachable)) {
                return TRUE;
            }
        }
    }
    return FALSE;
}

static Boolean check_red_win_v1(enum HexMark (*board)[BOARD_WIDTH]) {
    Boolean reachable_prev[BOARD_WIDTH];
    Boolean reachable[BOARD_WIDTH];
    Boolean segment_reachable;
    /* Initialize */
    unsigned short b, r;
    unsigned short rlb, rub;  /* Red label lower bound and upper bound */

    /* Set reachable to FALSE */
    memset(reachable_prev, 0, BOARD_WIDTH * sizeof(Boolean));
    memset(reachable, 0, BOARD_WIDTH * sizeof(Boolean));

    for (r = 0; r < BOARD_WIDTH; r++) {
        if (board[0][r] == Red_Stone) {
            reachable[r] = TRUE;
        }
    }

    /* For a hexagon (I1, J1), the neighboring hexagons with the blue
     * label I2 where I2 is the successor of I1 are (I2, J1) and (I2, J2),
     * where J2 is the predecessor of J1 (if it exists).
     * Reversing this relationship, when iterating over the
     * red labels for the next blue label, we look back to the
     * at the connection value for the previous blue label and
     * the same and successor red labels. */
    for (b = 1; b < BOARD_WIDTH; b++) {
        memcpy(reachable_prev, reachable, sizeof(reachable));
        rub = 0;
        while (rub < BOARD_WIDTH) {
            /* Reset the search for the next lower bound to the prior upper bound */
            rlb = rub;
            /* Find the next red stone */
            while ((rlb < BOARD_WIDTH) && (board[b][rlb] != Red_Stone)) {
                rlb++;
            }
            if (rub < rlb) {
                /* In this gap between the previous upper bound and the next
                 * lower bound, the cells do not have red stones, and
                 * so are not regarded as reachable. */
                for (r = rub; r < rlb; r++) {
                    reachable[r] = FALSE;
                }
            }
            segment_reachable = FALSE;
            /* Find the next cell that does not contain a red stone,
             * tracking whether it is possible to reach the segment
             * from the previous blue row */
            rub = rlb;
            while ((rub < BOARD_WIDTH) && (board[b][rub] == Red_Stone)) {
                if (reachable_prev[rub]) {
                    segment_reachable = TRUE;
                }
                rub++;
            }
            /* Note that rub is one past the last red stone, so based on
             * the description above, the segment is still reachable if
             * in the previous blue row the hexagon with this successor red label
             * was reachable. */
            if (rub < BOARD_WIDTH) {
                if (reachable_prev[rub]) {
                    segment_reachable = TRUE;
                }
            }

            /* If the is a reachable segment of red stones, we set the
             * slice of the array to TRUE. */
            if (rlb < rub) {
                /* It follows that rlb < BOARD_WIDTH here */
                /* It follows that board[b][r] == Red_Stone for r in [rlb, rub) */
                for (r = rlb; r < rub; r++) {
                    reachable[r] = segment_reachable;
                }
            }
        }

        /* TODO: Keeping the legacy approach, which does not properly handle
         *       path connections for neighboring hexagons in the same blue row.
        for (r = 0; r < BOARD_WIDTH; r++) {
            if (board[b][r] == Red_Stone) {
                if ((r != (BOARD_WIDTH - 1)) && reachable_prev[r+1]) {
                    reachable[r] = TRUE;
                } else if (reachable_prev[r]) {
                    reachable[r] = TRUE;
                } else {
                    reachable[r] = FALSE;
                }
            } else  {
                reachable[r] = FALSE;
            }
        }
        */
    }

    /* If any  of the hexagons for the last blue label are reachable, then Red wins */
    for (r = 0; r < BOARD_WIDTH; r++) {
        if (reachable[r]) {
            return TRUE;
        }
    }
    return FALSE;
}

static Boolean check_blue_win_v1(enum HexMark (*board)[BOARD_WIDTH]) {
    Boolean reachable_prev[BOARD_WIDTH] /*= {FALSE} TODO */;
    Boolean reachable[BOARD_WIDTH] /*= {FALSE} TODO*/;
    Boolean segment_reachable;
    /* Initialize */
    unsigned short b, r;
    unsigned short blb, bub;  /* Blue label lower bound and upper bound */

    /* Set reachable to FALSE */
    memset(reachable_prev, 0, BOARD_WIDTH * sizeof(Boolean));
    memset(reachable, 0, BOARD_WIDTH * sizeof(Boolean));

    for (b = 0; b < BOARD_WIDTH; b++) {
        if (board[b][0] == Blue_Stone) {
            reachable[b] = TRUE;
        }
    }

    /* For a hexagon (I1, J1), the neighboring hexagons with the red
     * label J2 where J2 is the successor of J1 are (I1, J2) and (I2, J2),
     * where I2 is the predecessor of I1 (if it exists).
     * Reversing this relationship, when iterating over the
     * blue labels for the next red label, we look back to the
     * the connection value for the previous red label and
     * the same and successor blue labels. */
    for (r = 1; r < BOARD_WIDTH; r++) {
        /* Save previous state of reachability */
        memcpy(reachable_prev, reachable, sizeof(reachable));
        bub = 0;
        while (bub < BOARD_WIDTH) {
            /* Reset the search for the next lower bound to the prior upper bound */
            blb = bub;
            /* Find the next blue stone */
            while ((blb < BOARD_WIDTH) && (board[blb][r] != Blue_Stone)) {
                blb++;
            }
            if (bub < blb) {
                /* In this gap between the previous upper bound and the next
                 * lower bound, the cells do not have blue stones, and
                 * so are not regarded as reachable. */
                for (b = bub; b < blb; b++) {
                    reachable[b] = FALSE;
                }
            }
            segment_reachable = FALSE;
            /* Find the next cell that does not contain a blue stone,
             * tracking whether it is possible to reach the segment
             * from the previous red column */
            bub = blb;
            while ((bub < BOARD_WIDTH) && (board[bub][r] == Blue_Stone)) {
                if (reachable_prev[bub]) {
                    segment_reachable = TRUE;
                }
                bub++;
            }
            /* Note that bub is one past the last blue stone, so based on
             * the description above, the segment is still reachable if
             * in the previous red column the hexagon with this successor blue label
             * was reachable. */
            if (bub < BOARD_WIDTH) {
                if (reachable_prev[bub]) {
                    segment_reachable = TRUE;
                }
            }

            /* If the is a reachable segment of red stones, we set the
             * slice of the array to TRUE. */
            if (blb < bub) {
                /* It follows that blb < BOARD_WIDTH here */
                /* It follows that board[b][r] == Blue_Stone for b in [blb, bub) */
                for (b = blb; b < bub; b++) {
                    reachable[b] = segment_reachable;
                }
            }
        }
        /* TODO: Keeping the legacy approach, which does not properly handle
         *       path connections for neighboring hexagons in the same blue row.
        for (b = 0; b < BOARD_WIDTH; b++) {
            if (board[b][r] == Blue_Stone) {
                if ((b != (BOARD_WIDTH - 1)) && reachable_prev[b+1]) {
                    reachable[b] = TRUE;
                } else if (reachable_prev[b]) {
                    reachable[b] = TRUE;
                } else {
                    reachable[b] = FALSE;
                }
            } else {
                reachable[b] = FALSE;
            }
        }
        */

    }

    /* If any of the hexagons for the last red label are reachable, then Blue wins */
    for (b = 0; b < BOARD_WIDTH; b++) {
        if (reachable[b]) {
            return TRUE;
        }
    }
    return FALSE;
}

static Boolean check_win(enum HexMark (*board)[BOARD_WIDTH], enum HexStoneColor stone) {
    switch (stone) {
        case Red:
            return check_red_win(board);
        case Blue:
            return check_blue_win(board);
        default:
            /* Unreachable, but we provide a return to address compiler warnings */
            return FALSE;
    }
}

struct HexState hex_act(struct HexState s, struct HexAction a) {
    /* Helper values */
    const unsigned short number_of_stones = get_number_of_stones(s);
    enum HexStoneColor player_color;  /* We could set here but instead set below */
    enum HexMark stone;

    /* Return value */
    /* Start with a copy of the current state to modify */
    struct HexState res = s;

    if ((number_of_stones == 1) && (res.board[a.row][a.col] == Red_Stone)) {
        /* Swap colors */
        res.player_colors[0] = Blue;
        res.player_colors[1] = Red;
        res.current_player = Player1;
    } else if (res.board[a.row][a.col] == No_Mark) {
        /* Color assigned to player */
        player_color = res.player_colors[res.current_player];
        /* Determine the color of the stone to place based on the color assigned
         * to the current player */
        switch (player_color) {
            case Red:
                stone = Red_Stone;
                break;
            case Blue:
                stone = Blue_Stone;
                break;
            default:
                /* Unreachable */
                /* Addresses compiler warnings about stone being uninitialized */
                stone = Red_Stone;
                break;
        }
        res.board[a.row][a.col] = stone;  /* Place the stone */

        switch (res.current_player) {
            case Player1:
                if (check_win(res.board, player_color)) {
                    res.status = Player1_Wins;
                } else {
                    res.current_player = Player2;  /* Switch player */
                }
                break;
            case Player2:
                if (check_win(res.board, player_color)) {
                    res.status = Player2_Wins;
                } else {
                    res.current_player = Player1;  /* Switch player */
                }
                break;
        }
    }  /* else do nothing if there is already a stone in the target cell */
    return res;
}

float hex_reward(enum HexPlayer player, struct HexState s) {
    switch (s.status) {
        case Player1_Wins:
            switch (player) {
                case Player1:
                    return 1.0;
                case Player2:
                    return -1.0;
                default:
                    /* Unreachable */
                    return 0.0;
            }
            break;
        case Player2_Wins:
            switch (player) {
                case Player1:
                    return -1.0;
                case Player2:
                    return 1.0;
                default:
                    /* Unreachable */
                    return 0.0;
            }
            break;
        case Active:
        default:
            return 0.0;  /* Game is still active, so no reward */
    }
}

/*
static unsigned short get_number_of_available_moves(unsigned short number_of_stones) {
    if (number_of_stones <= 1) {
        return BOARD_SIZE;  \/\* Allow for Player 2 to swap \*\/
    } else {
        return BOARD_SIZE - number_of_stones;
    }
}
*/

static void print_board(struct HexState s) {
    /*  The maximum line length below is
     *  1 + 3*BOARD_WIDTH. */

    unsigned short b, r;

    for (r = 0; r < BOARD_WIDTH; r++) {
        if (r == 0) {
            printf("  ");
        }
        printf("%-2u", r);
    }
    printf("\n");

    for (b = 0; b < BOARD_WIDTH; b++) {
        printf("%2u", b);
        if (b > 0) {
            /* Have to put a conditional here as the following still
             * prints a space with b == 0 */
            printf("%*s", b, " ");
        }
        /* At this point we've printed 2 + b characters, so
         * the next character will be the 3 + b character (using 1-indexing).
         * Below we will print 2*BOARD_WIDTH more characters,
         * for a total of 2 + b + 2*BOARD_WIDTH characters (excluding newline). */

        for (r = 0; r < BOARD_WIDTH; r++) {
            switch (s.board[b][r]) {
                case Red_Stone: 
                    printf(" %c", 'R');
                    break;
                case Blue_Stone: 
                    printf(" %c", 'B');
                    break;
                case No_Mark: 
                    printf(" %c", '*');
                    break;
            }
        }
        printf("\n");
    }
}

static void print_game_status(struct HexState s) {
    switch (s.status) {
        case Active:
            printf("Next Player: %s (%s)\n",
                   player_names[s.current_player],
                   color_names[s.player_colors[s.current_player]]
            );
            break;
        case Player1_Wins:
            printf("Player 1 (%s) won\n", color_names[s.player_colors[Player1]]);
            break;
        case Player2_Wins: 
            printf("Player 2 (%s) won\n", color_names[s.player_colors[Player2]]);
            break;
    }
}

void hex_print_state (struct HexState s) {
    print_board(s);
    print_game_status(s);
    fflush(stdout);
}

struct HexAction hex_mctsenv_get_random_action(struct HexState state) {
    unsigned short int num_actions = 0;
    struct HexAction actions[BOARD_SIZE];

    /*
    unsigned short number_of_stones = get_number_of_stones(s);
    unsigned short number_of_available_moves = get_number_of_available_moves(number_of_stones);
    */
    Boolean red_stone_found = FALSE;

    /*
    ret.num_actions = number_of_available_moves;
    unsigned short next_index = 0;
    */

    unsigned short b, r;

    for (b = 0; b < BOARD_WIDTH; b++) {
        for (r = 0; r < BOARD_WIDTH; r++) {
            switch (state.board[b][r]) {
                case No_Mark:
                    actions[num_actions++] = (struct HexAction) {b, r};
                    break;
                case Red_Stone:
                    /* Allow for Player 2 to swap if there is exactly one
                     * stone on the board. */
                    red_stone_found = TRUE;
                    actions[BOARD_SIZE - 1] = (struct HexAction) {b, r};
                    /*
                    if (number_of_stones == 1) {
                        ret.actions[next_index++] = (struct HexAction) {b, r};
                    }
                    */
                    break;
                case Blue_Stone:
                    break;
            }
        }
    }

    /* There's exactly one stone on the board and it is red,
     * so allow for Player 2 to swap. */
    if ((num_actions == (BOARD_SIZE - 1)) && red_stone_found) {
        /* We've already set the position of the red stone as
         * the last action in the array, so we just increment
         * the length of the array (num_actions). */
        num_actions++;
    }
    /* TODO
    if (num_actions <= 1) {
        printf("Number of valid actions: %u\n", num_actions);
    }
    if (num_actions == 0) {
        fprintf(stderr, "hex_mctsenv_get_random_action: no valid actions available");
        hex_print_state(state);
    }
    */
    return actions[rand() % num_actions];
}

/* The following is the source for the methods for HexActionList */
#define ENVIRONMENT_PREFIX hex
#define ENVIRONMENT_STRUCT_PREFIX Hex
#define ACTION_TYPE struct HexAction
#include <reinforcementlearning/action_array_template.inc>

struct HexActionList* hex_experimental_get_valid_actions(struct HexState state) {
    unsigned short number_of_stones = get_number_of_stones(state);
    size_t max_actions = (number_of_stones <= 1) ? BOARD_SIZE : (BOARD_SIZE - number_of_stones);
    struct HexActionList* vas = hex_action_list_create(max_actions);

    struct HexAction temp_action, take_over_action;
    Boolean red_stone_found = FALSE;

    unsigned short b, r;

    for (b = 0; b < BOARD_WIDTH; b++) {
        for (r = 0; r < BOARD_WIDTH; r++) {
            switch (state.board[b][r]) {
                case No_Mark:
                    temp_action = (struct HexAction) {b, r};
                    if(hex_action_list_push(&vas, temp_action)) {
                        fprintf(stderr, "hex_experimental_get_valid_actions: could not push action to list, returning NULL");
                        hex_action_list_destroy(vas);
                        return NULL;
                    }
                    break;
                case Red_Stone:
                    /* Allow for Player 2 to swap if there is exactly one
                     * stone on the board. */
                    red_stone_found = TRUE;
                    take_over_action = (struct HexAction) {b, r};
                    break;
                case Blue_Stone:
                    break;
            }
        }
    }

    /* There's exactly one stone on the board and it is red,
     * so allow for Player 2 to swap. */
    if ((hex_action_list_length(vas) == (BOARD_SIZE - 1)) && red_stone_found) {
        if (hex_action_list_push(&vas, take_over_action)) {
            fprintf(stderr, "hex_experimental_get_valid_actions: could not push action to list, returning NULL");
            hex_action_list_destroy(vas);
            return NULL;
        }
    }
    return vas;
}

static Boolean hex_action_eq(struct HexAction a1, struct HexAction a2) {
    return (a1.row == a2.row) &&
           (a1.col == a2.col);
}

#define ENVIRONMENT_PREFIX hex
#define ENVIRONMENT_STRUCT_PREFIX Hex
#define CONFIG_TYPE struct HexConfig
#define STATE_TYPE struct HexState
#define ACTION_TYPE struct HexAction
#define PLAYER_TYPE enum HexPlayer
#define INITIAL_STATE_METHOD hex_initial_state
#define STEP_METHOD hex_act
#define GET_PLAYER_METHOD hex_get_player
#define RANDOM_ACTION_METHOD hex_mctsenv_get_random_action
#define IS_TERMINAL_METHOD hex_is_terminal
#define REWARD_METHOD hex_reward
#define ACTION_LIST_TYPE struct HexActionList
#define GET_VALID_ACTIONS_METHOD hex_experimental_get_valid_actions
#define ACTION_LIST_GET_METHOD hex_action_list_get
#define ACTION_LIST_LENGTH_METHOD hex_action_list_length
#define ACTION_LIST_SHUFFLE_METHOD hex_action_list_shuffle
#define ACTION_LIST_DESTROY_METHOD hex_action_list_destroy
#define ACTION_EQ_METHOD hex_action_eq
#include <reinforcementlearning/algorithms/uct.inc>


/* For a hexagon (b1, r1), the neighboring hexagons with the blue
 * label b2 where b2 is the successor of b1 are (b2, r1) and (b2, r2),
 * where r2 is the predecessor of r1 (if it exists).
 * Reversing this relationship, when iterating over the
 * red labels for the next blue label, we look at
 * the hexagon value for the previous blue label and
 * the same and successor red labels.
 * 
 * For a hexagon (b1, r1), the neighboring hexagons with the red
 * label r2 where r2 is the successor of r1 are (b1, r2) and (b2, r2),
 * where b2 is the predecessor of b1 (if it exists).
 * Reversing this relationship, when iterating over the
 * blue labels for the next red label, we look at
 * the hexagon value for the previous red label and
 * the same and successor blue labels.
 * 
 * A hexagon (b, r) will have at most six neighbors depending on where
 * it sits in relation to the edge of the board.
 * While the neighbors can be identified using the two paragraphs above,
 * as an alternative specification, the six neighbors are
 * (b, r-1), (b, r+1),
 * (b-1, r), (b+1, r), and
 * (b-1, r+1), (b+1, r-1),
 * omitting from this list any hexagons not on the board.
 * The first two are the neighbors to the left and right (same blue label),
 * the next two are the neighbors above and below (same red label), and
 * the last two are the neighbors above and below to the right and left,
 * respectively. */
Boolean neighboring_hexagons(unsigned short b1, unsigned short r1, unsigned short b2, unsigned short r2) {
    Boolean blue_is_succ = (b1 != (BOARD_WIDTH-1)) && (b2 == (b1+1));
    Boolean blue_is_prev = (b1 != 0) && (b2 == (b1-1));
    Boolean red_is_succ = (r1 != (BOARD_WIDTH-1)) && (r2 == (r1+1));
    Boolean red_is_prev = (r1 != 0) && (r2 == (r1-1));

    if ((r1 == r2) && (blue_is_succ || blue_is_prev)) {
        return TRUE;
    } else if ((b1 == b2) && (red_is_succ || red_is_prev)) {
         return TRUE;
    } else if ((blue_is_succ && red_is_prev) || (blue_is_prev && red_is_succ)) {
         return TRUE;
    } else {
        return FALSE;
    }
}

int hex_example_main() {
    printf("Hex example!\n");
    struct HexState s = hex_initial_state();
    hex_print_state(s);
    return 0;
}

/* TODO: Eventually remove or adapt the following */
int hex_uct_example() {
    struct UCTParams uctparams = { sqrt (2.0) };
    struct HexConfig config; /* No configuration needed for tic-tac-toe */
    struct HexAction a;
    float reward_est;
    struct HexState s;
    enum HexPlayer p;
    struct HexTree* tree = hex_mcts_tree_new(config);

    /*
    if (hex_uct_take_action(tree, hex_mctsenv_get_random_action(hex_uct_get_state(tree)))) {
        fprintf(stderr, "hex_uct_example: failed to take initial action\n");
    }
    */
    /* TODO: Taking a bad initial action 
    if (hex_uct_take_action(tree, (struct HexAction) {1, 1})) {
        fprintf(stderr, "hex_uct_example: failed to take initial action\n");
    }
    if (hex_uct_take_action(tree, (struct HexAction) {1, 0})) {
        fprintf(stderr, "hex_uct_example: failed to take initial action\n");
    }
    */
    /*
    srand(123);
    */
    s = hex_uct_get_state(tree);
    p = hex_get_player(s);
    hex_print_state(s);
    while (!hex_is_terminal(s)) {
        if (hex_uct_search(10000, uctparams, tree, &a, &reward_est)) {
            /* Encountered an error during search */
            break;
        }
        printf("Number of visits after search: %u\n", tree_visits(*tree));
        printf("Player %s (%s) took action (%u, %u) with reward estimate %f\n",
                player_names[p],
                color_names[s.player_colors[p]],
                a.row, a.col,
                reward_est
        );
        s = hex_uct_get_state(tree);
        p = hex_get_player(s);
        hex_print_state(s);
    }
    hex_mcts_tree_free(tree);
    return 0;
};

