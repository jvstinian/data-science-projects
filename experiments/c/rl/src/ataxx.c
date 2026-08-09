#include <reinforcementlearning/envs/ataxx.h>
#include <stdio.h>
#include <string.h> /* memset */
#include <stdlib.h> /* abs */

#if defined(__STDC__) && !defined(__STDC_VERSION__)
    unsigned short int min_ushort(unsigned short int x, unsigned short int y) {
        return (x < y) ? x : y;
    }
    unsigned short int max_ushort(unsigned short int x, unsigned short int y) {
        return (x > y) ? x : y;
    }
#endif

const char* player_names[MAX_PLAYER_COUNT] = {
    "Red", "Blue", "White", "Black"
};

struct AtaxxState ataxx_initial_state(struct AtaxxConfig config) {
    struct AtaxxState ret;
    unsigned short r, c;

    memset(ret.player_indicators, 0, MAX_PLAYER_COUNT * sizeof(Boolean));
    memset(ret.scores, 0, MAX_PLAYER_COUNT * sizeof(unsigned short int));
    for(r=0; r < BOARD_WIDTH; r++){
        for(c=0; c < BOARD_WIDTH; c++){
            ret.board[r][c] = No_Mark;
        }
    }

    switch (config.player_count) {
        case Two_Player:
            ret.player_indicators[Red] = TRUE;
            ret.player_indicators[Blue] = TRUE;
            ret.board[0][0] = Mark_Red;
            ret.board[BOARD_WIDTH-1][BOARD_WIDTH-1] = Mark_Red;
            ret.board[0][BOARD_WIDTH-1] = Mark_Blue;
            ret.board[BOARD_WIDTH-1][0] = Mark_Blue;
            ret.scores[Red] = 2;
            ret.scores[Blue] = 2;
            break;
        case Four_Player:
            ret.player_indicators[Red] = TRUE;
            ret.player_indicators[Blue] = TRUE;
            ret.player_indicators[White] = TRUE;
            ret.player_indicators[Black] = TRUE;
            ret.board[0][0] = Mark_Red;
            ret.board[BOARD_WIDTH-1][0] = Mark_Blue;
            ret.board[BOARD_WIDTH][BOARD_WIDTH-1] = Mark_White;
            ret.board[0][BOARD_WIDTH-1] = Mark_Black;
            ret.scores[Red] = 1;
            ret.scores[Blue] = 1;
            ret.scores[White] = 1;
            ret.scores[Black] = 1;
            break;
    }
    ret.status = Active;
    ret.current_player = Red;
    return ret;
}
    
static Boolean board_full(enum AtaxxMark (*board)[BOARD_WIDTH]) {
    unsigned short int r, c;
    for (r = 0; r < BOARD_WIDTH; r++){
        for (c = 0; c < BOARD_WIDTH; c++){
            if(board[r][c] == No_Mark) {
                return FALSE;
            }
        }
    }
    return TRUE;
}

static Boolean available_move_from(enum AtaxxMark (*board)[BOARD_WIDTH], unsigned short int row, unsigned short int col) {
    unsigned short int i0, i1, j0, j1;
    unsigned short int r, c;
    i0 = 0;
    if (row >= 2) {
        i0 = row - 2;
    }
    i1 = BOARD_WIDTH - 1;
    if (row < (BOARD_WIDTH - 2)) {
        i1 = row + 2;
    }
    j0 = 0;
    if (col >= 2) {
        j0 = col - 2;
    }
    j1 = BOARD_WIDTH - 1;
    if (col < (BOARD_WIDTH - 2)) {
        j1 = col + 2;
    }

    for (r = i0; r <= i1; r++){
        for (c = j0; c <= j1; c++) {
            if (!((r == row) && (c == col)) && (board[r][c] == No_Mark)) {
                return TRUE;
            }
        }
    }
    return FALSE;
}
  
static void can_move(enum AtaxxMark (*board)[BOARD_WIDTH], Boolean *players_can_move) {
    enum AtaxxMark temp_mark;
    unsigned short int r, c;

    /* Set to FALSE */
    memset(players_can_move, 0, MAX_PLAYER_COUNT * sizeof(Boolean));

    for(r = 0; r < BOARD_WIDTH; r++) {
        for(c = 0; c < BOARD_WIDTH; c++) {
            temp_mark = board[r][c];
            if (
                (
                    (temp_mark == Mark_Red) || (temp_mark == Mark_Blue)
                    || (temp_mark == Mark_White) || (temp_mark == Mark_Black)
                ) && available_move_from(board, r, c)
            ) {
                switch (temp_mark) {
                    case Mark_Red:
                        players_can_move[Red] = TRUE;
                        break;
                    case Mark_Blue:
                        players_can_move[Blue] = TRUE;
                        break;
                    case Mark_White:
                        players_can_move[White] = TRUE;
                        break;
                    case Mark_Black:
                        players_can_move[Black] = TRUE;
                        break;
                    case Mark_X:  /* Cannot happen, here for compiler */
                    case No_Mark: /* Cannot happen, here for compiler */
                    default:
                        break;
                }
            }
        }
    }
}

Boolean ataxx_is_terminal(struct AtaxxState state) {
    Boolean players_can_move[MAX_PLAYER_COUNT];
    can_move(state.board, players_can_move);
    enum AtaxxPlayer p;
    /* NOTE: The Board_Full check might not be necessary, as
     *       the Players_Can_Move values should be sufficient, but
     *       we include it for completeness. */
    if (board_full(state.board)) {
        return TRUE;
    }

    for (p = Red; p <= Black; p++) {
        if(state.player_indicators[p] && players_can_move[p]) {
            /* At least one active player can move, so not terminal */
            return FALSE;
        }
    }
    return TRUE;  /* No active players can move, so terminal */
}

enum AtaxxPlayer ataxx_get_player(struct AtaxxState state) {
    return state.current_player;
}
   
static enum AtaxxMark player_to_mark(enum AtaxxPlayer player) {
    switch (player) {
        case Red: 
            return Mark_Red;
        case Blue: 
            return Mark_Blue;
        case White: 
            return Mark_White;
        case Black: 
            return Mark_Black;
        default:
            /* Should be unreachable */
            return Mark_Red;
    }
}

static enum AtaxxPlayer mark_to_player(enum AtaxxMark mark) {
    switch (mark) {
        case Mark_Red: 
            return Red;
        case Mark_Blue: 
            return Blue;
        case Mark_White: 
            return White;
        case Mark_Black: 
            return Black;
        case Mark_X:  /* Cannot happen, here for compiler */
        case No_Mark: /* Cannot happen, here for compiler */
        default:
            /* Should not happen in practice */
            fprintf(stderr, "Attempting conversion from non-player mark to player");
            return Red;
    }
}

static enum AtaxxPlayer next_player_in_ring(struct AtaxxState state, enum AtaxxPlayer p) {
    enum AtaxxPlayer res = p;
    unsigned int i;
    /* Use a for loop over i, setting res = p + i */
    for (i = 1; i <= MAX_PLAYER_COUNT; i++) {
        res = (enum AtaxxPlayer) ((((unsigned int) p) + i) % MAX_PLAYER_COUNT);
        if (state.player_indicators[res]) {
            break;
        }
    }
    return res;
}

static unsigned short int distance(struct AtaxxCellIndices from, struct AtaxxCellIndices to) {
    return max_ushort(
        (unsigned short int) abs(((int) from.row) - ((int) to.row)),
        (unsigned short int) abs(((int) from.col) - ((int) to.col))
    );
}
    
struct AtaxxState ataxx_act(struct AtaxxState state, struct AtaxxAction action) {
    /* Helper functions */
    struct AtaxxCellIndices source = action.source;
    struct AtaxxCellIndices target = action.target;
    
    unsigned short int dist = distance(source, target);
    const enum AtaxxMark p_mark = player_to_mark(state.current_player);

    /* Local variables */
    unsigned short int i0, i1, j0, j1;
    unsigned short int r, c;
    enum AtaxxPlayer overwritten_player;
    enum AtaxxPlayer next_player;
    enum AtaxxPlayer stop_player;
    Boolean players_can_move[MAX_PLAYER_COUNT];
    memset(players_can_move, 0, MAX_PLAYER_COUNT * sizeof(Boolean));

    /* Return value */
    struct AtaxxState res = state;  /* Start with a copy of the current state to modify */

    if ((res.board[source.row][source.col] == p_mark) && (res.board[target.row][target.col] == No_Mark)) {
        res.board[target.row][target.col] = p_mark;
        res.scores[res.current_player] += 1;

        if (dist == 2) {
            res.board[source.row][source.col] = No_Mark;  /* Remove piece if it's a jump */
            res.scores[res.current_player] -= 1;
        }

        i0 = 0;
        if (target.row >= 1) {
            i0 = target.row - 1;
        }
        i1 = BOARD_WIDTH - 1;
        if (target.row < (BOARD_WIDTH - 1)) {
            i1 = target.row + 1;
        }
        j0 = 0;
        if (target.col >= 1) {
            j0 = target.col - 1;
        }
        j1 = BOARD_WIDTH - 1;
        if (target.col < (BOARD_WIDTH - 1)) {
            j1 = target.col + 1;
        }

        for (r = i0; r <= i1; r++){
            for (c = j0; c <= j1; c++) {
                if (
                        !((r == target.row) && (c == target.col))
                        && (res.board[r][c] != p_mark)
                        && (res.board[r][c] != Mark_X)
                        && (res.board[r][c] != No_Mark)
                ) {
                    /* The mark must belong to a player other than p_mark (current player) */
                    overwritten_player = mark_to_player(res.board[r][c]);
                    res.board[r][c] = p_mark;  /* Convert adjacent pieces to current player's mark */
                    res.scores[overwritten_player] -= 1;
                    res.scores[res.current_player] += 1;
                }
            }
        }
    } /* else do nothing if the source cell doesn't match the current player */

    can_move(res.board, players_can_move);
    /* Determine the game status and next player */
    next_player = next_player_in_ring(state, res.current_player);
    stop_player = next_player;
    while (!players_can_move[next_player]) {
        next_player = next_player_in_ring(state, next_player);
        if (next_player == stop_player) {
            /* We've looped through all players and none can move */
            break;
        }
    }
    res.current_player = next_player; /* Set next player */
    if (!players_can_move[next_player]) {
        res.status = Finished;  /* No players can move, so game is finished */
    }

    return res;
}

float ataxx_reward(enum AtaxxPlayer player, struct AtaxxState state) {
    /* We return the number of occupied cells regardless of whether the
     * game is finished. */
    return (float) state.scores[player];
}

static void print_board(struct AtaxxState state) {
    unsigned short int r, c;

    printf(" ");
    for (c=0; c<BOARD_WIDTH; c++) {
        printf("%u", c); /* Print column label as integer */
    }
    printf("\n");

    for (r=0; r<BOARD_WIDTH; r++) {
        printf("%u", r); /* Print row label as integer */
        for (c=0; c<BOARD_WIDTH; c++) {
            switch (state.board[r][c]) {
                case Mark_Red: 
                    printf("R");
                    break;
                case Mark_Blue: 
                    printf("B");
                    break;
                case Mark_White: 
                    printf("W");
                    break;
                case Mark_Black: 
                    printf("K");
                    break;
                case Mark_X: 
                    printf("X");
                    break;
                case No_Mark:  /* Here for compiler warning */
                default:
                    printf(" ");
                    break;
            }
        }
        printf("\n");
    }
}

static void get_winners(unsigned short int *scores, Boolean* winning_players, unsigned short int* num_winners) {
    unsigned short int i;
    unsigned short int max_score = 0;

    /* Set the winning players vector to FALSE throughout */
    memset(winning_players, 0, MAX_PLAYER_COUNT * sizeof(Boolean));
    *num_winners = 0;

    /* Find the max score */
    for (i=0; i < MAX_PLAYER_COUNT; i++) {
        if (scores[i] > max_score) {
            max_score = scores[i];
        }
    }

    /* Identify the players with the max score */
    for (i=0; i < MAX_PLAYER_COUNT; i++) {
        if (scores[i] >= max_score) {
            winning_players[i] = TRUE;
            *num_winners += 1;
        }
    }
}

static void print_game_status(struct AtaxxState state) {
    enum AtaxxPlayer p;
    unsigned short int i, remaining_winners;
    Boolean winning_players[MAX_PLAYER_COUNT];

    switch (state.status) {
        case Active:
            printf("Next Player: %s", player_names[ataxx_get_player(state)]);
            break;
        case Finished:
            get_winners(state.scores, winning_players, &remaining_winners);
            printf("Game Over: ");
            for (i = 0; i < MAX_PLAYER_COUNT; i++) {
                if (winning_players[i]) {
                    printf("%s", player_names[i]);
                    remaining_winners--;
                    if (remaining_winners > 1) {
                        printf(", ");
                    } else if (remaining_winners > 0) {
                        /* Exactly one winner remaining to print */
                        printf(", and ");
                    }
                }
            }
            break;
    }
    printf("\n");

    printf("Scores: \n");
    for (p = Red; p <= Black; p++) {
        if (state.player_indicators[p]) {
            printf("   %5s: %d\n", player_names[p], state.scores[p]);
        }
    }
}

void ataxx_print_state(struct AtaxxState state) {
    print_board(state);
    print_game_status(state);
}

struct AtaxxAction ataxx_mctsenv_get_random_action(struct AtaxxState state) {
    struct AtaxxAction vas[BOARD_WIDTH * BOARD_WIDTH * 24];
    unsigned int num_actions = 0;

    enum AtaxxPlayer player = state.current_player;
    /* We use player_to_mark in the following */
    enum AtaxxMark player_mark = player_to_mark(player);

    /* Temp variables for loop below */
    struct AtaxxCellIndices source;
    struct AtaxxAction temp_action;
    unsigned short int i0, i1, j0, j1;
    unsigned short int r, c;
    unsigned short int r_target, c_target;

    for (r=0; r < BOARD_WIDTH; r++){
        for (c=0; c < BOARD_WIDTH; c++){
            if (state.board[r][c] == player_mark) {
                source = (struct AtaxxCellIndices) { r, c };
                /*  We just want
                i0 = (unsigned short int) max_short(0, ((short int) r) - 2);
                i1 = (unsigned short int) min_short(BOARD_WIDTH - 1, ((short int) r) + 2);
                but below rather we set the extreme value and then adjust to the
                range value if valid.  This way we avoid type casts but introduce
                branches. */
                i0 = 0;
                if (r >= 2) {
                    i0 = r - 2;
                }
                i1 = BOARD_WIDTH - 1;
                if (r < (BOARD_WIDTH - 2)) {
                    i1 = r + 2;
                }
                j0 = 0;
                if (c >= 2) {
                    j0 = c - 2;
                }
                j1 = BOARD_WIDTH - 1;
                if (c < (BOARD_WIDTH - 2)) {
                    j1 = c + 2;
                }
                for (r_target = i0; r_target <= i1; r_target++) {
                    for (c_target = j0; c_target <= j1; c_target++) {
                        if (
                            !((r_target == r) && (c_target == c))
                            && (state.board[r_target][c_target] == No_Mark)
                        ) {
                            temp_action = (struct AtaxxAction) {
                                source,
                                (struct AtaxxCellIndices) { r_target, c_target }
                             };
                            vas[num_actions++] = temp_action;
                        }
                    }
                }
            }
        }
    }
    return vas[rand() % num_actions];
}

#define ENVIRONMENT_PREFIX ataxx
#define CONFIG_TYPE struct AtaxxConfig
#define STATE_TYPE struct AtaxxState
#define ACTION_TYPE struct AtaxxAction
#define PLAYER_TYPE enum AtaxxPlayer
#define INITIAL_STATE_METHOD ataxx_initial_state
#define STEP_METHOD ataxx_act
#define GET_PLAYER_METHOD ataxx_get_player
#define RANDOM_ACTION_METHOD ataxx_mctsenv_get_random_action
#define IS_TERMINAL_METHOD ataxx_is_terminal
#define REWARD_METHOD ataxx_reward
#include <reinforcementlearning/algorithms/mctsenv_uniform_random_actions.inc>
    
/* The following is the source for the methods for AtaxxActionList */
#define ENVIRONMENT_PREFIX ataxx
#define ENVIRONMENT_STRUCT_PREFIX Ataxx
#define ACTION_TYPE struct AtaxxAction
#include <reinforcementlearning/action_array_template.inc>

static Boolean ataxx_action_eq(struct AtaxxAction a1, struct AtaxxAction a2) {
    return (a1.source.row == a2.source.row) &&
           (a1.source.col == a2.source.col) &&
           (a1.target.row == a2.target.row) &&
           (a1.target.col == a2.target.col);
}

struct AtaxxActionList* ataxx_experimental_get_valid_actions (struct AtaxxState state) {
    enum AtaxxPlayer player = state.current_player;
    enum AtaxxMark player_mark = player_to_mark(player);
    const size_t max_actions = state.scores[player] * 24;
    struct AtaxxActionList* vas = ataxx_action_list_create(max_actions);
    int vas_status = 0;

    /* Temp variables for loop below */
    struct AtaxxCellIndices source;
    struct AtaxxAction temp_action;
    unsigned short int i0, i1, j0, j1;
    unsigned short int r, c;
    unsigned short int r_target, c_target;

    if (vas == NULL) {
        return NULL;
    }

    for (r=0; r < BOARD_WIDTH; r++){
        for (c=0; c < BOARD_WIDTH; c++){
            if (state.board[r][c] == player_mark) {
                source = (struct AtaxxCellIndices) { r, c };
                /*  We just want
                i0 = (unsigned short int) max_short(0, ((short int) r) - 2);
                i1 = (unsigned short int) min_short(BOARD_WIDTH - 1, ((short int) r) + 2);
                but below rather we set the extreme value and then adjust to the
                range value if valid.  This way we avoid type casts but introduce
                branches. */
                i0 = 0;
                if (r >= 2) {
                    i0 = r - 2;
                }
                i1 = BOARD_WIDTH - 1;
                if (r < (BOARD_WIDTH - 2)) {
                    i1 = r + 2;
                }
                j0 = 0;
                if (c >= 2) {
                    j0 = c - 2;
                }
                j1 = BOARD_WIDTH - 1;
                if (c < (BOARD_WIDTH - 2)) {
                    j1 = c + 2;
                }
                for (r_target = i0; r_target <= i1; r_target++) {
                    for (c_target = j0; c_target <= j1; c_target++) {
                        if (
                            !((r_target == r) && (c_target == c))
                            && (state.board[r_target][c_target] == No_Mark)
                        ) {
                            temp_action = (struct AtaxxAction) {
                                source,
                                (struct AtaxxCellIndices) { r_target, c_target }
                             };
                            vas_status = ataxx_action_list_push(&vas, temp_action);
                            if (vas_status) {
                                fprintf(stderr, "get_valid_actions: could not push action to list, returning NULL");
                                ataxx_action_list_destroy(vas);
                                return NULL;
                            }
                        }
                    }
                }
            }
        }
    }
    return vas;
}

#define ENVIRONMENT_PREFIX ataxx
#define ENVIRONMENT_STRUCT_PREFIX Ataxx
#define CONFIG_TYPE struct AtaxxConfig
#define STATE_TYPE struct AtaxxState
#define ACTION_TYPE struct AtaxxAction
#define PLAYER_TYPE enum AtaxxPlayer
#define INITIAL_STATE_METHOD ataxx_initial_state
#define STEP_METHOD ataxx_act
#define GET_PLAYER_METHOD ataxx_get_player
#define RANDOM_ACTION_METHOD ataxx_mctsenv_get_random_action
#define IS_TERMINAL_METHOD ataxx_is_terminal
#define REWARD_METHOD ataxx_reward
#define ACTION_LIST_TYPE struct AtaxxActionList
#define GET_VALID_ACTIONS_METHOD ataxx_experimental_get_valid_actions
#define ACTION_LIST_GET_METHOD ataxx_action_list_get
#define ACTION_LIST_LENGTH_METHOD ataxx_action_list_length
#define ACTION_LIST_SHUFFLE_METHOD ataxx_action_list_shuffle
#define ACTION_LIST_DESTROY_METHOD ataxx_action_list_destroy
#define ACTION_EQ_METHOD ataxx_action_eq
#include <reinforcementlearning/algorithms/uct.inc>

int ataxx_example_main() {
    printf("Ataxx prototype under construction.\n");
    struct AtaxxConfig conf = { Two_Player };
    struct AtaxxState state = ataxx_initial_state(conf);

    ataxx_print_state(state);

    size_t i;
    struct AtaxxActionList* vas;
    struct AtaxxAction a;
    vas = ataxx_experimental_get_valid_actions(state);
    if (vas == NULL) {
        fprintf(stderr, "main: error in get_valid_actions, exiting.");
        return 1;
    }
    for(i=0; i < ataxx_action_list_length(vas); i++) {
        a = ataxx_action_list_get(vas, i);
        printf("Action %lu: (%u, %u) -> (%u, %u)\n",
                i,
                a.source.row,
                a.source.col,
                a.target.row,
                a.target.col
              );
    }
    return 0;
}

/* TODO: Eventually remove or adapt the following */
int ataxx_uct_example() {
    struct UCTParams uctparams = { sqrt (2.0) };
    struct AtaxxConfig config; /* No configuration needed for tic-tac-toe */
    struct AtaxxAction a;
    float reward_est;
    struct AtaxxState s;
    enum AtaxxPlayer p;
    struct AtaxxTree* tree = ataxx_mcts_tree_new(config);

    /*
    a = ataxx_mctsenv_get_random_action(ataxx_uct_get_state(tree));
    printf("Random action (%u, %u) -> (%u, %u)\n", 
                a.source.row, a.source.col,
                a.target.row, a.target.col);
    ataxx_act(ataxx_uct_get_state(tree), a);
    */
    /*
    if (ataxx_uct_take_action(tree, ataxx_mctsenv_get_random_action(ataxx_uct_get_state(tree)))) {
        fprintf(stderr, "ataxx_uct_example: failed to take initial action\n");
    }
    */
    /* TODO: Taking a bad initial action 
    if (ataxx_uct_take_action(tree, (struct AtaxxAction) {1, 1})) {
        fprintf(stderr, "ataxx_uct_example: failed to take initial action\n");
    }
    if (ataxx_uct_take_action(tree, (struct AtaxxAction) {1, 0})) {
        fprintf(stderr, "ataxx_uct_example: failed to take initial action\n");
    }
    */
    s = ataxx_uct_get_state(tree);
    p = ataxx_get_player(s);
    ataxx_print_state(s);
    while (!ataxx_is_terminal(s)) {
        if (ataxx_uct_search(50000, uctparams, tree, &a, &reward_est)) {
            /* Encountered an error during search */
            break;
        }
        printf("Number of visits after search: %u\n", tree_visits(*tree));
        printf("Player %d Took action (%u, %u) -> (%u, %u) with reward estimate %f\n",
                p,
                a.source.row, a.source.col,
                a.target.row, a.target.col,
                reward_est
        );
        s = ataxx_uct_get_state(tree);
        p = ataxx_get_player(s);
        ataxx_print_state(s);
    }
    ataxx_mcts_tree_free(tree);
    return 0;
};

