#include "hex.h"
#include <stdio.h>
#include <string.h> /* memset, memcpy */
#include <assert.h>

struct State initial_state() {
    /* enum Mark board[BOARD_WIDTH][BOARD_WIDTH] = {{No_Mark}}; */
    unsigned int b, r;

    struct State ret;

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

Boolean is_terminal(struct State s) {
    if (s.status != Active) {
        return TRUE;  /* Game is already finished */
    }
    return FALSE;
}

enum HexPlayer get_player(struct State s) {
    return s.current_player;
}

static unsigned short get_number_of_stones(struct State s) {
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

static Boolean check_red_win(enum Mark (*board)[BOARD_WIDTH]) {
    Boolean reachable_prev[BOARD_WIDTH] = {FALSE};
    Boolean reachable[BOARD_WIDTH] = {FALSE};
    /* Initialize */
    unsigned short b, r;

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
        /* Reachable_Prev := Reachable; */  /* Save previous state of reachability */
        memcpy(reachable_prev, reachable, sizeof(reachable));
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
    }

    /* If any  of the hexagons for the last blue label are reachable, then Red wins */
    for (r = 0; r < BOARD_WIDTH; r++) {
        if (reachable[r]) {
            return TRUE;
        }
    }
    return FALSE;
}

static Boolean check_blue_win(enum Mark (*board)[BOARD_WIDTH]) {
    Boolean reachable_prev[BOARD_WIDTH] = {FALSE};
    Boolean reachable[BOARD_WIDTH] = {FALSE};
    /* Initialize */
    unsigned short b, r;

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
    }

    /* If any of the hexagons for the last red label are reachable, then Blue wins */
    for (b = 0; b < BOARD_WIDTH; b++) {
        if (reachable[b]) {
            return TRUE;
        }
    }
    return FALSE;
}

static Boolean check_win(enum Mark (*board)[BOARD_WIDTH], enum StoneColor stone) {
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

struct State step(struct State s, struct Action a) {
    /* Helper values */
    const unsigned short number_of_stones = get_number_of_stones(s);
    enum StoneColor player_color;  /* We could set here but instead set below */
    enum Mark stone;

    /* Return value */
    /* Start with a copy of the current state to modify */
    struct State res = s;

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

float reward(enum HexPlayer player, struct State s) {
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
        default:
            return 0.0;  /* Game is still active, so no reward */
    }
}

static unsigned short get_number_of_available_moves(unsigned short number_of_stones) {
    if (number_of_stones <= 1) {
        return BOARD_SIZE;  /* Allow for Player 2 to swap */
    } else {
        return BOARD_SIZE - number_of_stones;
    }
}

struct ValidActions get_valid_actions(struct State s) {
    unsigned short number_of_stones = get_number_of_stones(s);
    unsigned short number_of_available_moves = get_number_of_available_moves(number_of_stones);
    struct ValidActions ret;

    ret.num_actions = number_of_available_moves;
    unsigned short next_index = 0;

    unsigned short b, r;

    for (b = 0; b < BOARD_WIDTH; b++) {
        for (r = 0; r < BOARD_WIDTH; r++) {
            switch (s.board[b][r]) {
                case No_Mark:
                    ret.actions[next_index++] = (struct Action) {b, r};
                    break;
                case Red_Stone:
                    /* Only increment if there is exactly one stone on the board,
                     * which we are now encountering at (I, J). */
                    if (number_of_stones == 1) {
                        /* Allow for Player 2 to swap */
                        ret.actions[next_index++] = (struct Action) {b, r};
                    }
                    break;
                case Blue_Stone:
                    break;
            }
        }
    }

    assert(next_index == ret.num_actions);
    return ret;
}

static void print_board(struct State s) {
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

static void print_game_status(struct State s) {
    switch (s.status) {
        case Active:
            printf("Next Player: %s\n", player_names[s.current_player]);
            break;
        case Player1_Wins:
            printf("Player 1 won\n");
            break;
        case Player2_Wins: 
            printf("Player 2 won\n");
            break;
    }
}

void print_state (struct State s) {
    print_board(s);
    print_game_status(s);
}

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

int main() {
    printf("Hex example!\n");
    struct State s = initial_state();
    print_state(s);
    return 0;
}
