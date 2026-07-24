#include <reinforcementlearning/envs/ttt.h>
#include <stdio.h>

struct TTTState initial_state() {
    /* unsigned short int r, c; */
    /* enum TTTMark board[3][3] = { { No_Mark } }; */
    /*struct Board board = {
        { { No_Mark } }
    };
    */

    return (struct TTTState) { {{ No_Mark }}, X_Move };
}

Boolean is_terminal(struct TTTState s) {
    switch (s.status) {
        case X_Move:
        case O_Move:
            return FALSE;
        case Draw:
        case X_Wins:
        case O_Wins:
            return TRUE;
        /* Should not be reachable, but needed to avoid compiler warning */
        default:  
            return FALSE;
    }
}

enum TTTPlayer get_player(struct TTTState s) {
    /* Returns PlayerX for a draw.  This is an arbitrary choice, and
     * should not be used in terminal state */
    switch (s.status) {
        case X_Move:
        case X_Wins:
            return PlayerX;
        case O_Move:
        case O_Wins:
            return PlayerO;
        case Draw:
        default:
            return PlayerX;
    }
}

static Boolean check_row_for_win(struct TTTState s, enum TTTMark m, unsigned short r) {
    unsigned short c;
    for (c = 0; c < 3; c++){
        if (s.board[r][c] != m) {
            return FALSE;
        }
    }
    return TRUE;
}
      
static Boolean check_col_for_win(struct TTTState s, enum TTTMark m, unsigned short c) {
    unsigned short r;
    for (r = 0; r < 3; r++) {
        if (s.board[r][c] != m) {
            return FALSE;
        }
    }
    return TRUE;
}
      
static Boolean is_move_on_diag1(struct TTTAction a) {
    return a.row == a.col;
}

static Boolean is_move_on_diag2(struct TTTAction a) {
    short r = (short) a.row;
    short c = (short) a.col;
    return (r + c) == 2;
}

static Boolean check_diag1_for_win(struct TTTState s, enum TTTMark m) {
    unsigned short rc;
    for (rc = 0; rc < 3; rc++) {
        if (s.board[rc][rc] != m) {
            return FALSE;
        }
    }
    return TRUE;
}

static Boolean check_diag2_for_win(struct TTTState s, enum TTTMark m) {
    unsigned short r, c;
    for (r = 0; r < 3; r++) {
        c = 2 - r;
        if (s.board[r][c] != m) {
            return FALSE;
        }
    }
    return TRUE;
}
      
static Boolean all_moves_exhausted(struct TTTState s) {
    unsigned short r, c;
    for (r = 0; r < 3; r++) {
        for (c = 0; c < 3; c++) {
            if (s.board[r][c] == No_Mark) {
                return FALSE;
            }
        }
    }
    return TRUE;
}

static enum TTTGameStatus check_status_for_action(struct TTTState s, struct TTTAction a) {
    /* Helper functions */
    enum TTTGameStatus result = s.status;  /* Default to current status */
    enum TTTMark m = s.board[a.row][a.col];
    enum TTTGameStatus w;

    switch (m) {
        case X:
            w = X_Wins;
            break;
        case O:
            w = O_Wins;
            break;
        case No_Mark:
            /* Invalid state, just return */
            return result;
        default:
            /* Unreachable state
             * Including this case to define w to avoid compiler warning */
            w = Draw;
            break;
    };

    /* M is either X or O */
    if (check_row_for_win(s, m, a.row)) {
        return w;
    } else if (check_col_for_win(s, m, a.col)) {
        return w;
    } else if (is_move_on_diag1(a) && check_diag1_for_win(s, m)) {
        return w;
    } else if (is_move_on_diag2(a) && check_diag2_for_win(s, m)) {
        return w;
    } else if (all_moves_exhausted(s)) {
        return Draw; /* No more moves and no winner, it's a draw */
    } else {
        return result; /* No win, return current status */
    }
}

struct TTTState step(struct TTTState s, struct TTTAction a) {
    struct TTTState result = s;  /* Start with the current state and modify it */
    if (result.status == X_Move) {
        result.board[a.row][a.col] = X;
        result.status = O_Move;  /* Next player's turn */
    } else if (result.status == O_Move) {
        result.board[a.row][a.col] = O;
        result.status = X_Move;  /* Next player's turn */
    }

    /* Check if the new move results in a win */
    result.status = check_status_for_action(result, a); 
    return result;
}

float reward(enum TTTPlayer p, struct TTTState s) {
    switch (s.status) {
        case X_Wins:
            if (p == PlayerX) {
                return 1.0;
            } else {
                return -1.0;
            }
        case O_Wins:
            if (p == PlayerO) {
                return 1.0;
            } else {
                return -1.0;
            }
        case X_Move:
        case O_Move:
        case Draw:
        default:
            return 0.0; /* Non-terminal state or a draw */
    }
}

struct TTTValidActions get_valid_actions(struct TTTState s) {
    /* Initialize with dummy values, will be overwritten */
    struct TTTValidActions result;
    unsigned short i, r, c;

    i = 0;
    for (r = 0; r < 3; r++) {
        for (c = 0; c < 3; c++) {
            if (s.board[r][c] == No_Mark) {
                result.actions[i++] = (struct TTTAction) {r, c};
            }
        }
    }
    result.num_actions = i;
    return result;
}
      
static void print_board(struct TTTState s) {
    unsigned int r, c;

    printf(" ABC\n");
    for (r = 0; r < 3; r++) {
        printf("%u", r);
        for (c = 0; c < 3; c++) {
            switch (s.board[r][c]) {
                case X:
                    printf("X");
                    break;
                case O:
                    printf("O");
                    break;
                case No_Mark:
                    printf(" ");
                    break;
            }
        }
        printf("\n");
    }
}
      
static void print_game_status(struct TTTState s) {
    printf("Status: ");
    switch (s.status) {
        case X_Move:
            printf("X's move");
            break;
        case O_Move:
            printf("O's move");
            break;
        case Draw:
            printf("Game is a draw");
            break;
        case X_Wins:
            printf("Player X wins");
            break;
        case O_Wins:
            printf("Player O wins");
            break;
    }
    printf("\n");
}

void print_state(struct TTTState s) {
    print_board(s);
    print_game_status(s);
}
