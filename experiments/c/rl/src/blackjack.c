#include <reinforcementlearning/envs/blackjack.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h> /* memset */

#define CARD_COUNT 13

enum Card {
    Ace,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King
};

/* We use arrays for the hands.
 * These represent the count of each card type in the hand. */
struct BlackjackEnvironment {
    struct BlackjackConfig config;
    /* Gen : Draw_Random.Generator; */ /* TODO: What to do for RNG */
    unsigned int player_hand[CARD_COUNT];
    unsigned int dealer_hand[CARD_COUNT];
    enum Card dealer_showing_card;
};
   
static const unsigned int card_values[CARD_COUNT] = {
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10
};
  
static const char* card_strs[CARD_COUNT] = {
      "Ace",   "Two", "Three", "Four", "Five",   "Six",
    "Seven", "Eight",  "Nine",  "Ten", "Jack", "Queen",
    "King"
};

static enum Card draw_card() {
    /* TODO: Just using rand() for now */
    return (enum Card)(rand() % CARD_COUNT);
}

struct HandSummary {
    unsigned int hand_sum;
    Boolean usable_ace;
};

static void draw_hand(unsigned int* hand) {
    memset(hand, 0, CARD_COUNT * sizeof(unsigned int));
    enum Card first_card = draw_card();
    enum Card second_card = draw_card();

    hand[first_card] += 1;  /* Should always be 1 */
    hand[second_card] += 1;
}

/* Helper to compute both sum with and without ace counted as 11,
 * and whether the hand has a usable ace. */
static struct HandSummary hand_sum_and_usable_ace(unsigned int* hand) {
    unsigned int hand_sum = 0;
    struct HandSummary res;

    enum Card card;

    for (card = Ace; card <= King; card++) {
        hand_sum += hand[card] * card_values[card];
    }

    if ((hand[Ace] > 0) && (hand_sum + 10 <= 21)) {
        res.hand_sum = hand_sum + 10;
        res.usable_ace = TRUE;
    } else {
        res.hand_sum = hand_sum;
        res.usable_ace = FALSE;
    }
    return res;
}

static unsigned int sum_hand(unsigned int* hand) {
    struct HandSummary summary = hand_sum_and_usable_ace(hand);
    return summary.hand_sum;
}

static struct BlackjackObservation get_obs(struct BlackjackEnvironment* env) {
    struct HandSummary temp_summary = hand_sum_and_usable_ace(env->player_hand);
    
    return (struct BlackjackObservation) {
        temp_summary.hand_sum,
        card_values[env->dealer_showing_card],
        temp_summary.usable_ace
    };
}

struct BlackjackEnvironment* blackjack_make(struct BlackjackConfig config) {
    struct BlackjackEnvironment* env = malloc(sizeof(struct BlackjackEnvironment));
    if (env == NULL) {
        fprintf(stderr, "blackjack_make: Failed to allocate memory for Environment\n");
        return NULL;
    }
    if (blackjack_init(config, env)) {
        fprintf(stderr, "blackjack_make: Failed to initialize Environment\n");
        free(env);
        return NULL;
    }
    return env;
};

int blackjack_init(struct BlackjackConfig config, struct BlackjackEnvironment* env) {
    env->config = config;
    /* placeholders until reset is called */
    memset(env->player_hand, 0, sizeof(env->player_hand));
    memset(env->dealer_hand, 0, sizeof(env->dealer_hand));
    env->dealer_showing_card = Ace;  
    return 0;
}

static void hit_till_12(unsigned int* hand) {
    enum Card new_card;
    while (sum_hand(hand) < 12) {
        new_card = draw_card();
        hand[new_card] += 1;
    }
}

struct BlackjackObservation blackjack_reset(struct BlackjackEnvironment* env/*, Seed_Reset : Seed_Reset_Type*/) {
    /* Helper function */
    enum Card dealer_card;
    enum Card other_dealer_card;

    /*
    case Seed_Reset.Kind is
         when Set_Default => Draw_Random.Reset(Env.Gen);
         when No_Set      => null;
         when Set_Seed    => Draw_Random.Reset(Env.Gen, Seed_Reset.Seed);
    end case;
    */
    /* Reset dealer hand */
    memset(env->dealer_hand, 0, sizeof(env->dealer_hand));
    dealer_card = draw_card();
    other_dealer_card = draw_card();
    env->dealer_hand[dealer_card] = 1;  /* The first card, should only be 1 card */
    env->dealer_hand[other_dealer_card] += 1;
    env->dealer_showing_card = dealer_card;
     
    /* Reset and draw player hand */
    draw_hand(env->player_hand);
      
    if (env->config.auto_hit) {
        hit_till_12(env->player_hand);
    }

    return get_obs(env);
}

/* Helper functions for the step method */
static Boolean is_bust(unsigned int* hand) {
    return sum_hand(hand) > 21;
}

static int score(unsigned int* hand) {
    if (is_bust(hand)) {
        return 0;
    } else {
        return sum_hand(hand);
    }
}

static float cmp(int a, int b) {
    return ((float) a > b) - ((float) a < b);
}

static Boolean is_natural(unsigned int* hand) {
    Boolean has_one_10 = ((hand[Ten] + hand[Jack] + hand[Queen] + hand[King]) == 1);
    return has_one_10 && (hand[Ace] == 1);
}

struct BlackjackStepReturn blackjack_step(struct BlackjackEnvironment* env, enum BlackjackAction action) {
    /* Temporary variables */
    float reward = 0.0;
    Boolean terminated = FALSE;
    enum Card new_card;

    switch (action) {
        case HIT:
            new_card = draw_card();
            env->player_hand[new_card] += 1;
            if (is_bust(env->player_hand)) {
                terminated = TRUE;
                reward = -1.0;
            } else {
                terminated = FALSE;
                reward = 0.0;
            }
            break;
        case STICK:
            terminated = TRUE;
            while (sum_hand(env->dealer_hand) < 17) {
                new_card = draw_card();
                env->dealer_hand[new_card] += 1;
            }
            reward = cmp(score(env->player_hand), score(env->dealer_hand));
            if ((env->config.natural_win_reward == SAB) && is_natural(env->player_hand) && !is_natural(env->dealer_hand)) {
                /* Player automatically wins. Rules consistent with S&B */
               reward = 1.0;
            } else if ((env->config.natural_win_reward == NATURAL_WIN) && is_natural(env->player_hand) && (reward == 1.0)) {
               /* Natural gives extra points, but doesn't autowin. Legacy implementation */
               reward = 1.5;
            }
            break;
    }
    return (struct BlackjackStepReturn) {
        get_obs(env),
        reward,
        terminated
    };
}

void blackjack_render_text(struct BlackjackEnvironment* env) {
    struct BlackjackObservation obs = get_obs(env);

    printf(
        "Player Sum: %u, Usable Ace: %s, Dealer Showing Card Value: %u (%s)\n",
        obs.player_sum,
        (obs.usable_ace == TRUE) ? "True" : "False",
        obs.dealer_showing_card_value,
        card_strs[env->dealer_showing_card]
    );
}
