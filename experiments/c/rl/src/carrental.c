#include <reinforcementlearning/envs/carrental.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h> /* memset */

#if defined(__STDC__) && !defined(__STDC_VERSION__)
    float fminf(float x, float y) {
        return (x < y) ? x : y;
    }
#endif

unsigned int umin(unsigned int a, unsigned int b) {
    return (a < b) ? a : b;
}

struct CarrentalConfig get_default_config() {
    return (struct CarrentalConfig) {
      3.0, 3.0, 4.0, 2.0, 0, 0
    };
};

float poisson_pmf(float lambda, unsigned int n) {
    /* PMF of the Poisson distribution: P(X=k) = (lambda^k * e^-lambda) / k!
     * We can calculate this using a loop to avoid overflow issues with large factorials.
     * For small values of N, this should be fine. For larger values, we might want to use
     * a more stable method, such as using logarithms. */
    float pmf = 1.0;
    unsigned int i;
    for (i = 1; i <= n; i++) { /* Empty product when N = 0 */
        pmf *= (lambda / (float)i);
    }
    pmf *= exp(-lambda);
    return pmf;
}

float poisson_cdf(float lambda, unsigned int n) {
    /* CDF of the Poisson distribution: P(N <= k) = sum_{i=0}^k PMF(i) */
    float pmf = exp(-lambda);
    float cdf = pmf; /* Start with PMF(0) */
    unsigned int i;
    for (i = 1; i <= n; i++) { /* Empty loop when n = 0 */
        pmf *= (lambda / (float) i); /* PMF(i) = PMF(i-1) * (lambda / i) */
        cdf += pmf;
    }
    return fminf(cdf, 1.0f);
}
   
float poisson_sf(float lambda, unsigned int n) {
    return 1.0 - poisson_cdf(lambda, n);
}

struct CarrentalEnvironment {
    /* Gen: Float_Random.Generator; */
    struct CarrentalConfig config;
    unsigned int lot_a_cars;
    unsigned int lot_b_cars;
};

/* Cars_Per_Lot_Type is the same as Observation_Type, but
 * it is kept distinct as it is used in private methods for
 * calculating transition probabilities. */
struct CarsPerLot {
    unsigned int lot_a_cars;
    unsigned int lot_b_cars;
};

struct CarsAfterAction {
    struct CarsPerLot cars_per_lot;
    unsigned int cars_moved;
};

unsigned int to_discrete_state(struct CarsPerLot cars_per_lot) {
    return cars_per_lot.lot_a_cars * (lot_size + 1) + cars_per_lot.lot_b_cars;
}

struct CarsPerLot from_discrete_state(unsigned int d) {
    return (struct CarsPerLot) {
        d / (lot_size + 1),
        d % (lot_size + 1)
    };
}
 
struct CarrentalEnvironment* carrental_make(struct CarrentalConfig config) {
    struct CarrentalEnvironment* env = malloc(sizeof(struct CarrentalEnvironment));
    if (env == NULL) {
        fprintf(stderr, "carrental_make: Failed to allocate memory for Environment\n");
        return NULL;
    }
    if (carrental_init(config, env)) {
        fprintf(stderr, "carrental_make: Failed to initialize Environment\n");
        free(env);
        return NULL;
    }
    return env;
}

int carrental_init(struct CarrentalConfig config, struct CarrentalEnvironment* env) {
    env->config = config;
    env->lot_a_cars = config.lot_a_init_cars;
    env->lot_b_cars = config.lot_b_init_cars;
    return 0;
}

struct CarrentalObservation reset(struct CarrentalEnvironment* env/*, Seed_Reset : Seed_Reset_Type*/) {
    /*
    case Seed_Reset.Kind is
        when Set_Default => Float_Random.Reset(Env.Gen);
        when No_Set      => null;
        when Set_Seed    => Float_Random.Reset(Env.Gen, Seed_Reset.Seed);
    end case;
    */
    env->lot_a_cars = env->config.lot_a_init_cars;
    env->lot_b_cars = env->config.lot_b_init_cars;
    return (struct CarrentalObservation) {
        env->lot_a_cars,
        env->lot_b_cars
    };
}

struct Step_Return step(struct CarrentalEnvironment* env, struct CarrentalAction action) {
    /* Requests and returns */
    unsigned int lot_a_requests = poisson(env->config.lot_a_request_lambda);
    unsigned int lot_b_requests = poisson(env->config.lot_b_request_lambda);
    unsigned int lot_a_returns = poisson(env->config.lot_a_return_lambda);
    unsigned int lot_b_returns = poisson(env->config.lot_b_return_lambda);

    /* Local variables */
    unsigned int local_cars_moved;
    unsigned int lot_a_rented_cars;
    unsigned int lot_b_rented_cars;
    unsigned int lot_a_updated_cars;
    unsigned int lot_b_updated_cars;

    /* First update for action */
    if (action.cars_to_move > 0) {
        /* Move cars from A to B */
        local_cars_moved = umin((unsigned int) action.cars_to_move, env->lot_a_cars);
        lot_a_updated_cars = env->lot_a_cars - local_cars_moved;
        lot_b_updated_cars = umin(env->lot_b_cars + local_cars_moved, lot_size);
    } else { /* cars_to_move < 0 */
        /* Move cars from B to A */
        local_cars_moved = umin((unsigned int) -action.cars_to_move, env->lot_b_cars);
        lot_a_updated_cars = umin(env->lot_a_cars + local_cars_moved, lot_size);
        lot_b_updated_cars = env->lot_b_cars - local_cars_moved;
    }
      
    /* Update for the next days rental requests and returns
     * The number of rentals are based on the currently available cars
     * and the requested number of cars. */
    lot_a_rented_cars = umin(lot_a_updated_cars, lot_a_requests);
    lot_b_rented_cars = umin(lot_b_updated_cars, lot_b_requests);

    /* Update the number of cars returned. */
    lot_a_updated_cars = umin(
        env->lot_a_cars - lot_a_rented_cars + lot_a_returns,
        lot_size
    );
    lot_b_updated_cars = umin(
        env->lot_b_cars - lot_b_rented_cars + lot_b_returns,
        lot_size
    );

    env->lot_a_cars = lot_a_updated_cars;
    env->lot_b_cars = lot_b_updated_cars;

    return (struct Step_Return) {
        (struct CarrentalObservation) { env->lot_a_cars, env->lot_b_cars },
        10.0*((float) (lot_a_rented_cars + lot_b_rented_cars)) - 2.0 * ((float) local_cars_moved),
        FALSE
    };
}

void carrental_deinit(struct CarrentalEnvironment* env) {
    (void)env;
    /* Nothing to do */
}

void carrental_close(struct CarrentalEnvironment* env) {
    carrental_deinit(env);
    free(env);
}

void render_text(struct CarrentalEnvironment* env) {
    printf("Lot A Cars: %u, Lot B Cars: %u", env->lot_a_cars, env->lot_b_cars);
}

struct TransProbByReqs {
    unsigned int req_length;
    unsigned int reqs[LOT_SIZE + 1];
    float probs[LOT_SIZE + 1];
};

static struct TransProbByReqs calculate_lot_request_probabilities(
    unsigned int cars_before,
    unsigned int cars_after,
    float request_lambda,
    float return_lambda
) {
    unsigned int max_interim_cars = umin(cars_before, cars_after);
    unsigned int min_requests = cars_before - max_interim_cars;
    unsigned int lot_returns;
    float prob_returns;
    struct TransProbByReqs probability_requests;
    unsigned lot_requests;  /* temp var */
    unsigned idx;  /* temp var */

    probability_requests.req_length = cars_before - min_requests + 1;
    for (lot_requests = min_requests; lot_requests <= cars_before; lot_requests++) {
        lot_returns = cars_after - (cars_before - lot_requests);
        prob_returns = poisson_pmf(return_lambda, lot_returns);
        if (cars_after >= lot_size) {
            /* If the resulting number of cars is greater than or equal to the lot size,
             * then returns exceeding Lot_Returns will also result in a full lot. */
            prob_returns += poisson_sf(return_lambda, lot_returns);
        }
        idx = lot_requests - min_requests;
        probability_requests.reqs[idx] = lot_requests;
        probability_requests.probs[idx] = poisson_pmf(request_lambda, lot_requests) * prob_returns;
        if (lot_requests == cars_before) {
            /* We add the logic here to address the warning about prob_returns being potentially uninitialized,
             * though given the definitions above this should not occur in practice. */
            /* For the value Cars_Before, we allow for any number of requests greater than
             * the current number of cars available, as this results in 0 cars remaining before returns are counted.
             * Note that we use Prob_Returns defined in the last iteration. */
            probability_requests.probs[idx] += poisson_sf(request_lambda, cars_before) * prob_returns;
        }
    }
    return probability_requests;
}

struct TransitionProbability calculate_transition_probability_between_states(
    struct CarrentalConfig config, unsigned int cars_moved, struct CarsPerLot prev_cars, struct CarsPerLot next_cars
) {
    /* In C we don't need the following as the TransProbByReqs struct
     * uses fixed length arrays with a length field and an array for
     * the number of requests.  For this reason, all the necessary information
     * is contained in the return of calculate_lot_request_probabilities.
    unsigned int max_interim_cars_lot_a = umin(prev_cars.lot_a_cars, next_cars.lot_a_cars);
    unsigned int max_interim_cars_lot_b = umin(prev_cars.lot_b_cars, next_cars.lot_b_cars);

    unsigned int min_requests_a = prev_cars.lot_a_cars - max_interim_cars_lot_a;
    unsigned int min_requests_b = prev_cars.lot_b_cars - max_interim_cars_lot_b;
    */

    unsigned int lot_a_idx;
    unsigned int lot_b_idx;
    unsigned int lot_a_requests;
    unsigned int lot_b_requests;
    /* unsigned int lot_a_returns;
       unsigned int lot_b_returns;
    */
    struct TransProbByReqs probability_requests_lot_a;
    /* Probability_Type(min_requests_a .. prev_cars.lot_a_cars) */
    struct TransProbByReqs probability_requests_lot_b;
    /*Probability_Type(min_requests_b .. prev_cars.lot_b_cars); */

    float temp_prob;
    float probability_weighted_reward = 0.0;
    float total_probability = 0.0;

    probability_requests_lot_a = calculate_lot_request_probabilities(
        prev_cars.lot_a_cars, next_cars.lot_a_cars,
        config.lot_a_request_lambda, config.lot_a_return_lambda
    );
    probability_requests_lot_b = calculate_lot_request_probabilities(
        prev_cars.lot_b_cars, next_cars.lot_b_cars,
        config.lot_b_request_lambda, config.lot_b_return_lambda
    );

    for (lot_a_idx = 0; lot_a_idx < probability_requests_lot_a.req_length; lot_a_idx++) {
        lot_a_requests = probability_requests_lot_a.reqs[lot_a_idx];
        for (lot_b_idx = 0; lot_b_idx < probability_requests_lot_b.req_length; lot_b_idx++) {
            lot_b_requests = probability_requests_lot_b.reqs[lot_b_idx];
            temp_prob = probability_requests_lot_a.probs[lot_a_idx] * probability_requests_lot_b.probs[lot_b_idx];
            total_probability += temp_prob;
            probability_weighted_reward += temp_prob * (10.0*((float) (lot_a_requests + lot_b_requests)));
        }
    }
    if (total_probability > 0.0) {
        return (struct TransitionProbability) {
            total_probability,
            (probability_weighted_reward / total_probability) - 2.0 * ((float) cars_moved)
        };
    } else {
        /* From testing it does not appear this case is reached,
         * but it is included for completeness. */
        return (struct TransitionProbability) { 0.0, 0.0 };
    }
}

struct CarsAfterAction step_cars(struct CarsPerLot cars_count, struct CarrentalAction action) {
    unsigned int cars_moved;
    struct CarsPerLot cars_per_lot;

    /* First update for action */
    if (action.cars_to_move > 0) {
        /* Move cars from A to B */
        cars_moved = umin((unsigned int) action.cars_to_move, cars_count.lot_a_cars);
        cars_per_lot.lot_a_cars = cars_count.lot_a_cars - cars_moved;
        cars_per_lot.lot_b_cars = umin(cars_count.lot_b_cars + cars_moved, lot_size);
    } else { 
        /* Action < 0
         * Move cars from B to A */
         cars_moved = umin((unsigned int) -action.cars_to_move, cars_count.lot_b_cars);
         cars_per_lot.lot_a_cars = umin(cars_count.lot_a_cars + cars_moved, lot_size);
         cars_per_lot.lot_b_cars = cars_count.lot_b_cars - cars_moved;
    }
    return (struct CarsAfterAction) {
         cars_per_lot, cars_moved
    };
}

struct CarrentalDPModel* dpmodel_new(struct CarrentalConfig config) {
    struct CarrentalDPModel* model = malloc(sizeof(struct CarrentalDPModel));
    if (model == NULL) {
        fprintf(stderr, "carrental_get_model: Failed to allocate memory for DP model\n");
        return NULL;
    }
    memset(model, 0, sizeof(struct CarrentalDPModel)); /* Initialize all probabilities and rewards to 0.0 */

    struct CarsPerLot cars_count0;
    struct CarsAfterAction cars_after_action;
    struct CarsPerLot cars_count1;
    struct CarsPerLot cars_count2;

    unsigned int s0, s2;
    int cars_to_move;
    struct CarrentalAction a;

    for(s0 = 0; s0 < NUM_DISCRETE_STATES; s0++){
        cars_count0 = from_discrete_state(s0);

        for (cars_to_move = -MAX_MOVE; cars_to_move <= MAX_MOVE; cars_to_move++) {
            a.cars_to_move = cars_to_move;

            cars_after_action = step_cars(cars_count0, a);
            cars_count1 = cars_after_action.cars_per_lot;

            for (s2 = 0; s2 < NUM_DISCRETE_STATES; s2++) {
               cars_count2 = from_discrete_state(s2);

               model->model[s0][cars_to_move + MAX_MOVE][s2] = calculate_transition_probability_between_states(
                  config, cars_after_action.cars_moved, cars_count1, cars_count2
               );
            }
        }
    }
    return model;
}

void dpmodel_free(struct CarrentalDPModel* model) {
    free(model);
}

/* Local types */
struct ExpectedReward {
    float probability_weighted_reward;
    float total_probability;
};

/* NOTE: The following is not currently used directly, but has
 *       been added for comparing with
 *       Calculate_Transition_Probability_Between_States.
 *       The testing will be done using the collected results
 *       from Get_Transition_Values and Get_Transition_Values2,
 *       which use the two different methods. */
struct TransitionArray calculate_transition_probabilities_from_state(
      struct CarrentalConfig config, unsigned int cars_moved, struct CarsPerLot prev_cars
) {
    struct TransitionArray res;
    memset(&res, 0, sizeof(res)); /* Initialize all probabilities and rewards to 0.0 */

    /* type Expected_Rewards_Type is array (Discrete_State_Type) of Expected_Reward_Type; */

    /* Local values */
    float prob_requests_lot_a;
    float prob_returns_lot_a;
    float prob_requests_lot_b;
    float prob_returns_lot_b;
    unsigned int max_lot_a_returns;
    unsigned int max_lot_b_returns;

    float temp_prob;
    float temp_reward;
    unsigned int temp_discrete_state;
    struct ExpectedReward interim_rewards[(LOT_SIZE + 1) * (LOT_SIZE + 1)];
    /* Initialize all probability-weighted rewards and total probabilities to 0.0 */
    memset(&interim_rewards, 0, sizeof(interim_rewards)); 

    unsigned int lot_a_requests;
    unsigned int lot_a_returns;
    unsigned int lot_b_requests;
    unsigned int lot_b_returns;
    /* const unsigned int num_discrete_states = (LOT_SIZE + 1) * (LOT_SIZE + 1); */ /* TODO */
    unsigned int s2;

    for(lot_a_requests = 0; lot_a_requests <= prev_cars.lot_a_cars; lot_a_requests++) {
        prob_requests_lot_a = poisson_pmf(config.lot_a_request_lambda, lot_a_requests);
        if (lot_a_requests == prev_cars.lot_a_cars) {
            /* For the value prev_cars.lot_a_cars, we allow for any number of requests greater than or equal to
             * the current number of cars available, as this results in 0 cars remaining before returns are counted.
             * We add the survival function value to the previous calculated value to obtain P(X >= x). */
            prob_requests_lot_a += poisson_sf(config.lot_a_request_lambda, prev_cars.lot_a_cars);
        }

        max_lot_a_returns = lot_size - (prev_cars.lot_a_cars - lot_a_requests);
        
        for (lot_a_returns = 0; lot_a_returns <= max_lot_a_returns; lot_a_returns++) {
            prob_returns_lot_a = poisson_pmf(config.lot_a_return_lambda, lot_a_returns);
            if (lot_a_returns == max_lot_a_returns) {
               /* Similar to requests, the maximum returns corresponds to the lot being full (Lot_Size),
                * and we add the probability for an excess number of returns (which are returned to a corporate lot)
                * to this last value. */
                prob_returns_lot_a += poisson_sf(config.lot_a_return_lambda, max_lot_a_returns);
            }

            for (lot_b_requests = 0; lot_b_requests <= prev_cars.lot_b_cars; lot_b_requests++) {
                prob_requests_lot_b = poisson_pmf(config.lot_b_request_lambda, lot_b_requests);
                if (lot_b_requests == prev_cars.lot_b_cars) {
                    prob_requests_lot_b += poisson_sf(config.lot_b_request_lambda, prev_cars.lot_b_cars);
                }

                max_lot_b_returns = lot_size - (prev_cars.lot_b_cars - lot_b_requests);
                for (lot_b_returns = 0; lot_b_returns <= max_lot_b_returns; lot_b_returns++) {
                    prob_returns_lot_b = poisson_pmf(config.lot_b_return_lambda, lot_b_returns);
                    if (lot_b_returns == max_lot_b_returns) {
                        prob_returns_lot_b += poisson_sf(config.lot_b_return_lambda, max_lot_b_returns);
                    }

                    /* Determine the output state and the associated reward and probability */
                    temp_prob = prob_requests_lot_a * prob_returns_lot_a * prob_requests_lot_b * prob_returns_lot_b;
                    temp_reward = 10.0*((float) (lot_a_requests + lot_b_requests)) - 2.0 * ((float) cars_moved);
                    temp_discrete_state = to_discrete_state( (struct CarsPerLot) {
                        prev_cars.lot_a_cars - lot_a_requests + lot_a_returns,
                        prev_cars.lot_b_cars - lot_b_requests + lot_b_returns
                    });
                    interim_rewards[temp_discrete_state].probability_weighted_reward += temp_prob * temp_reward;
                    interim_rewards[temp_discrete_state].total_probability += temp_prob;
                }
            }
        }
    }

    for (s2 = 0; s2 < NUM_DISCRETE_STATES; s2++) {
         if (interim_rewards[s2].total_probability > 0.0) {
            res.transitions[s2] = (struct TransitionProbability) {
               interim_rewards[s2].total_probability,
               interim_rewards[s2].probability_weighted_reward / interim_rewards[s2].total_probability
            };
         }
    }
    return res;
}

struct TransitionArray collect_transition_values (struct CarrentalConfig config, unsigned int dstate, struct CarrentalAction action) {
    /* Declarations */
    struct TransitionArray res;
    memset(&res, 0, sizeof(res)); /* Initialize all probabilities and rewards to 0.0 */
      
    struct CarsPerLot cars_count0;
    struct CarsAfterAction cars_after_action;
    struct CarsPerLot cars_count1;
    struct CarsPerLot cars_count2;
    unsigned int s2;
    /* const unsigned int num_discrete_states = (LOT_SIZE + 1) * (LOT_SIZE + 1); */ /* TODO */

    cars_count0 = from_discrete_state(dstate);
    cars_after_action = step_cars(cars_count0, action);
    cars_count1 = cars_after_action.cars_per_lot;
    for (s2 = 0; s2 < NUM_DISCRETE_STATES; s2++) {
        cars_count2 = from_discrete_state(s2);
        res.transitions[s2] = calculate_transition_probability_between_states(
            config, cars_after_action.cars_moved, cars_count1, cars_count2
        );
    }
    return res;
}

struct TransitionArray get_transition_values_from_state(struct CarrentalConfig config, unsigned int dstate, struct CarrentalAction action) {
    struct CarsPerLot cars_count0;
    struct CarsAfterAction cars_after_action;
    struct CarsPerLot cars_count1;

    cars_count0 = from_discrete_state(dstate);
    cars_after_action = step_cars(cars_count0, action);
    cars_count1 = cars_after_action.cars_per_lot;

    return calculate_transition_probabilities_from_state(config, cars_after_action.cars_moved, cars_count1);
}

int carrental_example_main() {
    unsigned int i = 0;
    printf("Hello, car rental environment users!\n");

    for (i = 0; i < 10; i++) {
        printf("Poisson PMFs for lambda=3 and i=%u: %f\n", i, poisson_pmf(3.0, i));
    }

    for (i = 0; i < 10; i++) {
        printf("Poisson random number with lambda=3: %u\n", poisson(3.0));
    }
    
    return 0;
}
