#include "poisson.h"

enum Boolean {
    FALSE,
    TRUE
};

/*  ## Description
 *  The Car Rental environment involves an owner of a car rental business
 *  which has two locations, which are referred to as lot A and B.  Each
 *  day customers come in to rent and return cars.  A car that is returned
 *  is only available for rental the next day.  At the end of each
 *  day the owner must make a choice about how many cars to move between
 *  the two sites with the aim being to make sure there
 *  are enough cars available for rental at each site.
 *  The owner makes a fixed profit for every car rented, and
 *  pays a fixed amount for each car moved overnight.
 *
 *  The number of rental requests and returns at each site follow
 *  independent Poisson processes.  The lot size at each location
 *  is fixed and so there is a maximum number of cars that can
 *  be made available for rental at each location.  If the number
 *  of returns would result in an overflow at the lot then the
 *  excess cars are returned to a lot maintained by the corporate
 *  headquarters and are not subsequently available for rent.
 *  
 *  The number of cars that can be moved overnight is fixed as well.
 *
 *  The parameters and constraints referenced above are mostly
 *  fixed (constant) in this implementation, i.e. we don't
 *  support specifying these parameters either as generic
 *  parameters or as values in the configuration type.
 *  We will provide further information about the values of
 *  these constants in the sections that follow.
 *  The Poisson mean request and returns rates and the initial
 *  number of cars in each lot can be specified though.
 * 
 *  This environment is adapted from Example 4.2 (page 98) of
 *  Reinforcement Learning: An Introduction (1998) by Sutton and Barto.
 *
 *  ## Action Space
 *  Actions represent the number of cars transferred between Lot A
 *  and Lot B.  The maximum number of cars that can be moved
 *  is 5.  The range for the action values is -5, -4, ..., +5.
 *  A positive number indicates cars were moved from Lot A to B,
 *  while a negative number indicates cars were moved in the
 *  opposite direction.
 * 
 *  ## Observation Space
 *  The observation space consists of two values, namely the number of
 *  cars present on each lot at the end of the day.  These numbers
 *  reflect the cars rented and returned that day.
 *  For each lot, the number of cars on each lot is an integer
 *  between 0 and 20 (inclusive).  If the returns would
 *  cause this number to be above 20, then those cars are assumed
 *  to be moved to a lot maintained by corporate headquarters
 *  and are not available for rent.
 *
 *  For the Dynamic Programming model, we require a discrete state,
 *  and so we convert the number of cars in the two lots to a
 *  single value.  The approach used is the same approach used to
 *  convert coordinates in other environments to a single
 *  position index.  If the number of cars is given by a tuple
 *  (a, b), then the corresponding discrete state value is given
 *  by a * 21 + b.
 *
 *  ## Rewards
 *  The owner makes a profit of $10 dollars for each car rented,
 *  and pays $2 for each car moved.
 *  Only cars available at the lot can be rented, and
 *  so no profit is made for any requests in excess of the
 *  number of cars available (the customers are turned away).
 *
 *  ## Environment Configuration
 *  The Poisson mean rate for returns and requests
 *  at each lot can be specified.  Also, the number of
 *  cars initially available at each site can be provided.
 *
 *  ## References
 *  Sutton, R. S., Barto, A. G. (2018).
 *  Reinforcement Learning: An Introduction. The MIT Press.
 *  http://incompleteideas.net/book/the-book-1st.html */

 /*
   package Float_Random renames Ada.Numerics.Float_Random;
   package GRF is new Mathpaqs.Generic_Random_Functions(Real => Float);
*/

#define LOT_SIZE 20
#define NUM_DISCRETE_STATES ((LOT_SIZE + 1) * (LOT_SIZE + 1))
#define MAX_MOVE 5

const unsigned int lot_size = LOT_SIZE;
const unsigned int max_move = MAX_MOVE;

struct Action {
    int cars_to_move;
};

struct Config {
    float lot_a_request_lambda;
  	float lot_a_return_lambda;
  	float lot_b_request_lambda;
  	float lot_b_return_lambda;
  	unsigned int lot_a_init_cars;
  	unsigned int lot_b_init_cars;
};

struct Config get_default_config();

struct Environment;

struct Observation {
    unsigned int lot_a_cars;
    unsigned int lot_b_cars;
};

struct Step_Return {
    struct Observation observation;
    float reward;
    enum Boolean terminated;
};

struct Environment* carrental_make(struct Config config);
int carrental_init(struct Config config, struct Environment*);
struct Observation reset(struct Environment* env/*, Seed_Reset : Seed_Reset_Type*/);
struct Step_Return step(struct Environment* env, struct Action action);
void carrental_deinit(struct Environment* env);
void carrental_close(struct Environment* env);

void render_text(struct Environment* env);

struct TransitionProbability {
    float probability;
    float reward;
};

/*
   type Discrete_State_Type is new Natural range 0 .. ((Lot_Size + 1) * (Lot_Size + 1) - 1);
*/
struct DPModel {
    struct TransitionProbability model[NUM_DISCRETE_STATES][2 * MAX_MOVE + 1][NUM_DISCRETE_STATES];
};

/*
   type DP_Model_Type is array (Discrete_State_Type, Action_Type, Discrete_State_Type) of Transition_Probability_Type;
   type DP_Model_Access_Type is access DP_Model_Type;
   DM_Pool : System.Pool_Local.Unbounded_Reclaim_Pool;
   for DP_Model_Access_Type'Storage_Pool use DM_Pool;
*/

/* NOTE: The following is the RL Ada tranlation of Get_Model
 *       We have chaged the function name to make it clear that
 *       a new model is allocated and returned. */
struct DPModel* dpmodel_new(struct Config config);
void dpmodel_free(struct DPModel* model);

struct TransitionArray {
    struct TransitionProbability transitions[NUM_DISCRETE_STATES];
};

/* The following uses Calculate_Transition_Probability_Between_States
 * to calculate the transition probabilities for all possible next states
 * from the given state and action. */
struct TransitionArray collect_transition_values (struct Config config, unsigned int dstate, struct Action action);
/* The following uses Calculate_Transition_Probabilities_From_State
 * to calculate the transition probabilities for all possible next states
 * from the given state and action. */
struct TransitionArray get_transition_values_from_state(struct Config config, unsigned int dstate, struct Action action);

float poisson_pmf(float lambda, unsigned int n);
float poisson_cdf(float lambda, unsigned int n);
float poisson_sf(float lambda, unsigned int n);

struct CarsPerLot;
struct CarsAfterAction;

/* The following convert the observation type to and from the
 * discrete representation. */
unsigned int to_discrete_state(struct CarsPerLot cars_per_lot);
struct CarsPerLot from_discrete_state(unsigned int discrete_state);

/* The following are for calculating the transition probabilities for the
 * dynamic programming model. */
struct CarsAfterAction step_cars(struct CarsPerLot cars_count, struct Action action);
/* There are two methods for calculating transition probabilities, though only one is
 * currently used in the discrete model calculations.
 * The first method calculates the transition probability between two specific states,
 * the "initial" state actually being the state immediately after the action has been applied
 * (or more specifically after the cars have been moved between sites overnight
 * but before the rental requests and returns that occur the next day). */
struct TransitionProbability calculate_transition_probability_between_states(
    struct Config config, unsigned int cars_moved, struct CarsPerLot prev_cars, struct CarsPerLot next_cars
);
/* The second method calculates all the transition probabilities starting from the
 * post-action state.  The output is an array of transition probabilities for
 * all possible next states. */
struct TransitionArray calculate_transition_probabilities_from_state(
      struct Config config, unsigned int cars_moved, struct CarsPerLot prev_cars
);

int main();
