#ifndef INC_RL_ENVS_CLIFFWALKING_H
#define INC_RL_ENVS_CLIFFWALKING_H

/* Cliff walking involves crossing a gridworld from start to goal while
 *  avoiding falling off a cliff.
 * 
 *  ## Description
 *  The game starts with the player at location (3, 0) of the 4x12 grid world
 *  with the goal located at (3, 11). If the player reaches the goal the
 *  episode ends.
 * 
 *  A cliff runs along (3, 1..10). If the player moves to a cliff location it
 *  returns to the start location.
 * 
 *  The player makes moves until they reach the goal.
 * 
 *  Adapted from Example 6.6 (page 132) from
 *  Reinforcement Learning: An Introduction
 *  by Sutton and Barto.
 * 
 *  The cliff can be chosen to be slippery (disabled by default) so the player
 *  may move perpendicular to the intended direction sometimes.
 * 
 *  With inspiration from:
 *  [https://github.com/dennybritz/reinforcement-learning/blob/master/lib/envs/cliff_walking.py](https://github.com/dennybritz/reinforcement-learning/blob/master/lib/envs/cliff_walking.py)
 * 
 *  ## Action Space
 *  Actions represent moving in one of four directions, namely
 *  Left, Down, Right, or Up.
 * 
 *  ## Observation Space
 *  There are 3 x 12 + 1 possible states. The player cannot be at the cliff,
 *  nor at the goal as the latter results in the end of the episode. What
 *  remains are all the positions of the first 3 rows plus the bottom-left cell.
 * 
 *  The observation is a value representing the player's current position as
 *  current_row * ncols + current_col (where both the row and col start at 0).
 * 
 *  For example, the starting position can be calculated as follows:
 *  3 * 12 + 0 = 36.
 * 
 *  ## Starting State
 *  The episode starts with the player in state `36` (location (3, 0)).
 * 
 *  ## Reward
 *  Each time step incurs -1 reward, unless the player stepped into the cliff,
 *  which incurs -100 reward.
 * 
 *  ## Episode End
 *  The episode terminates when the player enters state `47` (location (3, 11)).
 * 
 *  As cliff walking is not stochastic, the transition probability returned always 1.0.
 * 
 *  ## References
 *  “Reinforcement Learning: An Introduction” 2020. [Online]. Available:
 *  [http://www.incompleteideas.net/book/RLbook2020.pdf](http://www.incompleteideas.net/book/RLbook2020.pdf)
 * 
 *  Some differences with the Python Gymnasium implementation include:
 *  - the Python version returns probability info (e.g., {"prob": 1})
 *    in the reset and step methods, whereas this implementation
 *    omits this info
 *  - contrary to the description above, in this Ada implementation the
 *    position of the agent is represented using a 1-based index for the row
 *    and column, but the public observation type is the flattened 0-based
 *    position index (the description above is taken from the Python 
 *    implementation with a few adjustments, and we decided to keep the
 *    explanation that consistently uses the 0-based indices to avoid
 *    confusion, particularly since the use of 1-based indices is 
 *    only used in the private part of the specification) */

#include <reinforcementlearning/bool.h>
#include <reinforcementlearning/algorithms/dp_transitions.h>

struct CliffwalkingConfig {
  Boolean is_slippery;
};
   
enum CliffwalkingAction {
    LEFT,
    DOWN,
    RIGHT,
    UP
};

#define CLIFFWALK_ACTION_COUNT 4
#define CLIFFWALK_NUM_ROWS 4
#define CLIFFWALK_NUM_COLS 12
#define CLIFFWALK_NUM_STATES (CLIFFWALK_NUM_ROWS * CLIFFWALK_NUM_COLS)

struct CliffwalkingObservation {
    unsigned int position_index;
};
   
struct CliffwalkingEnvironment;

struct CliffwalkingStepReturn {
    struct CliffwalkingObservation observation;
    float reward;
    Boolean terminated;
};
  
struct CliffwalkingEnvironment* cliffwalking_make(struct CliffwalkingConfig config);
struct CliffwalkingObservation cliffwalking_reset(struct CliffwalkingEnvironment* env);
struct CliffwalkingStepReturn cliffwalking_step(struct CliffwalkingEnvironment* env, enum CliffwalkingAction action);
void cliffwalking_close(struct CliffwalkingEnvironment* env);
void cliffwalking_render_text(struct CliffwalkingEnvironment env);

struct CliffwalkingDPModel;
struct CliffwalkingDPModel* cliffwalking_dpmodel_new(struct CliffwalkingConfig config);
void cliffwalking_dpmodel_free(struct CliffwalkingDPModel* model);
struct TransitionProbability cliffwalking_get_transition(const struct CliffwalkingDPModel* model, unsigned int s, enum CliffwalkingAction action, unsigned int next_s);

#endif
