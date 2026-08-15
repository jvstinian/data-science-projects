#ifndef INC_RL_ENVS_CARTPOLE_H
#define INC_RL_ENVS_CARTPOLE_H

#include <reinforcementlearning/bool.h>
    
/* Sutton_Barto_Reward determines the reward values.
   When True, a reward of 0 is awarded for non-terminating steps and -1 for the terminating step
   When False (the default), 1 is awarded for each step.

   Kinematics_Integrator_Type determines how the position and angle are updated.
*/

enum KinematicsIntegrator {
    Euler,
    Semi_Implicit
};

struct CartpoleConfig {
    Boolean sutton_barto_reward; /* False */
    enum KinematicsIntegrator kinematics_integrator; /* Euler */
};

enum CartpoleAction {
    Left,
    Right
};
    
struct CartpoleObservation {
    float x;
    float x_dot;
    float theta;
    float theta_dot;
};

struct CartpoleEnvironment;

struct CartpoleStepReturn {
    struct CartpoleObservation observation;
    float reward;
    Boolean terminated;
};

struct CartpoleConfig cartpole_default_config();
struct CartpoleEnvironment* cartpole_make(struct CartpoleConfig config);
/* TODO: Consider specifying how to set the seed */
struct CartpoleObservation cartpole_reset(struct CartpoleEnvironment* env);
struct CartpoleStepReturn cartpole_step(struct CartpoleEnvironment* env, enum CartpoleAction action);
void cartpole_close(struct CartpoleEnvironment* env);

#endif
