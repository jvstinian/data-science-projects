#include <reinforcementlearning/envs/cartpole.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif
/* Using macro definition specified in GNU documentation
 * https://ftp.gnu.org/old-gnu/Manuals/glibc-2.2.3/html_chapter/libc_19.html
 * Looks like the next digit is 9 so it should probably be rounded to
 * 3.14...328.
 * Alternatively: M_PI acos(-1.0) */

struct CartpoleConfig cartpole_default_config() {
    return (struct CartpoleConfig) { FALSE, Euler };
}

struct CartpoleEnvironment {
    struct CartpoleConfig config;
    struct CartpoleObservation state;
};

struct CartpoleEnvironment* cartpole_make(struct CartpoleConfig config) {
    struct CartpoleEnvironment* env = malloc(sizeof(struct CartpoleEnvironment));
    if (env == NULL) {
        fprintf(stderr, "cartpole_make: Failed to allocate memory for CartpoleEnvironment\n");
        return NULL;
    }
    env->config = config;
    env->state = (struct CartpoleObservation) { 0.0, 0.0, 0.0, 0.0 };
    return env;
}

void cartpole_close(struct CartpoleEnvironment* env) {
    free(env);
}

static const float x_threshold = 2.4;
static const float theta_threshold_radians = 12.0 * 2.0 * M_PI / 360.0;
static const float gravity = 9.8;
static const float masscart = 1.0;
static const float masspole = 0.1;
static const float total_mass = masspole + masscart;
static const float length = 0.5;  /* actually half the pole's length */
static const float polemass_length = masspole * length;
static const float force_mag = 10.0;
static const float tau = 0.02; /* seconds between state updates */


float rand_float() {
    return (float)rand() / (float)RAND_MAX;
};

struct CartpoleObservation cartpole_reset(struct CartpoleEnvironment* env) {
    const float low = -0.05;
    const float high = 0.05;
    const float high_low_diff = high - low;

    /* Reset RNG */
    srand(time(NULL));

    env->state = (struct CartpoleObservation) {
        low + high_low_diff * rand_float(), /* x */
        low + high_low_diff * rand_float(), /* x_dot */
        low + high_low_diff * rand_float(), /* theta */
        low + high_low_diff * rand_float() /* theta_dot */
    };
    return env->state;
}

struct CartpoleStepReturn cartpole_step(struct CartpoleEnvironment* env, enum CartpoleAction action) {
    struct CartpoleObservation state = env->state;
    float x = state.x;
    float x_dot = state.x_dot;
    float theta = state.theta;
    float theta_dot = state.theta_dot;

    float force;
    if (action == Right) {
        force = force_mag;
    } else {
        force = -force_mag;
    }
    float costheta = cos(theta);
    float sintheta = sin(theta);
    float temp;
    float thetaacc;
    float xacc;

    /* Return components */
    float reward;
    Boolean terminated;
    /* From the python code:
     * # For the interested reader:
     * # https://coneural.org/florian/papers/05_cart_pole.pdf */
    temp = (
        force + polemass_length * (theta_dot * theta_dot) * sintheta
    ) / total_mass;
    thetaacc = (gravity * sintheta - costheta * temp) / (
        length
        * (4.0 / 3.0 - masspole * (costheta * costheta) / total_mass)
    );
    xacc = temp - polemass_length * thetaacc * costheta / total_mass;

    switch (env->config.kinematics_integrator) {
        case Semi_Implicit:
            x_dot = x_dot + tau * xacc;
            x = x + tau * x_dot;
            theta_dot = theta_dot + tau * thetaacc;
            theta = theta + tau * theta_dot;
            break;
        case Euler:
        default:
            x = x + tau * x_dot;
            x_dot = x_dot + tau * xacc;
            theta = theta + tau * theta_dot;
            theta_dot = theta_dot + tau * thetaacc;
            break;
    };
    env->state = (struct CartpoleObservation) {
        x, x_dot, theta, theta_dot 
    };

    terminated = (x < -x_threshold)
        || (x > x_threshold)
        || (theta < -theta_threshold_radians)
        || (theta > theta_threshold_radians);

    /* We stick to the logic of the Gymnasium implementation, but
     * it might be clearer to have
     * if not Sutton_Barto_Reward then 1.0
     * else ... */
    if (!terminated) {
        if (env->config.sutton_barto_reward) {
            reward = 0.0;
        } else {
            reward = 1.0;
        }
    } else {
        if (env->config.sutton_barto_reward) {
            reward = -1.0;
        } else {
            reward = 1.0;
        }
    }
    return (struct CartpoleStepReturn) { env->state, reward, terminated };
}

int cartpole_main() {
    struct CartpoleConfig config = cartpole_default_config();
    struct CartpoleEnvironment* env = cartpole_make(config);
    if (env == NULL) {
        return 1;
    }
    struct CartpoleObservation obs;
    enum CartpoleAction action;
    struct CartpoleStepReturn step_return;
    Boolean terminated = FALSE;
    float total_reward = 0.0f;

    obs = cartpole_reset(env);
    while (!terminated) {
        action = (enum CartpoleAction) rand() % 2;
        step_return = cartpole_step(env, action);
        obs = step_return.observation;
        (void)obs;
        terminated = step_return.terminated;
        total_reward += step_return.reward;
    }
    printf("Episode total reward: %f\n", total_reward);
    cartpole_close(env);
    return 0;
}
