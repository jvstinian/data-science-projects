#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

int gcd(int a, int b) {
    int r;
    r = a % b;

    if (r == 0) {
        return b;
    } else {
        return gcd(b, r);
    }
}

typedef enum Boolean {
    FALSE,
    TRUE
} Boolean;
    
struct CartpoleEnvironment {
    float x;
    float x_dot;
    float theta;
    float theta_dot;
};

/* TODO: Might need to add a prefix for the action values */
enum CartpoleAction {
    Left,
    Right
};
    
struct CartpoleStepReturn {
    struct CartpoleEnvironment observation;
    float reward;
    Boolean terminated;
};

static Boolean sutton_barto_reward = FALSE;
    
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

Boolean get_sutton_barto_reward() {
    return sutton_barto_reward;
}

void set_sutton_barto_reward(Boolean use_sutton_barto_reward) {
    sutton_barto_reward = use_sutton_barto_reward;
};


float rand_float() {
    return (float)rand() / (float)RAND_MAX;
};

struct CartpoleEnvironment reset() {
    /* The following aren't currently used
     * seed: int | None = None,
     * options: dict | None = None, */
    const float low = -0.05;
    const float high = 0.05;
    const float high_low_diff = high - low;
    struct CartpoleEnvironment state;

    /* super().reset(seed=seed) */
    /* Reset RNG */
    srand(time(NULL));
    /* # Note that if you use custom reset bounds, it may lead to out-of-bound
     * # state/observations.
     * NOTE (JS): I haven't looked at the details of the implementation of the following method, 
     *            I am just guessing here.
     * low, high = utils.maybe_parse_reset_bounds(
     *     options,
     *     -0.05,
     *     0.05,  # default low
     * )  # default high */

    state = (struct CartpoleEnvironment) { 
        low + high_low_diff * rand_float(), /* x */
        low + high_low_diff * rand_float(), /* x_dot */
        low + high_low_diff * rand_float(), /* theta */
        low + high_low_diff * rand_float() /* theta_dot */
    };
    /*
    self.steps_beyond_terminated = None
    if self.render_mode == "human":
        self.render()
    return np.array(self.state, dtype=np.float32), {}
    */
    return state;
}

struct CartpoleStepReturn step(struct CartpoleEnvironment state, enum CartpoleAction action) {
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
    struct CartpoleEnvironment output_state;
    Boolean terminated;
    /* struct CartpoleStepReturn ret; */
    /* # For the interested reader:
     * # https://coneural.org/florian/papers/05_cart_pole.pdf */
    temp = (
        force + polemass_length * (theta_dot * theta_dot) * sintheta
    ) / total_mass;
    thetaacc = (gravity * sintheta - costheta * temp) / (
        length
        * (4.0 / 3.0 - masspole * (costheta * costheta) / total_mass)
    );
    xacc = temp - polemass_length * thetaacc * costheta / total_mass;

    /* if self.kinematics_integrator == "euler": */
    x = x + tau * x_dot;
    x_dot = x_dot + tau * xacc;
    theta = theta + tau * theta_dot;
    theta_dot = theta_dot + tau * thetaacc;
    /*
    -- else:  # semi-implicit euler
    --     X_Dot = X_Dot + self.Tau * Xacc
    --        x = x + self.Tau * X_Dot
    --        Theta_Dot = Theta_Dot + self.Tau * thetaacc
    --        Theta = theta + self.Tau * Theta_Dot
    --
    */
    /* TODO: overwrite state */
    output_state = (struct CartpoleEnvironment) {
        x, x_dot, theta, theta_dot 
    };

    terminated = (x < -x_threshold)
        || (x > x_threshold)
        || (theta < -theta_threshold_radians)
        || (theta > theta_threshold_radians);

    if (!terminated) {
        if (sutton_barto_reward) {
            reward = 0.0;
        } else {
            reward = 1.0;
        }
    } else {
        if (sutton_barto_reward) {
            reward = -1.0;
        } else {
            reward = 1.0;
        }
    }
    /*
        -- else if steps_beyond_terminated is None:
        --     # Pole just fell!
        --     self.steps_beyond_terminated = 0

        --     reward = -1.0 if self._sutton_barto_reward else 1.0
        -- else:
        --     if self.steps_beyond_terminated == 0:
        --         logger.warn(
        --             "You are calling 'step()' even though this environment has already returned terminated = True. "
        --             "You should always call 'reset()' once you receive 'terminated = True' -- any further steps are undefined behavior."
        --         )
        --     self.steps_beyond_terminated += 1

        --     reward = -1.0 if self._sutton_barto_reward else 0.0

        -- if self.render_mode == "human":
        --     self.render()

        -- # truncation=False as the time limit is handled by the `TimeLimit` wrapper added during `make`
        -- return np.array(self.state, dtype=np.float32), reward, terminated, False, {}
    */
    return (struct CartpoleStepReturn) { output_state, reward, terminated };
}

int main() {
    int a = 221;
    int b = 26;
    int r;

    r = gcd(a, b);
    printf("gcd(%d, %d) = %d\n", a, b, r);
    
    struct CartpoleEnvironment env = reset();
    enum CartpoleAction action;
    struct CartpoleStepReturn step_return;
    Boolean terminated = FALSE;
    float total_reward = 0.0f;
    while (!terminated) {
        action = (enum CartpoleAction) rand() % 2;
        step_return = step(env, action);
        env = step_return.observation;
        terminated = step_return.terminated;
        total_reward += step_return.reward;
    }
    printf("Episode total reward: %f\n", total_reward);
    return 0;
}

