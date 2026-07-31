#include <reinforcementlearning/algorithms/epsilon_policies.h>
#include <reinforcementlearning/math.h> /* fmaxf */

/* TODO: The epsilon trajectory in the following needs improvement */
float update_epsilon(struct SARSAConfig sarsa_config, unsigned int episode) {
    float init_eps = sarsa_config.initial_epsilon;
    float min_eps = sarsa_config.minimum_epsilon;
    float episode_to_min_eps = (float) sarsa_config.episodes_to_minimum_epsilon;
    float k = episode_to_min_eps * min_eps / init_eps;

    return fmaxf(min_eps, k * init_eps / ((float) episode));
}
