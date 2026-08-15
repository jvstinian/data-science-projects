#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <reinforcementlearning/envs/linewalk.h>
#include <reinforcementlearning/envs/frozenlake.h>
#include <reinforcementlearning/envs/cliffwalking.h>
#include <reinforcementlearning/envs/carrental.h>


START_TEST (test_example1)
{
    ck_assert(1);
}
END_TEST

START_TEST (test_example2)
{
    ck_assert_msg(1, "Example failure!");
}

START_TEST (test_linewalk_random_actions)
{
    struct LineWalkConfig config = { 7 };
    struct SimulationSummary sim_summary;
    sim_summary = linewalk_uniform_random_actions(config, /* verbose */ FALSE);

    ck_assert_msg(sim_summary.num_steps <= 10, "Steps exceeded 10");
    ck_assert_msg(sim_summary.total_reward >= -1.0, "Reward is less than -1");
    ck_assert_msg(sim_summary.total_reward <= 1.0, "Reward exceeds 1");
}

START_TEST (test_linewalk_mcts_random_actions)
{
    struct LineWalkConfig config = { 5 };
    struct SimulationSummary sim_summary;
    sim_summary = linewalk_mctsenv_uniform_random_actions(config, 10);

    ck_assert_msg(sim_summary.num_steps <= 10, "Steps exceeded 10");
    ck_assert_msg(sim_summary.total_reward >= -1.0, "Reward is less than -1");
    ck_assert_msg(sim_summary.total_reward <= 1.0, "Reward exceeds 1");
}

START_TEST (test_frozenlake_random_actions)
{
    /* Test the slippery map */
    struct FrozenlakeConfig config = { MAP_4X4, TRUE };
    struct SimulationSummary sim_summary;
    sim_summary = frozenlake_uniform_random_actions(config, /* verbose */ FALSE);

    ck_assert_msg(sim_summary.num_steps <= 30, "Steps exceeded 30");
    ck_assert_msg(sim_summary.total_reward >= 0.0, "Reward is less than 0");
    ck_assert_msg(sim_summary.total_reward <= 1.0, "Reward exceeds 1");
}

START_TEST (test_frozenlake_dp_model_nonslippery) {
    /* Test the non-slippery map */
    struct FrozenlakeConfig config = { MAP_4X4, FALSE };
    struct FrozenlakeDPModel *model;
    float total_transition_prob;
    unsigned int s, s1;
    enum FrozenlakeAction a;
    unsigned int num_states = frozenlake_get_num_states(config);

    if ((model = frozenlake_dpmodel_new(config)) == NULL) {
        ck_assert_msg(FALSE, "Failed to initialize Frozenlake DP model");
    }

    for(s = 0; s < num_states; s++) {
        for (a = 0; a < FROZENLAKE_ACTION_COUNT; a++) {
            total_transition_prob = 0.0f;
            for(s1 = 0; s1 < num_states; s1++) {
                total_transition_prob += frozenlake_get_transition(model, s, a, s1).probability;
            }
            /* ck_assert_float_eq_tol seems to fail with C89, even when using an expression like
               ck_assert_float_eq_tol(total_transition_prob, total_transition_prob, 0.01f).
               ck_assert_float_eq_tol expands to use fabsl, which is not part of C89, so perhaps
               this is the issue, or there's some other issue arising from the use of Nix or
               my compiler settings.

               This issue seems to have occurred for others as well:
               https://github.com/libcheck/check/issues/252
               
               I am going to manually check using fabs instead of fabsl. */
            /* ck_assert_float_eq_tol(total_transition_prob, 1.0f, 1e-6f); */
            ck_assert_msg(
                    fabs(total_transition_prob - 1.0f) < 1e-6f,
                    "Total transition probability not approximately equal to 1"
            );
        }
    }
    frozenlake_dpmodel_free(model);
}

START_TEST (test_frozenlake_dp_model_slippery) {
    /* Test the slippery map */
    struct FrozenlakeConfig config = { MAP_4X4, TRUE };
    struct FrozenlakeDPModel *model;
    float total_transition_prob;
    unsigned int s, s1;
    enum FrozenlakeAction a;
    unsigned int num_states = frozenlake_get_num_states(config);

    if ((model = frozenlake_dpmodel_new(config)) == NULL) {
        ck_assert_msg(FALSE, "Failed to initialize Frozenlake DP model");
    }

    for(s = 0; s < num_states; s++) {
        for (a = 0; a < FROZENLAKE_ACTION_COUNT; a++) {
            total_transition_prob = 0.0f;
            for(s1 = 0; s1 < num_states; s1++) {
                total_transition_prob += frozenlake_get_transition(model, s, a, s1).probability;
            }
            /* See discussion regarding ck_assert_float_eq_tol above */
            ck_assert_msg(
                    fabs(total_transition_prob - 1.0f) < 1e-6f,
                    "Total transition probability not approximately equal to 1"
            );
            /*
            ck_assert_float_eq_tol(total_transition_prob, 1.0f, 1e-6f);
            */
        }
    }
    frozenlake_dpmodel_free(model);
}

START_TEST (test_frozenlake_dp_iterative_policy_evaluation) {
    struct FrozenlakeConfig config = { MAP_4X4, TRUE };
    struct FrozenlakeDPModel* model = frozenlake_dpmodel_new(config);
    if (model == NULL) {
        ck_assert_msg(FALSE, "Failed to initialize Frozenlake DP model");
    }

    unsigned int s;
    int iterations;
    float stoch_policy[16][FROZENLAKE_ACTION_COUNT];
    float value_array[16];
    for (s = 0; s < 16; s++) {
        stoch_policy[s][LEFT] = 0.25;
        stoch_policy[s][DOWN] = 0.25;
        stoch_policy[s][RIGHT] = 0.25;
        stoch_policy[s][UP] = 0.25;
    }
    iterations = frozenlake_iterative_policy_evaluation(model, stoch_policy, 0.9, value_array);

    ck_assert_msg(iterations <= 30, "frozenlake_iterative_policy_evaluation iterations exceeded 30");

    for (s = 0; s < 16; s++) {
        ck_assert_msg(
            value_array[s] >= 0.0,
            "value function is negative"
        );
    }

    frozenlake_dpmodel_free(model);
}

START_TEST (test_frozenlake_mc_policy_evaluation) {
    unsigned int s;
    struct FrozenlakeConfig config = { MAP_4X4, FALSE };
    enum FrozenlakeAction dpolicy[16];
    dpolicy[0] = DOWN;
    dpolicy[1] = RIGHT;
    dpolicy[2] = DOWN;
    dpolicy[3] = LEFT;
    dpolicy[4] = DOWN;
    dpolicy[5] = LEFT;
    dpolicy[6] = DOWN;
    dpolicy[7] = LEFT;
    dpolicy[8] = RIGHT;
    dpolicy[9] = DOWN;
    dpolicy[10] = DOWN;
    dpolicy[11] = LEFT;
    dpolicy[12] = LEFT;
    dpolicy[13] = RIGHT;
    dpolicy[14] = RIGHT;
    dpolicy[15] = LEFT;
    struct MCConfig mc_config = { 100, 50, FIRST_VISIT, 0.9 };
    float svalue_func[16];
    /* In the current implementation, MC policy evaluation for a
     * deterministic policy results in zero for those states that
     * are not reachable. */
    float exp_value_func[16] = { 
        0.5905, 0.0 , 0.0, 0.0, 
        0.6561, 0.0 , 0.0, 0.0, 
        0.7290, 0.81, 0.0, 0.0,
        0.0   , 0.9 , 1.0, 0.0
    };

    int status = frozenlake_mc_policy_evaluation(config, dpolicy, mc_config, svalue_func);

    ck_assert_msg(status == 0, "frozenlake_mc_policy_evaluation failed");
    for (s = 0; s < 16; s++) {
        ck_assert_msg(
            fabs(svalue_func[s] - exp_value_func[s]) < 1e-4f,
            "the expected and actual state value functions differ"
        );
    }
}

START_TEST (test_cliffwalking_random_actions)
{
    /* Test the slippery map */
    struct CliffwalkingConfig config = { TRUE };
    struct SimulationSummary sim_summary;
    sim_summary = cliffwalking_uniform_random_actions(config, /* verbose */ FALSE);

    ck_assert_msg(sim_summary.num_steps <= 1000, "Steps exceeded 1000");
    ck_assert_msg(sim_summary.total_reward <= 0.0, "Reward exceeds 0");
}

START_TEST (test_cliffwalking_dp_model_nonslippery) {
    /* Test the non-slippery map */
    struct CliffwalkingConfig config = { FALSE };
    struct CliffwalkingDPModel *model;
    float total_transition_prob;
    unsigned int s, s1;
    enum CliffwalkingAction a;
    unsigned int num_states;

    if ((model = cliffwalking_dpmodel_new(config)) == NULL) {
        ck_assert_msg(FALSE, "Failed to initialize Cliffwalking DP model");
    }
    /* NOTE: In frozenlake, we used frozenlake_get_num_states(config) to get the
     *       number of states, but here we use the value provided with the model. */
    num_states = cliffwalking_get_num_states(model);

    for(s = 0; s < num_states; s++) {
        for (a = 0; a < CLIFFWALK_ACTION_COUNT; a++) {
            total_transition_prob = 0.0f;
            for(s1 = 0; s1 < num_states; s1++) {
                total_transition_prob += cliffwalking_get_transition(model, s, a, s1).probability;
            }
            /* See discussion regarding ck_assert_float_eq_tol above */
            ck_assert_msg(
                    fabs(total_transition_prob - 1.0f) < 1e-6f,
                    "Total transition probability not approximately equal to 1"
            );
        }
    }
    cliffwalking_dpmodel_free(model);
}

START_TEST (test_cliffwalking_dp_model_slippery) {
    /* Test the slippery map */
    struct CliffwalkingConfig config = { TRUE };
    struct CliffwalkingDPModel *model;
    float total_transition_prob;
    unsigned int s, s1;
    enum CliffwalkingAction a;
    unsigned int num_states;

    if ((model = cliffwalking_dpmodel_new(config)) == NULL) {
        ck_assert_msg(FALSE, "Failed to initialize Cliffwalking DP model");
    }
    num_states = cliffwalking_get_num_states(model);

    for(s = 0; s < num_states; s++) {
        for (a = 0; a < CLIFFWALK_ACTION_COUNT; a++) {
            total_transition_prob = 0.0f;
            for(s1 = 0; s1 < num_states; s1++) {
                total_transition_prob += cliffwalking_get_transition(model, s, a, s1).probability;
            }
            /* See discussion regarding ck_assert_float_eq_tol above */
            ck_assert_msg(
                    fabs(total_transition_prob - 1.0f) < 1e-6f,
                    "Total transition probability not approximately equal to 1"
            );
        }
    }
    cliffwalking_dpmodel_free(model);
}


START_TEST (test_carrental_no_terminate)
{
    /* Test the slippery map */
    struct CarrentalConfig config = get_default_config();
    struct CarrentalEnvironment *env = carrental_make(config);
    carrental_reset(env);  /* Output not used */
    struct CarrentalAction a = carrental_get_random_action();
    struct CarrentalStepReturn step_ret = carrental_step(env, a);
    ck_assert_msg(!step_ret.terminated, "Carrental environment should not terminate");
}

START_TEST (test_carrental_dp_model) {
    struct CarrentalConfig config = get_default_config();
    struct CarrentalDPModel *model;
    float total_transition_prob;
    unsigned int s, s1;
    unsigned int a; /* Discrete action */

    if ((model = carrental_dpmodel_new(config)) == NULL) {
        ck_assert_msg(FALSE, "Failed to initialize Carrental DP model");
    }
    unsigned int num_states = model->num_states;

    for(s = 0; s < num_states; s++) {
        for (a = 0; a < (2*MAX_MOVE + 1); a++) {
            total_transition_prob = 0.0f;
            for(s1 = 0; s1 < num_states; s1++) {
                total_transition_prob += carrental_get_transition(model, s, a, s1).probability;
            }
            /* See discussion regarding ck_assert_float_eq_tol above */
            ck_assert_msg(
                    fabs(total_transition_prob - 1.0f) < 1e-6f,
                    "Total transition probability not approximately equal to 1"
            );
        }
    }
    carrental_dpmodel_free(model);
}

Suite * example_test_suite(void)
{
    Suite *s;
    TCase *tc_example1, *tc_example2;

    s = suite_create("Examples");

    tc_example1 = tcase_create("Example 1");
    tc_example2 = tcase_create("Example 2");

    tcase_add_test(tc_example1, test_example1);
    suite_add_tcase(s, tc_example1);
    
    tcase_add_test(tc_example2, test_example2);
    suite_add_tcase(s, tc_example2);

    return s;
}

Suite * rl_environments_test_suite(void)
{
    Suite *s;
    TCase *tc_linewalk_random_actions, *tc_linewalk_mcts_random_actions,
          *tc_frozenlake_random_actions, *tc_frozenlake_mc_policy_evaluation,
          *tc_frozenlake_dp_iterative_policy_evaluation,
          *tc_frozenlake_dp_model,
          *tc_cliffwalking_random_actions, *tc_cliffwalking_dp_model,
          *tc_carrental_no_terminate, *tc_carrental_dp_model;

    s = suite_create("RL Environments");

    tc_linewalk_random_actions = tcase_create("Linewalk Random Actions");
    tc_linewalk_mcts_random_actions = tcase_create("Linewalk MCTS Random Actions");
    tc_frozenlake_random_actions = tcase_create("Frozenlake Random Actions");
    tc_frozenlake_mc_policy_evaluation = tcase_create("Frozenlake MC Policy Evaluation");
    tc_frozenlake_dp_iterative_policy_evaluation = tcase_create("Frozenlake DP Iterative Policy Evaluation");
    tc_frozenlake_dp_model = tcase_create("Frozenlake DP Model");
    tc_cliffwalking_random_actions = tcase_create("Cliffwalking Random Actions");
    tc_cliffwalking_dp_model = tcase_create("Cliffwalking DP Model");
    tc_carrental_no_terminate = tcase_create("Carrental Not Terminated");
    tc_carrental_dp_model = tcase_create("Carrental DP Model");

    tcase_add_test(tc_linewalk_random_actions, test_linewalk_random_actions);
    tcase_add_test(tc_linewalk_mcts_random_actions, test_linewalk_mcts_random_actions);
    tcase_add_test(tc_frozenlake_random_actions, test_frozenlake_random_actions);
    tcase_add_test(tc_frozenlake_mc_policy_evaluation, test_frozenlake_mc_policy_evaluation);
    tcase_add_test(tc_frozenlake_dp_iterative_policy_evaluation, test_frozenlake_dp_iterative_policy_evaluation);
    tcase_add_test(tc_frozenlake_dp_model, test_frozenlake_dp_model_nonslippery);
    tcase_add_test(tc_frozenlake_dp_model, test_frozenlake_dp_model_slippery);
    tcase_add_test(tc_cliffwalking_random_actions, test_cliffwalking_random_actions);
    tcase_add_test(tc_cliffwalking_dp_model, test_cliffwalking_dp_model_nonslippery);
    tcase_add_test(tc_cliffwalking_dp_model, test_cliffwalking_dp_model_slippery);
    tcase_add_test(tc_carrental_no_terminate, test_carrental_no_terminate);
    tcase_add_test(tc_carrental_dp_model, test_carrental_dp_model);
    suite_add_tcase(s, tc_linewalk_random_actions);
    suite_add_tcase(s, tc_linewalk_mcts_random_actions);
    suite_add_tcase(s, tc_frozenlake_random_actions);
    suite_add_tcase(s, tc_frozenlake_mc_policy_evaluation);
    suite_add_tcase(s, tc_frozenlake_dp_iterative_policy_evaluation);
    suite_add_tcase(s, tc_frozenlake_dp_model);
    suite_add_tcase(s, tc_cliffwalking_random_actions);
    suite_add_tcase(s, tc_cliffwalking_dp_model);
    suite_add_tcase(s, tc_carrental_no_terminate);
    suite_add_tcase(s, tc_carrental_dp_model);
    
    return s;
}

int main(void)
{
    int number_failed;
    Suite *s, *rlenv_suite;
    SRunner *sr;

    s = example_test_suite();
    rlenv_suite = rl_environments_test_suite();
    sr = srunner_create(s);
    srunner_add_suite(sr, rlenv_suite);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}

