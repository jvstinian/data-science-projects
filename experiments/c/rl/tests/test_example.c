#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <reinforcementlearning/envs/linewalk.h>
#include <reinforcementlearning/envs/frozenlake.h>
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
    TCase *tc_linewalk_random_actions, *tc_frozenlake_random_actions,
          *tc_frozenlake_dp_model, *tc_carrental_no_terminate;

    s = suite_create("RL Environments");

    tc_linewalk_random_actions = tcase_create("Linewalk Random Actions");
    tc_frozenlake_random_actions = tcase_create("Frozenlake Random Actions");
    tc_frozenlake_dp_model = tcase_create("Frozenlake DP Model");
    tc_carrental_no_terminate = tcase_create("Carrental Not Terminated");

    tcase_add_test(tc_linewalk_random_actions, test_linewalk_random_actions);
    tcase_add_test(tc_frozenlake_random_actions, test_frozenlake_random_actions);
    tcase_add_test(tc_frozenlake_dp_model, test_frozenlake_dp_model_nonslippery);
    tcase_add_test(tc_frozenlake_dp_model, test_frozenlake_dp_model_slippery);
    tcase_add_test(tc_carrental_no_terminate, test_carrental_no_terminate);
    suite_add_tcase(s, tc_linewalk_random_actions);
    suite_add_tcase(s, tc_frozenlake_random_actions);
    suite_add_tcase(s, tc_frozenlake_dp_model);
    suite_add_tcase(s, tc_carrental_no_terminate);
    
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

