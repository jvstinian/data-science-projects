#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <reinforcementlearning/envs/linewalk.h>


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
    /* Seed_Reset_Type'(Kind => Set_Seed, Seed => 123) */
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
    TCase *tc_linewalk_random_actions;

    s = suite_create("RL Environments");

    tc_linewalk_random_actions = tcase_create("Linewalk Random Actions");

    tcase_add_test(tc_linewalk_random_actions, test_linewalk_random_actions);
    suite_add_tcase(s, tc_linewalk_random_actions);
    
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

