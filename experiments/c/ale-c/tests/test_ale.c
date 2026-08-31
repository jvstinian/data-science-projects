#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <gym.h>

START_TEST (test_ale_seed)
{
    /* Set the Atari config using the run config */
    struct AtariConfig config = default_atari_env_config_init();
    struct AtariRGBStepReturn state;

    enum AtariAction action;
    config.rom_dir = "./resources";
    config.rom_name = "tetris";
    
    struct AtariEnv* env = atari_make(config);

    atarirgb_reset(env, 123u);
    size_t step_count_1 = 0;
    while (1) {
        action = atari_random_action(env);
        state = atarirgb_step(env, action);
        step_count_1++;
        if (state.terminated) break;
    }

    atarirgb_reset(env, 123u);
    size_t step_count_2 = 0;
    while (1) {
        action = atari_random_action(env);
        state = atarirgb_step(env, action);
        step_count_2++;
        if (state.terminated) break;
    }

    atari_destroy(env);

    ck_assert_uint_eq(step_count_1, step_count_2);
}

/*
START_TEST (test_linewalk_random_actions)
{
    struct LineWalkConfig config = { 7 };
    struct SimulationSummary sim_summary;
    sim_summary = linewalk_uniform_random_actions(config, FALSE);

    ck_assert_msg(sim_summary.num_steps <= 10, "Steps exceeded 10");
    ck_assert_msg(sim_summary.total_reward >= -1.0, "Reward is less than -1");
    ck_assert_msg(sim_summary.total_reward <= 1.0, "Reward exceeds 1");
}
*/

Suite * ale_test_suite(void)
{
    Suite *s;
    TCase  *tc_ale_seed;

    s = suite_create("ALE C Tests");

    tc_ale_seed = tcase_create("ALE Seed Test");

    tcase_add_test(tc_ale_seed, test_ale_seed);
    suite_add_tcase(s, tc_ale_seed);

    return s;
}

int main(void)
{
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = ale_test_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}

