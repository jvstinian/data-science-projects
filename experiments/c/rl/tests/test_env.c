#include <check.h>
#include <stdlib.h>


#if defined(__STDC__) && !defined(__STDC_VERSION__)
    /* Defining as static to avoid name collisions */
    static float floorf(float arg) {
        return (float) floor((double) arg);
    }
#endif

START_TEST (test_float_memset_0)
{
    float f_exp = 0.0f;
    float f_act;
    memset(&f_act, 0, sizeof(float));
    ck_assert_float_eq(f_exp, f_act);
}
END_TEST

START_TEST (test_uint_floorf)
{
    unsigned int i;
    unsigned int act_uint;

    for (i=0; i < 100; i++) {
        act_uint = (unsigned int) floorf((float) i);
        ck_assert_uint_eq(i, act_uint);
    }
}

Suite * environment_test_suite(void)
{
    Suite *s;
    TCase *tc_float_memset_0, *tc_uint_floorf;

    s = suite_create("Environment Tests");

    tc_float_memset_0 = tcase_create("Float memset to 0");
    tc_uint_floorf = tcase_create("floorf applied to cast unsigned ints");

    tcase_add_test(tc_float_memset_0, test_float_memset_0);
    suite_add_tcase(s, tc_float_memset_0);
    
    tcase_add_test(tc_uint_floorf, test_uint_floorf);
    suite_add_tcase(s, tc_uint_floorf);

    return s;
}

int main(void)
{
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = environment_test_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}

