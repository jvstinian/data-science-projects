#include <check.h>
#include <stdlib.h>
#include <stdio.h>


START_TEST (test_example1)
{
    ck_assert(1);
}
END_TEST

START_TEST (test_example2)
{
    ck_assert_msg(1, "Example failure!");
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

int main(void)
{
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = example_test_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}

