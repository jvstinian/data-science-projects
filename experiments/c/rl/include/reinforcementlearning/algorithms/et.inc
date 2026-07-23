#ifndef ENVIRONMENT_PREFIX
#error "ENVIRONMENT_PREFIX must be defined in td.inc"
#endif

#ifndef CONFIG_TYPE
#error "CONFIG_TYPE is not defined in tc.inc"
#endif

#ifndef ENVIRONMENT_TYPE
#error "ENVIRONMENT_TYPE is not defined in tc.inc"
#endif

#ifndef ACTION_TYPE
#error "ACTION_TYPE is not defined in tc.inc"
#endif

#ifndef OBSERVATION_TYPE
#error "OBSERVATION_TYPE is not defined in tc.inc"
#endif

#ifndef STEP_RETURN_TYPE
#error "STEP_RETURN_TYPE is not defined in tc.inc"
#endif

#ifndef ENVIRONMENT_ACTION_COUNT
#error "ENVIRONMENT_ACTION_COUNT is not defined in tc.inc"
#endif

#ifndef GET_NUM_STATES_METHOD
#error "GET_NUM_STATES_METHOD is not defined in tc.inc"
#endif

#ifndef MAKE_METHOD
#error "MAKE_METHOD is not defined in tc.inc"
#endif

#ifndef RESET_METHOD
#error "RESET_METHOD is not defined in tc.inc"
#endif

#ifndef STEP_METHOD
#error "STEP_METHOD is not defined in tc.inc"
#endif

#ifndef CLOSE_METHOD
#error "CLOSE_METHOD is not defined in tc.inc"
#endif

#ifndef GET_RANDOM_ACTION_ES_METHOD
#error "GET_RANDOM_ACTION_ES_METHOD is not defined in tc.inc"
#endif

#ifndef TO_DISCRETE_OBSERVATION_METHOD
#error "TO_DISCRETE_OBSERVATION is not defined in tc.inc"
#endif

/*
#include <reinforcementlearning/algorithms/td_config.h>
#include <reinforcementlearning/algorithms/epsilon_policies.h>
#include <string.h>
#include <time.h>
#include <math.h> */ /* abs */

#define ET_FUNC_NAME(name) ET_CONCAT1(ENVIRONMENT_PREFIX, name)

#define ET_CONCAT1(prefix, name) ET_CONCAT2(prefix, name)
#define ET_CONCAT2(prefix, name) prefix##_##name

#ifdef ET_DECLS_ONLY

#else

#endif

#undef ENVIRONMENT_PREFIX
#undef CONFIG_TYPE
#undef ENVIRONMENT_TYPE
#undef ACTION_TYPE
#undef OBSERVATION_TYPE
#undef STEP_RETURN_TYPE
#undef ENVIRONMENT_ACTION_COUNT
#undef GET_NUM_STATES_METHOD
#undef MAKE_METHOD
#undef RESET_METHOD
#undef STEP_METHOD
#undef CLOSE_METHOD
#undef GET_RANDOM_ACTION_ES_METHOD
#undef TO_DISCRETE_OBSERVATION_METHOD
#ifdef ET_DECLS_ONLY
#undef ET_DECLS_ONLY
#endif

