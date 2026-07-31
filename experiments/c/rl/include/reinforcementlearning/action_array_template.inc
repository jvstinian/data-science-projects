#ifndef ENVIRONMENT_PREFIX
#error "ENVIRONMENT_PREFIX must be defined in array_action_template.inc"
#endif

#ifndef ENVIRONMENT_STRUCT_PREFIX
#error "ENVIRONMENT_STRUCT_PREFIX must be defined in array_action_template.inc"
#endif

#ifndef ACTION_TYPE
#error "ACTION_TYPE is not defined in array_action_template.inc"
#endif

#define AA_FUNC_NAME(name) AA_CONCAT1(ENVIRONMENT_PREFIX, name)

#define AA_CONCAT1(prefix, name) AA_CONCAT2(prefix, name)
#define AA_CONCAT2(prefix, name) prefix##_##name

#define AA_STRUCT_CONCAT1(prefix, name) AA_STRUCT_CONCAT2(prefix, name)
#define AA_STRUCT_CONCAT2(prefix, name) prefix##name

#define AA_ACTION_LIST_TYPE struct AA_STRUCT_CONCAT1(ENVIRONMENT_STRUCT_PREFIX, ActionList)

#ifdef AA_DECLS_ONLY

/* Forward declaration of struct ENVIRONMENT_STRUCT_PREFIXActionList */
AA_ACTION_LIST_TYPE;

AA_ACTION_LIST_TYPE* AA_FUNC_NAME(action_list_create) (size_t cpty);
int AA_FUNC_NAME(action_list_realloc) (AA_ACTION_LIST_TYPE** lpp, size_t new_capacity);
int AA_FUNC_NAME(action_list_push) (AA_ACTION_LIST_TYPE** lpp, ACTION_TYPE val);
size_t AA_FUNC_NAME(action_list_length) (AA_ACTION_LIST_TYPE* lp);
ACTION_TYPE AA_FUNC_NAME(action_list_get) (AA_ACTION_LIST_TYPE* lp, size_t i);
void AA_FUNC_NAME(action_list_shuffle) (AA_ACTION_LIST_TYPE* lp);
void AA_FUNC_NAME(action_list_destroy) (AA_ACTION_LIST_TYPE* lp);

#else

#include <reinforcementlearning/math.h> /* floorf */

/* TODO: Need to consolidate the rand definitions */
/* Defining as static to avoid name collisions */
static float rand_float() {
    return (float)rand() / (float)RAND_MAX;
};

AA_ACTION_LIST_TYPE {
	size_t capacity;
	size_t length;
	ACTION_TYPE list[1];
};

/* AtaxxAction Array List */
AA_ACTION_LIST_TYPE* AA_FUNC_NAME(action_list_create) (size_t cpty){
    if (cpty < 1) {
        cpty = 1;
    }
	AA_ACTION_LIST_TYPE* ret = (AA_ACTION_LIST_TYPE*) malloc(sizeof(AA_ACTION_LIST_TYPE) + (cpty-1)*sizeof(ACTION_TYPE));
	if (ret != NULL) {
		ret->capacity = cpty;
		ret->length = 0;
	}
	return ret;
}

/* TODO: Should the following be static? */
int AA_FUNC_NAME(action_list_realloc)(AA_ACTION_LIST_TYPE** lpp, size_t new_capacity){
    *lpp = realloc(*lpp, sizeof(AA_ACTION_LIST_TYPE) + (new_capacity-1)*sizeof(ACTION_TYPE));
    if (*lpp == NULL) {
        return 1;
    }
    (*lpp)->capacity = new_capacity;
	return 0;
}

int AA_FUNC_NAME(action_list_push) (AA_ACTION_LIST_TYPE** lpp, ACTION_TYPE val){
	(*lpp)->list[(*lpp)->length] = val;
	(*lpp)->length++;

	if ((*lpp)->length >= (*lpp)->capacity) {
		/* realloc */
		int newcap = 2 * (*lpp)->capacity;
		*lpp = realloc(*lpp, sizeof(AA_ACTION_LIST_TYPE) + (newcap-1)*sizeof(ACTION_TYPE));
		if (*lpp == NULL) {
			return -1;
		}
		(*lpp)->capacity = newcap;
	}
	return 0;
}

size_t AA_FUNC_NAME(action_list_length) (AA_ACTION_LIST_TYPE* lp) {
    return lp->length;
}

ACTION_TYPE AA_FUNC_NAME(action_list_get) (AA_ACTION_LIST_TYPE* lp, size_t i) {
    return lp->list[i];
}

void AA_FUNC_NAME(action_list_shuffle) (AA_ACTION_LIST_TYPE* lp) {
    size_t i = lp->length;
    size_t swap_i;
    ACTION_TYPE tmp;

    /* No need to handle i == 1 in the following as this corresponds
       to reassigning or swapping index 0, but the only option here
       is to swap with index 0, so no operation is needed. */
    while (i > 1) {
        swap_i = (size_t) floorf(rand_float() * ((float) i));
        /* We decrement i as (i-1) is the actual index to
           be swapped with swap_i.
           Also decrementing sets us up for the next iteration. */
        i--;
        if (swap_i != i) {
            tmp = lp->list[i];
            lp->list[i] = lp->list[swap_i];
            lp->list[swap_i] = tmp;
        }
    }
}

void AA_FUNC_NAME(action_list_destroy) (AA_ACTION_LIST_TYPE* lp) {
	free(lp);
}

#endif

#undef AA_ACTION_LIST_TYPE
#undef ENVIRONMENT_PREFIX
#undef ENVIRONMENT_STRUCT_PREFIX
#undef ACTION_TYPE
#ifdef AA_DECLS_ONLY
#undef AA_DECLS_ONLY
#endif

