#include <string.h>

#include "cpg-object-private.h"
#include "cpg-state-private.h"
#include "cpg-utils.h"

/**
 * cpg_state_new:
 * @name: the name of the state
 *
 * Returns a newly created #CpgState object
 *
 * Return value: a new #CpgState object
 *
 **/
CpgState *
cpg_state_new(char const *name)
{
	CpgState *res = cpg_new1(CpgState);
	cpg_object_initialize(&res->parent, CPG_OBJECT_TYPE_STATE);
		
	res->name = strdup(name);

	return res;
}

/**
 * cpg_state_free:
 * @state: the #CpgState
 *
 * Destroy the #CpgState object
 *
 **/
void
cpg_state_free(CpgState *state)
{
	if (!state)
		return;

	cpg_object_destroy(&state->parent);

	free(state->name);
	free(state);
}

/**
 * cpg_state_name:
 * @state: the #CpgState
 *
 * Returns the name of the #CpgState
 *
 * Return value: the name of the #CpgState
 *
 **/
char const *
cpg_state_name(CpgState *state)
{
	return state->name;
}
