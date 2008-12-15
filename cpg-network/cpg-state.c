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
cpg_state_new(char const *id)
{
	return cpg_object_create(CpgState, CPG_OBJECT_TYPE_STATE, id);
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
	free(state);
}
