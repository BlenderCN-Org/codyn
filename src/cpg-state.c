#include <string.h>

#include "cpg-state.h"
#include "cpg-utils.h"

CpgState *
cpg_state_new(char const *name)
{
	CpgState *res = cpg_new1(CpgState);
	cpg_object_initialize(&res->parent, CPG_OBJECT_TYPE_STATE);
		
	res->name = strdup(name);

	return res;
}

void
cpg_state_free(CpgState *state)
{
	if (!state)
		return;

	cpg_object_destroy(&state->parent);

	free(state->name);
	free(state);
}
