#ifndef __CPG_STATE_H__
#define __CPG_STATE_H__

#include "cpg-object.h"

typedef struct _CpgState CpgState;

CpgState 	*cpg_state_new		(char const *name);
void		 cpg_state_free		(CpgState *state);
char const  *cpg_state_name		(CpgState *state);

#endif /* __CPG_STATE_H__ */
