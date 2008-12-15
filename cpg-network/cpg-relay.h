#ifndef __CPG_RELAY_H__
#define __CPG_RELAY_H__

#include "cpg-object.h"

typedef struct _CpgRelay CpgRelay;

CpgRelay 	*cpg_relay_new		(char const *id);
void		 cpg_relay_free		(CpgRelay *state);

#endif /* __CPG_RELAY_H__ */
