#ifndef __CPG_NETWORK_H__
#define __CPG_NETWORK_H__

#include "cpg-object.h"
#include "cpg-state.h"

typedef struct
{
	char *filename;
	
	CpgObject **objects;
	unsigned num_states;
	unsigned num_links;
	
	/* simulation */
	float timestep;
	float time;
} CpgNetwork;

CpgNetwork 	*cpg_network_new_from_file		(char const *filename);
void 		 cpg_network_free				(CpgNetwork *network);

CpgState	*cpg_network_get_state_by_name	(CpgNetwork *network, char const *name);
void		 cpg_network_simulation_step	(CpgNetwork *network, float step);

#endif /* __CPG_NETWORK_H__ */
