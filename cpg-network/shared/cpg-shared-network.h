#ifndef __CPG_SHARED_SHARED_NETWORK_H__
#define __CPG_SHARED_SHARED_NETWORK_H__

#include <cpg-network/shared/cpg-shared-utils.h>
#include <cpg-network/shared/cpg-shared-property.h>

typedef struct
{
	/* actors */
	unsigned num_actors;
	CpgSharedPointer actors; /* CpgSharedPointer to num_actors CpgSharedPointer's */
	
	/* links */
	unsigned num_links;
	CpgSharedPointer links;
	
	/* timing */
	float timestep;
	float time;
	
	CpgSharedProperty timeprop;
	CpgSharedProperty timestepprop;
} CpgSharedNetwork;

void			  cpg_shared_network_simulation_step (CpgSharedNetwork *network, 
												 	  float				timestep,
												 	  void			   *base);

void			  cpg_shared_network_simulation_run	 (CpgSharedNetwork *network, 
												 	  float			    from, 
												 	  float			    timestep, 
													  float			    to,
													  void			   *base);

#endif /* __CPG_SHARED_NETWORK_H__ */

