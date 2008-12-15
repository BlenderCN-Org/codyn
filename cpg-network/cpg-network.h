#ifndef __CPG_NETWORK_H__
#define __CPG_NETWORK_H__

#include "cpg-object.h"
#include "cpg-state.h"
#include "cpg-relay.h"
#include "cpg-link.h"
#include "shared/cpg-shared-network.h"

typedef struct _CpgNetwork CpgNetwork;

CpgNetwork 		 *cpg_network_new_from_file		(char const *filename);
CpgNetwork		 *cpg_network_new				(void);

void			  cpg_network_clear				(CpgNetwork *network);
void 			  cpg_network_free				(CpgNetwork *network);

CpgObject		 *cpg_network_object			(CpgNetwork *network, 
												 char const *id);

int			 	  cpg_network_set_value			(CpgNetwork  *network,
												 CpgObject   *object,
												 CpgProperty *property,
												 char const  *expression);
int			 	  cpg_network_set_initial		(CpgNetwork  *network,
												 CpgObject   *object,
												 CpgProperty *property,
												 char const  *expression);

/* network manipulation */
void			  cpg_network_add_object		(CpgNetwork *network, 
												 CpgObject  *object);

int				  cpg_network_compile			(CpgNetwork *network);
void			  cpg_network_taint				(CpgNetwork *network);

CpgState * const *cpg_network_states			(CpgNetwork *network,
												 unsigned   *size);

CpgLink * const  *cpg_network_links				(CpgNetwork *network,
												 unsigned   *size);

/* monitor functions */
void			  cpg_network_set_monitor		(CpgNetwork *network, 
												 CpgObject  *object, 
												 char const *propname);
void			  cpg_network_unset_monitor		(CpgNetwork *network, 
												 CpgObject  *object, 
												 char const *propname);
double const 	 *cpg_network_monitor_data		(CpgNetwork *network,
												 CpgObject  *object,
												 char const *propname,
												 unsigned   *size);

double			 *cpg_network_monitor_data_resampled (CpgNetwork   *network,
												      CpgObject    *object,
												      char const   *propname,
												      double const *sites,
												      unsigned      size);
												 
/* simulation functions */
void			  cpg_network_simulation_run	(CpgNetwork *network, 
												 float 		 from, 
												 float 		 timestep, 
												 float 		 to);
void			  cpg_network_simulation_step	(CpgNetwork *network, 
												 float 		 timestep);
void			  cpg_network_simulation_reset	(CpgNetwork *network);

#endif /* __CPG_NETWORK_H__ */
