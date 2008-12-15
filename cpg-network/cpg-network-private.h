#ifndef __CPG_NETWORK_PRIVATE_H__
#define __CPG_NETWORK_PRIVATE_H__

#include "cpg-network.h"
#include "cpg-object-private.h"
#include "cpg-link-private.h"
#include "cpg-state-private.h"
#include "cpg-thread.h"
#include "cpg-mutex.h"

typedef struct _CpgMonitor CpgMonitor;

struct _CpgMonitor
{
	CpgObject *object;
	CpgProperty *property;
	
	double *values;
	double *sites;
	unsigned num_values;
	unsigned size;

	CpgMonitor *next;
};

typedef struct
{
	CpgNetwork *network;
	CpgThread *thread;
	CpgMutex *mutex;

	unsigned from;
	unsigned to;
	
	int running;
} CpgSimulationWorker;

struct _CpgNetwork
{
	char *filename;
	
	/* states */
	CpgState **states;
	unsigned num_states;
	unsigned num_relays;
	
	/* links */
	CpgLink **links;
	unsigned num_links;
	
	/* simulation */
	float timestep;
	float time;
	int compiled;
	
	CpgSimulationWorker *worker_threads;
	int num_worker_threads;
	
	/* monitors */
	CpgMonitor *monitors;
	
	/* context */
	CpgContext context[3];
	CpgObject *constants;
	CpgProperty *timeprop;
	CpgProperty *timestepprop;
};

#endif /* __CPG_NETWORK_PRIVATE_H__ */

