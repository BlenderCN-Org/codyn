#ifndef __CPG_SHARED_NETWORK_H__
#define __CPG_SHARED_NETWORK_H__

#include "cpg-network.h"
#include "shared/cpg-shared-network.h"

typedef struct _CpgMemoryMap CpgMemoryMap;

void cpg_memory_map_init						(CpgMemoryMap     *map);
void cpg_memory_map_destroy						(CpgMemoryMap     *map);
CpgSharedPointer cpg_memory_map_find			(CpgMemoryMap     *iter, 
												 void             *source);

void cpg_shared_network_copy 					(CpgSharedNetwork  *ret,
												 CpgNetwork        *network,
												 CpgMemoryMap      *map,
												 void              *base,
												 void			  **ptr,
												 unsigned           maxsize);

#endif /* __CPG_SHARED_NETWORK_H__ */

