#ifndef __CPG_NETWORK_WEBOTS_H__
#define __CPG_NETWORK_WEBOTS_H__

#include <cpg-network/cpg-network.h>
#include <glib.h>

G_BEGIN_DECLS

typedef struct _CpgNetworkWebots CpgNetworkWebots;

CpgNetworkWebots *cpg_network_webots_new(CpgNetwork *network);
void cpg_network_webots_free(CpgNetworkWebots *webots);

void cpg_network_webots_initial(CpgNetworkWebots *webots, guint ms);
void cpg_network_webots_scale_initial(CpgNetworkWebots *webots, gdouble fraction);

void cpg_network_webots_enable(CpgNetworkWebots *webots, guint ms);
void cpg_network_webots_disable(CpgNetworkWebots *webots);

void cpg_network_webots_update(CpgNetworkWebots *webots);
guint cpg_network_webots_size(CpgNetworkWebots *webots);

G_END_DECLS

#endif /* __CPG_NETWORK_WEBOTS_H__ */

