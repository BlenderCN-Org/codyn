#ifndef __CPG_NETWORK_H__
#define __CPG_NETWORK_H__

#include <glib-object.h>

#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-state.h>
#include <cpg-network/cpg-relay.h>
#include <cpg-network/cpg-link.h>
#include <cpg-network/cpg-monitor.h>
#include <cpg-network/shared/cpg-shared-network.h>
#include <cpg-network/cpg-compile-error.h>

G_BEGIN_DECLS

#define CPG_TYPE_NETWORK			(cpg_network_get_type ())
#define CPG_NETWORK(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_NETWORK, CpgNetwork))
#define CPG_NETWORK_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_NETWORK, CpgNetwork const))
#define CPG_NETWORK_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_NETWORK, CpgNetworkClass))
#define CPG_IS_NETWORK(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_NETWORK))
#define CPG_IS_NETWORK_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_NETWORK))
#define CPG_NETWORK_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_NETWORK, CpgNetworkClass))

typedef struct _CpgNetwork			CpgNetwork;
typedef struct _CpgNetworkClass		CpgNetworkClass;
typedef struct _CpgNetworkPrivate	CpgNetworkPrivate;

struct _CpgNetwork {
	GObject parent;
	
	CpgNetworkPrivate *priv;
};

struct _CpgNetworkClass {
	GObjectClass parent_class;
	
	void (*reset)			(CpgNetwork *network);
	void (*update)			(CpgNetwork *network, gdouble timestep);
	void (*compile_error)	(CpgNetwork *network, CpgCompileError *error);
};

GType cpg_network_get_type (void) G_GNUC_CONST;

CpgNetwork 		 *cpg_network_new_from_file		(gchar const *filename);
CpgNetwork 		 *cpg_network_new_from_xml		(gchar const *xml);
CpgNetwork		 *cpg_network_new				(void);

gchar 			 *cpg_network_write_to_xml		(CpgNetwork  *network);
void			  cpg_network_write_to_file		(CpgNetwork  *network,
												 gchar const *filename);

void              cpg_network_merge             (CpgNetwork *network,
                                                 CpgNetwork *other);

CpgObject 		 *cpg_network_get_globals       (CpgNetwork  *network);

void			  cpg_network_clear				(CpgNetwork *network);

CpgObject		 *cpg_network_get_object		(CpgNetwork *network, 
												 gchar const *id);

gboolean	 	  cpg_network_set_expression	(CpgNetwork  *network,
												 CpgProperty *property,
												 gchar const  *expression);

/* network manipulation */
void			  cpg_network_add_object		(CpgNetwork *network, 
												 CpgObject  *object);
void			  cpg_network_remove_object		(CpgNetwork *network, 
												 CpgObject  *object);

gboolean		  cpg_network_compile			(CpgNetwork      *network,
                                                 CpgCompileError *error);

void			  cpg_network_taint				(CpgNetwork *network);

GSList			 *cpg_network_get_states		(CpgNetwork *network);
GSList 			 *cpg_network_get_links			(CpgNetwork *network);
												 
/* simulation functions */
void			  cpg_network_run				(CpgNetwork *network, 
												 gdouble	 from, 
												 gdouble	 timestep, 
												 gdouble	 to);
void			  cpg_network_step				(CpgNetwork *network, 
												 gdouble	 timestep);
void			  cpg_network_reset				(CpgNetwork *network);

G_END_DECLS

#endif /* __CPG_NETWORK_H__ */
