#ifndef __CPG_NETWORK_H__
#define __CPG_NETWORK_H__

#include <glib-object.h>

#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-state.h>
#include <cpg-network/cpg-link.h>
#include <cpg-network/cpg-monitor.h>
#include <cpg-network/cpg-compile-error.h>
#include <cpg-network/cpg-function.h>
#include <cpg-network/cpg-integrator.h>
#include <cpg-network/cpg-group.h>

G_BEGIN_DECLS

#define CPG_TYPE_NETWORK            (cpg_network_get_type ())
#define CPG_NETWORK(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_NETWORK, CpgNetwork))
#define CPG_NETWORK_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_NETWORK, CpgNetwork const))
#define CPG_NETWORK_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_NETWORK, CpgNetworkClass))
#define CPG_IS_NETWORK(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_NETWORK))
#define CPG_IS_NETWORK_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_NETWORK))
#define CPG_NETWORK_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_NETWORK, CpgNetworkClass))

typedef struct _CpgNetwork        CpgNetwork;
typedef struct _CpgNetworkClass   CpgNetworkClass;
typedef struct _CpgNetworkPrivate CpgNetworkPrivate;

#define CPG_NETWORK_LOAD_ERROR (cpg_network_load_error_quark ())

typedef enum
{
	CPG_NETWORK_LOAD_ERROR_XML,
	CPG_NETWORK_LOAD_ERROR_PROPERTY,
	CPG_NETWORK_LOAD_ERROR_OBJECT,
	CPG_NETWORK_LOAD_ERROR_LINK,
	CPG_NETWORK_LOAD_ERROR_FUNCTION
} CpgNetworkLoadError;

struct _CpgNetwork
{
	CpgGroup parent;

	CpgNetworkPrivate *priv;
};

struct _CpgNetworkClass
{
	CpgGroupClass parent_class;

	void (*compile_error) (CpgNetwork      *network,
	                       CpgCompileError *error);
};

GType cpg_network_get_type (void) G_GNUC_CONST;

GQuark            cpg_network_load_error_quark  (void);

CpgNetwork       *cpg_network_new                    (void);

CpgNetwork       *cpg_network_new_from_file          (const gchar    *filename,
                                                      GError        **error);
CpgNetwork       *cpg_network_new_from_xml           (const gchar    *xml,
                                                      GError        **error);

void              cpg_network_set_integrator         (CpgNetwork     *network,
                                                      CpgIntegrator  *integrator);

CpgIntegrator    *cpg_network_get_integrator         (CpgNetwork     *network);

gchar            *cpg_network_write_to_xml           (CpgNetwork     *network);
void              cpg_network_write_to_file          (CpgNetwork     *network,
                                                      const gchar    *filename);

void              cpg_network_merge                  (CpgNetwork     *network,
                                                      CpgNetwork     *other);

void              cpg_network_merge_from_file        (CpgNetwork     *network,
                                                      const gchar    *filename,
                                                      GError        **error);

void              cpg_network_merge_from_xml         (CpgNetwork     *network,
                                                      const gchar    *xml,
                                                      GError        **error);

/* simulation functions */
void              cpg_network_run                    (CpgNetwork *network,
                                                      gdouble     from,
                                                      gdouble     timestep,
                                                      gdouble     to);
void              cpg_network_step                   (CpgNetwork *network,
                                                      gdouble     timestep);

GSList const     *cpg_network_get_templates          (CpgNetwork   *network);
void              cpg_network_add_template           (CpgNetwork   *network,
                                                      const gchar  *name,
                                                      CpgObject    *object);

CpgObject        *cpg_network_get_template           (CpgNetwork   *network,
                                                      const gchar  *name);

void              cpg_network_remove_template        (CpgNetwork   *network,
                                                      const gchar  *name);

CpgObject        *cpg_network_add_from_template      (CpgNetwork   *network,
                                                      const gchar  *name);

CpgObject        *cpg_network_add_link_from_template (CpgNetwork   *network,
                                                      const gchar  *name,
                                                      CpgObject    *from,
                                                      CpgObject    *to);

void              cpg_network_add_function           (CpgNetwork   *network,
                                                     CpgFunction  *function);

void              cpg_network_remove_function        (CpgNetwork   *network,
                                                      CpgFunction  *function);

GSList           *cpg_network_get_functions          (CpgNetwork   *network);

CpgFunction      *cpg_network_get_function           (CpgNetwork   *network,
                                                      gchar const  *name);

G_END_DECLS

#endif /* __CPG_NETWORK_H__ */
