#ifndef __CPG_NETWORK_H__
#define __CPG_NETWORK_H__

#include <gio/gio.h>

#include <cpg-network/cpg-object.h>
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

/* Forward declaration */
CPG_FORWARD_DECL (CpgImport);

#define CPG_NETWORK_LOAD_ERROR (cpg_network_load_error_quark ())

/**
 * CpgNetworkLoadError:
 * @CPG_NETWORK_LOAD_ERROR_XML: error occurred in parsing xml
 * @CPG_NETWORK_LOAD_ERROR_PROPERTY: error occurred in loading a property
 * @CPG_NETWORK_LOAD_ERROR_OBJECT: error occurred in loading an object
 * @CPG_NETWORK_LOAD_ERROR_LINK: error occurred in loading a link
 * @CPG_NETWORK_LOAD_ERROR_FUNCTION: error occurred in loading a function
 * @CPG_NETWORK_LOAD_ERROR_IMPORT: error occurred in an import
 *
 * Network load error types.
 *
 */
typedef enum
{
	CPG_NETWORK_LOAD_ERROR_XML,
	CPG_NETWORK_LOAD_ERROR_PROPERTY,
	CPG_NETWORK_LOAD_ERROR_OBJECT,
	CPG_NETWORK_LOAD_ERROR_LINK,
	CPG_NETWORK_LOAD_ERROR_FUNCTION,
	CPG_NETWORK_LOAD_ERROR_IMPORT,
	CPG_NETWORK_LOAD_ERROR_INPUT_FILE
} CpgNetworkLoadError;

#define CPG_NETWORK_ERROR (cpg_network_error_quark ())

typedef enum
{
	CPG_NETWORK_ERROR_UNOWNED_TEMPLATE,
	CPG_NETWORK_ERROR_NUM
} CpgNetworkError;

struct _CpgNetwork
{
	/*< private >*/
	CpgGroup parent;

	CpgNetworkPrivate *priv;
};

/**
 * CpgNetworkClass:
 * @compile_error: compile error default signal handler
 *
 * The CpgNetwork class
 *
 */
struct _CpgNetworkClass
{
	/*< private >*/
	CpgGroupClass parent_class;

	/*< public >*/
	void (*compile_error) (CpgNetwork      *network,
	                       CpgCompileError *error);
};

GType             cpg_network_get_type              (void) G_GNUC_CONST;

GQuark            cpg_network_load_error_quark      (void);
GQuark            cpg_network_error_quark           (void);

CpgNetwork       *cpg_network_new                    (void);

CpgNetwork       *cpg_network_new_from_file          (GFile          *file,
                                                      GError        **error);

CpgNetwork       *cpg_network_new_from_path          (const gchar    *path,
                                                      GError        **error);

CpgNetwork       *cpg_network_new_from_xml           (const gchar    *xml,
                                                      GError        **error);

gboolean          cpg_network_load_from_file         (CpgNetwork     *network,
                                                      GFile          *file,
                                                      GError        **error);

gboolean          cpg_network_load_from_path         (CpgNetwork     *network,
                                                      const gchar    *path,
                                                      GError        **error);

gboolean          cpg_network_load_from_xml          (CpgNetwork     *network,
                                                      const gchar    *xml,
                                                      GError        **error);

GFile            *cpg_network_get_file               (CpgNetwork     *network);
gchar            *cpg_network_get_path               (CpgNetwork     *network);

void              cpg_network_set_integrator         (CpgNetwork     *network,
                                                      CpgIntegrator  *integrator);

CpgIntegrator    *cpg_network_get_integrator         (CpgNetwork     *network);

void              cpg_network_merge                  (CpgNetwork     *network,
                                                      CpgNetwork     *other);

void              cpg_network_merge_from_file        (CpgNetwork     *network,
                                                      GFile          *file,
                                                      GError        **error);

void              cpg_network_merge_from_path        (CpgNetwork     *network,
                                                      const gchar    *path,
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

CpgGroup         *cpg_network_get_template_group     (CpgNetwork   *network);
CpgGroup         *cpg_network_get_function_group     (CpgNetwork   *network);

CPG_FORWARD_DECL (CpgImport) *cpg_network_get_import  (CpgNetwork   *network,
                                                       GFile        *file);

CPG_FORWARD_DECL (CpgImport) *cpg_network_get_import_from_path  (CpgNetwork   *network,
                                                                 const gchar  *path);


void              _cpg_network_register_import       (CpgNetwork        *network,
                                                      CPG_FORWARD_DECL (CpgImport) *import);

G_END_DECLS

#endif /* __CPG_NETWORK_H__ */
