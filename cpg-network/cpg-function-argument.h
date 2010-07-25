#ifndef __CPG_FUNCTION_ARGUMENT_H__
#define __CPG_FUNCTION_ARGUMENT_H__

#include <glib-object.h>
#include <cpg-network/cpg-property.h>

G_BEGIN_DECLS

#define CPG_TYPE_FUNCTION_ARGUMENT		(cpg_function_argument_get_type ())
#define CPG_FUNCTION_ARGUMENT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_FUNCTION_ARGUMENT, CpgFunctionArgument))
#define CPG_FUNCTION_ARGUMENT_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_FUNCTION_ARGUMENT, CpgFunctionArgument const))
#define CPG_FUNCTION_ARGUMENT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_FUNCTION_ARGUMENT, CpgFunctionArgumentClass))
#define CPG_IS_FUNCTION_ARGUMENT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_FUNCTION_ARGUMENT))
#define CPG_IS_FUNCTION_ARGUMENT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_FUNCTION_ARGUMENT))
#define CPG_FUNCTION_ARGUMENT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_FUNCTION_ARGUMENT, CpgFunctionArgumentClass))

typedef struct _CpgFunctionArgument		CpgFunctionArgument;
typedef struct _CpgFunctionArgumentClass	CpgFunctionArgumentClass;
typedef struct _CpgFunctionArgumentPrivate	CpgFunctionArgumentPrivate;

struct _CpgFunctionArgument
{
	/*< private >*/
	GInitiallyUnowned parent;

	CpgFunctionArgumentPrivate *priv;
};

struct _CpgFunctionArgumentClass
{
	/*< private >*/
	GInitiallyUnownedClass parent_class;

	/*< public >*/

	/* signals */
	gboolean (*invalidate_name) (CpgFunctionArgument *argument,
	                             const gchar         *name);
};

GType cpg_function_argument_get_type (void) G_GNUC_CONST;

CpgFunctionArgument *cpg_function_argument_new                (const gchar         *name,
                                                               gboolean             optional,
                                                               gdouble              def);

CpgFunctionArgument *cpg_function_argument_copy               (CpgFunctionArgument *argument);

const gchar         *cpg_function_argument_get_name           (CpgFunctionArgument *argument);
gboolean             cpg_function_argument_set_name           (CpgFunctionArgument *argument,
                                                               const gchar         *name);

gboolean             cpg_function_argument_get_optional       (CpgFunctionArgument *argument);
void                 cpg_function_argument_set_optional       (CpgFunctionArgument *argument,
                                                               gboolean             optional);

gdouble              cpg_function_argument_get_default_value  (CpgFunctionArgument *argument);
void                 cpg_function_argument_set_default_value  (CpgFunctionArgument *argument,
                                                               gdouble              def);

void                 _cpg_function_argument_set_property      (CpgFunctionArgument *argument,
                                                               CpgProperty         *property);

CpgProperty         *_cpg_function_argument_get_property      (CpgFunctionArgument *argument);

G_END_DECLS

#endif /* __CPG_FUNCTION_ARGUMENT_H__ */
