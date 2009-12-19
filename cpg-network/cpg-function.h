#ifndef __CPG_FUNCTION_H__
#define __CPG_FUNCTION_H__

#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-stack.h>

#include <stdarg.h>

G_BEGIN_DECLS

#define CPG_TYPE_FUNCTION				(cpg_function_get_type ())
#define CPG_FUNCTION(obj)				(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_FUNCTION, CpgFunction))
#define CPG_FUNCTION_CONST(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_FUNCTION, CpgFunction const))
#define CPG_FUNCTION_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_FUNCTION, CpgFunctionClass))
#define CPG_IS_FUNCTION(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_FUNCTION))
#define CPG_IS_FUNCTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_FUNCTION))
#define CPG_FUNCTION_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_FUNCTION, CpgFunctionClass))

struct _CpgExpression;

typedef struct _CpgFunction			CpgFunction;
typedef struct _CpgFunctionClass	CpgFunctionClass;
typedef struct _CpgFunctionPrivate	CpgFunctionPrivate;

struct _CpgFunction {
	CpgObject parent;
	
	CpgFunctionPrivate *priv;
};

struct _CpgFunctionClass {
	CpgObjectClass parent_class;

	gdouble (*evaluate) (CpgFunction *function);
	void (*execute) (CpgFunction *function, CpgStack *stack);
};

GType cpg_function_get_type (void) G_GNUC_CONST;

CpgFunction *cpg_function_new (gchar const *name, gchar const *expression, ...) G_GNUC_NULL_TERMINATED;

void cpg_function_set_arguments (CpgFunction *function, ...) G_GNUC_NULL_TERMINATED;
void cpg_function_set_argumentsv (CpgFunction *function, va_list args);
void cpg_function_add_argument (CpgFunction *function, gchar const *name);

GSList *cpg_function_get_arguments (CpgFunction *function);
guint cpg_function_get_n_arguments (CpgFunction *function);

void cpg_function_execute (CpgFunction *function, CpgStack *stack);

struct _CpgExpression *cpg_function_get_expression (CpgFunction *function);

G_END_DECLS

#endif /* __CPG_FUNCTION_H__ */
