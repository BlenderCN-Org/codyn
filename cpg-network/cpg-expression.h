#ifndef __CPG_EXPRESSION_H__
#define __CPG_EXPRESSION_H__

#include <stdio.h>
#include <glib-object.h>
#include <cpg-network/cpg-compile-context.h>
#include <cpg-network/cpg-utils.h>

G_BEGIN_DECLS

#define CPG_TYPE_EXPRESSION             (cpg_expression_get_type ())
#define CPG_EXPRESSION(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_EXPRESSION, CpgExpression))
#define CPG_EXPRESSION_CONST(obj)       (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_EXPRESSION, CpgExpression const))
#define CPG_EXPRESSION_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_EXPRESSION, CpgExpressionClass))
#define CPG_IS_EXPRESSION(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_EXPRESSION))
#define CPG_IS_EXPRESSION_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_EXPRESSION))
#define CPG_EXPRESSION_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_EXPRESSION, CpgExpressionClass))

typedef struct _CpgExpression		CpgExpression;
typedef struct _CpgExpressionClass	CpgExpressionClass;
typedef struct _CpgExpressionPrivate	CpgExpressionPrivate;

struct _CpgExpression
{
	/*< private >*/
	GInitiallyUnowned parent;

	CpgExpressionPrivate *priv;
};

struct _CpgExpressionClass
{
	/*< private >*/
	GInitiallyUnownedClass parent_class;
};

GType          cpg_expression_get_type         (void) G_GNUC_CONST;

CpgExpression *cpg_expression_new              (const gchar        *expression);
CpgExpression *cpg_expression_copy             (CpgExpression      *expression);

const GSList  *cpg_expression_get_dependencies (CpgExpression      *expression);

const gchar   *cpg_expression_get_as_string    (CpgExpression      *expression);

gboolean       cpg_expression_compile          (CpgExpression      *expression,
                                                CpgCompileContext  *context,
                                                GError            **error);

gdouble        cpg_expression_evaluate         (CpgExpression      *expression);
void           cpg_expression_set_value        (CpgExpression      *expression,
                                                gdouble             value);

void           cpg_expression_reset            (CpgExpression      *expression);
void           cpg_expression_reset_variadic   (CpgExpression      *expression);

gboolean       cpg_expression_equal            (CpgExpression      *expression,
                                                CpgExpression      *other);

void           cpg_expression_set_from_string  (CpgExpression      *expression,
                                                const gchar        *value);

void           cpg_expression_reset_cache      (CpgExpression      *expression);

const GSList  *cpg_expression_get_instructions (CpgExpression      *expression);
gboolean       cpg_expression_set_instructions (CpgExpression      *expression,
                                                const GSList       *instructions);

gboolean       _cpg_expression_set_instructions_take (CpgExpression      *expression,
                                                      GSList             *instructions);

gboolean       cpg_expression_get_once         (CpgExpression      *expression);
void           cpg_expression_set_once         (CpgExpression      *expression,
                                                gboolean            instant);

G_END_DECLS

#endif /* __CPG_EXPRESSION_H__ */
