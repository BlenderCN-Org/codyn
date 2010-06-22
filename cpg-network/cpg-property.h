#ifndef __CPG_PROPERTY_H__
#define __CPG_PROPERTY_H__

#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-utils.h>

G_BEGIN_DECLS

#define CPG_TYPE_PROPERTY            (cpg_property_get_type ())
#define CPG_PROPERTY(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_PROPERTY, CpgProperty))
#define CPG_PROPERTY_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_PROPERTY, CpgProperty const))
#define CPG_PROPERTY_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_PROPERTY, CpgPropertyClass))
#define CPG_IS_PROPERTY(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_PROPERTY))
#define CPG_IS_PROPERTY_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_PROPERTY))
#define CPG_PROPERTY_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_PROPERTY, CpgPropertyClass))

typedef struct _CpgProperty            CpgProperty;
typedef struct _CpgPropertyClass    CpgPropertyClass;
typedef struct _CpgPropertyPrivate    CpgPropertyPrivate;

/**
 * CpgPropertyFlags:
 * @CPG_PROPERTY_FLAG_NONE: none
 * @CPG_PROPERTY_FLAG_INTEGRATED: integrated
 * @CPG_PROPERTY_FLAG_IN: in
 * @CPG_PROPERTY_FLAG_OUT: out
 * @CPG_PROPERTY_FLAG_ONCE: once
 *
 * Property flags.
 *
 */
typedef enum
{
	CPG_PROPERTY_FLAG_NONE = 0,
	CPG_PROPERTY_FLAG_INTEGRATED = 1 << 0,
	CPG_PROPERTY_FLAG_IN = 1 << 1,
	CPG_PROPERTY_FLAG_OUT = 1 << 2,
	CPG_PROPERTY_FLAG_ONCE = 1 << 3,
} CpgPropertyFlags;

struct _CpgProperty
{
	/*< private >*/
	GObject parent;

	CpgPropertyPrivate *priv;
};

struct _CpgPropertyClass
{
	/*< private >*/
	GObjectClass parent_class;
};

/* forward declaration */
CPG_FORWARD_DECL (CpgObject);

GType cpg_property_get_type (void) G_GNUC_CONST;

CpgProperty       *cpg_property_new                     (const gchar                  *name,
                                                         const gchar                  *expression,
                                                         CpgPropertyFlags              flags,
                                                         CPG_FORWARD_DECL (CpgObject) *object);

const gchar       *cpg_property_get_name                (CpgProperty        *property);

CPG_FORWARD_DECL (CpgObject) *
                   cpg_property_get_object              (CpgProperty        *property);

gboolean           cpg_property_get_integrated          (CpgProperty        *property);
void               cpg_property_set_integrated          (CpgProperty        *property,
                                                         gboolean            integrated);

CpgPropertyFlags   cpg_property_get_flags               (CpgProperty        *property);
void               cpg_property_set_flags               (CpgProperty        *property,
                                                         CpgPropertyFlags    flags);
void               cpg_property_add_flags               (CpgProperty        *property,
                                                         CpgPropertyFlags    flags);
void               cpg_property_remove_flags            (CpgProperty        *property,
                                                         CpgPropertyFlags    flags);
void               cpg_property_reset_cache             (CpgProperty        *property);

gdouble            cpg_property_get_value               (CpgProperty        *property);
CpgExpression     *cpg_property_get_expression          (CpgProperty        *property);

void               cpg_property_set_value               (CpgProperty        *property,
                                                         gdouble             value);

void               cpg_property_set_expression          (CpgProperty        *property,
                                                         CpgExpression      *expression);

gboolean           cpg_property_equal                   (CpgProperty        *property,
                                                         CpgProperty        *other);

guint              cpg_property_get_used                (CpgProperty        *property);

void               cpg_property_set_update              (CpgProperty        *property,
                                                          gdouble             value);
gdouble            cpg_property_get_update              (CpgProperty        *property);

void               _cpg_property_use                    (CpgProperty        *property);
gboolean           _cpg_property_unuse                  (CpgProperty        *property);

void               _cpg_property_set_object             (CpgProperty                  *property,
                                                         CPG_FORWARD_DECL (CpgObject) *object);

CpgProperty       *_cpg_property_copy                   (CpgProperty        *property);


G_END_DECLS

#endif /* __CPG_PROPERTY_H__ */
