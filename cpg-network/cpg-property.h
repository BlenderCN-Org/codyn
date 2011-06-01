#ifndef __CPG_PROPERTY_H__
#define __CPG_PROPERTY_H__

#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-utils.h>
#include <cpg-network/cpg-modifiable.h>
#include <cpg-network/cpg-usable.h>

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
 * @CPG_PROPERTY_FLAG_INOUT: convenience for CPG_PROPERTY_FLAG_IN | CPG_PROPERTY_FLAG_OUT
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

	CPG_PROPERTY_FLAG_INOUT = CPG_PROPERTY_FLAG_IN | CPG_PROPERTY_FLAG_OUT
} CpgPropertyFlags;

struct _CpgProperty
{
	/*< private >*/
	GInitiallyUnowned parent;

	CpgPropertyPrivate *priv;
};

struct _CpgPropertyClass
{
	/*< private >*/
	GInitiallyUnownedClass parent_class;

	/*< public >*/

	/* signals */
	gboolean (*invalidate_name) (CpgProperty *property,
	                             const gchar *name);

	void (*expression_changed) (CpgProperty      *property,
	                            CpgExpression    *expression);
	void (*flags_changed)      (CpgProperty      *property,
	                            CpgPropertyFlags  flags);
};

/* forward declaration */
CPG_FORWARD_DECL (CpgObject);

GType cpg_property_get_type (void) G_GNUC_CONST;

CpgProperty       *cpg_property_new                     (const gchar      *name,
                                                         const gchar      *expression,
                                                         CpgPropertyFlags  flags);

const gchar       *cpg_property_get_name                (CpgProperty        *property);
gboolean           cpg_property_set_name                (CpgProperty        *property,
                                                         const gchar        *name);

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
void               cpg_property_reset                   (CpgProperty        *property);

gdouble            cpg_property_get_value               (CpgProperty        *property);
gdouble            cpg_property_get_last_value          (CpgProperty        *property);
void               cpg_property_update_last_value       (CpgProperty        *property);

CpgExpression     *cpg_property_get_expression          (CpgProperty        *property);

void               cpg_property_set_value               (CpgProperty        *property,
                                                         gdouble             value);

void               cpg_property_set_expression          (CpgProperty        *property,
                                                         CpgExpression      *expression);

gboolean           cpg_property_equal                   (CpgProperty        *property,
                                                         CpgProperty        *other);

void               cpg_property_set_update              (CpgProperty        *property,
                                                          gdouble             value);
gdouble            cpg_property_get_update              (CpgProperty        *property);

gchar             *cpg_property_flags_to_string         (CpgPropertyFlags    add_flags,
                                                         CpgPropertyFlags    remove_flags);

void               cpg_property_flags_from_string       (const gchar        *flags,
                                                         CpgPropertyFlags   *add_flags,
                                                         CpgPropertyFlags   *remove_flags);

gchar             *cpg_property_get_full_name           (CpgProperty        *property);

CpgProperty       *cpg_property_copy                    (CpgProperty        *property);

void               _cpg_property_set_object             (CpgProperty                  *property,
                                                         CPG_FORWARD_DECL (CpgObject) *object);

G_END_DECLS

#endif /* __CPG_PROPERTY_H__ */
