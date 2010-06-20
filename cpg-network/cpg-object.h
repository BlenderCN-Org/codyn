#ifndef __CPG_OBJECT_H__
#define __CPG_OBJECT_H__

#include <glib-object.h>
#include <cpg-network/cpg-property.h>
#include <cpg-network/cpg-compile-context.h>
#include <cpg-network/cpg-utils.h>

G_BEGIN_DECLS

#define CPG_OBJECT_ERROR (cpg_object_error_quark ())

#define CPG_TYPE_OBJECT            (cpg_object_get_type ())
#define CPG_OBJECT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OBJECT, CpgObject))
#define CPG_OBJECT_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OBJECT, CpgObject const))
#define CPG_OBJECT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_OBJECT, CpgObjectClass))
#define CPG_IS_OBJECT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_OBJECT))
#define CPG_IS_OBJECT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_OBJECT))
#define CPG_OBJECT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_OBJECT, CpgObjectClass))

typedef struct _CpgObject			CpgObject;
typedef struct _CpgObjectClass		CpgObjectClass;
typedef struct _CpgObjectPrivate	CpgObjectPrivate;

CPG_FORWARD_DECL (CpgLink);
CPG_FORWARD_DECL (CpgCompileError);

/**
 * CpgObjectError:
 * @CPG_OBJECT_ERROR_PROP_UNKNOWN: unknown
 * @CPG_OBJECT_ERROR_PROP_NOT_FOUND: property not found
 * @CPG_OBJECT_ERROR_PROP_IN_USE: property in use
 * @CPG_OBJECT_NUM_ERRORS: num errors
 *
 * Enum used to indicate an error when removing a property
 *
 **/
typedef enum
{
	CPG_OBJECT_ERROR_PROP_UNKNOWN,
	CPG_OBJECT_ERROR_PROP_NOT_FOUND,
	CPG_OBJECT_ERROR_PROP_IN_USE,
	CPG_OBJECT_NUM_ERRORS
} CpgObjectError;

struct _CpgObject
{
	GObject parent;

	CpgObjectPrivate *priv;
};

struct _CpgObjectClass {
	GObjectClass parent_class;

	gboolean      (*compile)         (CpgObject                          *object,
	                                  CpgCompileContext                  *context,
	                                  CPG_FORWARD_DECL (CpgCompileError) *error);

	void          (*reset)           (CpgObject *object);
	void          (*evaluate)        (CpgObject *object);


	void          (*reset_cache)     (CpgObject *object);

	void          (*apply_template)  (CpgObject *object,
	                                  CpgObject *templ);

	void          (*copy)            (CpgObject *object,
	                                  CpgObject *source);

	void          (*taint)           (CpgObject *object);

	GSList       *(*get_properties)  (CpgObject    *object);
	CpgProperty  *(*get_property)    (CpgObject    *object,
	                                  const gchar  *name);

	gboolean      (*has_property)    (CpgObject    *object,
	                                  const gchar  *name);

	CpgProperty  *(*add_property)    (CpgObject    *object,
	                                  const gchar  *name,
	                                  const gchar  *expression,
	                                  gboolean      integrated);

	gboolean      (*remove_property) (CpgObject    *object,
	                                  const gchar  *name,
	                                  GError      **error);

	void          (*clear)           (CpgObject    *object);

	/* signals */
	void          (*compiled)        (CpgObject *object);
	void          (*resetted)        (CpgObject *object);
	void          (*tainted)         (CpgObject *object);
};

GQuark cpg_object_error_quark (void);

GType cpg_object_get_type (void) G_GNUC_CONST;
CpgObject *cpg_object_new (const gchar *id);

const gchar      *cpg_object_get_id          (CpgObject   *object);
void              cpg_object_set_id          (CpgObject   *object,
                                              const gchar *id);

CpgProperty      *cpg_object_add_property    (CpgObject   *object,
                                              const gchar *name,
                                              const gchar *expression,
                                              gboolean     integrated);
CpgProperty      *cpg_object_get_property    (CpgObject   *object,
                                              const gchar *name);
gboolean          cpg_object_has_property    (CpgObject   *object,
                                              const gchar *name);
gboolean          cpg_object_remove_property (CpgObject    *object,
                                              const gchar  *name,
                                              GError      **error);

GSList           *cpg_object_get_properties  (CpgObject   *object);

/* evaluation */
void              cpg_object_reset          (CpgObject   *object);
void              cpg_object_evaluate       (CpgObject   *object);

void              cpg_object_reset_cache    (CpgObject	 *object);
void              cpg_object_taint          (CpgObject   *object);

gboolean          cpg_object_compile        (CpgObject                          *object,
                                             CpgCompileContext                  *context,
                                             CPG_FORWARD_DECL (CpgCompileError) *error);

void              cpg_object_clear          (CpgObject   *object);
GSList const     *cpg_object_get_actors     (CpgObject   *object);

/* used for referencing links */
void             _cpg_object_link           (CpgObject                  *object,
                                             CPG_FORWARD_DECL (CpgLink) *link);

void             _cpg_object_unlink         (CpgObject                  *object,
                                             CPG_FORWARD_DECL (CpgLink) *link);

GSList const    *_cpg_object_get_links      (CpgObject *object);

void             _cpg_object_apply_template (CpgObject *object,
                                             CpgObject *templ);

CpgObject       *_cpg_object_copy           (CpgObject *object);

G_END_DECLS

#endif /* __CPG_OBJECT_H__ */
