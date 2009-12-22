#ifndef __CPG_OBJECT_H__
#define __CPG_OBJECT_H__

#include <glib-object.h>
#include "cpg-property.h"
#include "cpg-compile-context.h"

G_BEGIN_DECLS

#define CPG_OBJECT_ERROR (cpg_object_error_quark ())

#define CPG_TYPE_OBJECT				(cpg_object_get_type ())
#define CPG_OBJECT(obj)				(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OBJECT, CpgObject))
#define CPG_OBJECT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OBJECT, CpgObject const))
#define CPG_OBJECT_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_OBJECT, CpgObjectClass))
#define CPG_IS_OBJECT(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_OBJECT))
#define CPG_IS_OBJECT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_OBJECT))
#define CPG_OBJECT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_OBJECT, CpgObjectClass))

typedef struct _CpgObject			CpgObject;
typedef struct _CpgObjectClass		CpgObjectClass;
typedef struct _CpgObjectPrivate	CpgObjectPrivate;

struct _CpgLink;
struct _CpgCompileError;

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

struct _CpgObject {
	/*< private >*/
	GObject parent;
	
	CpgObjectPrivate *priv;
};

struct _CpgObjectClass {
	/*< private >*/
	GObjectClass parent_class;

	/*< public >*/	
	gboolean (*compile)	(CpgObject *object,
	                     CpgCompileContext *context,
	                     struct _CpgCompileError *error);
	void (*reset)		(CpgObject *object);
	void (*evaluate)	(CpgObject *object);
	
	void (*tainted)		(CpgObject *object);
	
	void (*reset_cache) (CpgObject *object);
	
	void (*copy)		(CpgObject *object,
	                     CpgObject *source);
};

GQuark cpg_object_error_quark (void);

GType cpg_object_get_type (void) G_GNUC_CONST;
CpgObject *cpg_object_new (const gchar *id);

CpgObject        *_cpg_object_copy			(CpgObject   *object);
const gchar 	 *cpg_object_get_id			(CpgObject   *object);
void              cpg_object_set_id			(CpgObject   *object,
                                             const gchar *id);
gchar 			 *cpg_object_get_local_id	(CpgObject   *object);

CpgProperty 	 *cpg_object_add_property	(CpgObject   *object, 
											 const gchar *name, 
											 const gchar *expression, 
											 gboolean     integrated);
CpgProperty 	 *cpg_object_get_property	(CpgObject   *object, 
											 const gchar *name);
gboolean		  cpg_object_has_property   (CpgObject   *object,
											 const gchar *name);
gboolean		  cpg_object_remove_property (CpgObject    *object,
											  const gchar  *name,
											  GError      **error);

GSList			*cpg_object_get_properties	(CpgObject   *object);

/* evaluation */
void			  cpg_object_reset			(CpgObject   *object);
void			  cpg_object_evaluate		(CpgObject   *object);

void			  cpg_object_reset_cache	(CpgObject	 *object);
void			  cpg_object_taint			(CpgObject   *object);

gboolean		  cpg_object_compile		(CpgObject         *object,
											 CpgCompileContext *context,
											 struct _CpgCompileError *error);

/* used for referencing links */
void 			 _cpg_object_link			(CpgObject       *object, 
											 struct _CpgLink *link);
GSList 			 *cpg_object_get_actors		(CpgObject       *object);
GSList 			*_cpg_object_get_links		(CpgObject       *object);

void 			 _cpg_object_taint			(CpgObject 		 *object);

G_END_DECLS

#endif /* __CPG_OBJECT_H__ */
