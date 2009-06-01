#ifndef __CPG_OBJECT_H__
#define __CPG_OBJECT_H__

#include <glib-object.h>
#include "cpg-property.h"

G_BEGIN_DECLS

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

struct _CpgObject {
	GObject parent;
	
	CpgObjectPrivate *priv;
};

struct _CpgObjectClass {
	GObjectClass parent_class;
	
	void (*compile)		(CpgObject *object);
	void (*reset)		(CpgObject *object);
	void (*update)		(CpgObject *object, 
						 gdouble    timestep);
	void (*evaluate)	(CpgObject *object, 
						 gdouble    timestep);
	
	void (*tainted)		(CpgObject *object);
};

GType cpg_object_get_type (void) G_GNUC_CONST;
CpgObject *cpg_object_new (gchar const *id);

gchar const 	 *cpg_object_get_id			(CpgObject   *object);
gchar 			 *cpg_object_get_local_id	(CpgObject   *object);

CpgProperty 	 *cpg_object_add_property	(CpgObject   *object, 
											 gchar const *name, 
											 gchar const *expression, 
											 gboolean     integrated);
CpgProperty 	 *cpg_object_get_property	(CpgObject   *object, 
											 gchar const *name);
gboolean		  cpg_object_has_property   (CpgObject   *object,
											 gchar const *name);
void			  cpg_object_remove_property (CpgObject  *object,
											  gchar const *name);

GSList			*cpg_object_get_properties	(CpgObject   *object);

/* evaluation */
void			  cpg_object_reset			(CpgObject   *object);
void			  cpg_object_update			(CpgObject   *object, 
											 gdouble      timestep);
void			  cpg_object_evaluate		(CpgObject   *object, 
											 gdouble      timestep);

void			  cpg_object_reset_cache	(CpgObject	 *object);
void			  cpg_object_taint			(CpgObject   *object);

/* used for referencing links */
void 			 _cpg_object_link			(CpgObject       *object, 
											 struct _CpgLink *link);
GSList 			*_cpg_object_get_actors		(CpgObject       *object);
GSList 			*_cpg_object_get_links		(CpgObject       *object);

void 			 _cpg_object_taint			(CpgObject 		 *object);

G_END_DECLS

#endif /* __CPG_OBJECT_H__ */
