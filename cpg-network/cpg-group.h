#ifndef __CPG_GROUP_H__
#define __CPG_GROUP_H__

#include <cpg-network/cpg-state.h>

G_BEGIN_DECLS

#define CPG_TYPE_GROUP			(cpg_group_get_type ())
#define CPG_GROUP(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_GROUP, CpgGroup))
#define CPG_GROUP_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_GROUP, CpgGroup const))
#define CPG_GROUP_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_GROUP, CpgGroupClass))
#define CPG_IS_GROUP(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_GROUP))
#define CPG_IS_GROUP_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_GROUP))
#define CPG_GROUP_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_GROUP, CpgGroupClass))

typedef struct _CpgGroup	CpgGroup;
typedef struct _CpgGroupClass	CpgGroupClass;
typedef struct _CpgGroupPrivate	CpgGroupPrivate;

struct _CpgGroup
{
	/*< private >*/
	CpgState parent;

	CpgGroupPrivate *priv;
};

/**
 * CpgGroupClass:
 * @add: add virtual function
 * @remove: remove virtual function
 * @child_added: child added default signal handler
 * @child_removed: child removed default signal handler
 * The CpgGroup class
 *
 */
struct _CpgGroupClass
{
	/*< private >*/
	CpgStateClass parent_class;

	/*< public >*/
	gboolean (*add)           (CpgGroup *group,
	                           CpgObject *object);
	gboolean (*remove)        (CpgGroup *group,
	                           CpgObject *object);

	GSList const *(*get_children) (CpgGroup *group);

	/* signals */
	void     (*child_added)   (CpgGroup *group,
	                           CpgObject *object);
	void     (*child_removed) (CpgGroup *group,
	                           CpgObject *object);
};

GType     cpg_group_get_type         (void) G_GNUC_CONST;

CpgGroup *cpg_group_new              (const gchar *id,
                                      CpgObject   *proxy);

GSList const *cpg_group_get_children (CpgGroup    *group);

gboolean      cpg_group_add          (CpgGroup    *group,
                                      CpgObject   *object);
gboolean      cpg_group_remove       (CpgGroup    *group,
                                      CpgObject   *object);

gboolean      cpg_group_set_proxy    (CpgGroup    *group,
                                      CpgObject   *proxy);
CpgObject    *cpg_group_get_proxy    (CpgGroup    *group);

void          cpg_group_foreach      (CpgGroup    *group,
                                      GFunc        func,
                                      gpointer     data);

CpgObject    *cpg_group_get_child    (CpgGroup    *group,
                                      const gchar *name);

CpgObject    *cpg_group_find_object  (CpgGroup    *group,
                                      const gchar *path);

CpgProperty  *cpg_group_find_property (CpgGroup    *group,
                                       const gchar *path);

G_END_DECLS

#endif /* __CPG_GROUP_H__ */
