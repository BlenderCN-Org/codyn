#ifndef __CPG_WHEN_APPLIED_H__
#define __CPG_WHEN_APPLIED_H__

#include <glib-object.h>
#include <cpg-network/cpg-embedded-context.h>
#include <cpg-network/cpg-utils.h>

G_BEGIN_DECLS

#define CPG_TYPE_WHEN_APPLIED			(cpg_when_applied_get_type ())
#define CPG_WHEN_APPLIED(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_WHEN_APPLIED, CpgWhenApplied))
#define CPG_WHEN_APPLIED_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_WHEN_APPLIED, CpgWhenApplied const))
#define CPG_WHEN_APPLIED_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_WHEN_APPLIED, CpgWhenAppliedClass))
#define CPG_IS_WHEN_APPLIED(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_WHEN_APPLIED))
#define CPG_IS_WHEN_APPLIED_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_WHEN_APPLIED))
#define CPG_WHEN_APPLIED_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_WHEN_APPLIED, CpgWhenAppliedClass))

typedef struct _CpgWhenApplied		CpgWhenApplied;
typedef struct _CpgWhenAppliedClass	CpgWhenAppliedClass;
typedef struct _CpgWhenAppliedPrivate	CpgWhenAppliedPrivate;

CPG_FORWARD_DECL (CpgObject);

struct _CpgWhenApplied
{
	/*< private >*/
	GObject parent;

	CpgWhenAppliedPrivate *priv;
};

struct _CpgWhenAppliedClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType               cpg_when_applied_get_type     (void) G_GNUC_CONST;

CpgWhenApplied     *cpg_when_applied_new          (CpgEmbeddedContext *closure,
                                                   gchar const        *code,
                                                   gboolean            isapply);

gboolean            cpg_when_applied_run          (CpgWhenApplied     *applied,
                                                   CPG_FORWARD_DECL (CpgObject) *object,
                                                   GError            **error);

CpgEmbeddedContext *cpg_when_applied_get_closure  (CpgWhenApplied     *applied);
gchar const        *cpg_when_applied_get_code     (CpgWhenApplied     *applied);
gboolean            cpg_when_applied_get_is_apply (CpgWhenApplied     *applied);

G_END_DECLS

#endif /* __CPG_WHEN_APPLIED_H__ */
