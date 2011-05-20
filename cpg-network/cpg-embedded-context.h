#ifndef __CPG_EMBEDDED_CONTEXT_H__
#define __CPG_EMBEDDED_CONTEXT_H__

#include <glib-object.h>
#include <cpg-network/cpg-expansion.h>

G_BEGIN_DECLS

#define CPG_TYPE_EMBEDDED_CONTEXT		(cpg_embedded_context_get_type ())
#define CPG_EMBEDDED_CONTEXT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_EMBEDDED_CONTEXT, CpgEmbeddedContext))
#define CPG_EMBEDDED_CONTEXT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_EMBEDDED_CONTEXT, CpgEmbeddedContext const))
#define CPG_EMBEDDED_CONTEXT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_EMBEDDED_CONTEXT, CpgEmbeddedContextClass))
#define CPG_IS_EMBEDDED_CONTEXT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_EMBEDDED_CONTEXT))
#define CPG_IS_EMBEDDED_CONTEXT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_EMBEDDED_CONTEXT))
#define CPG_EMBEDDED_CONTEXT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_EMBEDDED_CONTEXT, CpgEmbeddedContextClass))

typedef struct _CpgEmbeddedContext		CpgEmbeddedContext;
typedef struct _CpgEmbeddedContextClass		CpgEmbeddedContextClass;
typedef struct _CpgEmbeddedContextPrivate	CpgEmbeddedContextPrivate;

struct _CpgEmbeddedContext
{
	GObject parent;

	CpgEmbeddedContextPrivate *priv;
};

struct _CpgEmbeddedContextClass
{
	GObjectClass parent_class;
};

GType cpg_embedded_context_get_type (void) G_GNUC_CONST;

CpgEmbeddedContext *cpg_embedded_context_new (void);

void cpg_embedded_context_define (CpgEmbeddedContext *context,
                                  gchar const        *name,
                                  gchar const        *value);

void cpg_embedded_context_push_expansion  (CpgEmbeddedContext *context,
                                           CpgExpansion       *expansion);

void cpg_embedded_context_push_expansions (CpgEmbeddedContext *context,
                                           GSList *expansions);

void cpg_embedded_context_set_expansions (CpgEmbeddedContext *context,
                                          GSList             *expansions);

GSList *cpg_embedded_context_get_expansions (CpgEmbeddedContext *context);

void cpg_embedded_context_pop_expansions (CpgEmbeddedContext *context);

gchar *cpg_embedded_context_lookup_define (CpgEmbeddedContext *context,
                                           gchar const        *name);

gchar *cpg_embedded_context_lookup_ref (CpgEmbeddedContext *context,
                                        gint                parent,
                                        gint                idx);

gchar *cpg_embedded_context_calculate (CpgEmbeddedContext *context,
                                       gchar const        *equation);

G_END_DECLS

#endif /* __CPG_EMBEDDED_CONTEXT_H__ */
