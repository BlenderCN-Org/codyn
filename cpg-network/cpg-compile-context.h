#ifndef __CPG_COMPILE_CONTEXT_H__
#define __CPG_COMPILE_CONTEXT_H__

#include <glib-object.h>
#include <cpg-network/cpg-utils.h>

G_BEGIN_DECLS

#define CPG_TYPE_COMPILE_CONTEXT		(cpg_compile_context_get_type ())
#define CPG_COMPILE_CONTEXT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_COMPILE_CONTEXT, CpgCompileContext))
#define CPG_COMPILE_CONTEXT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_COMPILE_CONTEXT, CpgCompileContext const))
#define CPG_COMPILE_CONTEXT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_COMPILE_CONTEXT, CpgCompileContextClass))
#define CPG_IS_COMPILE_CONTEXT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_COMPILE_CONTEXT))
#define CPG_IS_COMPILE_CONTEXT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_COMPILE_CONTEXT))
#define CPG_COMPILE_CONTEXT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_COMPILE_CONTEXT, CpgCompileContextClass))

typedef struct _CpgCompileContext		CpgCompileContext;
typedef struct _CpgCompileContextClass		CpgCompileContextClass;
typedef struct _CpgCompileContextPrivate	CpgCompileContextPrivate;

CPG_FORWARD_DECL (CpgObject);
CPG_FORWARD_DECL (CpgProperty);
CPG_FORWARD_DECL (CpgFunction);

struct _CpgCompileContext
{
	GObject parent;

	CpgCompileContextPrivate *priv;
};

struct _CpgCompileContextClass
{
	GObjectClass parent_class;
};

GType cpg_compile_context_get_type (void) G_GNUC_CONST;
CpgCompileContext *cpg_compile_context_new (void);

void cpg_compile_context_prepend_object (CpgCompileContext *context,
                                         CPG_FORWARD_DECL (CpgObject) *object);

void cpg_compile_context_save (CpgCompileContext *context);
void cpg_compile_context_restore (CpgCompileContext *context);

void cpg_compile_context_append_object (CpgCompileContext *context,
                                        CPG_FORWARD_DECL (CpgObject) *object);

void cpg_compile_context_set_functions (CpgCompileContext *context,
                                        const GSList      *functions);

CPG_FORWARD_DECL (CpgProperty) *cpg_compile_context_lookup_property (CpgCompileContext *context,
                                                          const gchar       *name);

CPG_FORWARD_DECL (CpgFunction) *cpg_compile_context_lookup_function (CpgCompileContext *context,
                                                          const gchar       *name);

const GSList *cpg_compile_context_get_objects (CpgCompileContext *context);
const GSList *cpg_compile_context_get_functions (CpgCompileContext *context);

G_END_DECLS

#endif /* __CPG_COMPILE_CONTEXT_H__ */

