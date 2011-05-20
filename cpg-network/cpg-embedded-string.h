#ifndef __CPG_EMBEDDED_STRING_H__
#define __CPG_EMBEDDED_STRING_H__

#include <glib-object.h>
#include <cpg-network/cpg-embedded-context.h>
#include <cpg-network/cpg-expansion.h>

G_BEGIN_DECLS

#define CPG_TYPE_EMBEDDED_STRING		(cpg_embedded_string_get_type ())
#define CPG_EMBEDDED_STRING(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_EMBEDDED_STRING, CpgEmbeddedString))
#define CPG_EMBEDDED_STRING_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_EMBEDDED_STRING, CpgEmbeddedString const))
#define CPG_EMBEDDED_STRING_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_EMBEDDED_STRING, CpgEmbeddedStringClass))
#define CPG_IS_EMBEDDED_STRING(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_EMBEDDED_STRING))
#define CPG_IS_EMBEDDED_STRING_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_EMBEDDED_STRING))
#define CPG_EMBEDDED_STRING_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_EMBEDDED_STRING, CpgEmbeddedStringClass))

typedef struct _CpgEmbeddedString		CpgEmbeddedString;
typedef struct _CpgEmbeddedStringClass		CpgEmbeddedStringClass;
typedef struct _CpgEmbeddedStringPrivate	CpgEmbeddedStringPrivate;

typedef enum
{
	CPG_EMBEDDED_STRING_NODE_TEXT,
	CPG_EMBEDDED_STRING_NODE_EQUATION,
	CPG_EMBEDDED_STRING_NODE_DEFINE,
	CPG_EMBEDDED_STRING_NODE_REF,
	CPG_EMBEDDED_STRING_NODE_INDIRECTION
} CpgEmbeddedStringNodeType;

struct _CpgEmbeddedString
{
	GObject parent;

	CpgEmbeddedStringPrivate *priv;
};

struct _CpgEmbeddedStringClass
{
	GObjectClass parent_class;
};

GType cpg_embedded_string_get_type (void) G_GNUC_CONST;

CpgEmbeddedString *cpg_embedded_string_new (void);

CpgEmbeddedString *cpg_embedded_string_new_from_string (gchar const *s);
CpgEmbeddedString *cpg_embedded_string_new_from_double (gdouble s);
CpgEmbeddedString *cpg_embedded_string_new_from_integer (gint s);

CpgEmbeddedString *cpg_embedded_string_push (CpgEmbeddedString *s,
                               CpgEmbeddedStringNodeType type,
                               gint num);

CpgEmbeddedString *cpg_embedded_string_pop (CpgEmbeddedString *s);

void cpg_embedded_string_add_text (CpgEmbeddedString *s,
                                   gchar const       *text);

void cpg_embedded_string_add_define (CpgEmbeddedString *s,
                                     gchar const       *text);

void cpg_embedded_string_add_reference (CpgEmbeddedString *s,
                                        gint               parent,
                                        gint               idx);

gchar const *cpg_embedded_string_expand (CpgEmbeddedString *s,
                                         CpgEmbeddedContext *ctx);

GSList *cpg_embedded_string_expand_multiple (CpgEmbeddedString  *s,
                                             CpgEmbeddedContext *ctx);

G_END_DECLS

#endif /* __CPG_EMBEDDED_STRING_H__ */
