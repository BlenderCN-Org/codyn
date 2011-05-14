#ifndef __CPG_PARSER_CONTEXT_H__
#define __CPG_PARSER_CONTEXT_H__

#include <glib-object.h>
#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-property.h>
#include <cpg-network/cpg-link.h>
#include <cpg-network/cpg-function.h>
#include <cpg-network/cpg-function-polynomial.h>
#include <cpg-network/cpg-import.h>
#include <cpg-network/cpg-import-alias.h>

G_BEGIN_DECLS

#define CPG_TYPE_PARSER_CONTEXT			(cpg_parser_context_get_type ())
#define CPG_PARSER_CONTEXT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_PARSER_CONTEXT, CpgParserContext))
#define CPG_PARSER_CONTEXT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_PARSER_CONTEXT, CpgParserContext const))
#define CPG_PARSER_CONTEXT_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_PARSER_CONTEXT, CpgParserContextClass))
#define CPG_IS_PARSER_CONTEXT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_PARSER_CONTEXT))
#define CPG_IS_PARSER_CONTEXT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_PARSER_CONTEXT))
#define CPG_PARSER_CONTEXT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_PARSER_CONTEXT, CpgParserContextClass))

typedef struct _CpgParserContext	CpgParserContext;
typedef struct _CpgParserContextClass	CpgParserContextClass;
typedef struct _CpgParserContextPrivate	CpgParserContextPrivate;

typedef enum
{
	CPG_PARSER_CONTEXT_SCOPE_NONE,
	CPG_PARSER_CONTEXT_SCOPE_STATE,
	CPG_PARSER_CONTEXT_SCOPE_LINK,
	CPG_PARSER_CONTEXT_SCOPE_NETWORK
} CpgParserContextScope;

struct _CpgParserContext
{
	/*< private >*/
	GObject parent;

	CpgParserContextPrivate *priv;
};

struct _CpgParserContextClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType                cpg_parser_context_get_type              (void) G_GNUC_CONST;

CpgParserContext    *cpg_parser_context_new                   (CpgNetwork *network,
                                                               GFile      *file);

CpgProperty         *cpg_parser_context_add_property          (CpgParserContext *context,
                                                               gchar const      *name,
                                                               gchar const      *expression);

void                 cpg_parser_context_add_identifier        (CpgParserContext *context,
                                                               gchar const      *name);

CpgLinkAction       *cpg_parser_context_add_action            (CpgParserContext *context,
                                                               gchar const      *target,
                                                               gchar const      *expression);

CpgFunction         *cpg_parser_context_add_function          (CpgParserContext *context,
                                                               gchar const      *name,
                                                               gchar const      *expression,
                                                               GArray           *arguments);

CpgFunctionPolynomial *cpg_parser_context_add_polynomial      (CpgParserContext *context,
                                                               gchar const      *name,
                                                               GArray           *pieces);

void                 cpg_parser_context_add_interface         (CpgParserContext *context,
                                                               gchar const      *name,
                                                               gchar const      *target);

void                 cpg_parser_context_link                  (CpgParserContext *context,
                                                               gchar const      *linkid,
                                                               gchar const      *from,
                                                               gchar const      *to);

void                 cpg_parser_context_import                (CpgParserContext *context,
                                                               gchar const      *id,
                                                               gchar const      *path,
                                                               gboolean          is_template);

void                 cpg_parser_context_error                 (CpgParserContext *context,
                                                               gint              lineno,
                                                               gchar const      *message);

void                 cpg_parser_context_push_scope            (CpgParserContext     *context,
                                                               gchar const          *id,
                                                               CpgParserContextScope scope);

CpgObject           *cpg_parser_context_pop_scope             (CpgParserContext *context);

void                 cpg_parser_context_push_template         (CpgParserContext *context);
void                 cpg_parser_context_pop_template          (CpgParserContext *context);


G_END_DECLS

#endif /* __CPG_PARSER_CONTEXT_H__ */
