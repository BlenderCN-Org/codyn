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
#include <cpg-network/cpg-selector.h>
#include <cpg-network/cpg-layout.h>

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
typedef struct _CpgParserContextClassPrivate	CpgParserContextClassPrivate;

typedef enum
{
	CPG_PARSER_CONTEXT_LINK_FLAG_BIDIRECTIONAL = 1 << 0,
	CPG_PARSER_CONTEXT_LINK_FLAG_ALL = 1 << 1
} CpgParserContextLinkFlags;

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

	CpgParserContextClassPrivate *priv;
};

GType                  cpg_parser_context_get_type             (void) G_GNUC_CONST;

CpgParserContext      *cpg_parser_context_new                  (CpgNetwork                 *network);

GFile                 *cpg_parser_context_get_file             (CpgParserContext           *context);

void                   cpg_parser_context_set_line             (CpgParserContext           *context,
                                                                gchar const                *line,
                                                                gint                        lineno);

gchar const           *cpg_parser_context_get_line             (CpgParserContext           *context,
                                                                gint                       *lineno);

void                   cpg_parser_context_set_column           (CpgParserContext           *context,
                                                                gint                        start,
                                                                gint                        end);

void                   cpg_parser_context_get_column           (CpgParserContext           *context,
                                                                gint                       *start,
                                                                gint                       *end);

void                   cpg_parser_context_set_token            (CpgParserContext           *context,
                                                                gchar const                *token);

gchar const           *cpg_parser_context_get_token            (CpgParserContext           *context);

gboolean               cpg_parser_context_parse                (CpgParserContext           *context,
                                                                GError                    **error);

void                   cpg_parser_context_add_property         (CpgParserContext           *context,
                                                                gchar const                *name,
                                                                gchar const                *expression,
                                                                CpgPropertyFlags            flags);

void                   cpg_parser_context_add_action           (CpgParserContext           *context,
                                                                gchar const                *target,
                                                                gchar const                *expression);

CpgFunction           *cpg_parser_context_add_function         (CpgParserContext           *context,
                                                                gchar const                *name,
                                                                gchar const                *expression,
                                                                GArray                     *arguments);

CpgFunctionPolynomial *cpg_parser_context_add_polynomial       (CpgParserContext           *context,
                                                                gchar const                *name,
                                                                GArray                     *pieces);

void                   cpg_parser_context_add_interface        (CpgParserContext           *context,
                                                                gchar const                *name,
                                                                CpgSelector                *target);

void                   cpg_parser_context_import               (CpgParserContext           *context,
                                                                gchar const                *id,
                                                                gchar const                *path);

void                   cpg_parser_context_error                (CpgParserContext           *context,
                                                                gchar const                *message);

GError                *cpg_parser_context_get_error            (CpgParserContext           *context);

void                   cpg_parser_context_push_object          (CpgParserContext           *context,
                                                                GSList                     *objects);

void                   cpg_parser_context_push_state           (CpgParserContext           *context,
                                                                gchar const                *id,
                                                                GArray                     *templates);

void                   cpg_parser_context_push_group           (CpgParserContext           *context,
                                                                gchar const                *id,
                                                                GArray                     *templates);

void                   cpg_parser_context_push_link            (CpgParserContext           *context,
                                                                gchar const                *id,
                                                                GArray                     *templates,
                                                                CpgParserContextLinkFlags   flags,
                                                                GArray                     *fromto);

void                   cpg_parser_context_push_network         (CpgParserContext           *context);
void                   cpg_parser_context_push_templates       (CpgParserContext           *context);
void                   cpg_parser_context_push_integrator      (CpgParserContext           *context);

void                   cpg_parser_context_set_proxy            (CpgParserContext           *context,
                                                                GSList                     *objects);

GSList                *cpg_parser_context_pop                  (CpgParserContext           *context);

void                   cpg_parser_context_push_selector        (CpgParserContext           *context,
                                                                gchar const                *identifier);
void                   cpg_parser_context_push_selector_regex  (CpgParserContext           *context,
                                                                gchar const                *regex);
void                   cpg_parser_context_push_selector_pseudo (CpgParserContext           *context,
                                                                gchar const                *identifier,
                                                                GArray                     *arguments);

CpgSelector           *cpg_parser_context_pop_selector         (CpgParserContext           *context);

gssize                 cpg_parser_context_read                 (CpgParserContext           *context,
                                                                gchar                      *buffer,
                                                                gsize                       max_size);

gpointer               cpg_parser_context_get_scanner          (CpgParserContext           *context);

void                   cpg_parser_context_define               (CpgParserContext           *context,
                                                                gchar const                *name,
                                                                gchar const                *define);

gchar                 *cpg_parser_context_embed_define         (CpgParserContext           *context,
                                                                gchar const                *define);

gchar                 *cpg_parser_context_embed_equation       (CpgParserContext           *context,
                                                                gchar const                *equation);

gchar                 *cpg_parser_context_embed_expansion      (CpgParserContext           *context,
                                                                gchar const                *expansion);

void                   cpg_parser_context_remove               (CpgParserContext           *context,
                                                                GArray                     *selectors);

void                   cpg_parser_context_set_integrator       (CpgParserContext           *context,
                                                                gchar const                *integrator);

void                   cpg_parser_context_push_input_from_path (CpgParserContext           *context,
                                                                gchar const                *filename);

void                   cpg_parser_context_push_input           (CpgParserContext           *context,
                                                                GFile                      *file,
                                                                GInputStream               *stream);

void                   cpg_parser_context_pop_input            (CpgParserContext           *context);

gint                   cpg_parser_context_steal_start_token    (CpgParserContext           *context);
gint                   cpg_parser_context_get_start_token      (CpgParserContext           *context);

void                   cpg_parser_context_set_start_token      (CpgParserContext           *context,
                                                                gint                        token);

void                   cpg_parser_context_push_annotation      (CpgParserContext           *context,
                                                                gchar const                *annotation);

void                   cpg_parser_context_push_layout          (CpgParserContext           *context);

void                   cpg_parser_context_add_layout           (CpgParserContext           *context,
                                                                CpgLayoutRelation           relation,
                                                                CpgSelector                *left,
                                                                CpgSelector                *right);

void                   cpg_parser_context_add_layout_position  (CpgParserContext           *context,
                                                                CpgSelector                *selector,
                                                                gchar const                *x,
                                                                gchar const                *y,
                                                                CpgSelector                *of);

void                   cpg_parser_context_add_integrator_property (CpgParserContext        *context,
                                                                   gchar const             *name,
                                                                   gchar const             *value);

G_END_DECLS

#endif /* __CPG_PARSER_CONTEXT_H__ */
