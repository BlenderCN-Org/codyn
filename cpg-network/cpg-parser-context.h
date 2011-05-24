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
#include <cpg-network/cpg-attribute.h>

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
	CPG_PARSER_CONTEXT_SELECTOR_TYPE_OBJECT,
	CPG_PARSER_CONTEXT_SELECTOR_TYPE_STATE,
	CPG_PARSER_CONTEXT_SELECTOR_TYPE_LINK,
	CPG_PARSER_CONTEXT_SELECTOR_TYPE_PROPERTY,
	CPG_PARSER_CONTEXT_SELECTOR_TYPE_ACTION,
	CPG_PARSER_CONTEXT_SELECTOR_TYPE_FUNCTION
} CpgParserContextDeleteType;

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

GType                  cpg_parser_context_get_type             (void) G_GNUC_CONST;

CpgParserContext      *cpg_parser_context_new                  (CpgNetwork                 *network);
CpgEmbeddedContext    *cpg_parser_context_get_embedded         (CpgParserContext           *context);

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
                                                                CpgEmbeddedString          *name,
                                                                CpgEmbeddedString           *expression,
                                                                CpgPropertyFlags            flags);

void                   cpg_parser_context_add_action           (CpgParserContext           *context,
                                                                CpgEmbeddedString          *target,
                                                                CpgEmbeddedString          *expression);

CpgFunction           *cpg_parser_context_add_function         (CpgParserContext           *context,
                                                                CpgEmbeddedString          *name,
                                                                CpgEmbeddedString          *expression,
                                                                GArray                     *arguments);

CpgFunctionPolynomial *cpg_parser_context_add_polynomial       (CpgParserContext           *context,
                                                                CpgEmbeddedString          *name,
                                                                GArray                     *pieces);

void                   cpg_parser_context_add_interface        (CpgParserContext           *context,
                                                                CpgEmbeddedString          *name,
                                                                CpgSelector                *target);

void                   cpg_parser_context_import               (CpgParserContext           *context,
                                                                CpgEmbeddedString          *id,
                                                                CpgEmbeddedString          *path);

void                   cpg_parser_context_set_error            (CpgParserContext           *context,
                                                                gchar const                *message);

GError                *cpg_parser_context_get_error            (CpgParserContext           *context);

void                   cpg_parser_context_push_object          (CpgParserContext           *context,
                                                                GSList                     *objects);

void                   cpg_parser_context_push_state           (CpgParserContext           *context,
                                                                CpgEmbeddedString          *id,
                                                                GArray                     *templates,
                                                                GSList                     *attributes);

void                   cpg_parser_context_push_group           (CpgParserContext           *context,
                                                                CpgEmbeddedString          *id,
                                                                GArray                     *templates,
                                                                GSList                     *attributes);

void                   cpg_parser_context_push_link            (CpgParserContext           *context,
                                                                CpgEmbeddedString          *id,
                                                                GArray                     *templates,
                                                                GSList                     *attributes,
                                                                GArray                     *fromto);

void                   cpg_parser_context_push_network         (CpgParserContext           *context,
                                                                GSList                     *attributes);
void                   cpg_parser_context_push_templates       (CpgParserContext           *context,
                                                                GSList                     *attributes);
void                   cpg_parser_context_push_integrator      (CpgParserContext           *context,
                                                                GSList                     *attributes);

void                   cpg_parser_context_set_proxy            (CpgParserContext           *context,
                                                                GSList                     *objects);

GSList                *cpg_parser_context_pop                  (CpgParserContext           *context);

void                   cpg_parser_context_push_selector_identifier (CpgParserContext           *context,
                                                                    CpgEmbeddedString          *identifier,
                                                                    gboolean                    onset);
void                   cpg_parser_context_push_selector_regex  (CpgParserContext           *context,
                                                                CpgEmbeddedString          *regex,
                                                                gboolean                    onset);
void                   cpg_parser_context_push_selector_pseudo (CpgParserContext           *context,
                                                                CpgSelectorPseudoType       type,
                                                                GSList                     *arguments);

CpgSelector           *cpg_parser_context_pop_selector         (CpgParserContext           *context);
void                   cpg_parser_context_push_selector        (CpgParserContext           *context);

gssize                 cpg_parser_context_read                 (CpgParserContext           *context,
                                                                gchar                      *buffer,
                                                                gsize                       max_size);

gpointer               cpg_parser_context_get_scanner          (CpgParserContext           *context);

void                   cpg_parser_context_define               (CpgParserContext           *context,
                                                                CpgEmbeddedString          *name,
                                                                GSList                     *values,
                                                                gboolean                    expand);

void                   cpg_parser_context_remove               (CpgParserContext           *context,
                                                                GArray                     *selectors);

void                   cpg_parser_context_set_integrator       (CpgParserContext           *context,
                                                                CpgEmbeddedString          *value);

void                   cpg_parser_context_push_input_from_path (CpgParserContext           *context,
                                                                CpgEmbeddedString          *path);

void                   cpg_parser_context_push_input_from_string (CpgParserContext         *context,
                                                                  gchar const              *s);

void                   cpg_parser_context_push_input           (CpgParserContext           *context,
                                                                GFile                      *file,
                                                                GInputStream               *stream);

void                   cpg_parser_context_pop_input            (CpgParserContext           *context);

gint                   cpg_parser_context_steal_start_token    (CpgParserContext           *context);
gint                   cpg_parser_context_get_start_token      (CpgParserContext           *context);

void                   cpg_parser_context_set_start_token      (CpgParserContext           *context,
                                                                gint                        token);

void                   cpg_parser_context_push_annotation      (CpgParserContext           *context,
                                                                CpgEmbeddedString          *annotation);

void                   cpg_parser_context_push_scope           (CpgParserContext           *context,
                                                                GSList                     *attributes);

void                   cpg_parser_context_push_layout          (CpgParserContext           *context,
                                                                GSList                     *attributes);
void                   cpg_parser_context_pop_layout           (CpgParserContext           *context);

void                   cpg_parser_context_add_layout           (CpgParserContext           *context,
                                                                CpgLayoutRelation           relation,
                                                                CpgSelector                *left,
                                                                CpgSelector                *right);

void                   cpg_parser_context_add_layout_position  (CpgParserContext           *context,
                                                                CpgSelector                *selector,
                                                                CpgEmbeddedString          *x,
                                                                CpgEmbeddedString          *y,
                                                                CpgSelector                *of);

void                   cpg_parser_context_add_integrator_property (CpgParserContext        *context,
                                                                   CpgEmbeddedString       *name,
                                                                   CpgEmbeddedString       *value);

CpgEmbeddedString     *cpg_parser_context_push_string           (CpgParserContext *context);
CpgEmbeddedString     *cpg_parser_context_peek_string           (CpgParserContext *context);
CpgEmbeddedString     *cpg_parser_context_pop_string            (CpgParserContext *context);

gboolean               cpg_parser_context_pop_equation_depth    (CpgParserContext *context);
void                   cpg_parser_context_push_equation_depth   (CpgParserContext *context);
void                   cpg_parser_context_push_equation         (CpgParserContext *context);

void                   cpg_parser_context_delete_selector       (CpgParserContext *context,
                                                                 CpgSelectorType   type,
                                                                 CpgSelector      *selector);

void                   cpg_parser_context_debug_selector        (CpgParserContext *context,
                                                                 CpgSelectorType   type,
                                                                 CpgSelector      *selector);

void                   cpg_parser_context_debug_string          (CpgParserContext  *context,
                                                                 CpgEmbeddedString *s);

G_END_DECLS

#endif /* __CPG_PARSER_CONTEXT_H__ */
