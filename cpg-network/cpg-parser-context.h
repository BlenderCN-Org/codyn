/*
 * cpg-parser-context.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

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
void                   cpg_parser_context_set_embedded         (CpgParserContext           *context,
                                                                CpgEmbeddedContext         *embedded);

GFile                 *cpg_parser_context_get_file             (CpgParserContext           *context);

void                   cpg_parser_context_set_line             (CpgParserContext           *context,
                                                                gchar const                *line,
                                                                gint                        lineno);

gchar const           *cpg_parser_context_get_line             (CpgParserContext           *context,
                                                                gint                       *lineno);

gchar const           *cpg_parser_context_get_line_at          (CpgParserContext           *context,
                                                                gint                        lineno);

void                   cpg_parser_context_set_column           (CpgParserContext           *context,
                                                                gint                        start,
                                                                gint                        end);

void                   cpg_parser_context_get_column           (CpgParserContext           *context,
                                                                gint                       *start,
                                                                gint                       *end);

void                   cpg_parser_context_get_error_location   (CpgParserContext           *context,
                                                                gint                       *lstart,
                                                                gint                       *lend,
                                                                gint                       *cstart,
                                                                gint                       *cend);

gchar                 *cpg_parser_context_get_error_lines      (CpgParserContext           *context);

void                   cpg_parser_context_get_last_selector_item_line (CpgParserContext *context,
                                                                       gint             *line_start,
                                                                       gint             *line_end);

void                   cpg_parser_context_get_last_selector_item_column (CpgParserContext *context,
                                                                         gint             *start,
                                                                         gint             *end);

void                   cpg_parser_context_set_token            (CpgParserContext           *context,
                                                                gchar const                *token);

gchar const           *cpg_parser_context_get_token            (CpgParserContext           *context);

gboolean               cpg_parser_context_parse                (CpgParserContext           *context,
                                                                gboolean                    push_network,
                                                                GError                    **error);

void                   cpg_parser_context_begin_selector_item  (CpgParserContext           *context);

void                   cpg_parser_context_add_property         (CpgParserContext           *context,
                                                                CpgEmbeddedString          *name,
                                                                CpgEmbeddedString          *count_name,
                                                                CpgEmbeddedString          *unexpanded_name,
                                                                CpgEmbeddedString          *expression,
                                                                CpgPropertyFlags            add_flags,
                                                                CpgPropertyFlags            remove_flags,
                                                                GSList                     *attributes,
                                                                gboolean                    assign_optional,
                                                                CpgEmbeddedString          *constraint);

void                   cpg_parser_context_add_action           (CpgParserContext           *context,
                                                                CpgEmbeddedString          *target,
                                                                CpgEmbeddedString          *expression,
                                                                GSList                     *attributes);

void                   cpg_parser_context_add_function         (CpgParserContext           *context,
                                                                CpgEmbeddedString          *name,
                                                                CpgEmbeddedString          *expression,
                                                                GSList                     *arguments,
                                                                GSList                     *attributes);

void                   cpg_parser_context_add_polynomial       (CpgParserContext           *context,
                                                                CpgEmbeddedString          *name,
                                                                GSList                     *pieces,
                                                                GSList                     *attributes);

void                   cpg_parser_context_add_interface        (CpgParserContext           *context,
                                                                CpgEmbeddedString          *name,
                                                                CpgEmbeddedString          *child_name,
                                                                CpgEmbeddedString          *property_name,
                                                                gboolean                    is_optional,
                                                                GSList                     *attributes);

void                   cpg_parser_context_import               (CpgParserContext           *context,
                                                                CpgEmbeddedString          *id,
                                                                CpgEmbeddedString          *path,
                                                                GSList                     *attributes);

void                   cpg_parser_context_set_error            (CpgParserContext           *context,
                                                                gchar const                *message);

GError                *cpg_parser_context_get_error            (CpgParserContext           *context);

void                   cpg_parser_context_push_selection       (CpgParserContext           *context,
                                                                CpgSelector                *selector,
                                                                CpgSelectorType             type,
                                                                GSList                     *templates,
                                                                GSList                     *attributes);

void                   cpg_parser_context_push_objects         (CpgParserContext           *context,
                                                                GSList                     *objects,
                                                                GSList                     *attributes);

void                   cpg_parser_context_push_object          (CpgParserContext           *context,
                                                                CpgEmbeddedString          *id,
                                                                GSList                     *templates,
                                                                GSList                     *attributes);

void                   cpg_parser_context_push_state           (CpgParserContext           *context,
                                                                CpgEmbeddedString          *id,
                                                                GSList                     *templates,
                                                                GSList                     *attributes);

void                   cpg_parser_context_push_group           (CpgParserContext           *context,
                                                                CpgEmbeddedString          *id,
                                                                GSList                     *templates,
                                                                GSList                     *attributes);

void                   cpg_parser_context_push_link            (CpgParserContext           *context,
                                                                CpgEmbeddedString          *id,
                                                                GSList                     *templates,
                                                                GSList                     *attributes,
                                                                GSList                     *fromto);

void                   cpg_parser_context_push_network         (CpgParserContext           *context,
                                                                GSList                     *attributes);
void                   cpg_parser_context_push_templates       (CpgParserContext           *context,
                                                                GSList                     *attributes);
void                   cpg_parser_context_push_integrator      (CpgParserContext           *context,
                                                                GSList                     *attributes);

void                   cpg_parser_context_push_input_file      (CpgParserContext           *context,
                                                                CpgEmbeddedString          *id,
                                                                CpgEmbeddedString          *path,
                                                                GSList                     *attributes);

void                   cpg_parser_context_set_input_file_setting (CpgParserContext         *context,
                                                                  CpgEmbeddedString        *name,
                                                                  CpgEmbeddedString        *value);

void                   cpg_parser_context_set_proxy            (CpgParserContext           *context,
                                                                GSList                     *objects);

GSList                *cpg_parser_context_pop                  (CpgParserContext           *context);
GSList const          *cpg_parser_context_current_selections   (CpgParserContext           *context);
GSList const          *cpg_parser_context_previous_selections  (CpgParserContext           *context);

void                   cpg_parser_context_push_selector_identifier (CpgParserContext           *context,
                                                                    CpgEmbeddedString          *identifier);
void                   cpg_parser_context_push_selector_regex  (CpgParserContext           *context,
                                                                CpgEmbeddedString          *regex);
void                   cpg_parser_context_push_selector_pseudo (CpgParserContext           *context,
                                                                CpgSelectorPseudoType       type,
                                                                GSList                     *arguments);

CpgSelector           *cpg_parser_context_pop_selector         (CpgParserContext           *context);
CpgSelector           *cpg_parser_context_peek_selector        (CpgParserContext           *context);
void                   cpg_parser_context_push_selector        (CpgParserContext           *context);

gssize                 cpg_parser_context_read                 (CpgParserContext           *context,
                                                                gchar                      *buffer,
                                                                gsize                       max_size);

gpointer               cpg_parser_context_get_scanner          (CpgParserContext           *context);

void                   cpg_parser_context_define               (CpgParserContext           *context,
                                                                CpgEmbeddedString          *name,
                                                                CpgEmbeddedString          *value,
                                                                gboolean                    optional,
                                                                CpgEmbeddedString          *count_name,
                                                                CpgEmbeddedString          *unexpanded_name);

void                   cpg_parser_context_remove               (CpgParserContext           *context,
                                                                GSList                     *selectors);

void                   cpg_parser_context_set_integrator       (CpgParserContext           *context,
                                                                CpgEmbeddedString          *value);

void                   cpg_parser_context_push_input_from_path (CpgParserContext           *context,
                                                                CpgEmbeddedString          *filename,
                                                                GSList                     *attributes);

void                   cpg_parser_context_push_input_from_string (CpgParserContext         *context,
                                                                  gchar const              *s,
                                                                  GSList                   *attributes);

void                   cpg_parser_context_push_input           (CpgParserContext           *context,
                                                                GFile                      *file,
                                                                GInputStream               *stream,
                                                                GSList                     *attributes);

void                   cpg_parser_context_include              (CpgParserContext           *context,
                                                                CpgEmbeddedString          *filename,
                                                                GSList                     *attributes);

void                   cpg_parser_context_pop_input            (CpgParserContext           *context);

gint                   cpg_parser_context_steal_start_token    (CpgParserContext           *context);
gint                   cpg_parser_context_get_start_token      (CpgParserContext           *context);

void                   cpg_parser_context_set_start_token      (CpgParserContext           *context,
                                                                gint                        token);

void                   cpg_parser_context_push_annotation      (CpgParserContext           *context,
                                                                CpgEmbeddedString          *annotation);

void                   cpg_parser_context_push_scope           (CpgParserContext           *context,
                                                                GSList                     *attributes);

void                   cpg_parser_context_push_define          (CpgParserContext           *context,
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
                                                                CpgSelector                *of,
                                                                gboolean                    cartesian);

void                   cpg_parser_context_add_integrator_property (CpgParserContext        *context,
                                                                   CpgEmbeddedString       *name,
                                                                   CpgEmbeddedString       *value);

CpgEmbeddedString     *cpg_parser_context_push_string           (CpgParserContext *context);
CpgEmbeddedString     *cpg_parser_context_peek_string           (CpgParserContext *context);
CpgEmbeddedString     *cpg_parser_context_pop_string            (CpgParserContext *context);

gboolean               cpg_parser_context_pop_equation_depth    (CpgParserContext *context);
void                   cpg_parser_context_push_equation_depth   (CpgParserContext *context);
gint                   cpg_parser_context_peek_equation_depth   (CpgParserContext *context);
void                   cpg_parser_context_push_equation         (CpgParserContext *context);

void                   cpg_parser_context_delete_selector       (CpgParserContext *context,
                                                                 CpgSelector      *selector);

void                   cpg_parser_context_debug_selector        (CpgParserContext *context,
                                                                 CpgSelector      *selector);

void                   cpg_parser_context_debug_string          (CpgParserContext  *context,
                                                                 CpgEmbeddedString *s);

void                   cpg_parser_context_debug_context         (CpgParserContext  *context);

void                   cpg_parser_context_apply_template        (CpgParserContext  *context,
                                                                 CpgSelector       *templates,
                                                                 CpgSelector       *targets);

void                   cpg_parser_context_unapply_template      (CpgParserContext  *context,
                                                                 CpgSelector       *templates,
                                                                 CpgSelector       *targets);

void                   cpg_parser_context_set_event_handler     (CpgParserContext   *context,
                                                                 CpgParserCodeEvent  event,
                                                                 GSList             *attributes);

void                   cpg_parser_context_unset_event_handler  (CpgParserContext  *context);

void                   cpg_parser_context_remove_record         (CpgParserContext  *context,
                                                                 gint               len,
                                                                 gint               offset);

gboolean               cpg_parser_context_get_first_eof         (CpgParserContext  *context);
void                   cpg_parser_context_set_first_eof         (CpgParserContext  *context,
                                                                 gboolean           firsteof);


G_END_DECLS

#endif /* __CPG_PARSER_CONTEXT_H__ */
