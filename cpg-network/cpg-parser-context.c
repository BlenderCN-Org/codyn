/*
 * cpg-parser-context.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-parser-context.h"
#include "cpg-network-parser-utils.h"
#include "cpg-integrators.h"
#include "cpg-parser.h"
#include "cpg-annotatable.h"
#include "cpg-embedded-context.h"
#include "cpg-selection.h"
#include "cpg-expansion.h"
#include "cpg-layoutable.h"
#include "cpg-input-file.h"
#include "cpg-statement.h"
#include "cpg-taggable.h"

#include <math.h>

#include <string.h>

#define CPG_PARSER_CONTEXT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_PARSER_CONTEXT, CpgParserContextPrivate))

#define embedded_string_expand(ret,s,parser)					\
do										\
{										\
	GError *__err = NULL;							\
	ret = cpg_embedded_string_expand (s, parser->priv->embedded, &__err);	\
										\
	if (!ret)								\
	{									\
		parser_failed_error (parser, __err);				\
		return;								\
	}									\
}										\
while (0);

#define embedded_string_expand_val(ret,s,parser,retval)				\
do										\
{										\
	GError *__err = NULL;							\
	ret = cpg_embedded_string_expand (s, parser->priv->embedded, &__err);	\
										\
	if (!ret)								\
	{									\
		parser_failed_error (parser, __err);				\
		return retval;							\
	}									\
}										\
while (0);

#define embedded_string_expand_multiple(ret,s,parser)				\
do										\
{										\
	GError *__err = NULL;							\
	ret = cpg_embedded_string_expand_multiple (s, parser->priv->embedded, &__err);	\
										\
	if (!ret && __err)							\
	{									\
		parser_failed_error (parser, __err);				\
		return;								\
	}									\
}										\
while (0);

#define embedded_string_expand_multiple_val(ret,s,parser,retval)		\
do										\
{										\
	GError *__err = NULL;							\
	ret = cpg_embedded_string_expand_multiple (s, parser->priv->embedded, &__err);	\
										\
	if (!ret && __err)							\
	{									\
		parser_failed_error (parser, __err);				\
		return retval;							\
	}									\
}										\
while (0);

void cpg_parser_lex_destroy (gpointer scanner);
void cpg_parser_lex_init_extra (gpointer context, gpointer *scanner);
void cpg_parser_tokens_push_input (gpointer scanner);

int cpg_parser_parse (gpointer context);

#define CURRENT_INPUT(ctx) ((ctx)->priv->inputs ? ((InputItem *)((ctx)->priv->inputs->data)) : NULL)

typedef struct
{
	GFile *file;
	GInputStream *stream;
	gint lineno;
	gint cstart;
	gint cend;
	gchar *token;

	GHashTable *lines;
} InputItem;

typedef struct
{
	GSList *objects;
} Context;

struct _CpgParserContextPrivate
{
	CpgNetwork *network;
	GSList *inputs;
	gpointer scanner;
	gint start_token;

	gint previous_annotation;
	GString *annotation;

	/* Stack of Context */
	GSList *context_stack;
	GSList *selectors;

	Context *is_template;
	CpgEmbeddedContext *embedded;

	GSList *strings;
	GSList *equations;

	GError *error;
	gboolean error_occurred;
	CpgStatement *error_statement;

	CpgLayout *layout;

	gboolean in_when_applied;
	gboolean when_applied;
	GSList *when_applied_attributes;
	GString *when_applied_text;
};

enum
{
	CONTEXT_PUSHED,
	CONTEXT_POPPED,
	SELECTOR_ITEM_PUSHED,
	NUM_SIGNALS
};

static guint signals[NUM_SIGNALS];

G_DEFINE_TYPE (CpgParserContext, cpg_parser_context, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NETWORK,
	PROP_FILE,
	PROP_STREAM
};

#define CONTEXT(x) ((Context *)x)
#define CURRENT_CONTEXT(context) CONTEXT(context->priv->context_stack->data)

static void
input_item_free (InputItem *self)
{
	if (self->file)
	{
		g_object_unref (self->file);
	}

	if (self->stream)
	{
		g_input_stream_close (self->stream, NULL, NULL);
		g_object_unref (self->stream);
	}

	g_free (self->token);

	g_hash_table_destroy (self->lines);

	g_slice_free (InputItem, self);
}

static InputItem *
input_item_new (GFile         *file,
                GInputStream  *stream,
                GError       **error)
{
	InputItem *ret;

	ret = g_slice_new0 (InputItem);

	ret->file = file ? g_file_dup (file) : NULL;

	if (stream)
	{
		ret->stream = g_object_ref (stream);
	}
	else if (ret->file)
	{
		GFileInputStream *is;

		is = g_file_read (ret->file,
		                  NULL,
		                  error);

		if (is)
		{
			ret->stream = G_INPUT_STREAM (is);
		}
		else
		{
			input_item_free (ret);
			ret = NULL;
		}
	}

	ret->lines = g_hash_table_new_full (g_direct_hash,
	                                    g_direct_equal,
	                                    NULL,
	                                    (GDestroyNotify)g_free);

	return ret;
}

static Context *
context_new (GSList *objects)
{
	Context *ret;

	ret = g_slice_new0 (Context);

	ret->objects = g_slist_copy (objects);

	return ret;
}

static void
context_free (Context *context)
{
	g_slist_foreach (context->objects, (GFunc)g_object_unref, NULL);
	g_slist_free (context->objects);

	g_slice_free (Context, context);
}

static void
cpg_parser_context_finalize (GObject *object)
{
	CpgParserContext *self;

	self = CPG_PARSER_CONTEXT (object);

	while (self->priv->context_stack)
	{
		cpg_parser_context_pop (self);
	}

	if (self->priv->network)
	{
		g_object_unref (self->priv->network);
	}

	if (self->priv->error_statement)
	{
		g_object_unref (self->priv->error_statement);
	}

	g_slist_foreach (self->priv->inputs, (GFunc)input_item_free, NULL);
	g_slist_free (self->priv->inputs);

	cpg_parser_lex_destroy (self->priv->scanner);
	g_string_free (self->priv->annotation, TRUE);

	G_OBJECT_CLASS (cpg_parser_context_parent_class)->finalize (object);
}

static void
cpg_parser_context_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
	CpgParserContext *self = CPG_PARSER_CONTEXT (object);

	switch (prop_id)
	{
		case PROP_NETWORK:
			self->priv->network = g_value_dup_object (value);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_parser_context_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
	CpgParserContext *self = CPG_PARSER_CONTEXT (object);

	switch (prop_id)
	{
		case PROP_NETWORK:
			g_value_set_object (value, self->priv->network);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_parser_context_constructed (GObject *object)
{
	CpgParserContext *self;

	self = CPG_PARSER_CONTEXT (object);

	cpg_parser_context_push_network (self, NULL);

	cpg_parser_lex_init_extra (self, &(self->priv->scanner));
}

static void
cpg_parser_context_class_init (CpgParserContextClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_parser_context_finalize;

	object_class->get_property = cpg_parser_context_get_property;
	object_class->set_property = cpg_parser_context_set_property;
	object_class->constructed = cpg_parser_context_constructed;

	g_type_class_add_private (object_class, sizeof(CpgParserContextPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_NETWORK,
	                                 g_param_spec_object ("network",
	                                                      "Network",
	                                                      "Network",
	                                                      CPG_TYPE_NETWORK,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	signals[CONTEXT_PUSHED] =
		g_signal_new ("context-pushed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE,
		              0);

	signals[CONTEXT_POPPED] =
		g_signal_new ("context-popped",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE,
		              0);

	signals[SELECTOR_ITEM_PUSHED] =
		g_signal_new ("selector-item-pushed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_SELECTOR);
}

static void
cpg_parser_context_init (CpgParserContext *self)
{
	self->priv = CPG_PARSER_CONTEXT_GET_PRIVATE (self);

	self->priv->start_token = T_START_DOCUMENT;
	self->priv->annotation = g_string_new ("");
	self->priv->embedded = cpg_embedded_context_new ();
}

CpgParserContext *
cpg_parser_context_new (CpgNetwork *network)
{
	return g_object_new (CPG_TYPE_PARSER_CONTEXT,
	                     "network", network,
	                     NULL);
}

static void
statement_start (CpgParserContext *context,
                 gpointer          obj)
{
	CpgStatement *st;
	InputItem *inp;

	if (!CPG_IS_STATEMENT (obj))
	{
		return;
	}

	st = CPG_STATEMENT (obj);
	inp = CURRENT_INPUT (context);

	cpg_statement_set_line (st, inp->lineno, inp->lineno);
	cpg_statement_set_column (st, inp->cstart, inp->cend);
}

static void
statement_end (CpgParserContext *context,
               gpointer          obj)
{
	CpgStatement *st;
	InputItem *inp;
	gint lstart;
	gint cstart;

	if (!CPG_IS_STATEMENT (obj))
	{
		return;
	}

	st = CPG_STATEMENT (obj);
	inp = CURRENT_INPUT (context);

	cpg_statement_get_line (st, &lstart, NULL);
	cpg_statement_get_column (st, &cstart, NULL);

	cpg_statement_set_line (st, lstart, inp->lineno);
	cpg_statement_set_column (st, cstart, inp->cend);
}

static gboolean
parser_failed_error_at (CpgParserContext *context,
                        CpgStatement     *statement,
                        GError           *error)
{
	if (context->priv->error_statement)
	{
		g_object_unref (context->priv->error_statement);
		context->priv->error_statement = NULL;
	}

	if (statement)
	{
		context->priv->error_statement = g_object_ref (statement);
	}

	context->priv->error = error;
	context->priv->in_when_applied = FALSE;

	return FALSE;
}

static gboolean
parser_failed_error (CpgParserContext *context,
                     GError           *error)
{
	return parser_failed_error_at (context, NULL, error);
}

static gboolean
parser_failed (CpgParserContext *context,
               gint              code,
               gchar const      *format,
               ...)
{
	if (context->priv->error == NULL)
	{
		va_list ap;
		GError *error;

		va_start (ap, format);

		error = g_error_new_valist (CPG_NETWORK_LOAD_ERROR,
		                            code,
		                            format,
		                            ap);

		parser_failed_error (context,
		                     error);

		va_end (ap);
	}

	return FALSE;
}

static gchar *
steal_annotation (CpgParserContext *context)
{
	gchar *ret;

	if (context->priv->annotation->len == 0)
	{
		return NULL;
	}

	ret = g_string_free (context->priv->annotation, FALSE);
	context->priv->annotation = g_string_new ("");

	return ret;
}

static GSList *
find_attributes (GSList *attributes,
                gchar const *name)
{
	GSList *ret = NULL;

	while (attributes)
	{
		if (g_strcmp0 (name, cpg_attribute_get_id (attributes->data)) == 0)
		{
			ret = g_slist_prepend (ret, attributes->data);
		}

		attributes = g_slist_next (attributes);
	}

	return g_slist_reverse (ret);
}

static CpgAttribute *
find_attribute (GSList *attributes,
                gchar const *name)
{
	GSList *all;
	CpgAttribute *ret = NULL;

	all = find_attributes (attributes, name);

	if (all)
	{
		ret = all->data;
		g_slist_free (all);
	}

	return ret;
}

static void
set_taggable (CpgParserContext *context,
              gpointer          obj,
              GSList           *attributes)
{
	GSList *attrs;
	GSList *attr;

	if (!CPG_IS_TAGGABLE (obj))
	{
		return;
	}

	attrs = find_attributes (attributes, "tag");

	for (attr = attrs; attr; attr = g_slist_next (attr))
	{
		CpgAttribute *a;
		gint i;

		a = attr->data;

		for (i = 0; i < cpg_attribute_num_arguments (a); ++i)
		{
			gpointer o;
			GSList *ex;
			GSList *item;

			o = cpg_attribute_get_argument (a, i);

			if (!CPG_IS_EMBEDDED_STRING (o))
			{
				continue;
			}

			ex = cpg_embedded_string_expand_multiple (o,
			                                          context->priv->embedded,
			                                          NULL);

			for (item = ex; item; item = g_slist_next (item))
			{
				cpg_taggable_add_tag (CPG_TAGGABLE (obj),
				                      cpg_expansion_get (item->data, 0),
				                      NULL);
			}

			g_slist_foreach (ex, (GFunc)g_object_unref, NULL);
			g_slist_free (ex);
		}
	}

	g_slist_free (attrs);
}

static GSList *
copy_selections (GSList   *selections,
                 gboolean  copy_defines)
{
	GSList *ret = NULL;

	while (selections)
	{
		ret = g_slist_prepend (ret, cpg_selection_copy_defines (selections->data,
		                                                        copy_defines));
		selections = g_slist_next (selections);
	}

	return g_slist_reverse (ret);
}

static gboolean
test_string_empty (gchar const *s)
{
	gdouble num;
	gchar *endptr;

	num = g_ascii_strtod (s, &endptr);

	if (!*endptr)
	{
		/* number == 0 */
		return num == 0;
	}
	else
	{
		/* Empty string */
		return !*s;
	}
}

static GSList *
each_selections_attr (CpgParserContext *context,
                      GSList           *selections,
                      GSList           *attributes,
                      CpgSelectorType   type,
                      gboolean         *selected,
                      gboolean         *couldselect,
                      gboolean          copy_defines,
                      gpointer          obj,
                      gboolean          isempty,
                      GSList           *ret)
{
	if (CPG_IS_EMBEDDED_STRING (obj))
	{
		GSList *item;

		/* Expand original selections */
		for (item = selections; item; item = g_slist_next (item))
		{
			GSList *exps;
			GSList *expp;

			cpg_embedded_context_save (context->priv->embedded);
			cpg_embedded_context_set_selection (context->priv->embedded,
			                                    item->data);

			embedded_string_expand_multiple_val (exps, obj, context, NULL);

			for (expp = exps; expp; expp = g_slist_next (expp))
			{
				CpgSelection *sel;
				GSList *expansions;

				if (isempty && test_string_empty (cpg_expansion_get (expp->data, 0)))
				{
					continue;
				}

				expansions = g_slist_copy (cpg_selection_get_expansions (item->data));
				expansions = g_slist_prepend (expansions,
				                              expp->data);

				sel = cpg_selection_new_defines (cpg_selection_get_object (item->data),
				                                 expansions,
				                                 cpg_selection_get_defines (item->data),
				                                 copy_defines);

				ret = g_slist_prepend (ret, sel);

				g_slist_free (expansions);
			}

			cpg_embedded_context_restore (context->priv->embedded);

			g_slist_foreach (exps, (GFunc)g_object_unref, NULL);
			g_slist_free (exps);
		}
	}
	else if (CPG_IS_SELECTOR (obj))
	{
		GSList *item;

		if (couldselect)
		{
			*couldselect = TRUE;
		}

		for (item = selections; item; item = g_slist_next (item))
		{
			GSList *sels;

			cpg_embedded_context_save (context->priv->embedded);

			cpg_embedded_context_set_selection (context->priv->embedded,
			                                    item->data);

			sels = cpg_selector_select (obj,
			                            cpg_selection_get_object (item->data),
			                            type,
			                            context->priv->embedded);

			if (selected && sels != NULL)
			{
				*selected = TRUE;
			}

			while (sels)
			{
				cpg_embedded_context_save (context->priv->embedded);

				cpg_embedded_context_add_expansions (context->priv->embedded,
				                                     cpg_selection_get_expansions (sels->data));

				ret = g_slist_prepend (ret,
				                       cpg_selection_new_defines (cpg_selection_get_object (item->data),
				                                                  cpg_embedded_context_get_expansions (context->priv->embedded),
				                                                  cpg_embedded_context_get_defines (context->priv->embedded),
				                                                  copy_defines));

				cpg_embedded_context_restore (context->priv->embedded);

				if (isempty)
				{
					g_slist_foreach (sels, (GFunc)g_object_unref, NULL);
					g_slist_free (sels);

					break;
				}

				g_object_unref (sels->data);
				sels = g_slist_delete_link (sels, sels);
			}

			cpg_embedded_context_restore (context->priv->embedded);
		}
	}

	return ret;
}

static GSList *
each_selections (CpgParserContext *context,
                 GSList           *selections,
                 GSList           *attributes,
                 CpgSelectorType   type,
                 gboolean         *selected,
                 gboolean         *couldselect,
                 gboolean          copy_defines)
{
	gint i;
	GSList *ret = NULL;
	CpgAttribute *attrif;
	CpgAttribute *attreach;

	attrif = find_attribute (attributes, "if");
	attreach = find_attribute (attributes, "each");

	if (selected)
	{
		*selected = FALSE;
	}

	if (couldselect)
	{
		*couldselect = FALSE;
	}

	if (!attrif && !attreach)
	{
		return copy_selections (selections, copy_defines);
	}

	if (attreach)
	{
		for (i = 0; i < cpg_attribute_num_arguments (attreach); ++i)
		{
			gpointer obj = cpg_attribute_get_argument (attreach, i);

			ret = each_selections_attr (context,
			                            selections,
			                            attributes,
			                            type,
			                            selected,
			                            couldselect,
			                            copy_defines,
			                            obj,
			                            FALSE,
			                            ret);
		}
	}

	if (attrif)
	{
		for (i = 0; i < cpg_attribute_num_arguments (attrif); ++i)
		{
			gpointer obj = cpg_attribute_get_argument (attrif, i);

			ret = each_selections_attr (context,
			                            selections,
			                            attributes,
			                            type,
			                            selected,
			                            couldselect,
			                            copy_defines,
			                            obj,
			                            TRUE,
			                            ret);
		}
	}

	return g_slist_reverse (ret);
}

static CpgSelectorType
selector_type_from_gtype (GType gtype)
{
	if (g_type_is_a (gtype, CPG_TYPE_GROUP) ||
	    gtype == CPG_TYPE_GROUP)
	{
		return CPG_SELECTOR_TYPE_GROUP;
	}
	else if (g_type_is_a (gtype, CPG_TYPE_LINK) ||
	         gtype == CPG_TYPE_LINK)
	{
		return CPG_SELECTOR_TYPE_LINK;
	}
	else if (gtype == CPG_TYPE_OBJECT)
	{
		return CPG_SELECTOR_TYPE_STATE;
	}
	else
	{
		return CPG_SELECTOR_TYPE_OBJECT;
	}
}

void
cpg_parser_context_add_property (CpgParserContext  *context,
                                 CpgEmbeddedString *name,
                                 CpgEmbeddedString *expression,
                                 CpgPropertyFlags   add_flags,
                                 CpgPropertyFlags   remove_flags,
                                 GSList            *attributes,
                                 gboolean           assign_optional)
{
	Context *ctx;
	GSList *item;
	GSList *objects;
	gchar *annotation;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	annotation = steal_annotation (context);

	objects = each_selections (context,
	                           ctx->objects,
	                           attributes,
	                           CPG_SELECTOR_TYPE_ANY,
	                           NULL,
	                           NULL,
	                           FALSE);

	for (item = objects; item; item = g_slist_next (item))
	{
		CpgObject *obj;
		GSList *exps;
		GSList *iteme;

		obj = cpg_selection_get_object (item->data);

		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);

		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    item->data);

		embedded_string_expand_multiple (exps, name, context);

		for (iteme = exps; iteme; iteme = g_slist_next (iteme))
		{
			GError *error = NULL;
			CpgProperty *property;
			gchar const *exname;
			gchar *exexpression = NULL;
			CpgPropertyFlags flags = CPG_PROPERTY_FLAG_NONE;

			cpg_embedded_context_save (context->priv->embedded);
			cpg_embedded_context_add_expansion (context->priv->embedded,
			                                    iteme->data);

			exname = cpg_expansion_get (iteme->data, 0);

			if (expression)
			{
				gchar const *expanded;

				embedded_string_expand (expanded, expression, context);
				exexpression = g_strdup (expanded);
			}

			property = cpg_object_get_property (obj, exname);

			if (property && assign_optional)
			{
				cpg_embedded_context_restore (context->priv->embedded);
				continue;
			}

			if (property)
			{
				flags = cpg_property_get_flags (property);

				if (!expression)
				{
					CpgExpression *expr;

					expr = cpg_property_get_expression (property);
					exexpression = g_strdup (cpg_expression_get_as_string (expr));
				}
			}

			if (!exexpression)
			{
				exexpression = g_strdup ("");
			}

			flags &= ~remove_flags;
			flags |= add_flags;

			if (!cpg_object_add_property (obj,
			                              cpg_property_new (exname, exexpression, flags),
			                              &error))
			{
				cpg_embedded_context_restore (context->priv->embedded);
				g_free (exexpression);

				parser_failed_error (context, error);
				break;
			}

			g_free (exexpression);

			property = cpg_object_get_property (obj, exname);
			cpg_modifiable_set_modified (CPG_MODIFIABLE (property), FALSE);

			if (annotation)
			{
				cpg_annotatable_set_annotation (CPG_ANNOTATABLE (property),
				                                annotation);
			}

			set_taggable (context, property, attributes);

			cpg_embedded_context_restore (context->priv->embedded);
		}

		g_slist_foreach (exps, (GFunc)g_object_unref, NULL);
		g_slist_free (exps);

		cpg_embedded_context_restore (context->priv->embedded);
	}

	g_slist_foreach (objects, (GFunc)g_object_unref, NULL);
	g_slist_free (objects);

	g_free (annotation);
	g_object_unref (name);

	if (expression)
	{
		g_object_unref (expression);
	}
}

void
cpg_parser_context_add_action (CpgParserContext  *context,
                               CpgEmbeddedString *target,
                               CpgEmbeddedString *expression,
                               GSList            *attributes)
{
	Context *ctx;
	GSList *item;
	gchar *annotation;
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (target != NULL);
	g_return_if_fail (expression != NULL);

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);
	annotation = steal_annotation (context);

	objects = each_selections (context,
	                           ctx->objects,
	                           attributes,
	                           CPG_SELECTOR_TYPE_ANY,
	                           NULL,
	                           NULL,
	                           FALSE);

	for (item = objects; item; item = g_slist_next (item))
	{
		GSList *exps;
		GSList *iteme;

		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);

		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    item->data);

		embedded_string_expand_multiple (exps, target, context);

		for (iteme = exps; iteme; iteme = g_slist_next (iteme))
		{
			gchar const *extarget;
			gchar const *exexpression;
			CpgLinkAction *action;

			cpg_embedded_context_save (context->priv->embedded);
			cpg_embedded_context_add_expansion (context->priv->embedded,
			                                    iteme->data);

			extarget = cpg_expansion_get (iteme->data, 0);
			embedded_string_expand (exexpression, expression, context);

			action = cpg_link_action_new (extarget,
			                              cpg_expression_new (exexpression));

			cpg_link_add_action (CPG_LINK (cpg_selection_get_object (item->data)),
			                     action);

			if (annotation)
			{
				cpg_annotatable_set_annotation (CPG_ANNOTATABLE (action),
				                                annotation);
			}

			set_taggable (context, action, attributes);

			cpg_embedded_context_restore (context->priv->embedded);
		}

		cpg_embedded_context_restore (context->priv->embedded);

		g_slist_foreach (exps, (GFunc)g_object_unref, NULL);
		g_slist_free (exps);
	}

	g_slist_foreach (objects, (GFunc)g_object_unref, NULL);
	g_slist_free (objects);

	g_object_unref (target);
	g_object_unref (expression);

	g_free (annotation);
}

void
cpg_parser_context_add_function (CpgParserContext  *context,
                                 CpgEmbeddedString *name,
                                 CpgEmbeddedString *expression,
                                 GSList            *arguments,
                                 GSList            *attributes)
{
	GSList *item;
	Context *ctx;
	gchar *annotation;
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (expression != NULL);

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);
	annotation = steal_annotation (context);

	objects = each_selections (context,
	                           ctx->objects,
	                           attributes,
	                           CPG_SELECTOR_TYPE_ANY,
	                           NULL,
	                           NULL,
	                           FALSE);

	for (item = objects; item; item = g_slist_next (item))
	{
		gchar const *exname;
		gchar const *exexpression;
		CpgFunction *function;
		GSList *arg;
		CpgGroup *parent;
		CpgObject *child;

		parent = cpg_selection_get_object (item->data);

		cpg_embedded_context_save_defines (context->priv->embedded, FALSE);
		cpg_embedded_context_set_selection (context->priv->embedded, item->data);

		embedded_string_expand (exname, name, context);
		embedded_string_expand (exexpression, expression, context);

		child = cpg_group_get_child (parent, exname);

		if (child && CPG_IS_FUNCTION (child))
		{
			function = CPG_FUNCTION (child);
			cpg_function_set_expression (function,
			                             cpg_expression_new (exexpression));
		}
		else
		{
			function = cpg_function_new (exname, exexpression);
			cpg_group_add (parent, CPG_OBJECT (function), NULL);
		}

		for (arg = arguments; arg; arg = g_slist_next (arg))
		{
			cpg_function_add_argument (function,
			                           cpg_function_argument_copy (arg->data));
		}

		if (annotation)
		{
			cpg_annotatable_set_annotation (CPG_ANNOTATABLE (function),
			                                annotation);
		}

		set_taggable (context, function, attributes);
	}

	g_slist_foreach (objects, (GFunc)g_object_unref, NULL);
	g_slist_free (objects);

	g_free (annotation);

	g_object_unref (name);
	g_object_unref (expression);
}

void
cpg_parser_context_add_polynomial (CpgParserContext  *context,
                                   CpgEmbeddedString *name,
                                   GSList            *pieces,
                                   GSList            *attributes)
{
	gchar *annotation;
	Context *ctx;
	GSList *item;
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	annotation = steal_annotation (context);

	objects = each_selections (context,
	                           ctx->objects,
	                           attributes,
	                           CPG_SELECTOR_TYPE_ANY,
	                           NULL,
	                           NULL,
	                           FALSE);

	for (item = objects; item; item = g_slist_next (item))
	{
		gchar const *exname;
		CpgFunctionPolynomial *function;
		CpgGroup *parent;

		parent = cpg_selection_get_object (item->data);

		embedded_string_expand (exname, name, context);
		function = cpg_function_polynomial_new (exname);

		while (pieces)
		{
			cpg_function_polynomial_add (function, pieces->data);
			pieces = g_slist_next (pieces);
		}

		cpg_group_add (parent, CPG_OBJECT (function), NULL);

		if (annotation)
		{
			cpg_annotatable_set_annotation (CPG_ANNOTATABLE (function),
			                                annotation);
		}

		set_taggable (context, function, attributes);
	}

	g_slist_foreach (objects, (GFunc)g_object_unref, NULL);
	g_slist_free (objects);

	g_free (annotation);
	g_object_unref (name);
}

void
cpg_parser_context_add_interface (CpgParserContext  *context,
                                  CpgEmbeddedString *name,
                                  CpgSelector       *target,
                                  GSList            *attributes)
{
	Context *ctx;
	GSList *item;
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (CPG_IS_SELECTOR (target));

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	objects = each_selections (context,
	                           ctx->objects,
	                           attributes,
	                           CPG_SELECTOR_TYPE_ANY,
	                           NULL,
	                           NULL,
	                           FALSE);

	for (item = objects; item; item = g_slist_next (item))
	{
		CpgPropertyInterface *iface;
		CpgGroup *parent;
		GSList *props;
		gboolean ret = TRUE;
		GError *error = NULL;
		GSList *exps;
		GSList *exp;

		parent = CPG_GROUP (cpg_selection_get_object (item->data));

		iface = cpg_group_get_property_interface (parent);

		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);

		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    item->data);

		embedded_string_expand_multiple (exps, name, context);

		for (exp = exps; exp; exp = g_slist_next (exp))
		{
			cpg_embedded_context_save (context->priv->embedded);

			cpg_embedded_context_add_expansion (context->priv->embedded,
			                                    exp->data);

			props = cpg_selector_select (target,
			                             G_OBJECT (parent),
			                             CPG_SELECTOR_TYPE_PROPERTY,
			                             context->priv->embedded);

			cpg_embedded_context_restore (context->priv->embedded);

			if (props)
			{
				gchar const *exname;

				exname = cpg_expansion_get (exp->data, 0);

				if (!cpg_property_interface_add (iface,
				                                 exname,
				                                 cpg_selection_get_object (props->data),
				                                 &error))
				{
					parser_failed_error (context, error);
					ret = FALSE;
					break;
				}
			}

			g_slist_foreach (props, (GFunc)g_object_unref, NULL);
			g_slist_free (props);
		}

		cpg_embedded_context_restore (context->priv->embedded);

		g_slist_foreach (exps, (GFunc)g_object_unref, NULL);
		g_slist_free (exps);

		if (!ret)
		{
			break;
		}
	}

	g_slist_foreach (objects, (GFunc)g_object_unref, NULL);
	g_slist_free (objects);

	g_object_unref (name);
}

void
cpg_parser_context_set_error (CpgParserContext *context,
                              gchar const      *message)
{
	gchar *fname = NULL;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->inputs)
	{
		InputItem *item = context->priv->inputs->data;

		if (item->file)
		{
			fname = g_file_get_basename (item->file);
		}
	}

	parser_failed (context,
	               CPG_NETWORK_LOAD_ERROR_SYNTAX,
	               "Unexpected token `%s' at %s%s%d.%d",
	               CURRENT_INPUT (context)->token,
	               fname ? fname : "",
	               fname ? ":" : "",
	               CURRENT_INPUT (context)->lineno,
	               CURRENT_INPUT (context)->cstart);

	g_free (fname);
}

/**
 * cpg_parser_context_get_error:
 * @context: A #CpgParserContext
 *
 * Get the parse error.
 *
 * Returns: (transfer none): A #GError
 *
 **/
GError *
cpg_parser_context_get_error (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	return context->priv->error;
}

static CpgSelection *
parse_object_single_id (CpgParserContext *context,
                        CpgExpansion     *id,
                        GSList           *temps,
                        CpgSelection     *parent,
                        GType             gtype)
{
	GSList *templates;
	CpgSelection *sel = NULL;
	gchar *missing;
	GSList *item;
	CpgObject *child = NULL;
	GError *error = NULL;
	CpgGroup *parent_group;

	if (id)
	{
		parent_group = CPG_GROUP (cpg_selection_get_object (parent));
	}
	else
	{
		parent_group = CPG_GROUP (cpg_object_get_parent (cpg_selection_get_object (parent)));
	}

	if (!cpg_network_parser_utils_get_templates (context->priv->network,
	                                             parent_group,
	                                             context->priv->is_template ? TRUE : FALSE,
	                                             temps,
	                                             context->priv->embedded,
	                                             &missing,
	                                             &templates))
	{
		parser_failed (context,
		               CPG_NETWORK_LOAD_ERROR_OBJECT,
		               "Could not find template `%s'",
		               missing);

		g_free (missing);

		goto cleanup;
	}

	gtype = cpg_network_parser_utils_type_from_templates (gtype, templates);

	/* Check if the template types can actually be applied to the
	   object type that we are constructing. Only template types
	   which are superclasses of the new object type can be
	   applied */
	for (item = templates; item; item = g_slist_next (item))
	{
		GType template_type = G_TYPE_FROM_INSTANCE (cpg_selection_get_object (item->data));

		if (!g_type_is_a (gtype, template_type))
		{
			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_OBJECT,
			               "Referenced template is of incorrect type %s (need %s)",
			               g_type_name (template_type),
			               g_type_name (gtype));

			goto cleanup;
		}
	}

	if (id)
	{
		child = cpg_group_get_child (parent_group,
		                             cpg_expansion_get (id, 0));
	}
	else
	{
		child = cpg_selection_get_object (parent);
	}

	if (!child)
	{
		/* Just construct a new object with the right type */
		child = g_object_new (gtype, "id", cpg_expansion_get (id, 0), NULL);

		if (!cpg_group_add (parent_group,
		                    child,
		                    &error))
		{
			parser_failed_error (context, error);

			goto cleanup;
		}
	}
	else
	{
		g_object_ref (child);

		if (!g_type_is_a (gtype, G_TYPE_FROM_INSTANCE (child)))
		{
			/* This means the object already existed (this can happen
			   because existing objects created by other templates can be
			   extended) and the type is incorrect */
			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_OBJECT,
			               "Cannot extend type %s with type %s",
			               g_type_name (G_TYPE_FROM_INSTANCE (child)),
			               g_type_name (gtype));

			goto cleanup;
		}
	}

	/* Apply all the templates */
	for (item = templates; item; item = g_slist_next (item))
	{
		if (!cpg_object_apply_template (child,
		                                cpg_selection_get_object (item->data),
		                                &error))
		{
			parser_failed_error (context,
			                     error);

			goto cleanup;
		}
	}

	sel = cpg_selection_new (child,
	                         cpg_embedded_context_get_expansions (context->priv->embedded),
	                         cpg_embedded_context_get_defines (context->priv->embedded));

cleanup:
	g_slist_foreach (templates, (GFunc)g_object_unref, NULL);
	g_slist_free (templates);

	if (child)
	{
		g_object_unref (child);
	}

	return sel;
}

static GSList *
parse_object_single (CpgParserContext  *context,
                     GSList            *ids,
                     GSList            *templates,
                     CpgSelection      *parent,
                     GType              gtype)
{
	GSList *ret = NULL;

	if (!ids)
	{
		CpgSelection *sel;

		sel = parse_object_single_id (context,
		                              NULL,
		                              templates,
		                              parent,
		                              gtype);

		if (sel)
		{
			ret = g_slist_prepend (NULL, sel);
		}

		return ret;
	}

	while (ids)
	{
		CpgSelection *sel;

		cpg_embedded_context_save (context->priv->embedded);

		cpg_embedded_context_add_expansion (context->priv->embedded,
		                                    ids->data);

		sel = parse_object_single_id (context,
		                              ids->data,
		                              templates,
		                              parent,
		                              gtype);

		cpg_embedded_context_restore (context->priv->embedded);

		if (!sel)
		{
			g_slist_free (ret);
			return NULL;
		}

		ret = g_slist_prepend (ret, sel);
		ids = g_slist_next (ids);
	}

	return g_slist_reverse (ret);
}

static gchar *
unique_id (CpgGroup    *parent,
           gchar const *prefix)
{
	gint i = 1;

	if (!cpg_group_get_child (parent, prefix))
	{
		return g_strdup (prefix);
	}

	while (TRUE)
	{
		gchar *id = g_strdup_printf ("%s_%d", prefix, i++);

		if (!cpg_group_get_child (parent, id))
		{
			return id;
		}

		g_free (id);
	}
}

static void
set_proxy (CpgParserContext *context,
           GSList           *objects)
{
	while (objects)
	{
		CpgObject *obj;
		CpgObject *parent;

		obj = cpg_selection_get_object (objects->data);
		objects = g_slist_next (objects);

		if (CPG_IS_LINK (obj))
		{
			continue;
		}

		parent = cpg_object_get_parent (obj);

		if (!parent)
		{
			continue;
		}

		cpg_group_set_proxy (CPG_GROUP (parent), obj);
	}
}

static GSList *
parse_objects (CpgParserContext  *context,
               CpgEmbeddedString *id,
               GSList            *templates,
               GType              gtype,
               GSList            *attributes)
{
	GSList *parents;
	GSList *parent;
	GSList *ret = NULL;
	gboolean selected;
	gboolean couldselect;
	gboolean isproxy;

	parents = each_selections (context,
	                           CURRENT_CONTEXT (context)->objects,
	                           attributes,
	                           selector_type_from_gtype (gtype),
	                           &selected,
	                           &couldselect,
	                           TRUE);

	if (id == NULL && couldselect && !selected)
	{
		return NULL;
	}

	isproxy = find_attribute (attributes, "proxy") != NULL;

	for (parent = parents; parent; parent = g_slist_next (parent))
	{
		GSList *objs;
		GSList *ids;
		CpgEmbeddedString *theid;

		theid = id;

		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);

		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    parent->data);

		if (!selected && theid == NULL)
		{
			gchar *newid;

			newid = unique_id (CPG_GROUP (cpg_selection_get_object (parent->data)),
			                  "object");

			theid = cpg_embedded_string_new_from_string (newid);
			g_free (newid);
		}
		else if (theid != NULL)
		{
			g_object_ref (theid);
		}

		if (theid)
		{
			embedded_string_expand_multiple_val (ids, theid, context, NULL);
		}
		else
		{
			ids = NULL;
		}

		objs = parse_object_single (context,
		                            ids,
		                            templates,
		                            parent->data,
		                            gtype);

		if (isproxy)
		{
			set_proxy (context, objs);
		}

		g_slist_foreach (ids, (GFunc)g_object_unref, NULL);
		g_slist_free (ids);

		cpg_embedded_context_restore (context->priv->embedded);

		if (theid != NULL)
		{
			g_object_unref (theid);
		}

		if (!objs)
		{
			g_slist_free (ret);
			ret = NULL;

			break;
		}

		ret = g_slist_concat (ret, objs);
	}

	g_slist_foreach (parents, (GFunc)g_object_unref, NULL);
	g_slist_free (parents);

	return ret;
}

static GSList *
link_pairs_sparse (CpgParserContext *context,
                   gdouble           probability,
                   gboolean          bidi,
                   gboolean          noself,
                   gboolean          onlyself,
                   GSList           *objs)
{
	/* Connect probabilistically */
	GSList *fromobj;
	GSList *ret = NULL;
	long int p;

	if (onlyself && noself)
	{
		return NULL;
	}

	p = (long int)(RAND_MAX * probability);

	for (fromobj = objs; fromobj; fromobj = g_slist_next (fromobj))
	{
		GSList *toobj;

		if (onlyself)
		{
			if (probability >= 1 || random () <= p)
			{
				ret = g_slist_prepend (ret,
				                       cpg_selection_copy (fromobj->data));

				ret = g_slist_prepend (ret,
				                       cpg_selection_copy (fromobj->data));
			}

			continue;
		}

		if (bidi)
		{
			/* Select diagonal */
			toobj = fromobj;

			if (noself)
			{
				toobj = toobj->next;
			}
		}
		else
		{
			toobj = objs;
		}

		while (toobj)
		{
			if ((probability < 1 && random () > p) || (noself &&
			                      cpg_selection_get_object (toobj->data) ==
			                      cpg_selection_get_object (fromobj->data)))
			{
				toobj = g_slist_next (toobj);
				continue;
			}

			ret = g_slist_prepend (ret,
			                       cpg_selection_copy (fromobj->data));

			ret = g_slist_prepend (ret,
			                       cpg_selection_copy (toobj->data));

			if (bidi)
			{
				ret = g_slist_prepend (ret,
				                       cpg_selection_copy (toobj->data));

				ret = g_slist_prepend (ret,
				                       cpg_selection_copy (fromobj->data));
			}

			toobj = g_slist_next (toobj);
		}
	}

	return g_slist_reverse (ret);
}

static GSList *
link_pairs (CpgParserContext *context,
            CpgExpansion     *id,
            gboolean          autoid,
            CpgSelection     *parent,
            GSList           *attributes,
            CpgSelector      *from,
            CpgSelector      *to,
            gboolean          onlyself)
{
	GSList *fromobjs;
	GSList *fromobj;
	GSList *ret = NULL;
	CpgAttribute *bidi;
	CpgAttribute *noself;
	CpgAttribute *iff;
	gdouble iffprob = 2.0; /* Something bigger than 1 */
	long int p;

	bidi = find_attribute (attributes, "bidirectional");
	iff = find_attribute (attributes, "probability");
	noself = find_attribute (attributes, "no-self");

	if (iff)
	{
		CpgEmbeddedString *s;
		GObject *obj;

		obj = cpg_attribute_get_argument (iff, 0);

		if (CPG_IS_EMBEDDED_STRING (obj))
		{
			gchar const *ex;
			s = CPG_EMBEDDED_STRING (obj);

			embedded_string_expand_val (ex, s, context, NULL);

			iffprob = g_ascii_strtod (ex, NULL);
			p = (long int)(RAND_MAX * iffprob);
		}
		else
		{
			iff = NULL;
		}
	}

	cpg_embedded_context_save (context->priv->embedded);

	if (!autoid)
	{
		cpg_embedded_context_add_expansion (context->priv->embedded, id);
	}

	fromobjs = cpg_selector_select (from,
	                                cpg_selection_get_object (parent),
	                                CPG_SELECTOR_TYPE_STATE |
	                                CPG_SELECTOR_TYPE_GROUP,
	                                context->priv->embedded);

	if (!fromobjs)
	{
		cpg_embedded_context_restore (context->priv->embedded);

		return NULL;
	}

	if (!to)
	{
		ret = link_pairs_sparse (context,
		                         iffprob,
		                         bidi != NULL,
		                         noself != NULL,
		                         onlyself,
		                         fromobjs);

		g_slist_foreach (fromobjs, (GFunc)g_object_unref, NULL);
		g_slist_free (fromobjs);

		cpg_embedded_context_restore (context->priv->embedded);

		return ret;
	}

	for (fromobj = fromobjs; fromobj; fromobj = g_slist_next (fromobj))
	{
		GSList *toobjs = NULL;
		GSList *toobj;

		cpg_embedded_context_save (context->priv->embedded);

		cpg_embedded_context_add_expansions (context->priv->embedded,
		                                      cpg_selection_get_expansions (fromobj->data));

		cpg_selector_set_from_set (to, fromobjs);

		/* Select TO states */
		toobjs = cpg_selector_select (to,
		                              cpg_selection_get_object (parent),
		                              CPG_SELECTOR_TYPE_STATE |
		                              CPG_SELECTOR_TYPE_GROUP,
		                              context->priv->embedded);

		cpg_embedded_context_restore (context->priv->embedded);

		for (toobj = toobjs; toobj; toobj = g_slist_next (toobj))
		{
			if ((iffprob < 1 && random () > p) || (noself &&
			    cpg_selection_get_object (toobj->data) ==
			    cpg_selection_get_object (fromobj->data)))
			{
				continue;
			}

			ret = g_slist_prepend (ret,
			                       cpg_selection_copy (fromobj->data));

			ret = g_slist_prepend (ret,
			                       cpg_selection_copy (toobj->data));

			if (bidi != NULL)
			{
				ret = g_slist_prepend (ret,
				                       cpg_selection_copy (toobj->data));

				ret = g_slist_prepend (ret,
				                       cpg_selection_copy (fromobj->data));
			}
		}

		g_slist_foreach (toobjs, (GFunc)g_object_unref, NULL);
		g_slist_free (toobjs);
	}

	g_slist_foreach (fromobjs, (GFunc)g_object_unref, NULL);
	g_slist_free (fromobjs);

	cpg_embedded_context_restore (context->priv->embedded);

	return g_slist_reverse (ret);
}

static void
store_annotation_objects (CpgParserContext *context,
                          GSList           *objects)
{
	gchar *s;

	s = steal_annotation (context);

	if (!s)
	{
		return;
	}

	while (objects)
	{
		CpgObject *obj;

		obj = cpg_selection_get_object (objects->data);

		cpg_annotatable_set_annotation (CPG_ANNOTATABLE (obj),
		                                s);

		objects = g_slist_next (objects);
	}

	g_free (s);
}

void
cpg_parser_context_push_object (CpgParserContext *context,
                                GSList           *objects,
                                GSList           *attributes)
{
	GSList *item;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->in_when_applied)
	{
		return;
	}

	store_annotation_objects (context, objects);

	for (item = objects; item; item = g_slist_next (item))
	{
		set_taggable (context,
		              cpg_selection_get_object (item->data),
		              attributes);
	}

	context->priv->context_stack =
		g_slist_prepend (context->priv->context_stack,
		                 context_new (objects));

	g_signal_emit (context, signals[CONTEXT_PUSHED], 0);
}

void
cpg_parser_context_push_selection (CpgParserContext *context,
                                   CpgSelector      *selector,
                                   CpgSelectorType   type,
                                   GSList           *attributes)
{
	Context *ctx;
	GSList *item;
	GSList *objs = NULL;
	GSList *parents;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->in_when_applied)
	{
		return;
	}

	/* Select on the current context and create a new context based on
	   the result */

	ctx = CURRENT_CONTEXT (context);

	parents = each_selections (context,
	                           ctx->objects,
	                           attributes,
	                           type,
	                           NULL,
	                           NULL,
	                           TRUE);

	for (item = parents; item; item = g_slist_next (item))
	{
		CpgSelection *sel;
		GSList *ret;
		GSList *it;

		sel = item->data;

		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);
		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    sel);

		ret = cpg_selector_select (selector,
		                           cpg_selection_get_object (sel),
		                           type,
		                           context->priv->embedded);

		for (it = ret; it; it = g_slist_next (it))
		{
			cpg_embedded_context_save (context->priv->embedded);

			cpg_embedded_context_add_expansions (context->priv->embedded,
			                                     cpg_selection_get_expansions (it->data));

			objs = g_slist_prepend (objs,
			                        cpg_selection_new_defines (cpg_selection_get_object (it->data),
			                                                   cpg_embedded_context_get_expansions (context->priv->embedded),
			                                                   cpg_embedded_context_get_defines (context->priv->embedded),
			                                                   TRUE));

			cpg_embedded_context_restore (context->priv->embedded);
		}

		g_slist_foreach (ret, (GFunc)g_object_unref, NULL);
		g_slist_free (ret);

		cpg_embedded_context_restore (context->priv->embedded);
	}

	g_slist_foreach (parents, (GFunc)g_object_unref, NULL);
	g_slist_free (parents);

	objs = g_slist_reverse (objs);

	cpg_parser_context_push_object (context, objs, attributes);

	g_slist_free (objs);
}

static GSList *
create_objects (CpgParserContext  *context,
                CpgEmbeddedString *id,
                GSList            *templates,
                GType              type,
                GSList            *attributes)
{
	return parse_objects (context, id, templates, type, attributes);
}

static GSList *
create_links_single (CpgParserContext          *context,
                     CpgExpansion              *id,
                     gboolean                   autoid,
                     GSList                    *templates,
                     CpgSelection              *parent,
                     GSList                    *attributes,
                     CpgSelector               *from,
                     CpgSelector               *to,
                     gboolean                   onlyself)
{
	GSList *pairs;
	GSList *item;
	GSList *ret = NULL;
	gboolean multiple;
	gint num = 1;
	gint idx = 0;
	CpgAttribute *bidi;

	/* For each pair FROM -> TO generate a link */
	pairs = link_pairs (context,
	                    id,
	                    autoid,
	                    parent,
	                    attributes,
	                    from,
	                    to,
	                    onlyself);
	item = pairs;

	bidi = find_attribute (attributes, "bidirectional");

	multiple = pairs && pairs->next && pairs->next->next;

	while (item)
	{
		CpgSelection *fromsel;
		CpgSelection *tosel;
		CpgExpansion *realid;
		CpgSelection *obj;
		CpgSelection *firstsel;
		CpgSelection *secondsel;
		gboolean argswitch;
		gchar *newid;
		gchar *uniq;

		++idx;

		fromsel = item->data;
		item = g_slist_next (item);

		tosel = item->data;
		item = g_slist_next (item);

		if (bidi == NULL || idx % 2 == 1)
		{
			firstsel = fromsel;
			secondsel = tosel;

			argswitch = FALSE;
		}
		else
		{
			firstsel = tosel;
			secondsel = fromsel;

			argswitch = TRUE;
		}

		if (cpg_object_get_parent (cpg_selection_get_object (fromsel)) !=
		    cpg_object_get_parent (cpg_selection_get_object (tosel)))
		{
			continue;
		}

		/* Alter id to be numeric incremental */

		realid = cpg_expansion_copy (id);

		if (multiple)
		{
			newid = g_strdup_printf ("%s_%d",
			                         cpg_expansion_get (id, 0),
			                         num);
		}
		else
		{
			newid = g_strdup (cpg_expansion_get (id, 0));
		}

		uniq = unique_id (CPG_GROUP (cpg_selection_get_object (parent)),
		                  newid);

		g_free (newid);

		cpg_expansion_set (realid, 0, uniq);
		g_free (uniq);

		cpg_embedded_context_save (context->priv->embedded);

		if (bidi != NULL)
		{
			gint widx = argswitch ? 1 : 0;
			CpgEmbeddedString *s;

			s = CPG_EMBEDDED_STRING (cpg_attribute_get_argument (bidi, widx));

			if (s)
			{
				gchar const *ex;
				CpgExpansion *expansion;

				embedded_string_expand_val (ex, s, context, NULL);

				expansion = cpg_expansion_new_one (ex);
				cpg_expansion_set_index (expansion, 0, widx);

				cpg_embedded_context_add_expansion (context->priv->embedded,
				                                    expansion);
				g_object_unref (expansion);
			}
		}

		if (!autoid)
		{
			cpg_embedded_context_add_expansion (context->priv->embedded,
			                                    realid);
		}

		cpg_embedded_context_add_expansions (context->priv->embedded,
		                                     cpg_selection_get_expansions (firstsel));

		cpg_embedded_context_add_expansions (context->priv->embedded,
		                                     cpg_selection_get_expansions (secondsel));

		obj = parse_object_single_id (context,
		                              realid,
		                              templates,
		                              parent,
		                              CPG_TYPE_LINK);

		cpg_embedded_context_restore (context->priv->embedded);

		g_object_unref (realid);

		if (!obj)
		{
			continue;
		}

		ret = g_slist_prepend (ret, obj);

		++num;

		cpg_link_attach (CPG_LINK (cpg_selection_get_object (obj)),
		                 cpg_selection_get_object (fromsel),
		                 cpg_selection_get_object (tosel));
	}

	g_slist_foreach (pairs, (GFunc)g_object_unref, NULL);
	g_slist_free (pairs);

	return g_slist_reverse (ret);
}

static GSList *
create_links (CpgParserContext          *context,
              CpgEmbeddedString         *id,
              gboolean                   autoid,
              GSList                    *templates,
              GSList                    *attributes,
              GSList                    *fromto)
{
	GSList *ids;
	GSList *ret = NULL;
	GSList *item;
	Context *ctx;
	CpgSelector *from;
	CpgSelector *to;
	gboolean onlyself = FALSE;
	GSList *parents;

	ctx = CURRENT_CONTEXT (context);

	from = fromto->data;

	if (fromto->next)
	{
		to = fromto->next->data;

		if (fromto->next->next)
		{
			onlyself = GPOINTER_TO_INT (fromto->next->next->data);
		}
	}
	else
	{
		to = NULL;
	}

	parents = each_selections (context,
	                           ctx->objects,
	                           attributes,
	                           CPG_SELECTOR_TYPE_LINK,
	                           NULL,
	                           NULL,
	                           TRUE);

	for (item = parents; item; item = g_slist_next (item))
	{
		GSList *it;

		/* Expand the id with the parent expansions */
		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);

		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    item->data);

		embedded_string_expand_multiple_val (ids, id, context, NULL);

		for (it = ids; it; it = g_slist_next (it))
		{
			ret = g_slist_concat (ret,
			                      create_links_single (context,
			                                           it->data,
			                                           autoid,
			                                           templates,
			                                           item->data,
			                                           attributes,
			                                           from,
			                                           to,
			                                           onlyself));
		}

		cpg_embedded_context_restore (context->priv->embedded);

		g_slist_foreach (ids, (GFunc)g_object_unref, NULL);
		g_slist_free (ids);
	}

	g_slist_foreach (parents, (GFunc)g_object_unref, NULL);
	g_slist_free (parents);

	return ret;
}

void
cpg_parser_context_push_state (CpgParserContext  *context,
                               CpgEmbeddedString *id,
                               GSList            *templates,
                               GSList            *attributes)
{
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->in_when_applied)
	{
		return;
	}

	objects = create_objects (context,
	                          id,
	                          templates,
	                          CPG_TYPE_OBJECT,
	                          attributes);

	cpg_parser_context_push_object (context, objects, attributes);
	g_slist_free (objects);
}

void
cpg_parser_context_push_group (CpgParserContext  *context,
                               CpgEmbeddedString *id,
                               GSList            *templates,
                               GSList            *attributes)
{
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->in_when_applied)
	{
		return;
	}

	objects = create_objects (context,
	                          id,
	                          templates,
	                          CPG_TYPE_GROUP,
	                          attributes);

	cpg_parser_context_push_object (context, objects, attributes);
	g_slist_free (objects);
}

void
cpg_parser_context_push_input_file (CpgParserContext  *context,
                                    CpgEmbeddedString *id,
                                    CpgEmbeddedString *path,
                                    GSList            *attributes)
{
	GSList *objects;
	GSList *paths;
	GSList *item;
	GSList *obj;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->in_when_applied)
	{
		return;
	}

	objects = create_objects (context,
	                          id,
	                          NULL,
	                          CPG_TYPE_INPUT_FILE,
	                          attributes);

	embedded_string_expand_multiple (paths, path, context);

	for (item = paths; item; item = g_slist_next (item))
	{
		CpgExpansion *ex;

		ex = item->data;

		for (obj = objects; obj; obj = g_slist_next (obj))
		{
			CpgInputFile *f;

			f = CPG_INPUT_FILE (cpg_selection_get_object (obj->data));

			cpg_input_file_set_file_path (f,
			                              cpg_expansion_get (ex, 0));
		}
	}

	g_slist_foreach (paths, (GFunc)g_object_unref, NULL);
	g_slist_free (paths);

	cpg_parser_context_push_object (context, objects, attributes);
	g_slist_free (objects);
}


void
cpg_parser_context_push_link (CpgParserContext          *context,
                              CpgEmbeddedString         *id,
                              GSList                    *templates,
                              GSList                    *attributes,
                              GSList                    *fromto)
{
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->in_when_applied)
	{
		return;
	}

	if (!fromto)
	{
		objects = create_objects (context,
		                          id,
		                          templates,
		                          CPG_TYPE_LINK,
		                          attributes);
	}
	else
	{
		gboolean autoid;
	
		autoid = id == NULL;

		if (id == NULL)
		{
			id = cpg_embedded_string_new_from_string ("link");
		}

		objects = create_links (context,
		                        id,
		                        autoid,
		                        templates,
		                        attributes,
		                        fromto);
	}

	cpg_parser_context_push_object (context, objects, attributes);
	g_slist_free (objects);

	if (id != NULL)
	{
		g_object_unref (id);
	}
}

static GSList *
selections_from_attributes_obj (CpgParserContext *context,
                                CpgObject        *obj,
                                GSList           *attributes)
{
	GSList *ret = NULL;
	GSList *parents;
	GSList *item;

	if (!context->priv->context_stack)
	{
		CpgSelection *sel;

		sel = cpg_selection_new (obj,
		                         cpg_embedded_context_get_expansions (context->priv->embedded),
		                         cpg_embedded_context_get_defines (context->priv->embedded));

		ret = g_slist_prepend (NULL, sel);

		return ret;
	}

	parents = each_selections (context,
	                           CURRENT_CONTEXT (context)->objects,
	                           attributes,
	                           CPG_SELECTOR_TYPE_OBJECT,
	                           NULL,
	                           NULL,
	                           TRUE);

	for (item = parents; item; item = g_slist_next (item))
	{
		CpgSelection *sel;

		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);
		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    item->data);

		sel = cpg_selection_new (obj,
		                         cpg_embedded_context_get_expansions (context->priv->embedded),
		                         cpg_embedded_context_get_defines (context->priv->embedded));

		cpg_embedded_context_restore (context->priv->embedded);

		ret = g_slist_prepend (ret, sel);
	}

	g_slist_foreach (parents, (GFunc)g_object_unref, NULL);
	g_slist_free (parents);

	return g_slist_reverse (ret);
}

static void
push_scope (CpgParserContext *context,
            GSList           *attributes,
            gboolean          copy_defines)
{
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->in_when_applied)
	{
		return;
	}

	objects = each_selections (context,
	                           CURRENT_CONTEXT (context)->objects,
	                           attributes,
	                           CPG_SELECTOR_TYPE_ANY,
	                           NULL,
	                           NULL,
	                           copy_defines);

	cpg_parser_context_push_object (context, objects, attributes);

	g_slist_free (objects);
}

void
cpg_parser_context_push_define (CpgParserContext *context,
                                GSList           *attributes)
{
	if (context->priv->in_when_applied)
	{
		return;
	}

	push_scope (context, attributes, FALSE);
}

void
cpg_parser_context_push_scope (CpgParserContext *context,
                               GSList           *attributes)
{
	if (context->priv->in_when_applied)
	{
		return;
	}

	push_scope (context, attributes, TRUE);
}

void
cpg_parser_context_push_network (CpgParserContext *context,
                                 GSList           *attributes)
{
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->in_when_applied)
	{
		return;
	}

	objects = selections_from_attributes_obj (context,
	                                          CPG_OBJECT (context->priv->network),
	                                          attributes);

	cpg_parser_context_push_object (context, objects, attributes);
	g_slist_free (objects);
}

void
cpg_parser_context_push_integrator (CpgParserContext *context,
                                    GSList           *attributes)
{
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->in_when_applied)
	{
		return;
	}

	objects = selections_from_attributes_obj (context,
	                                          CPG_OBJECT (cpg_network_get_integrator (context->priv->network)),
	                                          attributes);

	cpg_parser_context_push_object (context, objects, attributes);

	g_slist_free (objects);
}

void
cpg_parser_context_push_templates (CpgParserContext *context,
                                   GSList           *attributes)
{
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->in_when_applied)
	{
		return;
	}

	objects = selections_from_attributes_obj (context,
	                                          CPG_OBJECT (cpg_network_get_template_group (context->priv->network)),
	                                          attributes);

	cpg_parser_context_push_object (context, objects, attributes);
	context->priv->is_template = CURRENT_CONTEXT (context);

	g_slist_free (objects);
}

/**
 * cpg_parser_context_pop:
 * @context: A #CpgParserContext
 *
 * Description.
 *
 * Returns: (transfer container) (element-type CpgObject): A #GSList
 *
 **/
GSList *
cpg_parser_context_pop (CpgParserContext *context)
{
	Context *ctx;
	GSList *ret = NULL;
	GSList *item;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	if (context->priv->in_when_applied)
	{
		return NULL;
	}

	if (!context->priv->context_stack)
	{
		return NULL;
	}

	ctx = CURRENT_CONTEXT (context);

	if (context->priv->is_template == ctx)
	{
		context->priv->is_template = NULL;
	}

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		ret = g_slist_prepend (ret,
		                       cpg_selection_get_object (item->data));
	}

	ret = g_slist_reverse (ret);

	context_free (ctx);

	context->priv->context_stack =
		g_slist_delete_link (context->priv->context_stack,
		                     context->priv->context_stack);

	g_signal_emit (context, signals[CONTEXT_POPPED], 0);

	return ret;
}

void
cpg_parser_context_import (CpgParserContext  *context,
                           CpgEmbeddedString *id,
                           CpgEmbeddedString *path,
                           GSList            *attributes)
{
	Context *ctx;
	GSList *item;
	gchar *annotation;
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (id != NULL);
	g_return_if_fail (path != NULL);

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);
	annotation = steal_annotation (context);

	objects = each_selections (context,
	                           ctx->objects,
	                           attributes,
	                           CPG_SELECTOR_TYPE_ANY,
	                           NULL,
	                           NULL,
	                           FALSE);

	for (item = objects; item; item = g_slist_next (item))
	{
		GSList *ids;
		GSList *idi;

		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);

		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    item->data);

		embedded_string_expand_multiple (ids, id, context);

		for (idi = ids; idi; idi = g_slist_next (idi))
		{
			gchar const *expath;
			GFile *file = NULL;
			CpgImport *import;
			GError *error = NULL;
			GFile *curfile;
			CpgGroup *parent_group;
			gchar const *exid;

			exid = cpg_expansion_get (idi->data, 0);

			cpg_embedded_context_save (context->priv->embedded);

			cpg_embedded_context_add_expansion (context->priv->embedded,
			                                     idi->data);

			embedded_string_expand (expath, path, context);

			cpg_embedded_context_restore (context->priv->embedded);

			curfile = cpg_parser_context_get_file (context);

			file = cpg_network_parser_utils_resolve_import (curfile,
			                                                expath);

			if (curfile)
			{
				g_object_unref (curfile);
			}

			if (!file)
			{
				parser_failed (context,
				               CPG_NETWORK_LOAD_ERROR_IMPORT,
				               "File `%s' for import `%s' could not be found",
				               expath,
				               exid);

				break;
			}

			parent_group = CPG_GROUP (cpg_selection_get_object (item->data));

			if (context->priv->is_template)
			{
				CpgGroup *template_group;

				template_group = cpg_network_get_template_group (context->priv->network);

				if (cpg_group_get_child (template_group, exid) != NULL)
				{
					parser_failed (context,
					               CPG_NETWORK_LOAD_ERROR_IMPORT,
					               "There is already an object with the id `%s'",
					               exid);

					cpg_embedded_context_restore (context->priv->embedded);

					g_slist_foreach (ids,
					                 (GFunc)g_object_unref,
					                 NULL);

					g_slist_free (ids);

					goto cleanup;
				}

				import = cpg_network_parser_utils_find_template_import (CPG_OBJECT (template_group), file);

				if (import)
				{
					CpgImportAlias *alias;

					alias = cpg_import_alias_new (import);

					if (!cpg_group_add (parent_group,
					                    CPG_OBJECT (alias),
					                    &error))
					{
						parser_failed_error (context, error);
						cpg_embedded_context_restore (context->priv->embedded);

						g_slist_foreach (ids,
						                 (GFunc)g_object_unref,
						                 NULL);

						g_slist_free (ids);
						goto cleanup;
					}

					if (annotation)
					{
						cpg_annotatable_set_annotation (CPG_ANNOTATABLE (alias),
						                                annotation);
					}

					set_taggable (context, alias, attributes);

					g_object_unref (alias);
					g_object_unref (file);

					continue;
				}
			}

			if (cpg_group_get_child (CPG_GROUP (context->priv->network), exid) != NULL)
			{
				parser_failed (context,
				               CPG_NETWORK_LOAD_ERROR_IMPORT,
				               "There is already an object with the id `%s'",
				               exid);

				cpg_embedded_context_restore (context->priv->embedded);

				g_slist_foreach (ids,
				                 (GFunc)g_object_unref,
				                 NULL);

				g_slist_free (ids);

				goto cleanup;
			}

			import = cpg_import_new (context->priv->network,
			                         parent_group,
			                         exid,
			                         file,
			                         &error);

			if (!import)
			{
				parser_failed_error (context, error);
				cpg_embedded_context_restore (context->priv->embedded);

				g_slist_foreach (ids,
				                 (GFunc)g_object_unref,
				                 NULL);

				g_slist_free (ids);

				goto cleanup;
			}

			if (annotation)
			{
				cpg_annotatable_set_annotation (CPG_ANNOTATABLE (import),
				                                annotation);
			}

			set_taggable (context, import, attributes);

			g_object_unref (import);
		}

		g_slist_foreach (ids, (GFunc)g_object_unref, NULL);
		g_slist_free (ids);

		ids = NULL;

		cpg_embedded_context_restore (context->priv->embedded);
	}

cleanup:
	g_slist_foreach (objects, (GFunc)g_object_unref, NULL);
	g_slist_free (objects);

	g_free (annotation);

	g_object_unref (id);
	g_object_unref (path);
}

static CpgSelector *
ensure_selector (CpgParserContext *context)
{
	if (!context->priv->selectors)
	{
		cpg_parser_context_push_selector (context);
	}

	return context->priv->selectors->data;
}

void
cpg_parser_context_push_selector (CpgParserContext *context)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	context->priv->selectors =
		g_slist_prepend (context->priv->selectors,
		                 cpg_selector_new (CPG_OBJECT (context->priv->network)));
}

void
cpg_parser_context_push_selector_identifier (CpgParserContext  *context,
                                             CpgEmbeddedString *identifier)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (identifier != NULL);

	cpg_selector_append (ensure_selector (context), identifier);
	g_object_unref (identifier);

	statement_end (context, ensure_selector (context));

	g_signal_emit (context,
	               signals[SELECTOR_ITEM_PUSHED],
	               0,
	               ensure_selector (context));
}

void
cpg_parser_context_push_selector_regex (CpgParserContext  *context,
                                        CpgEmbeddedString *regex)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (regex != NULL);

	cpg_selector_append_regex (ensure_selector (context), regex);
	g_object_unref (regex);

	statement_end (context, ensure_selector (context));

	g_signal_emit (context,
	               signals[SELECTOR_ITEM_PUSHED],
	               0,
	               ensure_selector (context));
}

void
cpg_parser_context_push_selector_pseudo (CpgParserContext      *context,
                                         CpgSelectorPseudoType  type,
                                         GSList                *argument)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	cpg_selector_append_pseudo (ensure_selector (context),
	                         type,
	                         argument);

	g_slist_foreach (argument, (GFunc)g_object_unref, NULL);
	g_slist_free (argument);

	statement_end (context, ensure_selector (context));

	g_signal_emit (context,
	               signals[SELECTOR_ITEM_PUSHED],
	               0,
	               ensure_selector (context));
}

/**
 * cpg_parser_context_peek_selector:
 * @context: A #CpgParserContext
 *
 * Description.
 *
 * Returns: (transfer none): A #CpgSelector
 *
 **/
CpgSelector *
cpg_parser_context_peek_selector (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	return ensure_selector (context);
}

/**
 * cpg_parser_context_pop_selector:
 * @context: A #CpgParserContext
 * 
 * Description.
 *
 * Returns: (transfer full): A #CpgSelector
 *
 **/
CpgSelector *
cpg_parser_context_pop_selector (CpgParserContext *context)
{
	CpgSelector *ret;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (context->priv->selectors, NULL);

	ret = context->priv->selectors->data;

	context->priv->selectors =
		g_slist_delete_link (context->priv->selectors,
		                     context->priv->selectors);

	return ret;
}

/**
 * cpg_parser_context_get_scanner:
 * @context: A #CpgParserContext
 *
 * Description.
 *
 * Returns: (transfer none): Description
 *
 **/
gpointer
cpg_parser_context_get_scanner (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	return context->priv->scanner;
}

gssize
cpg_parser_context_read (CpgParserContext *context,
                         gchar            *buffer,
                         gsize             max_size)
{
	InputItem *item;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), EOF);
	g_return_val_if_fail (buffer != NULL, EOF);

	if (!context->priv->inputs)
	{
		return 0;
	}

	item = context->priv->inputs->data;

	if (!item->stream)
	{
		return 0;
	}

	return g_input_stream_read (item->stream,
	                            buffer,
	                            max_size,
	                            NULL,
	                            NULL);
}

gboolean
cpg_parser_context_parse (CpgParserContext  *context,
                          gboolean           push_network,
                          GError           **error)
{
	gboolean ret;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), FALSE);
	g_return_val_if_fail (context->priv->error != NULL || context->priv->inputs, FALSE);

	if (context->priv->error == NULL)
	{
		if (push_network)
		{
			cpg_parser_context_push_network (context, NULL);
		}

		ret = cpg_parser_parse (context) == 0;

		if (push_network)
		{
			cpg_parser_context_pop (context);
		}
	}
	else
	{
		ret = FALSE;
	}

	if (!ret)
	{
		if (error && context->priv->error)
		{
			*error = g_error_copy (context->priv->error);
		}
	}

	return ret;
}

void
cpg_parser_context_set_line (CpgParserContext *context,
                             gchar const      *line,
                             gint              lineno)
{
	InputItem *input;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	input = CURRENT_INPUT (context);

	input->lineno = lineno;

	g_hash_table_insert (input->lines,
	                     GINT_TO_POINTER (lineno),
	                     g_strdup (line));
}

void
cpg_parser_context_set_column (CpgParserContext *context,
                               gint              start,
                               gint              end)
{
	InputItem *input;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	input = CURRENT_INPUT (context);

	input->cstart = start;
	input->cend = end;
}

gchar const *
cpg_parser_context_get_line (CpgParserContext *context,
                             gint             *lineno)
{
	InputItem *input;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	input = CURRENT_INPUT (context);

	if (lineno)
	{
		*lineno = input ? input->lineno : 0;
	}

	return cpg_parser_context_get_line_at (context, input->lineno);
}

gchar const *
cpg_parser_context_get_line_at (CpgParserContext *context,
                                gint              lineno)
{
	InputItem *input;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	input = CURRENT_INPUT (context);

	if (!input)
	{
		return NULL;
	}

	return g_hash_table_lookup (input->lines, GINT_TO_POINTER (lineno));
}

void
cpg_parser_context_get_column (CpgParserContext *context,
                               gint             *start,
                               gint             *end)
{
	InputItem *input;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	input = CURRENT_INPUT (context);

	if (start)
	{
		*start = 0;
	}

	if (end)
	{
		*end = 0;
	}

	if (!input)
	{
		return;
	}

	if (start)
	{
		*start = input->cstart;
	}

	if (end)
	{
		*end = input->cend;
	}
}

void
cpg_parser_context_set_token (CpgParserContext *context,
                              gchar const      *token)
{
	InputItem *input;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	input = CURRENT_INPUT (context);

	g_free (input->token);

	input->token = g_strdup (token);

	if (context->priv->in_when_applied)
	{
		g_string_append (context->priv->when_applied_text, token);
	}
}

gchar const *
cpg_parser_context_get_token (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	return CURRENT_INPUT (context)->token;
}

void
cpg_parser_context_define (CpgParserContext  *context,
                           CpgEmbeddedString *name,
                           GSList            *defines,
                           gboolean           expand,
                           gboolean           optional)
{
	GSList *ob;
	Context *ctx;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (defines != NULL);

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	for (ob = ctx->objects; ob; ob = g_slist_next (ob))
	{
		GSList *names;
		GSList *nameit;
		CpgSelection *sel;

		sel = ob->data;

		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);
		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    sel);

		embedded_string_expand_multiple (names, name, context);

		for (nameit = names; nameit; nameit = g_slist_next (nameit))
		{
			gchar const *exname;
			gchar const *exdefine;
			CpgEmbeddedString *define = NULL;
			GSList *item;

			exname = cpg_expansion_get (nameit->data, 0);

			if (optional)
			{
				gchar *d;
				gboolean exists;

				d = cpg_embedded_context_get_define (context->priv->embedded,
				                                     exname);

				exists = (d && *d);
				g_free (d);

				if (exists)
				{
					continue;
				}
			}

			cpg_embedded_context_save (context->priv->embedded);
			cpg_embedded_context_add_expansion (context->priv->embedded,
			                                    nameit->data);

			for (item = defines; item; item = g_slist_next (item))
			{
				gchar const *s;

				embedded_string_expand (s, item->data, context);

				/* Note that if we are not optional (in the case
				   where ?= syntax is not used), we also consider
				   a numeric 0 string to be empty */
				if (s && *s && (optional || !test_string_empty (s)))
				{
					define = item->data;
					break;
				}
			}

			if (!define)
			{
				define = defines->data;
			}

			embedded_string_expand (exdefine, define, context);

			if (expand)
			{
				GSList *items;
				GSList *item;
				gint num = 0;
				gchar *cntname;
				gchar *cntval;
				gchar *s0;

				s0 = g_strdup_printf ("%s0", exname);

				cpg_selection_add_define (sel,
				                          s0,
				                          exdefine);

				g_free (s0);

				embedded_string_expand_multiple (items, define, context);

				for (item = items; item; item = g_slist_next (item))
				{
					CpgExpansion *ex = item->data;
					gint i;
					gchar *name;

					name = g_strdup_printf ("%s%d", exname, ++num);

					cpg_selection_add_define (sel,
					                          name,
					                          cpg_expansion_get (ex, 0));

					for (i = 0; i < cpg_expansion_num (ex); ++i)
					{
						gchar *sub;

						sub = g_strdup_printf ("%s,%d", name, i);

						cpg_selection_add_define (sel,
						                          sub,
						                          cpg_expansion_get (ex, i));

						g_free (sub);
					}

					g_free (name);
				}

				g_slist_foreach (items, (GFunc)g_object_unref, NULL);
				g_slist_free (items);

				cntname = g_strconcat (exname, "~", NULL);
				cntval = g_strdup_printf ("%d", num);

				cpg_selection_add_define (sel,
				                          cntname,
				                          cntval);

				g_free (cntname);
				g_free (cntval);
			}
			else
			{
				cpg_selection_add_define (sel, exname, exdefine);
			}

			cpg_embedded_context_restore (context->priv->embedded);
		}

		cpg_embedded_context_restore (context->priv->embedded);

		g_slist_foreach (names, (GFunc)g_object_unref, NULL);
		g_slist_free (names);
	}

	g_object_unref (name);

	g_slist_foreach (defines, (GFunc)g_object_unref, NULL);
	g_slist_free (defines);
}

void
cpg_parser_context_push_input (CpgParserContext *context,
                               GFile            *file,
                               GInputStream     *stream,
                               GSList           *attributes)
{
	InputItem *item;
	GError *error = NULL;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (file != NULL || stream != NULL);

	if (context->priv->in_when_applied)
	{
		return;
	}

	item = input_item_new (file, stream, &error);

	if (item)
	{
		context->priv->inputs = g_slist_prepend (context->priv->inputs,
		                                         item);

		if (context->priv->inputs->next)
		{
			cpg_parser_tokens_push_input (context->priv->scanner);
		}

		push_scope (context, attributes, FALSE);
	}
	else
	{
		parser_failed_error (context, error);
	}
}

void
cpg_parser_context_push_input_from_path (CpgParserContext  *context,
                                         CpgEmbeddedString *filename,
                                         GSList            *attributes)
{
	GSList *items;
	GSList *item;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (filename != NULL);

	embedded_string_expand_multiple (items, filename, context);

	for (item = items; item; item = g_slist_next (item))
	{
		gchar const *res;
		GFile *file = NULL;

		res = cpg_expansion_get (item->data, 0);

		if (g_path_is_absolute (res))
		{
			file = g_file_new_for_path (res);
		}
		else
		{
			GSList *it;

			for (it = context->priv->inputs; it; it = g_slist_next (it))
			{
				InputItem *ip = it->data;
				GFile *dir;

				if (!ip->file)
				{
					continue;
				}

				dir = g_file_get_parent (ip->file);

				if (!dir)
				{
					continue;
				}

				file = g_file_resolve_relative_path (dir, res);
				g_object_unref (dir);

				if (file && !g_file_query_exists (file, NULL))
				{
					g_object_unref (file);
					file = NULL;
				}
				else
				{
					break;
				}
			}

			if (!file)
			{
				file = g_file_new_for_commandline_arg (res);

				if (!g_file_query_exists (file, NULL))
				{
					g_object_unref (file);
					file = NULL;
				}
			}
		}

		if (!file)
		{
			parser_failed_error_at (context,
			                        CPG_STATEMENT (filename),
			                        g_error_new (G_IO_ERROR,
			                                     G_IO_ERROR_NOT_FOUND,
			                                     "Could not find file `%s'",
			                                     res));

			break;
		}

		cpg_parser_context_push_input (context, file, NULL, attributes);
		g_object_unref (file);
	}

	g_slist_foreach (items, (GFunc)g_object_unref, NULL);
	g_slist_free (items);

	g_object_unref (filename);
}

void
cpg_parser_context_push_input_from_string (CpgParserContext *context,
                                           gchar const      *s,
                                           GSList           *attributes)
{
	GInputStream *stream;
	gchar *ret;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (s != NULL);

	if (context->priv->in_when_applied)
	{
		return;
	}

	ret = g_strdup (s);

	stream = g_memory_input_stream_new_from_data (ret,
	                                              strlen (ret),
	                                              (GDestroyNotify)g_free);

	cpg_parser_context_push_input (context, NULL, stream, attributes);

	g_object_unref (stream);
}


void
cpg_parser_context_pop_input (CpgParserContext *context)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->inputs)
	{
		InputItem *input;

		input = CURRENT_INPUT (context);

		++(input->lineno);
		input->cstart = 0;
		input->cend = 0;

		cpg_parser_context_pop (context);

		if (!context->priv->inputs->next)
		{
			/* Pop all the scopes */
			while (context->priv->context_stack)
			{
				cpg_parser_context_pop (context);
			}
		}

		input_item_free (context->priv->inputs->data);

		context->priv->inputs = g_slist_delete_link (context->priv->inputs,
		                                             context->priv->inputs);
	}
}

/**
 * cpg_parser_context_get_file:
 * @context: A #CpgParserContext
 *
 * Get the current parsed file. If the current parsing stage is from memory
 * or a stream only, the result will be %NULL.
 *
 * Returns: (transfer full) (allow-none): A #GFile
 *
 **/
GFile *
cpg_parser_context_get_file (CpgParserContext *context)
{
	GSList *item;

	for (item = context->priv->inputs; item; item = g_slist_next (item))
	{
		InputItem *ip = item->data;

		if (ip->file)
		{
			return g_file_dup (ip->file);
		}
	}

	return NULL;
}

gint
cpg_parser_context_get_start_token (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), 0);

	return context->priv->start_token;
}

gint
cpg_parser_context_steal_start_token (CpgParserContext *context)
{
	gint ret;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), 0);

	ret = context->priv->start_token;
	context->priv->start_token = 0;

	return ret;
}

void
cpg_parser_context_set_start_token (CpgParserContext *context,
                                    gint              token)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	context->priv->start_token = token;
}

void
cpg_parser_context_push_annotation (CpgParserContext  *context,
                                    CpgEmbeddedString *annotation)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (annotation != NULL);

	if (context->priv->in_when_applied)
	{
		return;
	}

	if (context->priv->previous_annotation != CURRENT_INPUT (context)->lineno - 1)
	{
		gchar const *expanded;

		embedded_string_expand (expanded, annotation, context);

		g_string_assign (context->priv->annotation, expanded);
	}
	else
	{
		gchar const *expanded;

		embedded_string_expand (expanded, annotation, context);

		g_string_append (context->priv->annotation, expanded);

		g_string_append_c (context->priv->annotation, '\n');
	}

	context->priv->previous_annotation = CURRENT_INPUT (context)->lineno;
	g_object_unref (annotation);
}

void
cpg_parser_context_push_layout (CpgParserContext *context,
                                GSList           *attributes)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->in_when_applied)
	{
		return;
	}

	if (!context->priv->layout)
	{
		context->priv->layout = cpg_layout_new (context->priv->network);
	}

	cpg_parser_context_push_scope (context, attributes);
}

void
cpg_parser_context_add_layout (CpgParserContext *context,
                               CpgLayoutRelation relation,
                               CpgSelector      *left,
                               CpgSelector      *right)
{
	GSList *leftobjs;
	GSList *leftobj;
	GSList *objs;
	Context *ctx;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (left == NULL || CPG_IS_SELECTOR (left));
	g_return_if_fail (CPG_IS_SELECTOR (right));

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = context->priv->context_stack->next->data;

	for (objs = ctx->objects; objs; objs = g_slist_next (objs))
	{
		CpgSelection *sel;

		sel = objs->data;

		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);

		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    sel);

		if (left != NULL)
		{
			leftobjs = cpg_selector_select (left,
			                                G_OBJECT (cpg_selection_get_object (sel)),
			                                CPG_SELECTOR_TYPE_STATE |
			                                CPG_SELECTOR_TYPE_GROUP,
			                                context->priv->embedded);
		}
		else
		{
			leftobjs = g_slist_prepend (NULL, cpg_selection_copy (sel));
		}

		if (!leftobjs)
		{
			cpg_embedded_context_restore (context->priv->embedded);
			return;
		}

		for (leftobj = leftobjs; leftobj; leftobj = g_slist_next (leftobj))
		{
			CpgSelection *leftsel = leftobj->data;
			GSList *rightobjs;

			cpg_embedded_context_save (context->priv->embedded);

			cpg_embedded_context_add_expansions (context->priv->embedded,
			                                      cpg_selection_get_expansions (leftsel));

			rightobjs = cpg_selector_select (right,
			                                 G_OBJECT (cpg_selection_get_object (sel)),
			                                 CPG_SELECTOR_TYPE_STATE |
			                                 CPG_SELECTOR_TYPE_GROUP,
			                                 context->priv->embedded);

			cpg_embedded_context_restore (context->priv->embedded);

			if (!rightobjs)
			{

				continue;
			}

			GSList *rightobj;

			for (rightobj = rightobjs; rightobj; rightobj = g_slist_next (rightobj))
			{
				cpg_layout_add (context->priv->layout,
				                CPG_LAYOUTABLE (cpg_selection_get_object (leftsel)),
				                CPG_LAYOUTABLE (cpg_selection_get_object (rightobj->data)),
				                relation);
			}

			g_slist_foreach (rightobjs, (GFunc)g_object_unref, NULL);
			g_slist_free (rightobjs);
		}

		g_slist_foreach (leftobjs, (GFunc)g_object_unref, NULL);
		g_slist_free (leftobjs);

		cpg_embedded_context_restore (context->priv->embedded);
	}
}

void
cpg_parser_context_add_layout_position (CpgParserContext  *context,
                                        CpgSelector       *selector,
                                        CpgEmbeddedString *x,
                                        CpgEmbeddedString *y,
                                        CpgSelector       *of,
                                        gboolean           cartesian)
{
	GSList *objs;
	GSList *cobjs;
	GSList *obj;
	Context *ctx;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (selector == NULL || CPG_IS_SELECTOR (selector));
	g_return_if_fail (x != NULL);
	g_return_if_fail (y != NULL);
	g_return_if_fail (of == NULL || CPG_IS_SELECTOR (of));

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = context->priv->context_stack->next->data;

	for (cobjs = ctx->objects; cobjs; cobjs = g_slist_next (cobjs))
	{
		CpgSelection *sel;

		sel = cobjs->data;

		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);

		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    sel);

		if (selector != NULL)
		{
			objs = cpg_selector_select (selector,
			                            cpg_selection_get_object (sel),
			                            CPG_SELECTOR_TYPE_OBJECT,
			                            context->priv->embedded);
		}
		else
		{
			objs = g_slist_prepend (NULL, cpg_selection_copy (sel));
		}

		for (obj = objs; obj; obj = g_slist_next (obj))
		{
			gchar const *exx;
			gchar const *exy;
			gint xx;
			gint yy;
			gdouble dx;
			gdouble dy;

			if (!CPG_IS_LAYOUTABLE (cpg_selection_get_object (obj->data)) ||
			    !cpg_layoutable_supports_location (CPG_LAYOUTABLE (cpg_selection_get_object (obj->data))))
			{
				continue;
			}

			cpg_embedded_context_save (context->priv->embedded);

			cpg_embedded_context_add_expansions (context->priv->embedded,
			                                     cpg_selection_get_expansions (obj->data));

			embedded_string_expand (exx, x, context);
			embedded_string_expand (exy, y, context);

			dx = g_strtod (exx, NULL);
			dy = g_strtod (exy, NULL);

			if (!cartesian)
			{
				xx = dx * cos (dy);
				yy = dx * sin (dy);
			}
			else
			{
				xx = (gint)dx;
				yy = (gint)dy;
			}

			if (of)
			{
				GSList *ofobjs;
				GSList *ofobj;
				gint mx = 0;
				gint my = 0;
				gint num = 0;

				ofobjs = cpg_selector_select (of,
				                              cpg_selection_get_object (sel),
				                              CPG_SELECTOR_TYPE_OBJECT,
				                              context->priv->embedded);

				for (ofobj = ofobjs; ofobj; ofobj = g_slist_next (ofobj))
				{
					CpgObject *o;
					gint ox;
					gint oy;

					o = cpg_selection_get_object (ofobj->data);

					if (CPG_IS_LAYOUTABLE (o) &&
					    cpg_layoutable_supports_location (CPG_LAYOUTABLE (o)))
					{
						cpg_layoutable_get_location (CPG_LAYOUTABLE (o),
						                             &ox,
						                             &oy);

						mx += ox;
						my += oy;

						++num;
					}
				}

				if (num > 0)
				{
					mx /= num;
					my /= num;
				}

				xx += mx;
				yy += my;
			}

			cpg_layoutable_set_location (CPG_LAYOUTABLE (cpg_selection_get_object (obj->data)),
			                             xx,
			                             yy);

			cpg_embedded_context_restore (context->priv->embedded);
		}

		cpg_embedded_context_restore (context->priv->embedded);
	}
}

void
cpg_parser_context_add_integrator_property (CpgParserContext  *context,
                                            CpgEmbeddedString *name,
                                            CpgEmbeddedString *value)
{
	gchar const *exname;
	gchar const *exval;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (value != NULL);

	if (context->priv->in_when_applied)
	{
		return;
	}

	embedded_string_expand (exname, name, context);
	embedded_string_expand (exval, value, context);

	if (g_strcmp0 (exname, "method") == 0)
	{
		GType type;
		CpgIntegrator *it;

		type = cpg_integrators_find (exval);

		if (type == G_TYPE_INVALID)
		{
			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_SYNTAX,
			               "Could not find integrator `%s'",
			               exval);

			g_object_unref (name);
			g_object_unref (value);

			return;
		}

		it = g_object_new (type, NULL);
		cpg_network_set_integrator (context->priv->network, it);

		g_object_unref (it);
	}
	else
	{
		/* General purpose */
		GObjectClass *klass;
		CpgIntegrator *it;
		GParamSpec *spec;
		GValue v = {0,};
		GValue dest = {0,};

		it = cpg_network_get_integrator (context->priv->network);
		klass = G_OBJECT_GET_CLASS (it);

		spec = g_object_class_find_property (klass, exname);

		if (spec == NULL)
		{
			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_SYNTAX,
			               "Invalid integrator property `%s' for `%s'",
			               exname,
			               cpg_integrator_get_name (it));

			g_object_unref (name);
			g_object_unref (value);

			return;
		}

		if (!g_value_type_transformable (G_TYPE_STRING, spec->value_type))
		{
			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_SYNTAX,
			               "Could not convert `%s' to `%s'",
			               exval,
			               g_type_name (spec->value_type));

			g_object_unref (name);
			g_object_unref (value);

			return;
		}

		g_value_init (&v, G_TYPE_STRING);
		g_value_set_string (&v, exval);

		g_value_init (&dest, spec->value_type);

		if (!g_value_transform (&v, &dest))
		{
			g_value_unset (&v);

			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_SYNTAX,
			               "Could not convert `%s' to `%s'",
			               exval,
			               g_type_name (spec->value_type));

			g_object_unref (name);
			g_object_unref (value);

			return;
		}

		g_object_set_property (G_OBJECT (it), exname, &v);
		g_value_unset (&v);
	}

	g_object_unref (name);
	g_object_unref (value);
}

/**
 * cpg_parser_context_push_string:
 * @context: A #CpgParserContext
 *
 * Description.
 *
 * Returns: (transfer none): A #CpgEmbeddedString
 *
 **/
CpgEmbeddedString *
cpg_parser_context_push_string (CpgParserContext *context)
{
	CpgEmbeddedString *s;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	s = cpg_embedded_string_new ();

	statement_start (context, s);

	context->priv->strings =
		g_slist_prepend (context->priv->strings, s);

	return s;
}

/**
 * cpg_parser_context_peek_string:
 * @context: A #CpgParserContext
 *
 * Description.
 *
 * Returns: (transfer none): A #CpgEmbeddedString
 *
 **/
CpgEmbeddedString *
cpg_parser_context_peek_string (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (context->priv->strings, NULL);

	return context->priv->strings->data;
}

/**
 * cpg_parser_context_pop_string:
 * @context: A #CpgParserContext
 *
 * Description.
 *
 * Returns: (transfer full): A #CpgEmbeddedString
 *
 **/
CpgEmbeddedString *
cpg_parser_context_pop_string (CpgParserContext *context)
{
	CpgEmbeddedString *s;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (context->priv->strings, NULL);

	s = context->priv->strings->data;

	statement_end (context, s);

	context->priv->strings =
		g_slist_delete_link (context->priv->strings,
		                     context->priv->strings);

	return s;
}

void
cpg_parser_context_push_equation (CpgParserContext *context)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	context->priv->equations =
		g_slist_prepend (context->priv->equations,
		                 GINT_TO_POINTER (1));
}

void
cpg_parser_context_push_equation_depth (CpgParserContext *context)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (context->priv->equations);

	context->priv->equations->data =
		GINT_TO_POINTER (GPOINTER_TO_INT (context->priv->equations->data) + 1);
}

gboolean
cpg_parser_context_pop_equation_depth (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), FALSE);
	g_return_val_if_fail (context->priv->equations, FALSE);

	context->priv->equations->data =
		GINT_TO_POINTER (GPOINTER_TO_INT (context->priv->equations->data) - 1);

	if (GPOINTER_TO_INT (context->priv->equations->data) == 0)
	{
		context->priv->equations =
			g_slist_delete_link (context->priv->equations,
			                     context->priv->equations);

		return TRUE;
	}

	return FALSE;
}

/**
 * cpg_parser_context_get_embedded:
 * @context: A #CpgParserContext
 *
 * Get the embedded context.
 *
 * Returns: (transfer none): A #CpgEmbeddedContext
 *
 **/
CpgEmbeddedContext *
cpg_parser_context_get_embedded (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	return context->priv->embedded;
}

void
cpg_parser_context_set_embedded (CpgParserContext   *context,
                                 CpgEmbeddedContext *embedded)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (embedded));

	if (context->priv->embedded)
	{
		g_object_unref (context->priv->embedded);
	}

	context->priv->embedded =
		cpg_embedded_context_copy_top (embedded);
}

static void
debug_selector (CpgParserContext *context,
                CpgSelection     *selection,
                GSList           *objects)
{
	gchar *fullid;
	GSList *orig = objects;

	fullid = cpg_object_get_full_id (cpg_selection_get_object (selection));

	while (objects)
	{
		gchar *msg;
		CpgSelection *sel;

		sel = objects->data;

		if (CPG_IS_OBJECT (cpg_selection_get_object (sel)))
		{
			msg = cpg_object_get_full_id (cpg_selection_get_object (sel));
		}
		else
		{
			msg = cpg_property_get_full_name (cpg_selection_get_object (sel));
		}

		g_printerr ("[debug] (%d): {%s} => %s\n",
		            CURRENT_INPUT (context)->lineno,
		            fullid,
		            msg);

		g_free (msg);
		objects = g_slist_next (objects);
	}

	g_free (fullid);

	g_slist_foreach (orig, (GFunc)g_object_unref, NULL);
	g_slist_free (orig);
}

void
cpg_parser_context_debug_selector (CpgParserContext *context,
                                   CpgSelector      *selector)
{
	GSList *item;
	Context *ctx;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CPG_IS_SELECTOR (selector));

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);

		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    item->data);

		debug_selector (context,
		                item->data,
		                cpg_selector_select (selector,
		                                     cpg_selection_get_object (item->data),
		                                     CPG_SELECTOR_TYPE_ANY,
		                                     context->priv->embedded));

		cpg_embedded_context_restore (context->priv->embedded);
	}
}

void
cpg_parser_context_debug_string (CpgParserContext  *context,
                                 CpgEmbeddedString *s)
{
	GSList *item;
	Context *ctx;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CPG_IS_EMBEDDED_STRING (s));

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		gchar const *ret;

		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);

		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    item->data);

		embedded_string_expand (ret, s, context);
		g_printerr ("[debug] (%d): %s\n",
		            CURRENT_INPUT (context)->lineno,
		            ret);

		cpg_embedded_context_restore (context->priv->embedded);
	}
}

static gchar *
expansion_as_string (CpgExpansion *expansion)
{
	GString *ret;
	gint i;

	ret = g_string_new ("{");

	for (i = 0; i < cpg_expansion_num (expansion); ++i)
	{
		if (i != 0)
		{
			g_string_append_c (ret, ',');
		}

		g_string_append (ret, cpg_expansion_get (expansion, i));
		g_string_append_printf (ret, ":%d", cpg_expansion_get_index (expansion, i));
	}

	g_string_append_c (ret, '}');

	return g_string_free (ret, FALSE);
}

static gchar *
expansions_as_string (GSList *expansions)
{
	GString *ret;

	ret = g_string_new ("");

	while (expansions)
	{
		gchar *s;

		s = expansion_as_string (expansions->data);
		g_string_append (ret, s);
		g_free (s);

		expansions = g_slist_next (expansions);

		if (expansions)
		{
			g_string_append (ret, ", ");
		}
	}

	return g_string_free (ret, FALSE);
}

static void
define_to_string (gchar const *key,
                  gchar const *value,
                  GString     *ret)
{
	if (ret->len != 0)
	{
		g_string_append (ret, ", ");
	}

	g_string_append_printf (ret, "%s=%s", key, value);
}

static gchar *
defines_as_string (GHashTable *table)
{
	GString *ret;

	ret = g_string_new ("");

	g_hash_table_foreach (table, (GHFunc)define_to_string, ret);
	return g_string_free (ret, FALSE);
}

void
cpg_parser_context_debug_context (CpgParserContext *context)
{
	GSList *item;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->in_when_applied)
	{
		return;
	}

	for (item = CURRENT_CONTEXT (context)->objects; item; item = g_slist_next (item))
	{
		gchar *s;
		CpgSelection *sel;

		sel = item->data;

		s = cpg_object_get_full_id (cpg_selection_get_object (sel));
		g_printerr ("[debug] Selection: %s\n", s);
		g_free (s);

		s = expansions_as_string (cpg_selection_get_expansions (sel));
		g_printerr ("[debug] Expansions: [%s]\n", s);
		g_free (s);

		s = defines_as_string (cpg_selection_get_defines (sel));
		g_printerr ("[debug] Defines: [%s]\n\n", s);
		g_free (s);
	}
}

void
cpg_parser_context_delete_selector (CpgParserContext *context,
                                    CpgSelector      *selector)
{
	GSList *ret;
	GSList *item;
	Context *ctx;
	GSList *oo;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CPG_IS_SELECTOR (selector));

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	for (oo = ctx->objects; oo; oo = g_slist_next (oo))
	{
		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);
		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    oo->data);

		ret = cpg_selector_select (selector,
		                           cpg_selection_get_object (oo->data),
		                           CPG_SELECTOR_TYPE_ANY,
		                           context->priv->embedded);

		for (item = ret; item; item = g_slist_next (item))
		{
			CpgSelection *sel = item->data;
			gpointer obj = cpg_selection_get_object (sel);
			GError *error = NULL;

			if (CPG_IS_PROPERTY (obj))
			{
				CpgObject *parent;

				parent = cpg_property_get_object (obj);

				if (!cpg_object_remove_property (parent,
				                                 cpg_property_get_name (obj),
				                                 &error))
				{
					parser_failed_error (context, error);
					break;
				}
			}
			else if (CPG_IS_LINK_ACTION (obj))
			{
				CpgLink *link;

				link = cpg_link_action_get_link (obj);

				cpg_link_remove_action (link, obj);
			}
			else if (CPG_IS_OBJECT (obj))
			{
				CpgObject *parent;

				parent = cpg_object_get_parent (obj);

				if (!cpg_group_remove (CPG_GROUP (parent), obj, &error))
				{
					parser_failed_error (context, error);
					break;
				}
			}
		}

		cpg_embedded_context_restore (context->priv->embedded);

		g_slist_foreach (ret, (GFunc)g_object_unref, NULL);
		g_slist_free (ret);
	}
}

static gchar **
multiexpand_to_strv (GSList *expansions)
{
	GPtrArray *ret;

	ret = g_ptr_array_new ();

	while (expansions)
	{
		g_ptr_array_add (ret, (gchar *)cpg_expansion_get (expansions->data, 0));
		expansions = g_slist_next (expansions);
	}

	g_ptr_array_add (ret, NULL);
	return (gchar **)g_ptr_array_free (ret, FALSE);
}

static gboolean
any_multiexpand_true (GSList *expansions)
{
	while (expansions)
	{
		gchar const *n;
		gchar *endptr;
		gdouble r;

		n = cpg_expansion_get (expansions->data, 0);

		if (*n)
		{
			r = g_ascii_strtod (n, &endptr);

			if (!*endptr)
			{
				if (r != 0)
				{
					return TRUE;
				}
			}
			else
			{
				return TRUE;
			}
		}

		expansions = g_slist_next (expansions);
	}

	return FALSE;
}

void
cpg_parser_context_set_input_file_setting (CpgParserContext  *context,
                                           CpgEmbeddedString *name,
                                           CpgEmbeddedString *value)
{
	Context *ctx;
	GSList *obj;

	gboolean ret = TRUE;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CPG_IS_EMBEDDED_STRING (name));
	g_return_if_fail (CPG_IS_EMBEDDED_STRING (value));

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	for (obj = ctx->objects; obj; obj = g_slist_next (obj))
	{
		CpgInputFile *f;
		GSList *names;
		GSList *nm;

		f = cpg_selection_get_object (obj->data);

		cpg_embedded_context_save_defines (context->priv->embedded, TRUE);
		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    obj->data);

		embedded_string_expand_multiple (names, name, context);

		for (nm = names; nm; nm = g_slist_next (nm))
		{
			gchar const *n;
			GSList *values;

			cpg_embedded_context_save (context->priv->embedded);
			cpg_embedded_context_add_expansion (context->priv->embedded,
			                                    nm->data);

			embedded_string_expand_multiple (values, value, context);

			cpg_embedded_context_restore (context->priv->embedded);

			n = cpg_expansion_get (nm->data, 0);

			if (g_strcmp0 (n, "columns") == 0)
			{
				gchar **r;

				r = multiexpand_to_strv (values);

				cpg_input_file_set_columns (f, (gchar const * const *)r);

				g_free (r);
			}
			else if (g_strcmp0 (n, "repeat") == 0)
			{
				cpg_input_file_set_repeat (f, any_multiexpand_true (values));
			}
			else if (g_strcmp0 (n, "time") == 0)
			{
				gint c = (gint)g_ascii_strtoll (values->data, NULL, 10);

				cpg_input_file_set_time_column (f, c);
			}
			else
			{
				parser_failed (context,
				               CPG_NETWORK_LOAD_ERROR_OBJECT,
				               "The input file setting `%s' does not exist",
				               n);

				ret = FALSE;
			}

			g_slist_foreach (values, (GFunc)g_object_unref, NULL);
			g_slist_free (values);

			if (!ret)
			{
				break;
			}
		}

		g_slist_foreach (names, (GFunc)g_object_unref, NULL);
		g_slist_free (names);

		cpg_embedded_context_restore (context->priv->embedded);

		if (!ret)
		{
			break;
		}
	}
}

/**
 * cpg_parser_context_current_selections:
 * @context: A #CpgParserContext
 *
 * Description.
 *
 * Returns: (element-type CpgSelection) (transfer none): A #GSList
 *
 **/
GSList const *
cpg_parser_context_current_selections (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	if (!context->priv->context_stack)
	{
		return NULL;
	}

	return CURRENT_CONTEXT (context)->objects;
}

/**
 * cpg_parser_context_previous_selections:
 * @context: A #CpgParserContext
 *
 * Description.
 *
 * Returns: (element-type CpgSelection) (transfer none): A #GSList
 *
 **/
GSList const *
cpg_parser_context_previous_selections (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	if (!context->priv->context_stack || !context->priv->context_stack->next)
	{
		return NULL;
	}

	return ((Context *)context->priv->context_stack->next->data)->objects;
}

void
cpg_parser_context_begin_selector_item (CpgParserContext *context)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	statement_start (context, ensure_selector (context));
}

void
cpg_parser_context_get_error_location (CpgParserContext *context,
                                       gint             *lstart,
                                       gint             *lend,
                                       gint             *cstart,
                                       gint             *cend)
{
	CpgStatement *st;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	st = context->priv->error_statement;

	if (st)
	{
		cpg_statement_get_line (st, lstart, lend);
		cpg_statement_get_column (st, cstart, cend);
	}
	else
	{
		cpg_parser_context_get_line (context, lstart);
		cpg_parser_context_get_line (context, lend);

		cpg_parser_context_get_column (context, cstart, cend);
	}
}

/**
 * cpg_parser_context_get_error_lines:
 * @context: A #CpgParserContext
 *
 * Get the lines of text on which the error occurred.
 *
 * Returns: (transfer full): The lines on which the error occurred
 *
 **/
gchar *
cpg_parser_context_get_error_lines (CpgParserContext *context)
{
	gint lstart;
	gint lend;
	gint cstart;
	gint cend;
	GString *ret;
	gboolean first;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	cpg_parser_context_get_error_location (context,
	                                       &lstart,
	                                       &lend,
	                                       &cstart,
	                                       &cend);

	ret = g_string_new ("");
	first = TRUE;

	while (lstart <= lend)
	{
		gchar const *line;

		line = cpg_parser_context_get_line_at (context, lstart);

		if (!first)
		{
			g_string_append_c (ret, '\n');
		}
		else
		{
			first = FALSE;
		}

		g_string_append (ret, line);

		++lstart;
	}

	return g_string_free (ret, FALSE);
}

static void
apply_unapply_template (CpgParserContext *context,
                        CpgSelector      *templates,
                        CpgSelector      *targets,
                        gboolean          apply)
{
	Context *ctx;
	GSList *obj;
	CpgGroup *template_group;
	gboolean ret = TRUE;

	if (context->priv->in_when_applied)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	template_group = cpg_network_get_template_group (context->priv->network);

	for (obj = ctx->objects; obj; obj = g_slist_next (obj))
	{
		GSList *temps;
		GSList *temp;

		cpg_embedded_context_save_defines (context->priv->embedded,
		                                   FALSE);

		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    obj->data);

		/* Select templates now */
		temps = cpg_selector_select (templates,
		                             G_OBJECT (template_group),
		                             CPG_SELECTOR_TYPE_TEMPLATE,
		                             context->priv->embedded);

		if (!temps)
		{
			cpg_embedded_context_restore (context->priv->embedded);
			continue;
		}

		for (temp = temps; temp; temp = g_slist_next (temp))
		{
			GSList *targobjs;
			GSList *targobj;
			gboolean freetargs;
			gpointer templobj;

			templobj = cpg_selection_get_object (temp->data);

			if (!targets)
			{
				targobjs = g_slist_prepend (NULL, obj->data);
				freetargs = FALSE;
			}
			else
			{
				cpg_embedded_context_save_defines (context->priv->embedded,
				                                   FALSE);

				targobjs = cpg_selector_select (targets,
				                                cpg_selection_get_object (obj->data),
				                                CPG_SELECTOR_TYPE_OBJECT,
				                                context->priv->embedded);

				cpg_embedded_context_restore (context->priv->embedded);
				freetargs = TRUE;
			}

			for (targobj = targobjs; targobj; targobj = g_slist_next (targobj))
			{
				CpgSelection *s;
				GError *error = NULL;
				gboolean ret;

				s = targobj->data;

				if (apply)
				{
					ret = cpg_object_apply_template (cpg_selection_get_object (s),
					                                 templobj,
					                                 &error);
				}
				else
				{
					ret = cpg_object_unapply_template (cpg_selection_get_object (s),
					                                   templobj,
					                                   &error);
				}

				if (!ret)
				{
					parser_failed_error (context, error);
					break;
				}
			}

			if (freetargs)
			{
				g_slist_foreach (targobjs, (GFunc)g_object_unref, NULL);
			}

			g_slist_free (targobjs);

			if (!ret)
			{
				break;
			}
		}

		g_slist_foreach (temps, (GFunc)g_object_unref, NULL);
		g_slist_free (temps);

		if (!ret)
		{
			break;
		}
	}
}

void
cpg_parser_context_apply_template (CpgParserContext *context,
                                   CpgSelector      *templates,
                                   CpgSelector      *targets)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CPG_IS_SELECTOR (templates));
	g_return_if_fail (targets == NULL || CPG_IS_SELECTOR (targets));

	if (context->priv->in_when_applied)
	{
		return;
	}

	apply_unapply_template (context, templates, targets, TRUE);
}

void
cpg_parser_context_unapply_template (CpgParserContext *context,
                                     CpgSelector      *templates,
                                     CpgSelector      *targets)

{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CPG_IS_SELECTOR (templates));
	g_return_if_fail (targets == NULL || CPG_IS_SELECTOR (targets));

	if (context->priv->in_when_applied)
	{
		return;
	}

	apply_unapply_template (context, templates, targets, FALSE);
}

void
cpg_parser_context_set_when_applied (CpgParserContext  *context,
                                     gboolean           apply,
                                     GSList            *attributes)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	context->priv->in_when_applied = TRUE;
	context->priv->when_applied = apply;
	context->priv->when_applied_attributes = attributes;
	context->priv->when_applied_text = g_string_new ("");
}

void
cpg_parser_context_unset_when_applied (CpgParserContext *context)
{
	gchar *code;
	Context *ctx;
	GSList *objects;
	GSList *item;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	context->priv->in_when_applied = FALSE;

	g_string_erase (context->priv->when_applied_text,
	                context->priv->when_applied_text->len - 1,
	                1);

	code = g_string_free (context->priv->when_applied_text, FALSE);

	ctx = CURRENT_CONTEXT (context);

	objects = each_selections (context,
	                           ctx->objects,
	                           context->priv->when_applied_attributes,
	                           CPG_SELECTOR_TYPE_ANY,
	                           NULL,
	                           NULL,
	                           FALSE);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		CpgWhenApplied *applied;
		gpointer obj;

		obj = cpg_selection_get_object (item->data);

		if (!CPG_IS_OBJECT (obj))
		{
			continue;
		}

		cpg_embedded_context_save_defines (context->priv->embedded,
		                                   FALSE);

		cpg_embedded_context_set_selection (context->priv->embedded,
		                                    item->data);

		applied = cpg_when_applied_new (context->priv->embedded,
		                                code,
		                                context->priv->when_applied);

		cpg_object_add_when_applied (obj, applied);
		g_object_unref (applied);

		cpg_embedded_context_restore (context->priv->embedded);
	}

	g_slist_foreach (objects, (GFunc)g_object_unref, NULL);
	g_slist_free (objects);

	g_free (code);
}

void
cpg_parser_context_remove_record (CpgParserContext *context,
                                  gint              len,
                                  gint              offset)

{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (!context->priv->in_when_applied)
	{
		return;
	}

	g_string_erase (context->priv->when_applied_text,
	                context->priv->when_applied_text->len - len - offset,
	                len);
}
