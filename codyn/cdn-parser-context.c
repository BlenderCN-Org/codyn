/*
 * cdn-parser-context.c
 * This file is part of codyn
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

#include "cdn-parser-context.h"
#include "cdn-network-parser-utils.h"
#include "cdn-integrators.h"
#include "cdn-parser.h"
#include "cdn-annotatable.h"
#include "cdn-expansion-context.h"
#include "cdn-selection.h"
#include "cdn-expansion.h"
#include "cdn-layoutable.h"
#include "cdn-statement.h"
#include "cdn-marshal.h"
#include "cdn-phaseable.h"
#include "cdn-io-method.h"

#include <math.h>
#include <string.h>

#ifdef MINGW
#define RANDOM rand
#else
#define RANDOM random
#endif

#define CDN_PARSER_CONTEXT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_PARSER_CONTEXT, CdnParserContextPrivate))

#define embedded_string_expand(ret,s,parser)					\
do										\
{										\
	GError *__err = NULL;							\
	ret = cdn_embedded_string_expand (s, expansion_context_peek (parser), &__err);	\
										\
	if (!ret)								\
	{									\
		parser_failed_error (parser, CDN_STATEMENT (s), __err);		\
		return;								\
	}									\
}										\
while (0);

#define embedded_string_expand_val(ret,s,parser,retval)				\
do										\
{										\
	GError *__err = NULL;							\
	ret = cdn_embedded_string_expand (s, expansion_context_peek (parser), &__err);	\
										\
	if (!ret)								\
	{									\
		parser_failed_error (parser, CDN_STATEMENT (s), __err);		\
		return retval;							\
	}									\
}										\
while (0);

#define embedded_string_expand_multiple(ret,s,parser)				\
do										\
{										\
	GError *__err = NULL;							\
	ret = cdn_embedded_string_expand_multiple (s, expansion_context_peek (parser), &__err);	\
										\
	if (!ret && __err)							\
	{									\
		parser_failed_error (parser, CDN_STATEMENT (s), __err);		\
		return;								\
	}									\
}										\
while (0);

#define embedded_string_expand_multiple_val(ret,s,parser,retval)		\
do										\
{										\
	GError *__err = NULL;							\
	ret = cdn_embedded_string_expand_multiple (s, expansion_context_peek (parser), &__err);	\
										\
	if (!ret && __err)							\
	{									\
		parser_failed_error (parser, CDN_STATEMENT (s), __err);		\
		return retval;							\
	}									\
}										\
while (0);

void cdn_parser_lex_destroy (gpointer scanner);
void cdn_parser_lex_init_extra (gpointer context, gpointer *scanner);
void cdn_parser_tokens_push_input (gpointer scanner);

int cdn_parser_parse (gpointer context);

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
	gboolean firsteof;
} InputItem;

typedef struct
{
	GSList *objects;
} Context;

struct _CdnParserContextPrivate
{
	CdnNetwork *network;
	GSList *inputs;
	gpointer scanner;
	gint start_token;

	CdnEmbeddedString *annotation;

	/* Stack of Context */
	GSList *context_stack;
	GSList *selectors;

	Context *is_template;
	GSList *context;

	GSList *strings;
	GSList *equations;

	GError *error;
	CdnStatement *error_statement;

	CdnLayout *layout;
	GHashTable *files;

	guint in_event_handler : 1;
	guint error_occurred : 1;
};

enum
{
	CONTEXT_PUSHED,
	CONTEXT_POPPED,
	SELECTOR_ITEM_PUSHED,
	FILE_USED,
	NUM_SIGNALS
};

static guint signals[NUM_SIGNALS];

G_DEFINE_TYPE (CdnParserContext, cdn_parser_context, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NETWORK,
	PROP_FILE,
	PROP_STREAM
};

#define CONTEXT(x) ((Context *)x)
#define CURRENT_CONTEXT(context) (context->priv->context_stack ? CONTEXT(context->priv->context_stack->data) : NULL)

struct _CdnFunctionArgumentSpec
{
	CdnEmbeddedString *name;
	gboolean isexplicit;
	CdnEmbeddedString *default_value;
};

struct _CdnFunctionPolynomialPieceSpec
{
	CdnEmbeddedString *from;
	CdnEmbeddedString *to;

	GSList *coefficients;
};

/**
 * cdn_function_polynomial_piece_spec_new: (skip):
 * @from: A #CdnEmbeddedString
 * @to: A #CdnEmbeddedString
 * @coefficients: (element-type CdnEmbeddedString): A #GSList of #CdnEmbeddedString
 *
 * Create a new function polynomial piece specification.
 *
 * Returns: (transfer full): A #CdnFunctionPolynomialPieceSpec
 *
 **/
CdnFunctionPolynomialPieceSpec *
cdn_function_polynomial_piece_spec_new (CdnEmbeddedString *from,
                                        CdnEmbeddedString *to,
                                        GSList            *coefficients)
{
	CdnFunctionPolynomialPieceSpec *ret;

	ret = g_slice_new0 (CdnFunctionPolynomialPieceSpec);

	ret->from = from;
	ret->to = to;
	ret->coefficients = coefficients;

	return ret;
}

void
cdn_function_polynomial_piece_spec_free (CdnFunctionPolynomialPieceSpec *spec)
{
	if (spec->from)
	{
		g_object_unref (spec->from);
	}

	if (spec->to)
	{
		g_object_unref (spec->to);
	}

	g_slist_foreach (spec->coefficients, (GFunc)g_object_unref, NULL);
	g_slist_free (spec->coefficients);

	g_slice_free (CdnFunctionPolynomialPieceSpec, spec);
}

/**
 * cdn_function_argument_spec_new: (skip):
 * @name: A #CdnEmbeddedString
 * @isexplicit: whether the argument is explicit
 *
 * Create a new function argument specification.
 *
 * Returns: (transfer full): A #CdnFunctionArgumentSpec
 *
 **/
CdnFunctionArgumentSpec *
cdn_function_argument_spec_new (CdnEmbeddedString *name,
                                gboolean           isexplicit,
                                CdnEmbeddedString *default_value)
{
	CdnFunctionArgumentSpec *ret;

	ret = g_slice_new0 (CdnFunctionArgumentSpec);
	ret->name = name;
	ret->isexplicit = isexplicit;
	ret->default_value = default_value;

	return ret;
}

void
cdn_function_argument_spec_free (CdnFunctionArgumentSpec *self)
{
	if (self->name)
	{
		g_object_unref (self->name);
	}

	if (self->default_value)
	{
		g_object_unref (self->default_value);
	}

	g_slice_free (CdnFunctionArgumentSpec, self);
}

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

	ret->firsteof = TRUE;

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
	g_slist_foreach (context->objects, (GFunc)cdn_selection_unref, NULL);
	g_slist_free (context->objects);

	g_slice_free (Context, context);
}

static void
cdn_parser_context_finalize (GObject *object)
{
	CdnParserContext *self;

	self = CDN_PARSER_CONTEXT (object);

	while (self->priv->context_stack)
	{
		cdn_parser_context_pop (self);
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

	cdn_parser_lex_destroy (self->priv->scanner);

	if (self->priv->annotation)
	{
		g_object_unref (self->priv->annotation);
	}

	while (self->priv->context)
	{
		cdn_expansion_context_unref (self->priv->context->data);

		self->priv->context =
			g_slist_delete_link (self->priv->context,
			                     self->priv->context);
	}

	g_hash_table_destroy (self->priv->files);

	G_OBJECT_CLASS (cdn_parser_context_parent_class)->finalize (object);
}

static void
cdn_parser_context_set_property (GObject      *object,
                                 guint         prop_id,
                                 const GValue *value,
                                 GParamSpec   *pspec)
{
	CdnParserContext *self = CDN_PARSER_CONTEXT (object);

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
cdn_parser_context_get_property (GObject    *object,
                                 guint       prop_id,
                                 GValue     *value,
                                 GParamSpec *pspec)
{
	CdnParserContext *self = CDN_PARSER_CONTEXT (object);

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
cdn_parser_context_constructed (GObject *object)
{
	CdnParserContext *self;

	self = CDN_PARSER_CONTEXT (object);

	cdn_parser_context_push_network (self);

	cdn_parser_lex_init_extra (self, &(self->priv->scanner));
}

static void
cdn_parser_context_class_init (CdnParserContextClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_parser_context_finalize;

	object_class->get_property = cdn_parser_context_get_property;
	object_class->set_property = cdn_parser_context_set_property;
	object_class->constructed = cdn_parser_context_constructed;

	g_type_class_add_private (object_class, sizeof(CdnParserContextPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_NETWORK,
	                                 g_param_spec_object ("network",
	                                                      "Network",
	                                                      "Network",
	                                                      CDN_TYPE_NETWORK,
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
		              CDN_TYPE_SELECTOR);

	signals[FILE_USED] =
		g_signal_new ("file-used",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL,
		              NULL,
		              cdn_marshal_VOID__OBJECT_STRING,
		              G_TYPE_NONE,
		              2,
		              G_TYPE_FILE,
		              G_TYPE_STRING);
}

static void
cdn_parser_context_init (CdnParserContext *self)
{
	self->priv = CDN_PARSER_CONTEXT_GET_PRIVATE (self);

	self->priv->start_token = T_START_DOCUMENT;

	self->priv->files = g_hash_table_new_full (g_file_hash,
	                                           (GEqualFunc)g_file_equal,
	                                           (GDestroyNotify)g_object_unref,
	                                           NULL);

	self->priv->context = g_slist_prepend (NULL,
	                                       cdn_expansion_context_new (NULL));
}

CdnParserContext *
cdn_parser_context_new (CdnNetwork *network)
{
	return g_object_new (CDN_TYPE_PARSER_CONTEXT,
	                     "network", network,
	                     NULL);
}

static void
statement_start (CdnParserContext *context,
                 gpointer          obj)
{
	CdnStatement *st;
	InputItem *inp;

	if (!CDN_IS_STATEMENT (obj))
	{
		return;
	}

	st = CDN_STATEMENT (obj);
	inp = CURRENT_INPUT (context);

	cdn_statement_set_line (st, inp->lineno, inp->lineno);
	cdn_statement_set_column (st, inp->cstart, inp->cend);
}

static void
statement_end (CdnParserContext *context,
               gpointer          obj)
{
	CdnStatement *st;
	InputItem *inp;
	gint lstart;
	gint cstart;

	if (!CDN_IS_STATEMENT (obj))
	{
		return;
	}

	st = CDN_STATEMENT (obj);
	inp = CURRENT_INPUT (context);

	cdn_statement_get_line (st, &lstart, NULL);
	cdn_statement_get_column (st, &cstart, NULL);

	cdn_statement_set_line (st, lstart, inp->lineno);
	cdn_statement_set_column (st, cstart, inp->cend);
}

static gboolean
parser_failed_error (CdnParserContext *context,
                     CdnStatement     *statement,
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
	context->priv->in_event_handler = 0;

	return FALSE;
}

static gboolean
parser_failed (CdnParserContext *context,
               CdnStatement     *statement,
               gint              code,
               gchar const      *format,
               ...)
{
	if (context->priv->error == NULL)
	{
		va_list ap;
		GError *error;

		va_start (ap, format);

		error = g_error_new_valist (CDN_NETWORK_LOAD_ERROR,
		                            code,
		                            format,
		                            ap);

		parser_failed_error (context,
		                     statement,
		                     error);

		va_end (ap);
	}

	return FALSE;
}

static gchar const *
current_annotation (CdnParserContext *context)
{
	if (!context->priv->annotation)
	{
		return NULL;
	}

	return cdn_embedded_string_expand (context->priv->annotation,
	                                   context->priv->context->data,
	                                   NULL);
}

static void
clear_annotation (CdnParserContext *context)
{
	if (context->priv->annotation)
	{
		g_object_unref (context->priv->annotation);
		context->priv->annotation = NULL;
	}
}

static GSList *
find_attributes (GSList *attributes,
                gchar const *name)
{
	GSList *ret = NULL;

	while (attributes)
	{
		if (g_strcmp0 (name, cdn_attribute_get_id (attributes->data)) == 0)
		{
			ret = g_slist_prepend (ret, attributes->data);
		}

		attributes = g_slist_next (attributes);
	}

	return g_slist_reverse (ret);
}

static CdnAttribute *
find_attribute (GSList *attributes,
                gchar const *name)
{
	GSList *all;
	CdnAttribute *ret = NULL;

	all = find_attributes (attributes, name);

	if (all)
	{
		ret = all->data;
		g_slist_free (all);
	}

	return ret;
}

static GSList *
each_selections (CdnParserContext *context,
                 gboolean  newctx)
{
	GSList *selection;
	GSList *ret = NULL;
	Context *ctx;

	ctx = CURRENT_CONTEXT (context);

	if (!ctx)
	{
		return NULL;
	}

	for (selection = ctx->objects; selection; selection = g_slist_next (selection));
	{
		if (newctx)
		{
			ret = g_slist_prepend (ret,
			                       cdn_selection_copy (selection->data));
		}
		else
		{
			CdnExpansionContext *ctx;
			gpointer obj;

			ctx = cdn_selection_get_context (selection->data);
			obj = cdn_selection_get_object (selection->data);

			ret = g_slist_prepend (ret,
			                       cdn_selection_new (obj,
			                                          ctx));
		}
	}

	return g_slist_reverse (ret);
}

static CdnExpansionContext *
expansion_context_peek (CdnParserContext *context)
{
	if (context->priv->context)
	{
		return context->priv->context->data;
	}
	else
	{
		return NULL;
	}
}

static CdnExpansionContext *
expansion_context_push (CdnParserContext    *context,
                        CdnExpansionContext *ctx)
{
	CdnExpansionContext *ret;

	ret = cdn_expansion_context_new (ctx);

	context->priv->context = g_slist_prepend (context->priv->context, ret);
	return ret;
}

static CdnExpansionContext *
expansion_context_push_base (CdnParserContext *context)
{
	return expansion_context_push (context,
	                               expansion_context_peek (context));
}

static CdnExpansionContext *
expansion_context_push_selection (CdnParserContext *context,
                                  CdnSelection     *selection)
{
	return expansion_context_push (context,
	                               selection ? cdn_selection_get_context (selection) : NULL);
}

static void
expansion_context_pop (CdnParserContext *context)
{
	if (context->priv->context)
	{
		cdn_expansion_context_unref (context->priv->context->data);

		context->priv->context =
			g_slist_delete_link (context->priv->context,
			                     context->priv->context);
	}
}

typedef struct
{
	CdnExpansion *name;
	CdnExpansion *value;
} NameValuePair;

static NameValuePair *
name_value_pair_new (CdnExpansion *name,
                     CdnExpansion *value)
{
	NameValuePair *ret;

	ret = g_slice_new0 (NameValuePair);

	ret->name = cdn_expansion_copy (name);

	if (value)
	{
		ret->value = cdn_expansion_copy (value);
	}

	return ret;
}

static void
name_value_pair_free (NameValuePair *self)
{
	cdn_expansion_unref (self->name);
	cdn_expansion_unref (self->value);

	g_slice_free (NameValuePair, self);
}

static gchar const *
name_from_selection (CdnSelection *selection)
{
	gpointer obj;

	obj = cdn_selection_get_object (selection);

	if (CDN_IS_OBJECT (obj))
	{
		return cdn_object_get_id (obj);
	}
	else if (CDN_IS_VARIABLE (obj))
	{
		return cdn_variable_get_name (obj);
	}
	else if (CDN_IS_EDGE_ACTION (obj))
	{
		return cdn_edge_action_get_target (obj);
	}

	return NULL;
}

static GSList *
generate_expand_multival (CdnParserContext *context,
                          CdnSelection     *sel,
                          GObject          *value,
                          gboolean         *ismulti)
{
	GSList *values = NULL;
	GSList *sels;
	CdnExpansion *origex;

	if (CDN_IS_EMBEDDED_STRING (value))
	{
		embedded_string_expand_multiple_val (values,
		                                     CDN_EMBEDDED_STRING (value),
		                                     context,
		                                     NULL);

		*ismulti = values && cdn_expansion_num (values->data) > 1;

		return values;
	}

	origex = cdn_expansion_context_get_expansion (expansion_context_peek (context),
	                                              0);

	sels = cdn_selector_select (CDN_SELECTOR (value),
	                            cdn_selection_get_object (sel),
	                            CDN_SELECTOR_TYPE_ANY,
	                            expansion_context_peek (context));

	*ismulti = TRUE;

	if (!sels)
	{
		values = g_slist_prepend (NULL,
		                          cdn_expansion_new_one (""));
	}

	while (sels)
	{
		CdnExpansionContext *ctx;
		CdnExpansion *ex;

		ctx = cdn_selection_get_context (sels->data);
		ex = cdn_expansion_context_get_expansion (ctx, 0);

		if (ex && ex != origex)
		{
			values = g_slist_prepend (values,
			                          cdn_expansion_ref (ex));
		}
		else
		{
			gchar const *nm;

			nm = name_from_selection (sels->data);

			if (nm)
			{
				values = g_slist_prepend (values,
				                          cdn_expansion_new_one (nm));
			}
		}

		cdn_selection_unref (sels->data);
		sels = g_slist_delete_link (sels, sels);
	}

	return g_slist_reverse (values);

}

static gchar *
generate_unexpanded (CdnParserContext  *context,
                     GObject           *value,
                     GSList            *values)
{
	GString *sret;
	GSList *item;

	if (CDN_IS_EMBEDDED_STRING (value))
	{
		GError *error = NULL;
		gchar *expanded;

		expanded = cdn_embedded_string_expand_escape (CDN_EMBEDDED_STRING (value),
		                                              expansion_context_peek (context),
		                                              &error);

		if (!expanded)
		{
			parser_failed_error (context,
			                     CDN_STATEMENT (value),
			                     error);
		}

		return expanded;
	}

	sret = g_string_new ("{");

	for (item = values; item; item = g_slist_next (item))
	{
		gchar *escaped;

		if (item != values)
		{
			g_string_append_c (sret, ',');
		}

		escaped = cdn_embedded_string_escape (cdn_expansion_get (item->data, 0));
		g_string_append (sret, escaped);
		g_free (escaped);
	}

	g_string_append_c (sret, '}');
	return g_string_free (sret, FALSE);
}

static CdnExpansion *
generate_unexpanded_expansion (gchar const *s,
                               GSList      *values)
{
	GPtrArray *ptr;
	gchar **p;
	CdnExpansion *ret;

	ptr = g_ptr_array_new ();
	g_ptr_array_add (ptr, (gpointer)s);

	while (values)
	{
		g_ptr_array_add (ptr, (gpointer)cdn_expansion_get (values->data, 0));
		values = g_slist_next (values);
	}

	g_ptr_array_add (ptr, NULL);
	p = (gchar **)g_ptr_array_free (ptr, FALSE);

	ret = cdn_expansion_new ((gchar const * const *)p);

	g_free (p);

	return ret;
}

static GSList *
generate_name_value_pairs (CdnParserContext  *context,
                           CdnSelection      *sel,
                           CdnEmbeddedString *name,
                           gpointer           value,
                           gboolean           single_unexpanded)
{
	GSList *names;
	GSList *nameit;
	gint cnt = 0;
	gint i;
	GSList *ret = NULL;
	gboolean nameismulti;
	GSList *valuesmulti = NULL;

	if (context->priv->in_event_handler)
	{
		return NULL;
	}

	if (sel)
	{
		expansion_context_push (context,
		                        cdn_selection_get_context (sel));
	}
	else
	{
		expansion_context_push_base (context);
	}

	embedded_string_expand_multiple_val (names, name, context, NULL);

	nameismulti = names && cdn_expansion_num (names->data) > 1;

	i = -1;

	for (nameit = names; nameit; nameit = g_slist_next (nameit))
	{
		gchar const *exname;
		GSList *values;
		gboolean valueismulti = FALSE;
		gboolean hascontext = FALSE;

		if (!value)
		{
			ret = g_slist_prepend (ret,
			                       name_value_pair_new (nameit->data,
			                                            NULL));

			continue;
		}

		++i;

		exname = cdn_expansion_get (nameit->data, 0);

		if (!valuesmulti)
		{
			CdnExpansionContext *pctx;

			pctx = expansion_context_push_base (context);
			cdn_expansion_context_shared_defines (pctx,
			                                      cdn_selection_get_context (sel));

			cdn_expansion_context_add_expansion (pctx,
			                                     nameit->data);

			values = generate_expand_multival (context,
			                                   sel,
			                                   value,
			                                   &valueismulti);

			if (!values)
			{
				g_slist_foreach (names, (GFunc)cdn_expansion_unref, NULL);
				g_slist_free (names);

				expansion_context_pop (context);
				return NULL;
			}

			hascontext = TRUE;

			if (!single_unexpanded && !values->next)
			{
				valueismulti = FALSE;
			}

			if (!values)
			{
				expansion_context_pop (context);
				continue;
			}
		}
		else
		{
			valueismulti = TRUE;
			values = valuesmulti;
		}

		if (!valueismulti)
		{
			// value is single
			ret = g_slist_prepend (ret,
			                       name_value_pair_new (nameit->data,
			                                            values->data));
		}
		else if (nameismulti && valueismulti)
		{
			// name and value are multi and need to be the same
			// size
			if (valuesmulti || g_slist_length (values) == g_slist_length (names))
			{
				if (!valuesmulti)
				{
					valuesmulti = values;
				}

				ret = g_slist_prepend (ret,
				                       name_value_pair_new (nameit->data,
				                                            valuesmulti->data));

				++cnt;
			}
			else
			{
				parser_failed (context,
				               CDN_STATEMENT (name),
				               CDN_NETWORK_LOAD_ERROR_SYNTAX,
				               "Number of names (%d) does not match number of values (%d)",
				               g_slist_length (names),
				               g_slist_length (values));

				break;
			}
		}
		else
		{
			// name is single, but value is multi
			GSList *item;
			gint num = 0;
			gchar *unex;
			CdnExpansion *ex;

			unex = generate_unexpanded (context,
			                            value,
			                            values);

			ex = generate_unexpanded_expansion (unex,
			                                    values);

			g_free (unex);

			ret = g_slist_prepend (ret,
			                       name_value_pair_new (names->data,
			                                            ex));

			cdn_expansion_unref (ex);

			if (!single_unexpanded)
			{
				for (item = values; item; item = g_slist_next (item))
				{
					gchar *name;
					gchar *nums;

					nums = g_strdup_printf ("%d", ++num);
					name = g_strconcat (exname, nums, NULL);

					gchar const *cc[] = {
						name,
						exname,
						nums,
						NULL
					};

					ex = cdn_expansion_new ((gchar const * const *)cc);

					ret = g_slist_prepend (ret,
					                       name_value_pair_new (ex,
					                                            item->data));

					cdn_expansion_unref (ex);
					g_free (name);
					g_free (nums);
				}

				cnt += num;
			}
			else
			{
				++cnt;
			}
		}

		if (hascontext)
		{
			expansion_context_pop (context);
		}

		if (valuesmulti)
		{
			cdn_expansion_unref (valuesmulti->data);

			valuesmulti = g_slist_delete_link (valuesmulti,
			                                   valuesmulti);
		}
		else
		{
			g_slist_foreach (values, (GFunc)cdn_expansion_unref, NULL);
			g_slist_free (values);
		}
	}

	expansion_context_pop (context);

	g_slist_foreach (names, (GFunc)cdn_expansion_unref, NULL);
	g_slist_free (names);

	return g_slist_reverse (ret);
}

static gboolean
add_variable_diff (CdnParserContext  *context,
                   CdnNode           *obj,
                   gchar const       *name,
                   gchar const       *dotname,
                   gint               order,
                   NameValuePair     *p,
                   CdnVariableFlags   add_flags,
                   CdnVariableFlags   remove_flags,
                   CdnEmbeddedString *constraint)
{
	gint i;
	CdnVariable *prev = NULL;

	for (i = 0; i < order; ++i)
	{
		gchar *dd;
		gchar *fname;
		gchar *dfname;
		gchar const *ex;
		CdnVariable *prop;
		CdnEdge *link;
		GError *error = NULL;

		dd = g_strnfill (i + 1, 'd');
		dfname = g_strconcat (dd, dotname, NULL);

		dd[i] = '\0';
		fname = g_strconcat (dd, dotname, NULL);

		g_free (dd);

		// Integrate 'name' on obj
		prop = cdn_object_get_variable (CDN_OBJECT (obj), fname);
		ex = cdn_expansion_get (p->value, 0);

		if (!prop)
		{
			if (!cdn_object_add_variable (CDN_OBJECT (obj),
			                              cdn_variable_new (fname,
			                                                cdn_expression_new0 (),
			                                                0),
			                              &error))
			{
				parser_failed_error (context, NULL, error);
				return FALSE;
			}

			prop = cdn_object_get_variable (CDN_OBJECT (obj), fname);
		}

		cdn_variable_set_flags (prop, (CDN_VARIABLE_FLAG_INTEGRATED | add_flags) & ~remove_flags);

		if (prev)
		{
			cdn_variable_set_derivative (prev, prop);
		}

		prev = prop;

		// Find the self link generated
		link = cdn_node_get_self_edge (obj);

		if (i == order - 1)
		{
			cdn_edge_add_action (link,
			                     cdn_edge_action_new (fname,
			                                          cdn_expression_new (ex)));
		}
		else
		{
			cdn_edge_add_action (link,
			                     cdn_edge_action_new (fname,
			                                          cdn_expression_new (dfname)));
		}

		if (constraint)
		{
			gchar const *cons;
			CdnExpansion *ex;
			CdnExpansionContext *pctx;

			ex = cdn_expansion_new_one (cdn_variable_get_name (prop));

			pctx = expansion_context_push_base (context);

			cdn_expansion_context_add_expansion (pctx, ex);
			cdn_expansion_context_add_expansion (pctx, p->value);

			embedded_string_expand_val (cons, constraint, context, FALSE);

			cdn_variable_set_constraint (prop,
			                             cdn_expression_new (cons));

			expansion_context_pop (context);

			cdn_expansion_unref (ex);
		}

		g_free (fname);
		g_free (dfname);
	}

	return TRUE;
}

/**
 * cdn_parser_context_set_variable: (skip):
 *
 */
void
cdn_parser_context_set_variable (CdnParserContext  *context,
                                 CdnSelector       *selector,
                                 CdnEmbeddedString *expression,
                                 CdnVariableFlags   add_flags,
                                 CdnVariableFlags   remove_flags)
{
	GSList *item;
	GSList *objects;
	gboolean ret = TRUE;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (selector != NULL);

	if (context->priv->in_event_handler)
	{
		return;
	}

	objects = each_selections (context, FALSE);

	for (item = objects; item; item = g_slist_next (item))
	{
		GSList *vars;
		GSList *exprcache = NULL;

		expansion_context_push_selection (context, item->data);

		vars = cdn_selector_select (selector,
		                            cdn_selection_get_object (item->data),
		                            CDN_SELECTOR_TYPE_VARIABLE,
		                            expansion_context_peek (context));

		while (vars)
		{
			GSList *exprs;
			CdnExpansion *ex;
			CdnVariable *v;

			v = cdn_selection_get_object (vars->data);

			if (exprcache)
			{
				ex = exprcache->data;
			}
			else
			{
				expansion_context_push_selection (context,
				                                  vars->data);

				embedded_string_expand_multiple (exprs,
				                                 expression,
				                                 context);

				expansion_context_pop (context);

				if (exprs->next && g_slist_length (exprs) != g_slist_length (vars))
				{
					parser_failed (context,
					               CDN_STATEMENT (expression),
					               CDN_NETWORK_LOAD_ERROR_SYNTAX,
					               "Number of variables (got %d) must be equal to the number of expressions (got %d)",
					               g_slist_length (vars),
					               g_slist_length (exprs));

					g_slist_foreach (exprs, (GFunc)cdn_expansion_unref, NULL);
					g_slist_free (exprs);

					ret = FALSE;
					break;
				}

				ex = exprs->data;

				if (exprs->next)
				{
					exprcache = exprs;
				}
				else
				{
					g_slist_free (exprs);
				}
			}

			if (*cdn_expansion_get (ex, 0) != '\0')
			{
				cdn_variable_set_expression (v,
				                             cdn_expression_new (cdn_expansion_get (ex, 0)));
			}

			cdn_variable_add_flags (v, add_flags);
			cdn_variable_remove_flags (v, remove_flags);

			cdn_expansion_unref (ex);

			exprcache = g_slist_delete_link (exprcache, exprcache);

			cdn_selection_unref (vars->data);
			vars = g_slist_delete_link (vars, vars);
		}

		expansion_context_pop (context);

		if (!ret)
		{
			break;
		}
	}
}

static gchar *
decompose_dot (gchar const *name,
               gint        *order)
{
	gunichar next;

	next = g_utf8_get_char (g_utf8_next_char (name));

#if GLIB_MINOR_VERSION >= 30
	gunichar a;
	gunichar b;

	if (g_unichar_decompose (g_utf8_get_char (name), &a, &b) &&
	    (b == 775 || b == 776))
	{
		GString *dc;

		dc = g_string_sized_new (strlen (name));
		g_string_append_unichar (dc, a);
		g_string_append (dc, g_utf8_next_char (name));

		if (b == 775)
		{
			*order = 1;
		}
		else
		{
			*order = 2;
		}

		return g_string_free (dc, FALSE);
	}
	else
#endif
	if (next == 775 || next == 776)
	{
		GString *dc;

		dc = g_string_sized_new (strlen (name));
		g_string_append_unichar (dc, g_utf8_get_char (name));
		g_string_append (dc, g_utf8_next_char (g_utf8_next_char (name)));

		if (next == 775)
		{
			*order = 1;
		}
		else
		{
			*order = 2;
		}

		return g_string_free (dc, FALSE);
	}
	else if (g_str_has_suffix (name, "'"))
	{
		gchar const *ptr;
		gchar const *last;

		ptr = last = name + strlen (name) - 1;

		while (ptr > name && g_utf8_get_char (ptr) == '\'')
		{
			ptr = g_utf8_prev_char (ptr);
		}

		*order = last - ptr;

		return g_strndup (name, ptr - name + 1);
	}

	return NULL;
}

/**
 * cdn_parser_context_add_variable: (skip):
 *
 */
void
cdn_parser_context_add_variable (CdnParserContext  *context,
                                 CdnEmbeddedString *name,
                                 CdnEmbeddedString *expression,
                                 CdnVariableFlags   add_flags,
                                 CdnVariableFlags   remove_flags,
                                 gboolean           assign_optional,
                                 CdnEmbeddedString *constraint)
{
	GSList *item;
	GSList *objects;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);

	if (context->priv->in_event_handler)
	{
		return;
	}

	objects = each_selections (context, FALSE);

	for (item = objects; item; item = g_slist_next (item))
	{
		CdnObject *obj;
		gchar const *annotation;
		GSList *pairs;
		GSList *pair;

		obj = cdn_selection_get_object (item->data);

		pairs = generate_name_value_pairs (context,
		                                   item->data,
		                                   name,
		                                   G_OBJECT (expression),
		                                   FALSE);

		expansion_context_push_selection (context, item->data);

		for (pair = pairs; pair; pair = g_slist_next (pair))
		{
			GError *error = NULL;
			CdnVariable *property;
			CdnVariableFlags flags = CDN_VARIABLE_FLAG_NONE;
			NameValuePair *p = pair->data;
			gchar *exexpression = NULL;
			gchar const *exname;
			gint order;
			gchar *dotname;
			CdnExpansionContext *pctx;
			CdnVariable *nv;

			exname = cdn_expansion_get (p->name, 0);

			if (CDN_IS_NODE (obj) && (dotname = decompose_dot (exname, &order)))
			{
				// This is a differential equation now...
				add_variable_diff (context,
				                   CDN_NODE (obj),
				                   exname,
				                   dotname,
				                   order,
				                   p,
				                   add_flags,
				                   remove_flags,
				                   constraint);

				g_free (dotname);

				if (context->priv->error)
				{
					break;
				}

				continue;
			}

			property = cdn_object_get_variable (obj,
			                                    cdn_expansion_get (p->name, 0));

			if (property && assign_optional)
			{
				continue;
			}

			if (property)
			{
				flags = cdn_variable_get_flags (property);

				if (!p->value)
				{
					CdnExpression *expr;

					expr = cdn_variable_get_expression (property);
					exexpression = g_strdup (cdn_expression_get_as_string (expr));
				}
			}

			if (!exexpression)
			{
				if (p->value)
				{
					exexpression = g_strdup (cdn_expansion_get (p->value, 0));
				}
				else
				{
					exexpression = g_strdup ("");
				}
			}

			flags &= ~remove_flags;
			flags |= add_flags;

			nv = cdn_variable_new (exname,
			                       cdn_expression_new (exexpression),
			                       flags);

			if (property)
			{
				cdn_variable_set_constraint (nv,
				                             cdn_expression_copy (cdn_variable_get_constraint (property)));
			}

			if (!cdn_object_add_variable (obj,
			                              nv,
			                              &error))
			{
				g_free (exexpression);

				parser_failed_error (context, NULL, error);
				break;
			}

			if (property)
			{
				
			}

			g_free (exexpression);

			property = cdn_object_get_variable (obj, exname);

			pctx = expansion_context_push_base (context);

			cdn_expansion_context_add_expansion (pctx, p->name);

			if (p->value)
			{
				cdn_expansion_context_add_expansion (pctx, p->value);
			}

			annotation = current_annotation (context);

			if (annotation && *annotation)
			{
				cdn_annotatable_set_annotation (CDN_ANNOTATABLE (property),
				                                annotation);
			}

			if (constraint)
			{
				gchar const *cons;

				embedded_string_expand (cons, constraint, context);

				cdn_variable_set_constraint (property,
				                             cdn_expression_new (cons));
			}

			expansion_context_pop (context);
		}

		g_slist_foreach (pairs, (GFunc)name_value_pair_free, NULL);
		g_slist_free (pairs);

		expansion_context_pop (context);
	}

	g_slist_foreach (objects, (GFunc)cdn_selection_unref, NULL);
	g_slist_free (objects);

	clear_annotation (context);

	g_object_unref (name);

	if (expression)
	{
		g_object_unref (expression);
	}

	if (constraint)
	{
		g_object_unref (constraint);
	}
}

/**
 * cdn_parser_context_add_action: (skip):
 *
 */
void
cdn_parser_context_add_action (CdnParserContext   *context,
                               CdnEmbeddedString  *target,
                               CdnEmbeddedString  *expression,
                               CdnEmbeddedString  *phase)
{
	GSList *item;

	GSList *objects;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (target != NULL);

	if (context->priv->in_event_handler)
	{
		return;
	}

	objects = each_selections (context, FALSE);

	for (item = objects; item; item = g_slist_next (item))
	{
		GSList *exps;
		GSList *iteme;
		gchar const *annotation;

		expansion_context_push_selection (context, item->data);

		annotation = current_annotation (context);

		embedded_string_expand_multiple (exps, target, context);

		for (iteme = exps; iteme; iteme = g_slist_next (iteme))
		{
			gchar const *extarget;
			gchar const *exexpression;
			CdnEdgeAction *action;
			CdnEdge *edge;
			gchar *name;
			CdnExpression *index = NULL;
			gchar const *ptr;
			CdnExpansionContext *pctx;

			pctx = expansion_context_push_base (context);
			cdn_expansion_context_add_expansion (pctx, iteme->data);

			extarget = cdn_expansion_get (iteme->data, 0);

			edge = cdn_selection_get_object (item->data);
			ptr = g_utf8_strchr (extarget, -1, '[');

			if (ptr)
			{
				gchar *comp;
				gchar *l;

				name = g_strndup (extarget, ptr - extarget);

				if (*(ptr + 1) == '[')
				{
					l = g_strndup (ptr + 1, strlen (ptr) - 2);

					comp = g_strdup_printf ("lindex(%s, size(output.%s)[1])",
					                        l,
					                        name);

					g_free (l);
				}
				else
				{
					comp = g_strdup (ptr);
				}

				index = cdn_expression_new (comp);
				g_free (comp);
			}
			else
			{
				name = g_strdup (extarget);
			}

			action = cdn_edge_get_action_with_index (edge,
			                                         name,
			                                         index);

			embedded_string_expand (exexpression, expression, context);

			if (!action || expression)
			{
				CdnEdge *edge;

				edge = CDN_EDGE (cdn_selection_get_object (item->data));

				if (action)
				{
					cdn_edge_remove_action (edge, action);
				}

				action = cdn_edge_action_new (name,
				                              cdn_expression_new (exexpression));

				cdn_edge_add_action (edge,
				                     action);
			}

			g_free (name);

			cdn_edge_action_set_index (action, index);

			if (annotation && *annotation)
			{
				cdn_annotatable_set_annotation (CDN_ANNOTATABLE (action),
				                                annotation);
			}

			if (phase)
			{
				GSList *phases;

				embedded_string_expand_multiple (phases, phase, context);

				while (phases)
				{
					cdn_phaseable_add_phase (CDN_PHASEABLE (action),
					                         cdn_expansion_get (phases->data, 0));
	
					cdn_expansion_unref (phases->data);
					phases = g_slist_delete_link (phases, phases);
				}
			}

			expansion_context_pop (context);
		}

		expansion_context_pop (context);

		g_slist_foreach (exps, (GFunc)cdn_expansion_unref, NULL);
		g_slist_free (exps);
	}

	g_slist_foreach (objects, (GFunc)cdn_selection_unref, NULL);
	g_slist_free (objects);

	g_object_unref (target);
	g_object_unref (expression);

	clear_annotation (context);
}

/**
 * cdn_parser_context_add_polynomial: (skip):
 *
 */
void
cdn_parser_context_add_polynomial (CdnParserContext  *context,
                                   CdnEmbeddedString *name,
                                   GSList            *pieces)
{
	GSList *item;
	GSList *objects;
	GSList *piecesi;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);

	if (context->priv->in_event_handler)
	{
		return;
	}

	objects = each_selections (context, FALSE);

	for (item = objects; item; item = g_slist_next (item))
	{
		gchar const *exname;
		CdnFunctionPolynomial *function;
		CdnNode *parent;
		gchar const *annotation;

		parent = cdn_selection_get_object (item->data);

		expansion_context_push_selection (context, item->data);
		annotation = current_annotation (context);

		embedded_string_expand (exname, name, context);
		function = cdn_function_polynomial_new (exname);

		for (piecesi = pieces; piecesi; piecesi = g_slist_next (piecesi))
		{
			CdnFunctionPolynomialPieceSpec *spec;
			gchar const *exfrom;
			gchar const *exto;
			gdouble from;
			gdouble to;
			GArray *coefs;
			GSList *coef;
			CdnFunctionPolynomialPiece *piece;

			spec = piecesi->data;

			embedded_string_expand (exfrom, spec->from, context);
			embedded_string_expand (exto, spec->to, context);

			from = g_ascii_strtod (exfrom, NULL);
			to = g_ascii_strtod (exto, NULL);

			coefs = g_array_new (FALSE, FALSE, sizeof (gdouble));

			for (coef = spec->coefficients; coef; coef = g_slist_next (coef))
			{
				CdnEmbeddedString *c;
				gchar const *excoef;
				gdouble cval;

				c = coef->data;
				embedded_string_expand (excoef, c, context);

				cval = g_ascii_strtod (excoef, NULL);

				g_array_append_val (coefs, cval);
			}

			piece = cdn_function_polynomial_piece_new (from,
			                                           to,
			                                           (gdouble *)coefs->data,
			                                           coefs->len);

			g_array_free (coefs, TRUE);

			cdn_function_polynomial_add (function, piece);
		}

		cdn_node_add (parent, CDN_OBJECT (function), NULL);

		if (annotation && *annotation)
		{
			cdn_annotatable_set_annotation (CDN_ANNOTATABLE (function),
			                                annotation);
		}

		expansion_context_pop (context);
	}

	g_slist_foreach (objects, (GFunc)cdn_selection_unref, NULL);
	g_slist_free (objects);

	g_slist_foreach (pieces, (GFunc)cdn_function_polynomial_piece_spec_free, NULL);
	g_slist_free (pieces);

	clear_annotation (context);
	g_object_unref (name);
}

/**
 * cdn_parser_context_add_interface: (skip):
 *
 */
void
cdn_parser_context_add_interface (CdnParserContext  *context,
                                  CdnEmbeddedString *name,
                                  CdnEmbeddedString *child_name,
                                  CdnEmbeddedString *property_name,
                                  gboolean           is_optional)
{
	GSList *item;
	GSList *objects;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (child_name != NULL);
	g_return_if_fail (property_name != NULL);

	if (context->priv->in_event_handler)
	{
		return;
	}

	objects = each_selections (context, FALSE);

	for (item = objects; item; item = g_slist_next (item))
	{
		CdnVariableInterface *iface;
		CdnNode *parent;
		gboolean ret = TRUE;
		GSList *properties;
		GSList *prop;

		parent = CDN_NODE (cdn_selection_get_object (item->data));

		iface = cdn_node_get_variable_interface (parent);

		expansion_context_push_selection (context, item->data);

		embedded_string_expand_multiple (properties, property_name, context);

		for (prop = properties; prop; prop = g_slist_next (prop))
		{
			GSList *exps;
			GSList *exp;
			CdnExpansionContext *pctx;

			pctx = expansion_context_push_base (context);

			cdn_expansion_context_add_expansion (pctx, prop->data);

			embedded_string_expand_multiple (exps, name, context);

			for (exp = exps; exp; exp = g_slist_next (exp))
			{
				GSList *children;
				GSList *child;

				cdn_expansion_context_add_expansion (expansion_context_push_base (context),
				                                     exp->data);

				embedded_string_expand_multiple (children, child_name, context);

				for (child = children; child; child = g_slist_next (child))
				{
					GError *error = NULL;
					gchar const *nm;

					nm = cdn_expansion_get (exp->data, 0);

					if (is_optional &&
					    cdn_variable_interface_implements (iface,
					                                       nm))
					{
						continue;
					}

					if (!cdn_variable_interface_add (iface,
					                                 nm,
					                                 cdn_expansion_get (child->data, 0),
					                                 cdn_expansion_get (prop->data, 0),
					                                 &error))
					{
						parser_failed_error (context,
						                     CDN_STATEMENT (name),
						                     error);

						ret = FALSE;
						break;
					}

				}

				expansion_context_pop (context);

				g_slist_foreach (children, (GFunc)cdn_expansion_unref, NULL);
				g_slist_free (children);

				if (!ret)
				{
					break;
				}
			}

			g_slist_foreach (exps, (GFunc)cdn_expansion_unref, NULL);
			g_slist_free (exps);

			expansion_context_pop (context);

			if (!ret)
			{
				break;
			}
		}

		expansion_context_pop (context);

		g_slist_foreach (properties, (GFunc)cdn_expansion_unref, NULL);
		g_slist_free (properties);

		if (!ret)
		{
			break;
		}
	}

	g_slist_foreach (objects, (GFunc)cdn_selection_unref, NULL);
	g_slist_free (objects);

	g_object_unref (name);
}

void
cdn_parser_context_set_error (CdnParserContext *context,
                              gchar const      *message)
{
	gchar *fname = NULL;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	if (context->priv->inputs)
	{
		InputItem *item = context->priv->inputs->data;

		if (item->file)
		{
			fname = g_file_get_basename (item->file);
		}
	}

	if (CURRENT_INPUT (context))
	{
		parser_failed (context,
		               NULL,
		               CDN_NETWORK_LOAD_ERROR_SYNTAX,
		               "Unexpected token `%s' at %s%s%d.%d",
		               CURRENT_INPUT (context)->token,
		               fname ? fname : "",
		               fname ? ":" : "",
		               CURRENT_INPUT (context)->lineno,
		               CURRENT_INPUT (context)->cstart);
	}
	else
	{
		parser_failed (context,
		               NULL,
		               CDN_NETWORK_LOAD_ERROR_SYNTAX,
		               "Unexpected end of file");
	}

	g_free (fname);
}

/**
 * cdn_parser_context_get_error:
 * @context: A #CdnParserContext
 *
 * Get the parse error.
 *
 * Returns: (transfer none): A #GError
 *
 **/
GError *
cdn_parser_context_get_error (CdnParserContext *context)
{
	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), NULL);

	return context->priv->error;
}

static GSList *
get_templates (CdnParserContext  *context,
               CdnObject         *object,
               CdnNode           *parent_group,
               GSList            *templates,
               GType              gtype)
{
	gchar *missing;
	GSList *ret = NULL;
	GSList *item;

	if (!cdn_network_parser_utils_get_templates (context->priv->network,
	                                             parent_group,
	                                             context->priv->is_template ? TRUE : FALSE,
	                                             templates,
	                                             expansion_context_peek (context),
	                                             &missing,
	                                             &ret))
	{
		parser_failed (context,
		               NULL,
		               CDN_NETWORK_LOAD_ERROR_OBJECT,
		               "Could not find template `%s'",
		               missing);

		g_slist_foreach (ret, (GFunc)cdn_selection_unref, NULL);
		g_slist_free (ret);

		g_free (missing);
		return NULL;
	}

	g_free (missing);
	gtype = cdn_network_parser_utils_type_from_templates (gtype, ret);

	/* Check if the template types can actually be applied to the
	   object type that we are constructing. Only template types
	   which are superclasses of the new object type can be
	   applied */
	for (item = ret; item; item = g_slist_next (item))
	{
		CdnObject *temp;
		GType template_type;

		temp = cdn_selection_get_object (item->data);
		template_type = G_TYPE_FROM_INSTANCE (temp);

		if (!g_type_is_a (gtype, template_type))
		{
			parser_failed (context,
			               NULL,
			               CDN_NETWORK_LOAD_ERROR_OBJECT,
			               "Referenced template is of incorrect type %s (need %s)",
			               g_type_name (template_type),
			               g_type_name (gtype));

			g_slist_foreach (ret, (GFunc)cdn_selection_unref, NULL);
			g_slist_free (ret);
			return NULL;
		}
	}

	return ret;
}

static CdnSelection *
parse_object_single_id (CdnParserContext *context,
                        CdnExpansion     *id,
                        GSList           *temps,
                        CdnSelection     *parent,
                        GType             gtype,
                        gboolean          allow_create,
                        gchar const      *firstprop,
                        ...) G_GNUC_NULL_TERMINATED;

static CdnSelection *
parse_object_single_id (CdnParserContext *context,
                        CdnExpansion     *id,
                        GSList           *temps,
                        CdnSelection     *parent,
                        GType             gtype,
                        gboolean          allow_create,
                        gchar const      *firstprop,
                        ...)
{
	GSList *templates;
	CdnSelection *sel = NULL;
	GSList *item;
	CdnObject *child = NULL;
	GError *error = NULL;
	CdnNode *parent_group;

	if (id)
	{
		parent_group = CDN_NODE (cdn_selection_get_object (parent));
	}
	else
	{
		parent_group = CDN_NODE (cdn_object_get_parent (cdn_selection_get_object (parent)));
	}

	templates = get_templates (context,
	                           cdn_selection_get_object (parent),
	                           parent_group,
	                           temps,
	                           gtype);

	if (context->priv->error_occurred)
	{
		goto cleanup;
	}

	if (id)
	{
		child = cdn_node_get_child (parent_group,
		                            cdn_expansion_get (id, 0));
	}
	else
	{
		child = cdn_selection_get_object (parent);
	}

	if (!child)
	{
		va_list ap;

		if (!allow_create)
		{
			sel = NULL;
			goto cleanup;
		}

		/* Just construct a new object with the right type */
		va_start (ap, firstprop);
		child = CDN_OBJECT (g_object_new_valist (gtype, firstprop, ap));
		va_end (ap);

		if (!cdn_node_add (parent_group,
		                    child,
		                    &error))
		{
			parser_failed_error (context, NULL, error);

			goto cleanup;
		}
	}
	else
	{
		g_object_ref (child);

		if (!g_type_is_a (G_TYPE_FROM_INSTANCE (child), gtype))
		{
			/* This means the object already existed (this can happen
			   because existing objects created by other templates can be
			   extended) and the type is incorrect */
			parser_failed (context,
			               NULL,
			               CDN_NETWORK_LOAD_ERROR_OBJECT,
			               "Cannot extend type %s with type %s",
			               g_type_name (G_TYPE_FROM_INSTANCE (child)),
			               g_type_name (gtype));

			goto cleanup;
		}
	}

	/* Apply all the templates */
	for (item = templates; item; item = g_slist_next (item))
	{
		if (!cdn_object_apply_template (child,
		                                cdn_selection_get_object (item->data),
		                                &error))
		{
			parser_failed_error (context,
			                     NULL,
			                     error);

			goto cleanup;
		}
	}

	sel = cdn_selection_new (child,
	                         expansion_context_peek (context));

cleanup:
	g_slist_foreach (templates, (GFunc)cdn_selection_unref, NULL);
	g_slist_free (templates);

	if (child)
	{
		g_object_unref (child);
	}

	return sel;
}

static GSList *
parse_object_single (CdnParserContext  *context,
                     GSList            *ids,
                     GSList            *templates,
                     CdnSelection      *parent,
                     GType              gtype,
                     gboolean           allow_create)
{
	GSList *ret = NULL;

	if (!ids)
	{
		CdnSelection *sel;

		sel = parse_object_single_id (context,
		                              NULL,
		                              templates,
		                              parent,
		                              gtype,
		                              allow_create,
		                              "id",
		                              NULL,
		                              NULL);

		if (sel)
		{
			ret = g_slist_prepend (NULL, sel);
		}

		return ret;
	}

	while (ids)
	{
		CdnSelection *sel;
		CdnExpansionContext *pctx;

		pctx = expansion_context_push_base (context);

		cdn_expansion_context_add_expansion (pctx,
		                                     ids->data);

		sel = parse_object_single_id (context,
		                              ids->data,
		                              templates,
		                              parent,
		                              gtype,
		                              allow_create,
		                              "id",
		                              cdn_expansion_get (ids->data, 0),
		                              NULL);

		expansion_context_pop (context);

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
unique_id (CdnNode    *parent,
           gchar const *prefix)
{
	gint i = 1;

	if (!cdn_node_get_child (parent, prefix))
	{
		return g_strdup (prefix);
	}

	while (TRUE)
	{
		gchar *id = g_strdup_printf ("%s_%d", prefix, i++);

		if (!cdn_node_get_child (parent, id))
		{
			return id;
		}

		g_free (id);
	}
}

static void
set_proxy (CdnParserContext *context,
           GSList           *objects)
{
	while (objects)
	{
		CdnObject *obj;
		CdnNode *parent;

		obj = cdn_selection_get_object (objects->data);
		objects = g_slist_next (objects);

		if (CDN_IS_EDGE (obj))
		{
			continue;
		}

		parent = cdn_object_get_parent (obj);

		if (!parent)
		{
			continue;
		}

		cdn_node_set_proxy (parent, obj);
	}
}

static GSList *
parse_objects (CdnParserContext  *context,
               CdnEmbeddedString *id,
               GSList            *templates,
               GType              gtype,
               gboolean           allow_create)
{
	GSList *parents;
	GSList *parent;
	GSList *ret = NULL;
	gboolean isproxy;

	parents = each_selections (context, TRUE);

	//isproxy = find_attribute (attributes, "proxy") != NULL;
	// TODO
	isproxy = FALSE;

	for (parent = parents; parent; parent = g_slist_next (parent))
	{
		GSList *objs;
		GSList *ids;
		CdnEmbeddedString *theid;

		theid = id;

		expansion_context_push_selection (context,
		                                  parent->data);

		if (theid == NULL)
		{
			gchar *newid;

			newid = unique_id (CDN_NODE (cdn_selection_get_object (parent->data)),
			                  "object");

			theid = cdn_embedded_string_new_from_string (newid);
			g_free (newid);
		}
		else
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
		                            gtype,
		                            allow_create);

		if (isproxy)
		{
			set_proxy (context, objs);
		}

		g_slist_foreach (ids, (GFunc)cdn_expansion_unref, NULL);
		g_slist_free (ids);

		expansion_context_pop (context);

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

	g_slist_foreach (parents, (GFunc)cdn_selection_unref, NULL);
	g_slist_free (parents);

	return ret;
}

static GSList *
edge_pairs_sparse (CdnParserContext *context,
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
			if (probability >= 1 || RANDOM () <= p)
			{
				ret = g_slist_prepend (ret,
				                       cdn_selection_copy (fromobj->data));

				ret = g_slist_prepend (ret,
				                       cdn_selection_copy (fromobj->data));
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
			if ((probability < 1 && RANDOM () > p) || (noself &&
			                      cdn_selection_get_object (toobj->data) ==
			                      cdn_selection_get_object (fromobj->data)))
			{
				toobj = g_slist_next (toobj);
				continue;
			}

			ret = g_slist_prepend (ret,
			                       cdn_selection_copy (fromobj->data));

			ret = g_slist_prepend (ret,
			                       cdn_selection_copy (toobj->data));

			if (bidi)
			{
				ret = g_slist_prepend (ret,
				                       cdn_selection_copy (toobj->data));

				ret = g_slist_prepend (ret,
				                       cdn_selection_copy (fromobj->data));
			}

			toobj = g_slist_next (toobj);
		}
	}

	return g_slist_reverse (ret);
}

static GSList *
edge_pairs (CdnParserContext *context,
            CdnExpansion     *id,
            gboolean          autoid,
            CdnSelection     *parent,
            GSList           *attributes,
            CdnSelector      *from,
            CdnSelector      *to,
            gboolean          onlyself)
{
	GSList *fromobjs;
	GSList *fromobj;
	GSList *ret = NULL;
	CdnAttribute *bidi;
	CdnAttribute *noself;
	CdnAttribute *iff;
	gdouble iffprob = 2.0; /* Something bigger than 1 */
	long int p = 1;
	CdnExpansionContext *pctx;

	bidi = find_attribute (attributes, "bidirectional");
	iff = find_attribute (attributes, "probability");
	noself = find_attribute (attributes, "no-self");

	if (iff)
	{
		CdnEmbeddedString *s;
		GObject *obj;

		obj = cdn_attribute_get_argument (iff, 0);

		if (CDN_IS_EMBEDDED_STRING (obj))
		{
			gchar const *ex;
			s = CDN_EMBEDDED_STRING (obj);

			embedded_string_expand_val (ex, s, context, NULL);

			iffprob = g_ascii_strtod (ex, NULL);
			p = (long int)(RAND_MAX * iffprob);
		}
		else
		{
			iff = NULL;
		}
	}

	pctx = expansion_context_push_base (context);

	if (!autoid)
	{
		cdn_expansion_context_add_expansion (pctx, id);
	}

	fromobjs = cdn_selector_select (from,
	                                cdn_selection_get_object (parent),
	                                CDN_SELECTOR_TYPE_NODE,
	                                expansion_context_peek (context));

	if (!fromobjs)
	{
		expansion_context_pop (context);

		return NULL;
	}

	if (!to)
	{
		// Truncate from obj
		for (fromobj = fromobjs; fromobj; fromobj = g_slist_next (fromobj))
		{
			cdn_expansion_context_truncate (cdn_selection_get_context (fromobj->data),
			                                expansion_context_peek (context));
		}

		ret = edge_pairs_sparse (context,
		                         iffprob,
		                         bidi != NULL,
		                         noself != NULL,
		                         onlyself,
		                         fromobjs);

		g_slist_foreach (fromobjs, (GFunc)cdn_selection_unref, NULL);
		g_slist_free (fromobjs);

		expansion_context_pop (context);

		return ret;
	}

	for (fromobj = fromobjs; fromobj; fromobj = g_slist_next (fromobj))
	{
		GSList *toobjs = NULL;
		GSList *toobj;

		expansion_context_push_selection (context, fromobj->data);
		cdn_selector_set_from_set (to, fromobjs);

		/* Select TO states */
		toobjs = cdn_selector_select (to,
		                              cdn_selection_get_object (parent),
		                              CDN_SELECTOR_TYPE_NODE,
		                              expansion_context_peek (context));

		for (toobj = toobjs; toobj; toobj = g_slist_next (toobj))
		{
			if ((iffprob < 1 && RANDOM () > p) || (noself &&
			    cdn_selection_get_object (toobj->data) ==
			    cdn_selection_get_object (fromobj->data)))
			{
				continue;
			}

			cdn_expansion_context_truncate (cdn_selection_get_context (toobj->data),
			                                expansion_context_peek (context));

			ret = g_slist_prepend (ret,
			                       cdn_selection_copy (fromobj->data));

			ret = g_slist_prepend (ret,
			                       cdn_selection_copy (toobj->data));

			if (bidi != NULL)
			{
				ret = g_slist_prepend (ret,
				                       cdn_selection_copy (toobj->data));

				ret = g_slist_prepend (ret,
				                       cdn_selection_copy (fromobj->data));
			}
		}

		expansion_context_pop (context);

		/* Truncate from selection now */
		cdn_expansion_context_truncate (cdn_selection_get_context (fromobj->data),
		                                expansion_context_peek (context));

		g_slist_foreach (toobjs, (GFunc)cdn_selection_unref, NULL);
		g_slist_free (toobjs);
	}

	g_slist_foreach (fromobjs, (GFunc)cdn_selection_unref, NULL);
	g_slist_free (fromobjs);

	expansion_context_pop (context);

	return g_slist_reverse (ret);
}

static void
store_annotation_objects (CdnParserContext *context,
                          GSList           *objects)
{
	if (!context->priv->annotation)
	{
		return;
	}

	while (objects)
	{
		CdnObject *obj;
		gchar const *annotation;

		expansion_context_push_selection (context,
		                                  objects->data);

		obj = cdn_selection_get_object (objects->data);

		annotation = current_annotation (context);

		/* TODO: not the right context */
		if (annotation && *annotation)
		{
			cdn_annotatable_set_annotation (CDN_ANNOTATABLE (obj),
			                                annotation);
		}

		expansion_context_pop (context);

		objects = g_slist_next (objects);
	}

	clear_annotation (context);
}

/**
 * cdn_parser_context_push_objects: (skip):
 *
 */
void
cdn_parser_context_push_objects (CdnParserContext *context,
                                 GSList           *objects)
{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	if (context->priv->in_event_handler)
	{
		return;
	}

	store_annotation_objects (context, objects);

	context->priv->context_stack =
		g_slist_prepend (context->priv->context_stack,
		                 context_new (objects));

	g_signal_emit (context, signals[CONTEXT_PUSHED], 0);
}

static GType
gtype_from_selector_type (CdnSelectorType type)
{
	if (type & CDN_SELECTOR_TYPE_OBJECT)
	{
		return CDN_TYPE_OBJECT;
	}
	else if (type & CDN_SELECTOR_TYPE_NODE)
	{
		return CDN_TYPE_NODE;
	}
	else if (type & CDN_SELECTOR_TYPE_EDGE)
	{
		return CDN_TYPE_EDGE;
	}
	else
	{
		return G_TYPE_INVALID;
	}
}

static gchar *
expansion_as_string (CdnExpansion *expansion)
{
	GString *ret;
	gint i;

	ret = g_string_new ("");

	if (cdn_expansion_num (expansion) > 1)
	{
		g_string_append_c (ret, '{');
	}

	for (i = 0; i < cdn_expansion_num (expansion); ++i)
	{
		if (i != 0)
		{
			g_string_append_c (ret, ',');
		}

		g_string_append (ret, cdn_expansion_get (expansion, i));
		g_string_append_printf (ret, ":%d", cdn_expansion_get_index (expansion, i));
	}

	if (cdn_expansion_num (expansion) > 1)
	{
		g_string_append_c (ret, '}');
	}

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

/**
 * cdn_parser_context_push_selection: (skip):
 *
 */
void
cdn_parser_context_push_selection (CdnParserContext *context,
                                   CdnSelector      *selector,
                                   CdnSelectorType   type,
                                   GSList           *templates)
{
	GSList *item;
	GSList *objs = NULL;
	GSList *parents;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	if (context->priv->in_event_handler)
	{
		return;
	}

	/* Select on the current context and create a new context based on
	   the result */

	parents = each_selections (context, TRUE);

	for (item = parents; item; item = g_slist_next (item))
	{
		CdnSelection *sel;
		GSList *ret;
		GSList *it;

		sel = item->data;

		expansion_context_push_selection (context, sel);

		ret = cdn_selector_select (selector,
		                           cdn_selection_get_object (sel),
		                           type,
		                           expansion_context_peek (context));

		for (it = ret; it; it = g_slist_next (it))
		{
			CdnObject *obj;
			GSList *temps;
			GSList *temp;

			obj = cdn_selection_get_object (it->data);

			expansion_context_push_selection (context,
			                                  it->data);

			temps = get_templates (context,
			                       obj,
			                       CDN_NODE (cdn_object_get_parent (obj)),
			                       templates,
			                       gtype_from_selector_type (type));

			if (context->priv->error_occurred)
			{
				return;
			}

			for (temp = temps; temp; temp = g_slist_next (temp))
			{
				GError *error = NULL;

				if (!cdn_object_apply_template (obj,
				                                cdn_selection_get_object (temp->data),
				                                &error))
				{
					parser_failed_error (context,
					                     CDN_STATEMENT (selector),
					                     error);
				}
			}

			g_slist_foreach (temps, (GFunc)cdn_selection_unref, NULL);
			g_slist_free (temps);

			objs = g_slist_prepend (objs,
			                        cdn_selection_new (cdn_selection_get_object (it->data),
			                                           expansion_context_peek (context)));

			expansion_context_pop (context);
		}

		g_slist_foreach (ret, (GFunc)cdn_selection_unref, NULL);
		g_slist_free (ret);

		expansion_context_pop (context);
	}

	g_slist_foreach (parents, (GFunc)cdn_selection_unref, NULL);
	g_slist_free (parents);

	objs = g_slist_reverse (objs);

	cdn_parser_context_push_objects (context, objs);

	g_slist_free (objs);
}

static GSList *
create_objects (CdnParserContext  *context,
                CdnEmbeddedString *id,
                GSList            *templates,
                GType              type,
                gboolean           allow_create)
{
	return parse_objects (context, id, templates, type, allow_create);
}

static GSList *
create_edges_single (CdnParserContext          *context,
                     CdnExpansion              *id,
                     gboolean                   autoid,
                     GSList                    *templates,
                     CdnSelection              *parent,
                     GSList                    *attributes,
                     CdnSelector               *from,
                     CdnSelector               *to,
                     gboolean                   onlyself)
{
	GSList *pairs;
	GSList *item;
	GSList *ret = NULL;
	gboolean multiple;
	gint num = 1;
	gint idx = 0;
	CdnAttribute *bidi;

	/* For each pair FROM -> TO generate a edge */
	pairs = edge_pairs (context,
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
		CdnSelection *fromsel;
		CdnSelection *tosel;
		CdnExpansion *realid;
		CdnSelection *obj;
		CdnSelection *firstsel;
		CdnSelection *secondsel;
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
		}
		else
		{
			firstsel = tosel;
			secondsel = fromsel;
		}

		if (cdn_object_get_parent (cdn_selection_get_object (fromsel)) !=
		    cdn_object_get_parent (cdn_selection_get_object (tosel)))
		{
			continue;
		}

		/* Alter id to be numeric incremental */

		realid = cdn_expansion_copy (id);

		if (multiple)
		{
			newid = g_strdup_printf ("%s_%d",
			                         cdn_expansion_get (id, 0),
			                         num);
		}
		else
		{
			newid = g_strdup (cdn_expansion_get (id, 0));
		}

		uniq = unique_id (CDN_NODE (cdn_selection_get_object (parent)),
		                  newid);

		g_free (newid);

		cdn_expansion_set (realid, 0, uniq);
		g_free (uniq);

		expansion_context_push_base (context);

		if (!autoid)
		{
			cdn_expansion_context_add_expansion (expansion_context_peek (context),
			                                     realid);
		}

		cdn_expansion_context_merge (expansion_context_peek (context),
		                             cdn_selection_get_context (firstsel));

		if (to || !onlyself)
		{
			cdn_expansion_context_merge (expansion_context_peek (context),
			                             cdn_selection_get_context (secondsel));
		}

		obj = parse_object_single_id (context,
		                              realid,
		                              templates,
		                              parent,
		                              CDN_TYPE_EDGE,
		                              TRUE,
		                              "id",
		                              cdn_expansion_get (realid, 0),
		                              NULL);

		expansion_context_pop (context);
		cdn_expansion_unref (realid);

		if (!obj)
		{
			continue;
		}

		ret = g_slist_prepend (ret, obj);

		++num;

		cdn_edge_attach (CDN_EDGE (cdn_selection_get_object (obj)),
		                 cdn_selection_get_object (fromsel),
		                 cdn_selection_get_object (tosel));
	}

	g_slist_foreach (pairs, (GFunc)cdn_selection_unref, NULL);
	g_slist_free (pairs);

	return g_slist_reverse (ret);
}

static GSList *
create_edges (CdnParserContext          *context,
              CdnEmbeddedString         *id,
              gboolean                   autoid,
              GSList                    *templates,
              GSList                    *attributes,
              GSList                    *fromto)
{
	GSList *ids;
	GSList *ret = NULL;
	GSList *item;
	CdnSelector *from;
	CdnSelector *to;
	gboolean onlyself = FALSE;
	GSList *parents;

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

	parents = each_selections (context, TRUE);

	for (item = parents; item; item = g_slist_next (item))
	{
		GSList *it;

		/* Expand the id with the parent expansions */
		expansion_context_push_selection (context,
		                                  item->data);

		embedded_string_expand_multiple_val (ids, id, context, NULL);

		for (it = ids; it; it = g_slist_next (it))
		{
			ret = g_slist_concat (ret,
			                      create_edges_single (context,
			                                           it->data,
			                                           autoid,
			                                           templates,
			                                           item->data,
			                                           attributes,
			                                           from,
			                                           to,
			                                           onlyself));
		}

		expansion_context_pop (context);

		g_slist_foreach (ids, (GFunc)cdn_expansion_unref, NULL);
		g_slist_free (ids);
	}

	g_slist_foreach (parents, (GFunc)cdn_selection_unref, NULL);
	g_slist_free (parents);

	return ret;
}

/**
 * cdn_parser_context_push_node: (skip):
 *
 */
void
cdn_parser_context_push_node (CdnParserContext  *context,
                              CdnEmbeddedString *id,
                              GSList            *templates)
{
	GSList *objects;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	if (context->priv->in_event_handler)
	{
		return;
	}

	objects = create_objects (context,
	                          id,
	                          templates,
	                          CDN_TYPE_NODE,
	                          TRUE);

	cdn_parser_context_push_objects (context, objects);
	g_slist_free (objects);

	if (id != NULL)
	{
		g_object_unref (id);
	}
}

/**
 * cdn_parser_context_push_edge: (skip):
 *
 */
void
cdn_parser_context_push_edge (CdnParserContext          *context,
                              CdnEmbeddedString         *id,
                              GSList                    *templates,
                              GSList                    *attributes,
                              GSList                    *fromto,
                              CdnEmbeddedString         *phase)
{
	GSList *objects;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	if (context->priv->in_event_handler)
	{
		return;
	}

	if (!fromto)
	{
		objects = create_objects (context,
		                          id,
		                          templates,
		                          CDN_TYPE_EDGE,
		                          TRUE);
	}
	else
	{
		gboolean autoid;
	
		autoid = id == NULL;

		if (id == NULL)
		{
			id = cdn_embedded_string_new_from_string ("edge");
		}

		objects = create_edges (context,
		                        id,
		                        autoid,
		                        templates,
		                        attributes,
		                        fromto);
	}

	if (phase)
	{
		GSList *item;

		for (item = objects; item; item = g_slist_next (item))
		{
			CdnSelection *sel = item->data;
			GSList *phases;

			expansion_context_push_selection (context,
			                                  sel);

			embedded_string_expand_multiple (phases, phase, context);

			while (phases)
			{
				cdn_phaseable_add_phase (CDN_PHASEABLE (cdn_selection_get_object (sel)),
				                         cdn_expansion_get (phases->data, 0));

				cdn_expansion_unref (phases->data);
				phases = g_slist_delete_link (phases, phases);
			}

			expansion_context_pop (context);
		}
	}

	cdn_parser_context_push_objects (context, objects);
	g_slist_free (objects);

	if (id != NULL)
	{
		g_object_unref (id);
	}

	if (phase != NULL)
	{
		g_object_unref (id);
	}
}

static void
push_scope (CdnParserContext *context,
            gboolean          newctx)
{
	GSList *objects;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	if (context->priv->in_event_handler)
	{
		return;
	}

	objects = each_selections (context, newctx);

	clear_annotation (context);

	cdn_parser_context_push_objects (context, objects);

	g_slist_free (objects);
}

/**
 * cdn_parser_context_push_define: (skip):
 *
 */
void
cdn_parser_context_push_define (CdnParserContext *context)
{
	if (context->priv->in_event_handler)
	{
		return;
	}

	push_scope (context, FALSE);
}

/**
 * cdn_parser_context_push_scope: (skip):
 *
 */

void
cdn_parser_context_push_scope (CdnParserContext *context)
{
	if (context->priv->in_event_handler)
	{
		return;
	}

	push_scope (context, TRUE);
}

static void
cdn_parser_context_push_object (CdnParserContext *context,
                                gpointer          obj)
{
	GSList *objects = NULL;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	if (context->priv->in_event_handler)
	{
		return;
	}

	objects = g_slist_prepend (objects, obj);
	cdn_parser_context_push_objects (context, objects);
	g_slist_free (objects);
}

/**
 * cdn_parser_context_push_network: (skip):
 *
 */
void
cdn_parser_context_push_network (CdnParserContext *context)
{
	cdn_parser_context_push_object (context, context->priv->network);
}

/**
 * cdn_parser_context_push_integrator: (skip):
 *
 */
void
cdn_parser_context_push_integrator (CdnParserContext *context)
{
	cdn_parser_context_push_object (context,
	                                cdn_network_get_integrator (context->priv->network));
}

/**
 * cdn_parser_context_push_templates: (skip):
 *
 */
void
cdn_parser_context_push_templates (CdnParserContext *context)
{
	cdn_parser_context_push_object (context,
	                                cdn_network_get_template_node (context->priv->network));

	context->priv->is_template = CURRENT_CONTEXT (context);
}

/**
 * cdn_parser_context_push_function: (skip):
 *
 */
void
cdn_parser_context_push_function (CdnParserContext  *context,
                                  CdnEmbeddedString *id,
                                  GSList            *args,
                                  CdnEmbeddedString *expression,
                                  gboolean           optional)
{
	GSList *objects;
	GSList *item;
	GSList *funcs = NULL;
	gboolean ret = TRUE;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CDN_IS_EMBEDDED_STRING (id));
	g_return_if_fail (CDN_IS_EMBEDDED_STRING (expression));

	if (context->priv->in_event_handler)
	{
		return;
	}

	objects = each_selections (context, TRUE);

	for (item = objects; item; item = g_slist_next (item))
	{
		CdnSelection *sel = item->data;
		GSList *ids;
		GSList *it;
		gint numfunc;
		gint fi = 0;

		expansion_context_push_selection (context, sel);
		embedded_string_expand_multiple (ids, id, context);

		numfunc = g_slist_length (ids);

		for (it = ids; it; it = g_slist_next (it))
		{
			CdnExpansion *ex = it->data;
			CdnFunction *func;
			CdnSelection *funcsel;
			CdnObject *child;
			GSList *exprs;
			CdnExpansion *expr;
			CdnNode *grp;
			GSList *argit;
			gboolean isoptional = FALSE;

			grp = CDN_NODE (cdn_selection_get_object (sel));

			child = cdn_node_get_child (grp,
			                             cdn_expansion_get (ex, 0));

			if (optional && child)
			{
				continue;
			}
			else if (child)
			{
				if (CDN_IS_FUNCTION (child))
				{
					GError *error = NULL;

					// Remove this function
					if (!cdn_node_remove (grp, child, &error))
					{
						parser_failed_error (context,
						                     CDN_STATEMENT (id),
						                     error);

						ret = FALSE;
						break;
					}
				}
				else
				{
					parser_failed (context,
					               CDN_STATEMENT (id),
					               CDN_NETWORK_LOAD_ERROR_FUNCTION,
					               "The function `%s' already exists as an object",
					               cdn_expansion_get (ex, 0));

					ret = FALSE;
					break;
				}
			}

			expansion_context_push_base (context);
			cdn_expansion_context_add_expansion (expansion_context_peek (context),
			                                     ex);

			embedded_string_expand_multiple (exprs, expression, context);

			if (numfunc == g_slist_length (exprs))
			{
				expr = g_slist_nth_data (exprs, fi);
			}
			else
			{
				expr = exprs->data;
			}

			func = cdn_function_new (cdn_expansion_get (ex, 0),
			                         cdn_expression_new (cdn_expansion_get (expr, 0)));

			g_slist_foreach (exprs, (GFunc)cdn_expansion_unref, NULL);
			g_slist_free (exprs);

			cdn_node_add (CDN_NODE (cdn_selection_get_object (sel)),
			               CDN_OBJECT (func),
			               NULL);

			// Add arguments from the specs
			for (argit = args; argit; argit = g_slist_next (argit))
			{
				CdnFunctionArgumentSpec *spec = argit->data;
				GSList *pairs;
				gint i = 0;

				if (isoptional && !spec->default_value && spec->isexplicit)
				{
					parser_failed (context,
					               CDN_STATEMENT (id),
					               CDN_NETWORK_LOAD_ERROR_FUNCTION,
					               "The function `%s' must declare all optional arguments after non-optional arguments",
					               cdn_expansion_get (ex, 0));

					expansion_context_pop (context);
					ret = FALSE;
					break;
				}

				if (spec->default_value)
				{
					isoptional = TRUE;
				}

				pairs = generate_name_value_pairs (context,
				                                   NULL,
				                                   spec->name,
				                                   spec->default_value,
				                                   FALSE);

				while (pairs)
				{
					NameValuePair *pair;
					CdnFunctionArgument *arg;
					CdnExpression *default_value = NULL;

					pair = pairs->data;

					if (pair->value)
					{
						default_value =
							cdn_expression_new (cdn_expansion_get (pair->value, 0));
					}

					arg = cdn_function_argument_new (cdn_expansion_get (pair->name, 0),
					                                 spec->isexplicit,
					                                 default_value);

					cdn_function_add_argument (func, arg);

					name_value_pair_free (pair);

					pairs = g_slist_delete_link (pairs,
					                             pairs);

					++i;
				}
			}

			funcsel = cdn_selection_new (func,
			                             expansion_context_peek (context));

			expansion_context_pop (context);

			funcs = g_slist_prepend (funcs, funcsel);

			++fi;
		}

		g_slist_foreach (ids, (GFunc)cdn_expansion_unref, NULL);
		g_slist_free (ids);

		expansion_context_pop (context);

		if (!ret)
		{
			break;
		}
	}

	funcs = g_slist_reverse (funcs);

	if (ret)
	{
		cdn_parser_context_push_objects (context, funcs);
	}
	else
	{
		g_slist_foreach (funcs, (GFunc)cdn_selection_unref, NULL);
	}

	g_slist_free (objects);
	g_slist_free (funcs);

	g_slist_foreach (args, (GFunc)cdn_function_argument_spec_free, NULL);
	g_slist_free (args);

	if (id)
	{
		g_object_unref (id);
	}

	if (expression)
	{
		g_object_unref (expression);
	}
}

/**
 * cdn_parser_context_pop:
 * @context: A #CdnParserContext
 *
 * Description.
 *
 **/
void
cdn_parser_context_pop (CdnParserContext *context)
{
	Context *ctx;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	if (context->priv->in_event_handler)
	{
		return;
	}

	if (!context->priv->context_stack)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	if (context->priv->is_template == ctx)
	{
		context->priv->is_template = NULL;
	}

	context_free (ctx);

	context->priv->context_stack =
		g_slist_delete_link (context->priv->context_stack,
		                     context->priv->context_stack);

	g_signal_emit (context, signals[CONTEXT_POPPED], 0);
}

/**
 * cdn_parser_context_import: (skip):
 *
 */
void
cdn_parser_context_import (CdnParserContext  *context,
                           CdnEmbeddedString *id,
                           CdnEmbeddedString *path)
{
	GSList *item;
	GSList *objects;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (id != NULL);
	g_return_if_fail (path != NULL);

	if (context->priv->in_event_handler)
	{
		return;
	}

	objects = each_selections (context, FALSE);

	for (item = objects; item; item = g_slist_next (item))
	{
		GSList *ids;
		GSList *idi;
		gchar const *annotation;

		expansion_context_push_selection (context,
		                                  item->data);

		annotation = current_annotation (context);

		embedded_string_expand_multiple (ids, id, context);

		for (idi = ids; idi; idi = g_slist_next (idi))
		{
			gchar const *expath;
			GFile *file = NULL;
			CdnImport *import;
			GError *error = NULL;
			GFile *curfile;
			CdnNode *parent_group;
			gchar const *exid;
			CdnExpansionContext *pctx;

			exid = cdn_expansion_get (idi->data, 0);

			pctx = expansion_context_push_base (context);

			cdn_expansion_context_add_expansion (pctx,
			                                     idi->data);

			embedded_string_expand (expath, path, context);

			expansion_context_pop (context);

			curfile = cdn_parser_context_get_file (context);

			file = cdn_network_parser_utils_resolve_import (curfile,
			                                                expath);

			if (curfile)
			{
				g_object_unref (curfile);
			}

			if (!file)
			{
				parser_failed (context,
				               CDN_STATEMENT (path),
				               CDN_NETWORK_LOAD_ERROR_IMPORT,
				               "File `%s' for import `%s' could not be found",
				               expath,
				               exid);

				break;
			}

			parent_group = CDN_NODE (cdn_selection_get_object (item->data));

			if (context->priv->is_template)
			{
				CdnNode *template_group;

				template_group = cdn_network_get_template_node (context->priv->network);

				if (cdn_node_get_child (template_group, exid) != NULL)
				{
					parser_failed (context,
					               CDN_STATEMENT (id),
					               CDN_NETWORK_LOAD_ERROR_IMPORT,
					               "There is already an object with the id `%s'",
					               exid);

					expansion_context_pop (context);

					g_slist_foreach (ids,
					                 (GFunc)cdn_expansion_unref,
					                 NULL);

					g_slist_free (ids);
					g_object_unref (file);

					goto cleanup;
				}

				import = cdn_network_parser_utils_find_template_import (CDN_OBJECT (template_group), file);

				if (import)
				{
					CdnImportAlias *alias;

					alias = cdn_import_alias_new (import);

					if (!cdn_node_add (parent_group,
					                    CDN_OBJECT (alias),
					                    &error))
					{
						parser_failed_error (context, NULL, error);
						expansion_context_pop (context);

						g_slist_foreach (ids,
						                 (GFunc)cdn_expansion_unref,
						                 NULL);

						g_slist_free (ids);
						g_object_unref (file);

						goto cleanup;
					}

					if (annotation && *annotation)
					{
						cdn_annotatable_set_annotation (CDN_ANNOTATABLE (alias),
						                                annotation);
					}

					g_object_unref (alias);
					g_object_unref (file);

					continue;
				}
			}

			if (cdn_node_get_child (CDN_NODE (context->priv->network), exid) != NULL)
			{
				parser_failed (context,
				               CDN_STATEMENT (id),
				               CDN_NETWORK_LOAD_ERROR_IMPORT,
				               "There is already an object with the id `%s'",
				               exid);

				expansion_context_pop (context);

				g_slist_foreach (ids,
				                 (GFunc)cdn_expansion_unref,
				                 NULL);

				g_slist_free (ids);
				g_object_unref (file);

				goto cleanup;
			}

			import = cdn_import_new (context->priv->network,
			                         parent_group,
			                         exid,
			                         file,
			                         &error);

			if (!import)
			{
				parser_failed_error (context, NULL, error);
				expansion_context_pop (context);

				g_slist_foreach (ids,
				                 (GFunc)cdn_expansion_unref,
				                 NULL);

				g_slist_free (ids);
				g_object_unref (file);

				goto cleanup;
			}

			g_signal_emit (context, signals[FILE_USED], 0, file, expath);

			if (annotation && *annotation)
			{
				cdn_annotatable_set_annotation (CDN_ANNOTATABLE (import),
				                                annotation);
			}

			g_object_unref (file);
			g_object_unref (import);
		}

		g_slist_foreach (ids, (GFunc)cdn_expansion_unref, NULL);
		g_slist_free (ids);

		ids = NULL;

		expansion_context_pop (context);
	}

cleanup:
	g_slist_foreach (objects, (GFunc)cdn_selection_unref, NULL);
	g_slist_free (objects);

	clear_annotation (context);

	g_object_unref (id);
	g_object_unref (path);
}

static CdnSelector *
ensure_selector (CdnParserContext *context)
{
	if (!context->priv->selectors)
	{
		cdn_parser_context_push_selector (context, TRUE);
	}

	return context->priv->selectors->data;
}

/**
 * cdn_parser_context_push_selector: (skip):
 *
 */

void
cdn_parser_context_push_selector (CdnParserContext *context,
                                  gboolean          with)
{
	CdnSelector *selector;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	selector = cdn_selector_new (CDN_OBJECT (context->priv->network));

	context->priv->selectors =
		g_slist_prepend (context->priv->selectors,
		                 selector);
}

/**
 * cdn_parser_context_push_selector_identifier: (skip):
 *
 */

void
cdn_parser_context_push_selector_identifier (CdnParserContext  *context,
                                             CdnEmbeddedString *identifier)
{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (identifier != NULL);

	cdn_selector_append (ensure_selector (context), identifier);
	g_object_unref (identifier);

	statement_end (context, ensure_selector (context));

	g_signal_emit (context,
	               signals[SELECTOR_ITEM_PUSHED],
	               0,
	               ensure_selector (context));
}

/**
 * cdn_parser_context_push_selector_regex: (skip):
 *
 */

void
cdn_parser_context_push_selector_regex (CdnParserContext  *context,
                                        CdnEmbeddedString *regex)
{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (regex != NULL);

	cdn_selector_append_regex (ensure_selector (context), regex);
	g_object_unref (regex);

	statement_end (context, ensure_selector (context));

	g_signal_emit (context,
	               signals[SELECTOR_ITEM_PUSHED],
	               0,
	               ensure_selector (context));
}

/**
 * cdn_parser_context_push_selector_pseudo: (skip):
 *
 */

void
cdn_parser_context_push_selector_pseudo (CdnParserContext      *context,
                                         CdnSelectorPseudoType  type,
                                         GSList                *argument)
{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	cdn_selector_append_pseudo (ensure_selector (context),
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
 * cdn_parser_context_peek_selector:
 * @context: A #CdnParserContext
 *
 * Description.
 *
 * Returns: (transfer none): A #CdnSelector
 *
 **/
CdnSelector *
cdn_parser_context_peek_selector (CdnParserContext *context)
{
	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), NULL);

	return ensure_selector (context);
}

/**
 * cdn_parser_context_pop_selector:
 * @context: A #CdnParserContext
 * 
 * Description.
 *
 * Returns: (transfer full): A #CdnSelector
 *
 **/
CdnSelector *
cdn_parser_context_pop_selector (CdnParserContext *context)
{
	CdnSelector *ret;

	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (context->priv->selectors, NULL);

	ret = context->priv->selectors->data;

	context->priv->selectors =
		g_slist_delete_link (context->priv->selectors,
		                     context->priv->selectors);

	return ret;
}

/**
 * cdn_parser_context_get_scanner:
 * @context: A #CdnParserContext
 *
 * Description.
 *
 * Returns: (transfer none): Description
 *
 **/
gpointer
cdn_parser_context_get_scanner (CdnParserContext *context)
{
	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), NULL);

	return context->priv->scanner;
}

/**
 * cdn_parser_context_read: (skip):
 *
 */
gssize
cdn_parser_context_read (CdnParserContext *context,
                         gchar            *buffer,
                         gsize             max_size)
{
	InputItem *item;
	gssize ret;

	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), EOF);
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

	ret = g_input_stream_read (item->stream,
	                           buffer,
	                           max_size,
	                           NULL,
	                           NULL);

	return ret;
}

gboolean
cdn_parser_context_parse (CdnParserContext  *context,
                          gboolean           push_network,
                          GError           **error)
{
	gboolean ret;

	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), FALSE);
	g_return_val_if_fail (context->priv->error != NULL || context->priv->inputs, FALSE);

	if (context->priv->error == NULL)
	{
		if (push_network)
		{
			cdn_parser_context_push_network (context);
		}

		ret = cdn_parser_parse (context) == 0;

		if (push_network)
		{
			cdn_parser_context_pop (context);
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

/**
 * cdn_parser_context_set_line: (skip):
 *
 */

void
cdn_parser_context_set_line (CdnParserContext *context,
                             gchar const      *line,
                             gint              lineno)
{
	InputItem *input;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	input = CURRENT_INPUT (context);

	input->lineno = lineno;

	g_hash_table_insert (input->lines,
	                     GINT_TO_POINTER (lineno),
	                     g_strdup (line));
}

/**
 * cdn_parser_context_set_column: (skip):
 *
 */

void
cdn_parser_context_set_column (CdnParserContext *context,
                               gint              start,
                               gint              end)
{
	InputItem *input;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	input = CURRENT_INPUT (context);

	input->cstart = start;
	input->cend = end;
}

gchar const *
cdn_parser_context_get_line (CdnParserContext *context,
                             gint             *lineno)
{
	InputItem *input;

	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), NULL);

	input = CURRENT_INPUT (context);

	if (lineno)
	{
		*lineno = input ? input->lineno : 0;
	}

	return cdn_parser_context_get_line_at (context, input ? input->lineno : 0);
}

gchar const *
cdn_parser_context_get_line_at (CdnParserContext *context,
                                gint              lineno)
{
	InputItem *input;

	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), NULL);

	input = CURRENT_INPUT (context);

	if (!input)
	{
		return NULL;
	}

	return g_hash_table_lookup (input->lines, GINT_TO_POINTER (lineno));
}

void
cdn_parser_context_get_column (CdnParserContext *context,
                               gint             *start,
                               gint             *end)
{
	InputItem *input;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

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
cdn_parser_context_set_token (CdnParserContext *context,
                              gchar const      *token)
{
	InputItem *input;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	input = CURRENT_INPUT (context);

	g_free (input->token);

	input->token = g_strdup (token);
}

gchar const *
cdn_parser_context_get_token (CdnParserContext *context)
{
	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), NULL);

	return CURRENT_INPUT (context)->token;
}

void
cdn_parser_context_define (CdnParserContext  *context,
                           CdnEmbeddedString *name,
                           GObject           *value,
                           gboolean           optional)
{
	GSList *ob;
	Context *ctx;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (value != NULL);

	if (context->priv->in_event_handler)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	for (ob = ctx->objects; ob; ob = g_slist_next (ob))
	{
		CdnSelection *sel;
		GSList *pairs;
		GSList *pair;

		sel = ob->data;

		pairs = generate_name_value_pairs (context,
		                                   sel,
		                                   name,
		                                   value,
		                                   TRUE);

		for (pair = pairs; pair; pair = g_slist_next (pair))
		{
			NameValuePair *p = pair->data;
			CdnExpansionContext *ctx;

			ctx = cdn_selection_get_context (sel);

			if (optional)
			{
				CdnExpansion *d;
				gboolean exists;

				d = cdn_expansion_context_get_define (ctx,
				                                      cdn_expansion_get (p->name, 0));

				exists = (d && *(cdn_expansion_get (d, 0)));

				if (exists)
				{
					name_value_pair_free (p);
					continue;
				}
			}

			cdn_expansion_context_add_define (ctx,
			                                  cdn_expansion_get (p->name, 0),
			                                  p->value);

			name_value_pair_free (p);
		}

		g_slist_free (pairs);
	}

	g_object_unref (name);
	g_object_unref (value);
}

/**
 * cdn_parser_context_push_input: (skip):
 *
 */
void
cdn_parser_context_push_input (CdnParserContext *context,
                               GFile            *file,
                               GInputStream     *stream,
                               gboolean          isonce)
{
	InputItem *item;
	GError *error = NULL;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (file != NULL || stream != NULL);

	if (context->priv->in_event_handler)
	{
		return;
	}

	if (file && isonce && g_hash_table_lookup (context->priv->files, file))
	{
		return;
	}

	item = input_item_new (file, stream, &error);

	if (item)
	{
		if (file)
		{
			g_hash_table_insert (context->priv->files,
			                     g_file_dup (file),
			                     GINT_TO_POINTER (1));
		}

		context->priv->inputs = g_slist_prepend (context->priv->inputs,
		                                         item);

		if (context->priv->inputs->next)
		{
			cdn_parser_tokens_push_input (context->priv->scanner);
		}

		push_scope (context, FALSE);
	}
	else
	{
		parser_failed_error (context, NULL, error);
	}
}

/**
 * cdn_parser_context_include: (skip):
 *
 */
void
cdn_parser_context_include (CdnParserContext  *context,
                            CdnEmbeddedString *filename)
{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (filename != NULL);

	if (context->priv->in_event_handler)
	{
		return;
	}

	cdn_parser_context_push_input_from_path (context,
	                                         filename,
	                                         TRUE);
}

/**
 * cdn_parser_context_link_library: (skip):
 *
 */
void
cdn_parser_context_link_library (CdnParserContext  *context,
                                 CdnEmbeddedString *filename)
{
	GSList *objs;
	Context *ctx;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (filename != NULL);

	if (context->priv->in_event_handler)
	{
		return;
	}

	ctx = context->priv->context_stack->next->data;

	for (objs = ctx->objects; objs; objs = g_slist_next (objs))
	{
		CdnSelection *sel;
		GSList *libs;

		sel = objs->data;

		expansion_context_push_selection (context, sel);
		embedded_string_expand_multiple (libs, filename, context);
		expansion_context_pop (context);

		while (libs)
		{
			CdnExpansion *ex;
			GError *error = NULL;

			ex = libs->data;

			if (!cdn_network_link_library (context->priv->network,
			                               cdn_expansion_get (ex, 0),
			                               &error))
			{
				parser_failed_error (context,
				                     CDN_STATEMENT (filename),
				                     error);

				g_slist_foreach (libs, (GFunc)cdn_expansion_unref, NULL);
				g_slist_free (libs);

				return;
			}

			cdn_expansion_unref (ex);
			libs = g_slist_delete_link (libs, libs);
		}
	}
}

/**
 * cdn_parser_context_push_input_from_path: (skip):
 *
 */
void
cdn_parser_context_push_input_from_path (CdnParserContext  *context,
                                         CdnEmbeddedString *filename,
                                         gboolean           only_in_context)
{
	GSList *items;
	GSList *item;
	InputItem *inp;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (filename != NULL);

	if (only_in_context)
	{
		Context *ctx;

		ctx = CURRENT_CONTEXT (context);

		if (!ctx || !ctx->objects)
		{
			return;
		}
	}

	if (context->priv->in_event_handler)
	{
		return;
	}

	embedded_string_expand_multiple (items, filename, context);
	inp = CURRENT_INPUT (context);

	for (item = items; item; item = g_slist_next (item))
	{
		gchar const *res;
		GFile *file = NULL;

		res = cdn_expansion_get (item->data, 0);
		
		file = cdn_network_parser_utils_resolve_import (inp ? inp->file : NULL,
		                                                res);

		if (!file)
		{
			parser_failed_error (context,
			                     CDN_STATEMENT (filename),
			                     g_error_new (G_IO_ERROR,
			                                  G_IO_ERROR_NOT_FOUND,
			                                  "Could not find file `%s'",
			                                  res));

			break;
		}

		cdn_parser_context_push_input (context,
		                               file,
		                               NULL,
		                               FALSE);

		if (!context->priv->error)
		{
			g_signal_emit (context,
			               signals[FILE_USED],
			               0,
			               file,
			               res);
		}

		g_object_unref (file);
	}

	g_slist_foreach (items, (GFunc)cdn_expansion_unref, NULL);
	g_slist_free (items);

	g_object_unref (filename);
}

/**
 * cdn_parser_context_push_input_from_string: (skip):
 *
 */
void
cdn_parser_context_push_input_from_string (CdnParserContext *context,
                                           gchar const      *s,
                                           gboolean          only_in_context)
{
	GInputStream *stream;
	gchar *ret;
	Context *ctx;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (s != NULL);

	if (context->priv->in_event_handler)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	if (only_in_context && (!ctx || !ctx->objects))
	{
		return;
	}

	ret = g_strdup (s);

	stream = g_memory_input_stream_new_from_data (ret,
	                                              strlen (ret),
	                                              (GDestroyNotify)g_free);

	cdn_parser_context_push_input (context, NULL, stream, FALSE);

	g_object_unref (stream);
}

/**
 * cdn_parser_context_pop_input: (skip):
 *
 */
void
cdn_parser_context_pop_input (CdnParserContext *context)
{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	if (context->priv->inputs)
	{
		InputItem *input;

		input = CURRENT_INPUT (context);

		++(input->lineno);
		input->cstart = 0;
		input->cend = 0;

		cdn_parser_context_pop (context);

		if (!context->priv->inputs->next)
		{
			/* Pop all the scopes */
			while (context->priv->context_stack)
			{
				cdn_parser_context_pop (context);
			}
		}

		input_item_free (context->priv->inputs->data);

		context->priv->inputs = g_slist_delete_link (context->priv->inputs,
		                                             context->priv->inputs);
	}
}

/**
 * cdn_parser_context_get_file:
 * @context: A #CdnParserContext
 *
 * Get the current parsed file. If the current parsing stage is from memory
 * or a stream only, the result will be %NULL.
 *
 * Returns: (transfer full) (allow-none): A #GFile
 *
 **/
GFile *
cdn_parser_context_get_file (CdnParserContext *context)
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
cdn_parser_context_get_start_token (CdnParserContext *context)
{
	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), 0);

	return context->priv->start_token;
}

gint
cdn_parser_context_steal_start_token (CdnParserContext *context)
{
	gint ret;

	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), 0);

	ret = context->priv->start_token;
	context->priv->start_token = 0;

	return ret;
}

void
cdn_parser_context_set_start_token (CdnParserContext *context,
                                    gint              token)
{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	context->priv->start_token = token;
}

/**
 * cdn_parser_context_push_annotation: (skip):
 *
 */
void
cdn_parser_context_push_annotation (CdnParserContext  *context,
                                    CdnEmbeddedString *annotation)
{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (annotation != NULL);

	if (context->priv->in_event_handler)
	{
		return;
	}

	if (context->priv->annotation)
	{
		g_object_unref (context->priv->annotation);
		context->priv->annotation = NULL;
	}

	context->priv->annotation = annotation;
}

/**
 * cdn_parser_context_push_layout: (skip):
 *
 */
void
cdn_parser_context_push_layout (CdnParserContext *context)
{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	if (context->priv->in_event_handler)
	{
		return;
	}

	if (!context->priv->layout)
	{
		context->priv->layout = cdn_layout_new (context->priv->network);
	}

	cdn_parser_context_push_scope (context);
}

void
cdn_parser_context_add_layout (CdnParserContext *context,
                               CdnLayoutRelation relation,
                               CdnSelector      *left,
                               CdnSelector      *right)
{
	GSList *leftobjs;
	GSList *leftobj;
	GSList *objs;
	Context *ctx;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (left == NULL || CDN_IS_SELECTOR (left));
	g_return_if_fail (CDN_IS_SELECTOR (right));

	if (context->priv->in_event_handler)
	{
		return;
	}

	ctx = context->priv->context_stack->next->data;

	for (objs = ctx->objects; objs; objs = g_slist_next (objs))
	{
		CdnSelection *sel;

		sel = objs->data;

		expansion_context_push_selection (context, sel);

		if (left != NULL)
		{
			leftobjs = cdn_selector_select (left,
			                                G_OBJECT (cdn_selection_get_object (sel)),
			                                CDN_SELECTOR_TYPE_NODE,
			                                expansion_context_peek (context));
		}
		else
		{
			leftobjs = g_slist_prepend (NULL, cdn_selection_copy (sel));
		}

		if (!leftobjs)
		{
			expansion_context_pop (context);
			return;
		}

		for (leftobj = leftobjs; leftobj; leftobj = g_slist_next (leftobj))
		{
			CdnSelection *leftsel = leftobj->data;
			GSList *rightobjs;
			CdnExpansionContext *pctx;

			pctx = expansion_context_push_base (context);

			cdn_expansion_context_merge (pctx,
			                             cdn_selection_get_context (leftsel));

			rightobjs = cdn_selector_select (right,
			                                 G_OBJECT (cdn_selection_get_object (sel)),
			                                 CDN_SELECTOR_TYPE_NODE,
			                                 expansion_context_peek (context));

			expansion_context_pop (context);

			if (!rightobjs)
			{
				continue;
			}

			GSList *rightobj;

			for (rightobj = rightobjs; rightobj; rightobj = g_slist_next (rightobj))
			{
				cdn_layout_add (context->priv->layout,
				                CDN_LAYOUTABLE (cdn_selection_get_object (leftsel)),
				                CDN_LAYOUTABLE (cdn_selection_get_object (rightobj->data)),
				                relation);
			}

			g_slist_foreach (rightobjs, (GFunc)cdn_selection_unref, NULL);
			g_slist_free (rightobjs);
		}

		g_slist_foreach (leftobjs, (GFunc)cdn_selection_unref, NULL);
		g_slist_free (leftobjs);

		expansion_context_pop (context);
	}
}

void
cdn_parser_context_add_layout_position (CdnParserContext  *context,
                                        CdnSelector       *selector,
                                        CdnEmbeddedString *x,
                                        CdnEmbeddedString *y,
                                        CdnSelector       *of,
                                        gboolean           cartesian)
{
	GSList *objs;
	GSList *cobjs;
	GSList *obj;
	Context *ctx;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (selector == NULL || CDN_IS_SELECTOR (selector));
	g_return_if_fail (x != NULL);
	g_return_if_fail (y != NULL);
	g_return_if_fail (of == NULL || CDN_IS_SELECTOR (of));

	if (context->priv->in_event_handler)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	for (cobjs = ctx->objects; cobjs; cobjs = g_slist_next (cobjs))
	{
		CdnSelection *sel;
		CdnExpansionContext *sharedctx;

		sel = cobjs->data;

		expansion_context_push_selection (context, sel);

		if (selector != NULL)
		{
			objs = cdn_selector_select (selector,
			                            cdn_selection_get_object (sel),
			                            CDN_SELECTOR_TYPE_OBJECT,
			                            expansion_context_peek (context));
		}
		else
		{
			objs = g_slist_prepend (NULL,
			                        cdn_selection_copy (sel));
		}

		sharedctx = expansion_context_push_base (context);

		for (obj = objs; obj; obj = g_slist_next (obj))
		{
			gchar const *exx;
			gchar const *exy;
			gint xx;
			gint yy;
			gdouble dx;
			gdouble dy;
			CdnExpansionContext *pctx;

			if (!CDN_IS_LAYOUTABLE (cdn_selection_get_object (obj->data)) ||
			    !cdn_layoutable_supports_location (CDN_LAYOUTABLE (cdn_selection_get_object (obj->data))))
			{
				continue;
			}

			pctx = expansion_context_push_base (context);
			cdn_expansion_context_shared_defines (pctx, sharedctx);

			cdn_expansion_context_merge (pctx,
			                             cdn_selection_get_context (obj->data));

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

				ofobjs = cdn_selector_select (of,
				                              cdn_selection_get_object (sel),
				                              CDN_SELECTOR_TYPE_OBJECT,
				                              expansion_context_peek (context));

				for (ofobj = ofobjs; ofobj; ofobj = g_slist_next (ofobj))
				{
					CdnObject *o;
					gint ox;
					gint oy;

					o = cdn_selection_get_object (ofobj->data);

					if (CDN_IS_LAYOUTABLE (o) &&
					    cdn_layoutable_supports_location (CDN_LAYOUTABLE (o)))
					{
						cdn_layoutable_get_location (CDN_LAYOUTABLE (o),
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

			cdn_layoutable_set_location (CDN_LAYOUTABLE (cdn_selection_get_object (obj->data)),
			                             xx,
			                             yy);

			expansion_context_pop (context);
		}

		expansion_context_pop (context);
		expansion_context_pop (context);
	}
}

static gboolean
set_gobject_property (CdnParserContext  *context,
                      GObject           *obj,
                      CdnEmbeddedString *nameemb,
                      CdnEmbeddedString *valueemb,
                      CdnExpansion      *name,
                      CdnExpansion      *value)
{
	/* General purpose */
	GObjectClass *klass;
	GParamSpec *spec;
	gchar const *exname;
	gchar const *exval;
	GValue dest = {0,};
	GError *error = NULL;

	klass = G_OBJECT_GET_CLASS (obj);

	exname = cdn_expansion_get (name, 0);
	exval = cdn_expansion_get (value, 0);

	spec = g_object_class_find_property (klass, exname);

	if (spec == NULL)
	{
		parser_failed (context,
		               CDN_STATEMENT (nameemb),
		               CDN_NETWORK_LOAD_ERROR_SYNTAX,
		               "Invalid property `%s' for `%s'",
		               exname,
		               g_type_name (G_TYPE_FROM_INSTANCE (obj)));

		return FALSE;
	}

	if (!cdn_string_to_value (exval,
	                          spec->value_type,
	                          &dest,
	                          &error))
	{
		parser_failed_error (context,
		                     CDN_STATEMENT (valueemb),
		                     error);

		return FALSE;
	}

	g_object_set_property (obj, exname, &dest);
	g_value_unset (&dest);

	return TRUE;
}

void
cdn_parser_context_add_integrator_variable (CdnParserContext  *context,
                                            CdnEmbeddedString *name,
                                            CdnEmbeddedString *value)
{
	GSList *item;
	Context *ctx;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (value != NULL);

	if (context->priv->in_event_handler)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		GSList *pairs;

		pairs = generate_name_value_pairs (context,
		                                   item->data,
		                                   name,
		                                   value,
		                                   FALSE);

		while (pairs)
		{
			gchar const *exname;
			gchar const *exval;
			NameValuePair *pair = pairs->data;

			exname = cdn_expansion_get (pair->name, 0);
			exval = cdn_expansion_get (pair->value, 0);

			if (g_strcmp0 (exname, "method") == 0)
			{
				GType type;
				CdnIntegrator *it;

				type = cdn_integrators_find (exval);

				if (type == G_TYPE_INVALID)
				{
					parser_failed (context,
					               CDN_STATEMENT (name),
					               CDN_NETWORK_LOAD_ERROR_SYNTAX,
					               "Could not find integrator `%s'",
					               exval);

					g_object_unref (name);
					g_object_unref (value);

					g_slist_foreach (pairs,
					                 (GFunc)name_value_pair_free,
					                 NULL);

					g_slist_free (pairs);

					return;
				}

				it = g_object_new (type, NULL);
				cdn_network_set_integrator (context->priv->network, it);

				g_object_unref (it);
			}
			else
			{
				if (!set_gobject_property (context,
				                           cdn_selection_get_object (item->data),
				                           name,
				                           value,
				                           pair->name,
				                           pair->value))
				{
					g_slist_foreach (pairs,
					                 (GFunc)name_value_pair_free,
					                 NULL);

					g_slist_free (pairs);

					return;
				}
			}

			name_value_pair_free (pairs->data);
			pairs = g_slist_delete_link (pairs, pairs);
		}
	}

	g_object_unref (name);
	g_object_unref (value);
}

/**
 * cdn_parser_context_push_string:
 * @context: A #CdnParserContext
 *
 * Description.
 *
 * Returns: (transfer none): A #CdnEmbeddedString
 *
 **/
CdnEmbeddedString *
cdn_parser_context_push_string (CdnParserContext *context)
{
	CdnEmbeddedString *s;

	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), NULL);

	s = cdn_embedded_string_new ();

	statement_start (context, s);

	context->priv->strings =
		g_slist_prepend (context->priv->strings, s);

	return s;
}

/**
 * cdn_parser_context_peek_string:
 * @context: A #CdnParserContext
 *
 * Description.
 *
 * Returns: (transfer none): A #CdnEmbeddedString
 *
 **/
CdnEmbeddedString *
cdn_parser_context_peek_string (CdnParserContext *context)
{
	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (context->priv->strings, NULL);

	return context->priv->strings->data;
}

/**
 * cdn_parser_context_pop_string:
 * @context: A #CdnParserContext
 *
 * Description.
 *
 * Returns: (transfer full): A #CdnEmbeddedString
 *
 **/
CdnEmbeddedString *
cdn_parser_context_pop_string (CdnParserContext *context)
{
	CdnEmbeddedString *s;

	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (context->priv->strings, NULL);

	s = context->priv->strings->data;

	statement_end (context, s);

	context->priv->strings =
		g_slist_delete_link (context->priv->strings,
		                     context->priv->strings);

	return s;
}

void
cdn_parser_context_push_equation (CdnParserContext *context)
{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	context->priv->equations =
		g_slist_prepend (context->priv->equations,
		                 GINT_TO_POINTER (1));
}

void
cdn_parser_context_push_equation_depth (CdnParserContext *context)
{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (context->priv->equations);

	context->priv->equations->data =
		GINT_TO_POINTER (GPOINTER_TO_INT (context->priv->equations->data) + 1);
}

gint
cdn_parser_context_peek_equation_depth (CdnParserContext *context)
{
	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), 0);
	g_return_val_if_fail (context->priv->equations, 0);

	return GPOINTER_TO_INT (context->priv->equations->data);
}

gboolean
cdn_parser_context_pop_equation_depth (CdnParserContext *context)
{
	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), FALSE);
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

static void
debug_selector (CdnParserContext *context,
                CdnSelection     *selection,
                GSList           *objects)
{
	gchar *fullid;
	GSList *orig = objects;

	fullid = cdn_object_get_full_id_for_display (cdn_selection_get_object (selection));

	while (objects)
	{
		gchar *msg;
		CdnSelection *sel;

		sel = objects->data;

		if (CDN_IS_OBJECT (cdn_selection_get_object (sel)))
		{
			msg = cdn_object_get_full_id_for_display (cdn_selection_get_object (sel));
		}
		else
		{
			msg = cdn_variable_get_full_name_for_display (cdn_selection_get_object (sel));
		}

		g_printerr ("[debug] (%d): {%s} => %s\n",
		            CURRENT_INPUT (context)->lineno,
		            fullid,
		            msg);

		g_free (msg);
		objects = g_slist_next (objects);
	}

	g_free (fullid);

	g_slist_foreach (orig, (GFunc)cdn_selection_unref, NULL);
	g_slist_free (orig);
}

void
cdn_parser_context_debug_selector (CdnParserContext *context,
                                   CdnSelector      *selector)
{
	GSList *item;
	Context *ctx;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CDN_IS_SELECTOR (selector));

	if (context->priv->in_event_handler)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		expansion_context_push_selection (context, item->data);

		debug_selector (context,
		                item->data,
		                cdn_selector_select (selector,
		                                     cdn_selection_get_object (item->data),
		                                     CDN_SELECTOR_TYPE_ANY,
		                                     expansion_context_peek (context)));

		expansion_context_pop (context);
	}
}

void
cdn_parser_context_debug_string (CdnParserContext  *context,
                                 CdnEmbeddedString *s)
{
	GSList *item;
	Context *ctx;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CDN_IS_EMBEDDED_STRING (s));

	if (context->priv->in_event_handler)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		GSList *ret;
		gboolean ismulti;

		expansion_context_push_selection (context, item->data);
		embedded_string_expand_multiple (ret, s, context);

		ismulti = ret && ret->next && cdn_expansion_num (ret->data) > 1;

		if (!ismulti)
		{
			g_printerr ("[debug] (%d): %s\n",
			            CURRENT_INPUT (context)->lineno,
			            cdn_expansion_get (ret->data, 0));
		}
		else
		{
			gchar *ss;

			ss = expansions_as_string (ret);

			g_printerr ("[debug] (%d): %s\n",
			            CURRENT_INPUT (context)->lineno,
			            ss);

			g_free (ss);
		}
	
		expansion_context_pop (context);
		g_slist_foreach (ret, (GFunc)cdn_expansion_unref, NULL);
		g_slist_free (ret);
	}
}

void
cdn_parser_context_debug_context (CdnParserContext *context)
{
	GSList *item;
	GFile *f;
	gchar *name;
	gint lineno;
	InputItem *input;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	if (context->priv->in_event_handler)
	{
		return;
	}

	f = cdn_parser_context_get_file (context);
	name = g_file_get_basename (f);
	g_object_unref (f);

	input = CURRENT_INPUT (context);
	lineno = input ? input->lineno : 0;

	for (item = CURRENT_CONTEXT (context)->objects; item; item = g_slist_next (item))
	{
		gchar *s;
		CdnSelection *sel;

		sel = item->data;

		s = cdn_object_get_full_id (cdn_selection_get_object (sel));
		g_printerr ("[debug(%s:%d)] Selection: %s\n",
		            name,
		            lineno,
		            s);
		g_free (s);

		cdn_expansion_context_debug_print (cdn_selection_get_context (sel),
		                                   stderr);
	}

	g_free (name);
}

void
cdn_parser_context_delete_selector (CdnParserContext *context,
                                    CdnSelector      *selector)
{
	GSList *ret;
	GSList *item;
	Context *ctx;
	GSList *oo;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CDN_IS_SELECTOR (selector));

	if (context->priv->in_event_handler)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	for (oo = ctx->objects; oo; oo = g_slist_next (oo))
	{
		expansion_context_push_selection (context, oo->data);

		ret = cdn_selector_select (selector,
		                           cdn_selection_get_object (oo->data),
		                           CDN_SELECTOR_TYPE_ANY,
		                           expansion_context_peek (context));

		for (item = ret; item; item = g_slist_next (item))
		{
			CdnSelection *sel = item->data;
			gpointer obj = cdn_selection_get_object (sel);
			GError *error = NULL;

			if (CDN_IS_VARIABLE (obj))
			{
				CdnObject *parent;

				parent = cdn_variable_get_object (obj);

				if (!cdn_object_remove_variable (parent,
				                                 cdn_variable_get_name (obj),
				                                 &error))
				{
					parser_failed_error (context,
					                     CDN_STATEMENT (selector),
					                     error);
					break;
				}
			}
			else if (CDN_IS_EDGE_ACTION (obj))
			{
				CdnEdge *link;

				link = cdn_edge_action_get_edge (obj);

				cdn_edge_remove_action (link, obj);
			}
			else if (CDN_IS_OBJECT (obj))
			{
				CdnNode *parent;

				parent = cdn_object_get_parent (obj);

				if (!cdn_node_remove (parent, obj, &error))
				{
					parser_failed_error (context,
					                     CDN_STATEMENT (selector),
					                     error);
					break;
				}
			}
		}

		expansion_context_pop (context);

		g_slist_foreach (ret, (GFunc)cdn_selection_unref, NULL);
		g_slist_free (ret);
	}
}

/**
 * cdn_parser_context_current_selections:
 * @context: A #CdnParserContext
 *
 * Description.
 *
 * Returns: (element-type CdnSelection) (transfer none): A #GSList
 *
 **/
GSList const *
cdn_parser_context_current_selections (CdnParserContext *context)
{
	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), NULL);

	if (!context->priv->context_stack)
	{
		return NULL;
	}

	return CURRENT_CONTEXT (context)->objects;
}

/**
 * cdn_parser_context_previous_selections:
 * @context: A #CdnParserContext
 *
 * Description.
 *
 * Returns: (element-type CdnSelection) (transfer none): A #GSList
 *
 **/
GSList const *
cdn_parser_context_previous_selections (CdnParserContext *context)
{
	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), NULL);

	if (!context->priv->context_stack || !context->priv->context_stack->next)
	{
		return NULL;
	}

	return ((Context *)context->priv->context_stack->next->data)->objects;
}

void
cdn_parser_context_begin_selector_item (CdnParserContext *context)
{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	statement_start (context, ensure_selector (context));
}

void
cdn_parser_context_get_error_location (CdnParserContext *context,
                                       gint             *lstart,
                                       gint             *lend,
                                       gint             *cstart,
                                       gint             *cend)
{
	CdnStatement *st;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	st = context->priv->error_statement;

	if (st)
	{
		cdn_statement_get_line (st, lstart, lend);
		cdn_statement_get_column (st, cstart, cend);
	}
	else
	{
		cdn_parser_context_get_line (context, lstart);
		cdn_parser_context_get_line (context, lend);

		cdn_parser_context_get_column (context, cstart, cend);
	}
}

/**
 * cdn_parser_context_get_error_lines:
 * @context: A #CdnParserContext
 *
 * Get the lines of text on which the error occurred.
 *
 * Returns: (transfer full): The lines on which the error occurred
 *
 **/
gchar *
cdn_parser_context_get_error_lines (CdnParserContext *context)
{
	gint lstart;
	gint lend;
	gint cstart;
	gint cend;
	GString *ret;
	gboolean first;

	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), NULL);

	cdn_parser_context_get_error_location (context,
	                                       &lstart,
	                                       &lend,
	                                       &cstart,
	                                       &cend);

	ret = g_string_new ("");
	first = TRUE;

	while (lstart <= lend)
	{
		gchar const *line;

		line = cdn_parser_context_get_line_at (context, lstart);

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
apply_unapply_template (CdnParserContext *context,
                        CdnSelector      *templates,
                        CdnSelector      *targets,
                        gboolean          apply)
{
	Context *ctx;
	GSList *obj;
	CdnNode *template_group;
	gboolean ret = TRUE;

	if (context->priv->in_event_handler)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	template_group = cdn_network_get_template_node (context->priv->network);

	for (obj = ctx->objects; obj; obj = g_slist_next (obj))
	{
		GSList *temps;
		GSList *temp;

		expansion_context_push_selection (context, obj->data);

		/* Select templates now */
		temps = cdn_selector_select (templates,
		                             G_OBJECT (template_group),
		                             CDN_SELECTOR_TYPE_TEMPLATE,
		                             expansion_context_peek (context));

		if (!temps)
		{
			expansion_context_pop (context);
			continue;
		}

		for (temp = temps; temp; temp = g_slist_next (temp))
		{
			GSList *targobjs;
			GSList *targobj;
			gboolean freetargs;
			gpointer templobj;

			templobj = cdn_selection_get_object (temp->data);

			if (!targets)
			{
				targobjs = g_slist_prepend (NULL, obj->data);
				freetargs = FALSE;
			}
			else
			{
				expansion_context_push_selection (context, temp->data);

				targobjs = cdn_selector_select (targets,
				                                cdn_selection_get_object (obj->data),
				                                CDN_SELECTOR_TYPE_OBJECT,
				                                expansion_context_peek (context));

				expansion_context_pop (context);
				freetargs = TRUE;
			}

			for (targobj = targobjs; targobj; targobj = g_slist_next (targobj))
			{
				CdnSelection *s;
				GError *error = NULL;
				gboolean ret;

				s = targobj->data;

				if (apply)
				{
					ret = cdn_object_apply_template (cdn_selection_get_object (s),
					                                 templobj,
					                                 &error);
				}
				else
				{
					ret = cdn_object_unapply_template (cdn_selection_get_object (s),
					                                   templobj,
					                                   &error);
				}

				if (!ret)
				{
					parser_failed_error (context,
					                     CDN_STATEMENT (targets),
					                     error);
					break;
				}
			}

			if (freetargs)
			{
				g_slist_foreach (targobjs, (GFunc)cdn_selection_unref, NULL);
			}

			g_slist_free (targobjs);

			if (!ret)
			{
				break;
			}
		}

		g_slist_foreach (temps, (GFunc)cdn_selection_unref, NULL);
		g_slist_free (temps);

		if (!ret)
		{
			break;
		}
	}
}

void
cdn_parser_context_apply_template (CdnParserContext *context,
                                   CdnSelector      *templates,
                                   CdnSelector      *targets)
{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CDN_IS_SELECTOR (templates));
	g_return_if_fail (targets == NULL || CDN_IS_SELECTOR (targets));

	if (context->priv->in_event_handler)
	{
		return;
	}

	apply_unapply_template (context, templates, targets, TRUE);
}

void
cdn_parser_context_unapply_template (CdnParserContext *context,
                                     CdnSelector      *templates,
                                     CdnSelector      *targets)

{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CDN_IS_SELECTOR (templates));
	g_return_if_fail (targets == NULL || CDN_IS_SELECTOR (targets));

	if (context->priv->in_event_handler)
	{
		return;
	}

	apply_unapply_template (context, templates, targets, FALSE);
}

/**
 * cdn_parser_context_remove_record: (skip):
 *
 */
void
cdn_parser_context_remove_record (CdnParserContext *context,
                                  gint              len,
                                  gint              offset)

{
	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	if (!context->priv->in_event_handler)
	{
		return;
	}
}

gboolean
cdn_parser_context_get_first_eof (CdnParserContext *context)
{
	InputItem *inp;

	g_return_val_if_fail (CDN_IS_PARSER_CONTEXT (context), FALSE);

	inp = CURRENT_INPUT (context);

	if (!inp)
	{
		return FALSE;
	}

	return inp->firsteof;
}

void
cdn_parser_context_set_first_eof (CdnParserContext *context,
                                  gboolean          firsteof)
{
	InputItem *inp;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));

	inp = CURRENT_INPUT (context);

	inp->firsteof = firsteof;
}

static gchar *
event_id (GSList   *from_phases,
          GSList   *to_phases,
          gboolean  terminal)
{
	GString *id;

	id = g_string_new ("from_");

	if (from_phases)
	{
		while (from_phases)
		{
			g_string_append (id, cdn_expansion_get (from_phases->data, 0));
			g_string_append_c (id, '_');

			from_phases = g_slist_next (from_phases);
		}
	}
	else
	{
		g_string_append (id, "any_");
	}

	g_string_append (id, "to_");

	if (to_phases)
	{
		g_string_append (id, cdn_expansion_get (to_phases->data, 0));
	}
	else if (terminal)
	{
		g_string_append (id, "terminal");
	}
	else
	{
		g_string_append (id, "any");
	}

	return g_string_free (id, FALSE);
}

/**
 * cdn_parser_context_push_event: (skip):
 *
 */
void
cdn_parser_context_push_event (CdnParserContext  *context,
                               CdnEmbeddedString *from_phase,
                               CdnEmbeddedString *to_phase,
                               CdnEmbeddedString *condition,
                               gboolean           terminal,
                               CdnEmbeddedString *approximation,
                               GSList            *templates)
{
	GSList *parents;
	GSList *ret = NULL;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CDN_IS_EMBEDDED_STRING (condition));

	if (context->priv->in_event_handler)
	{
		return;
	}

	parents = each_selections (context, TRUE);

	while (parents)
	{
		GSList *conds;
		CdnExpression *expr;
		CdnEvent *ev;
		GSList *to_phases = NULL;
		GSList *from_phases = NULL;
		gchar *id;
		gchar *newid;
		CdnExpansion *ex;
		CdnSelection *sel;

		expansion_context_push_selection (context, parents->data);
		embedded_string_expand_multiple (conds, condition, context);

		while (conds)
		{
			expr = cdn_expression_new (cdn_expansion_get (conds->data, 0));

			expansion_context_push_base (context);

			cdn_expansion_context_add_expansion (expansion_context_peek (context),
			                                     conds->data);

			if (from_phase)
			{
				embedded_string_expand_multiple (from_phases,
				                                 from_phase,
				                                 context);
			}

			if (to_phase)
			{
				embedded_string_expand_multiple (to_phases,
				                                 to_phase,
				                                 context);

				if (to_phases->next)
				{
					parser_failed (context,
					               CDN_STATEMENT (to_phase),
					               CDN_NETWORK_LOAD_ERROR_SYNTAX,
					               "Cannot transfer to multiple phases after event");

					g_slist_foreach (to_phases, (GFunc)cdn_expansion_unref, NULL);
					g_slist_free (to_phases);

					g_slist_foreach (from_phases, (GFunc)cdn_expansion_unref, NULL);
					g_slist_free (from_phases);

					g_slist_foreach (ret, (GFunc)cdn_selection_unref, NULL);
					g_slist_free (ret);

					g_slist_foreach (parents, (GFunc)cdn_selection_unref, NULL);
					g_slist_free (parents);

					expansion_context_pop (context);
					expansion_context_pop (context);
					break;
				}
			}

			id = event_id (from_phases, to_phases, terminal);

			newid = unique_id (cdn_selection_get_object (parents->data),
			                   id);

			ex = cdn_expansion_new_one (newid);

			g_free (id);
			g_free (newid);

			sel = parse_object_single_id (context,
			                              ex,
			                              templates,
			                              parents->data,
			                              CDN_TYPE_EVENT,
			                              TRUE,
			                              "id",
			                              cdn_expansion_get (ex, 0),
			                              NULL);

			ret = g_slist_prepend (ret, sel);

			cdn_expansion_unref (ex);

			ev = cdn_selection_get_object (sel);

			cdn_event_set_condition (ev, expr);

			if (approximation)
			{
				GSList *approx;

				embedded_string_expand_multiple (approx,
				                                 approximation,
				                                 context);

				if (approx)
				{
					gdouble val;

					val = g_ascii_strtod (cdn_expansion_get (approx->data, 0),
					                      NULL);

					cdn_event_set_approximation (ev, val);
				}

				g_slist_foreach (approx,
				                 (GFunc)cdn_expansion_unref,
				                 NULL);

				g_slist_free (approx);
			}

			while (from_phases)
			{
				cdn_phaseable_add_phase (CDN_PHASEABLE (ev),
				                         cdn_expansion_get (from_phases->data, 0));

				cdn_expansion_unref (from_phases->data);
				from_phases = g_slist_delete_link (from_phases,
				                                   from_phases);
			}

			if (to_phases)
			{
				cdn_event_set_goto_phase (ev,
				                          cdn_expansion_get (to_phases->data, 0));
			}

			if (terminal)
			{
				cdn_event_set_terminal (ev, TRUE);
			}

			g_slist_foreach (to_phases, (GFunc)cdn_expansion_unref, NULL);
			g_slist_free (to_phases);

			g_slist_foreach (from_phases, (GFunc)cdn_expansion_unref, NULL);
			g_slist_free (from_phases);

			cdn_expansion_unref (conds->data);
			conds = g_slist_delete_link (conds, conds);

			expansion_context_pop (context);
		}

		expansion_context_pop (context);

		cdn_selection_unref (parents->data);
		parents = g_slist_delete_link (parents, parents);
	}

	cdn_parser_context_push_objects (context, ret);
	g_slist_free (ret);
}

void
cdn_parser_context_add_event_set_variable (CdnParserContext  *context,
                                           CdnSelector       *selector,
                                           CdnEmbeddedString *value)
{
	Context *ctx;
	GSList *item;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CDN_IS_SELECTOR (selector));
	g_return_if_fail (CDN_IS_EMBEDDED_STRING (value));

	if (context->priv->in_event_handler)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		GSList *ret;
		gint i;
		gint num;
		CdnEvent *ev;
		CdnObject *self;

		ev = cdn_selection_get_object (item->data);

		if (!CDN_IS_EVENT (ev))
		{
			continue;
		}

		expansion_context_push_selection (context, item->data);
		self = cdn_selection_get_object (item->data);

		ret = cdn_selector_select (selector,
		                           G_OBJECT (cdn_object_get_parent (self)),
		                           CDN_SELECTOR_TYPE_VARIABLE,
		                           expansion_context_peek (context));

		i = 0;
		num = g_slist_length (ret);

		while (ret)
		{
			GSList *vals = NULL;
			CdnExpansion *val;

			expansion_context_push_selection (context, ret->data);
			embedded_string_expand_multiple (vals, value, context);

			if (g_slist_length (vals) == num)
			{
				val = g_slist_nth_data (vals, i);
			}
			else
			{
				val = vals->data;
			}

			cdn_event_add_set_variable (ev,
			                            CDN_VARIABLE (cdn_selection_get_object (ret->data)),
			                            cdn_expression_new (cdn_expansion_get (val, 0)));

			g_slist_foreach (vals, (GFunc)cdn_expansion_unref, NULL);
			g_slist_free (vals);

			expansion_context_pop (context);

			cdn_selection_unref (ret->data);
			ret = g_slist_delete_link (ret, ret);

			++i;
		}

		expansion_context_pop (context);
	}
}

/**
 * cdn_parser_context_push_io_type: (skip):
 *
 */
void
cdn_parser_context_push_io_type (CdnParserContext  *context,
                                 CdnIoMode          mode,
                                 CdnEmbeddedString *id,
                                 CdnEmbeddedString *type)
{
	Context *ctx;
	GSList *item;
	GSList *ret = NULL;

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CDN_IS_EMBEDDED_STRING (id));
	g_return_if_fail (CDN_IS_EMBEDDED_STRING (type));

	if (context->priv->in_event_handler)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		CdnSelection *sel;
		GSList *pairs;

		sel = item->data;

		pairs = generate_name_value_pairs (context,
		                                   sel,
		                                   id,
		                                   type,
		                                   FALSE);

		while (pairs)
		{
			NameValuePair *pair = pairs->data;
			GType tp;
			CdnSelection *newsel;

			tp = cdn_io_method_find (cdn_expansion_get (pair->value, 0),
			                         mode);

			if (tp == G_TYPE_INVALID)
			{
				parser_failed (context,
				               CDN_STATEMENT (type),
				               CDN_NETWORK_LOAD_ERROR_IO,
				               "Could not find io type `%s'",
				               cdn_expansion_get (pair->value, 0));

				g_slist_foreach (pairs, (GFunc)name_value_pair_free, NULL);
				g_slist_free (pairs);

				g_slist_foreach (ret, (GFunc)cdn_selection_unref, NULL);
				g_slist_free (ret);
				return;
			}

			newsel = parse_object_single_id (context,
			                                 pair->name,
			                                 NULL,
			                                 sel,
			                                 tp,
			                                 TRUE,
			                                 "id",
			                                 cdn_expansion_get (pair->name, 0),
			                                 "mode",
			                                 mode,
			                                 NULL);

			if (!newsel)
			{
				g_slist_foreach (pairs, (GFunc)name_value_pair_free, NULL);
				g_slist_free (pairs);

				g_slist_foreach (ret, (GFunc)cdn_selection_unref, NULL);
				g_slist_free (ret);
				return;
			}

			ret = g_slist_prepend (ret, newsel);

			name_value_pair_free (pair);
			pairs = g_slist_delete_link (pairs, pairs);
		}
	}

	cdn_parser_context_push_objects (context,
	                                 g_slist_reverse (ret));
	g_slist_free (ret);

	g_object_unref (id);
	g_object_unref (type);
}

void
cdn_parser_context_set_io_setting (CdnParserContext  *context,
                                   CdnEmbeddedString *name,
                                   CdnEmbeddedString *value)
{
	Context *ctx;
	GSList *obj;

	if (context->priv->in_event_handler)
	{
		return;
	}

	g_return_if_fail (CDN_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CDN_IS_EMBEDDED_STRING (name));
	g_return_if_fail (CDN_IS_EMBEDDED_STRING (value));

	ctx = CURRENT_CONTEXT (context);

	for (obj = ctx->objects; obj; obj = g_slist_next (obj))
	{
		GSList *pairs;
		CdnSelection *sel;

		sel = obj->data;

		pairs = generate_name_value_pairs (context,
		                                   sel,
		                                   name,
		                                   value,
		                                   FALSE);

		while (pairs)
		{
			NameValuePair *pair = pairs->data;

			if (!set_gobject_property (context,
			                           cdn_selection_get_object (sel),
			                           name,
			                           value,
			                           pair->name,
			                           pair->value))
			{
				g_slist_foreach (pairs, (GFunc)name_value_pair_free, NULL);
				g_slist_free (pairs);

				return;
			}

			name_value_pair_free (pair);
			pairs = g_slist_delete_link (pairs, pairs);
		}
	}

	g_object_unref (name);
	g_object_unref (value);
}
