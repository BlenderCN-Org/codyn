/*
 * cdn-selector.c
 * This file is part of cdn-root
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

#include "cdn-selector.h"
#include "cdn-parser-context.h"
#include "cdn-parser.h"
#include "cdn-expansion.h"
#include "cdn-selection.h"
#include "cdn-statement.h"
#include "cdn-taggable.h"

#include <string.h>

#define CDN_SELECTOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_SELECTOR, CdnSelectorPrivate))

#define CDN_SELECTOR_KEY_OVERRIDE_NAME "CdnSelectorKeyOverrideName"

typedef enum
{
	SELECTOR_TYPE_IDENTIFIER,
	SELECTOR_TYPE_REGEX,
	SELECTOR_TYPE_PSEUDO
} SelectorType;

typedef struct
{
	gint a;
	gint b;
	gint max;
} Nth;

typedef struct
{
	SelectorType type;
	guint id;
	GSList *selections_in;
	GSList *selections_out;

	union
	{
		struct
		{
			CdnEmbeddedString *identifier;
			gboolean partial;
		} identifier;

		struct
		{
			CdnEmbeddedString *regex;
			gboolean partial;
		} regex;

		struct
		{
			CdnSelectorPseudoType type;
			GSList *arguments;
		} pseudo;
	};
} Selector;

struct _CdnSelectorPrivate
{
	CdnObject *root;
	CdnSelection *self;

	GSList *selectors;

	gboolean has_selected;
	GSList *from_set;

	guint last_id;

	gint line_start;
	gint line_end;
	gint column_start;
	gint column_end;

	gboolean implicit_children;
	gboolean prevent_implicit_children;
};

enum
{
	SELECT,
	NUM_SIGNALS
};

static gchar const *selector_pseudo_names[CDN_SELECTOR_PSEUDO_NUM] =
{
	"root",
	"templates-root",
	"children",
	"parent",
	"first",
	"last",
	"subset",
	"states",
	"links",
	"groups",
	"imports",
	"variables",
	"actions",
	"functions",
	"objects",
	"siblings",
	"templates",
	"count",
	"source",
	"sink",
	"self",
	"debug",
	"name",
	"descendants",
	"ancestors",
	"unique",
	"if",
	"is-empty",
	"remove",
	"from-set",
	"type",
	"has-flag",
	"has-template",
	"has-tag",
	"reverse",
	"source-name",
	"sink-name"
};

static guint signals[NUM_SIGNALS];

static void cdn_statement_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CdnSelector,
                         cdn_selector,
                         G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_STATEMENT, cdn_statement_iface_init))

enum
{
	PROP_0,
	PROP_LINE_START,
	PROP_LINE_END,
	PROP_COLUMN_START,
	PROP_COLUMN_END,
	PROP_ROOT
};

static void
cdn_statement_iface_init (gpointer iface)
{
}

static Selector *
selector_new (CdnSelector  *selector,
              SelectorType  type)
{
	Selector *ret;

	ret = g_slice_new0 (Selector);
	ret->type = type;
	ret->id = ++selector->priv->last_id;

	return ret;
}

static void
free_current_selections (Selector *selector)
{
	g_slist_foreach (selector->selections_in, (GFunc)g_object_unref, NULL);
	selector->selections_in = NULL;

	g_slist_foreach (selector->selections_out, (GFunc)g_object_unref, NULL);
	selector->selections_out = NULL;
}

static void
selector_free (Selector *selector)
{
	switch (selector->type)
	{
		case SELECTOR_TYPE_IDENTIFIER:
			g_object_unref (selector->identifier.identifier);
		break;
		case SELECTOR_TYPE_REGEX:
			g_object_unref (selector->regex.regex);
		break;
		case SELECTOR_TYPE_PSEUDO:
			g_slist_foreach (selector->pseudo.arguments, (GFunc)g_object_unref, NULL);
			g_slist_free (selector->pseudo.arguments);
		break;
	}

	free_current_selections (selector);

	g_slice_free (Selector, selector);
}

static gchar *
selector_identifier_to_string (Selector *selector)
{
	return g_strdup_printf ("\"%s\"",
	                        cdn_embedded_string_expand (selector->identifier.identifier,
	                                                    NULL,
	                                                    NULL));
}

static gchar *
selector_regex_to_string (Selector *selector)
{
	return g_strdup_printf ("/%s/",
	                        cdn_embedded_string_expand (selector->identifier.identifier,
	                                                    NULL,
	                                                    NULL));
}

static Selector *
selector_identifier_new (CdnSelector       *sel,
                         CdnEmbeddedString *identifier,
                         gboolean           partial)
{
	Selector *selector;

	selector = selector_new (sel, SELECTOR_TYPE_IDENTIFIER);

	selector->identifier.identifier = g_object_ref (identifier);
	selector->identifier.partial = partial;

	return selector;
}

static Selector *
selector_regex_new (CdnSelector       *sel,
                    CdnEmbeddedString *regex,
                    gboolean           partial)
{
	Selector *selector;

	selector = selector_new (sel, SELECTOR_TYPE_REGEX);

	selector->regex.regex = g_object_ref (regex);
	selector->regex.partial = partial;

	return selector;
}

static gchar const *
pseudo_name (CdnSelectorPseudoType type)
{
	g_return_val_if_fail (type >= 0 && type < CDN_SELECTOR_PSEUDO_NUM, NULL);

	return selector_pseudo_names[type];
}

static gchar *
selector_pseudo_to_string (Selector *selector)
{
	GString *ret;
	GSList *arguments;
	gboolean first = TRUE;

	if (!selector->pseudo.arguments)
	{
		return g_strdup (pseudo_name (selector->pseudo.type));
	}

	ret = g_string_new ("");

	g_string_append (ret, pseudo_name (selector->pseudo.type));
	g_string_append_c (ret, '(');

	arguments = selector->pseudo.arguments;

	while (arguments)
	{
		if (!first)
		{
			g_string_append (ret, ", ");
		}
		else
		{
			first = FALSE;
		}

		if (CDN_IS_EMBEDDED_STRING (arguments->data))
		{
			g_string_append_printf (ret,
			                        "\"%s\"",
			                        cdn_embedded_string_expand (arguments->data,
			                                                    NULL,
			                                                    NULL));
		}
		else if (CDN_IS_SELECTOR (arguments->data))
		{
			gchar *s;

			s = cdn_selector_as_string (arguments->data);

			g_string_append (ret, s);
			g_free (s);
		}

		arguments = g_slist_next (arguments);
	}

	g_string_append_c (ret, ')');

	return g_string_free (ret, FALSE);
}

static gchar *
selector_to_string (Selector *selector)
{
	switch (selector->type)
	{
		case SELECTOR_TYPE_IDENTIFIER:
			return selector_identifier_to_string (selector);
		break;
		case SELECTOR_TYPE_REGEX:
			return selector_regex_to_string (selector);
		break;
		case SELECTOR_TYPE_PSEUDO:
			return selector_pseudo_to_string (selector);
		break;
	}

	return NULL;
}

static Selector *
selector_pseudo_new (CdnSelector           *sel,
                     CdnSelectorPseudoType  type,
                     GSList                *arguments)
{
	Selector *selector;

	selector = selector_new (sel, SELECTOR_TYPE_PSEUDO);

	selector->pseudo.type = type;
	selector->pseudo.arguments = g_slist_copy (arguments);

	g_slist_foreach (selector->pseudo.arguments, (GFunc)g_object_ref, NULL);

	return selector;
}

static Selector *
selector_copy (CdnSelector *self,
               Selector    *selector)
{
	switch (selector->type)
	{
		case SELECTOR_TYPE_IDENTIFIER:
			return selector_identifier_new (self,
			                                selector->identifier.identifier,
			                                selector->identifier.partial);
		break;
		case SELECTOR_TYPE_REGEX:
			return selector_regex_new (self,
			                           selector->regex.regex,
			                           selector->regex.partial);
		break;
		case SELECTOR_TYPE_PSEUDO:
			return selector_pseudo_new (self,
			                            selector->pseudo.type,
			                            selector->pseudo.arguments);
		break;
	}

	return NULL;
}

static Nth
parse_nth (GSList             *arguments,
           CdnEmbeddedContext *context)
{
	Nth ret = {1, 1, -1};
	GSList *maxpos = NULL;
	GSList *apos = NULL;
	gchar const *first;

	if (!arguments)
	{
		return ret;
	}

	first = cdn_embedded_string_expand (arguments->data, context, NULL);

	if (g_strcmp0 (first, "odd") == 0)
	{
		ret.a = 2;
		ret.b = 1;

		maxpos = arguments->next;
	}
	else if (g_strcmp0 (first, "even") == 0)
	{
		ret.a = 2;
		ret.b = 0;

		maxpos = arguments->next;
	}
	else if (arguments->next)
	{
		gchar const *second;

		second = cdn_embedded_string_expand (arguments->next->data,
		                                     context,
		                                     NULL);

		ret.b = (gint)g_ascii_strtoll (first, NULL, 10);
		ret.max = (gint)g_ascii_strtoll (second, NULL, 10);

		apos = arguments->next->next;
	}
	else
	{
		ret.b = (gint)g_ascii_strtoll (first, NULL, 10);
	}

	if (maxpos)
	{
		ret.max = (gint)g_ascii_strtoll (cdn_embedded_string_expand (maxpos->data, context, NULL), NULL, 10);
	}

	if (apos)
	{
		ret.a = (gint)g_ascii_strtoll (cdn_embedded_string_expand (apos->data, context, NULL), NULL, 10);
	}

	return ret;
}

static void
cdn_selector_finalize (GObject *object)
{
	CdnSelector *selector;

	selector = CDN_SELECTOR (object);

	g_slist_foreach (selector->priv->selectors, (GFunc)selector_free, NULL);
	g_slist_free (selector->priv->selectors);

	g_slist_foreach (selector->priv->from_set, (GFunc)g_object_unref, NULL);
	g_slist_free (selector->priv->from_set);

	if (selector->priv->self)
	{
		g_object_unref (selector->priv->self);
	}

	G_OBJECT_CLASS (cdn_selector_parent_class)->finalize (object);
}

static void
cdn_selector_dispose (GObject *object)
{
	CdnSelector *selector;

	selector = CDN_SELECTOR (object);

	if (selector->priv->root)
	{
		g_object_unref (selector->priv->root);
		selector->priv->root = NULL;
	}

	G_OBJECT_CLASS (cdn_selector_parent_class)->dispose (object);
}

static void
cdn_selector_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
	CdnSelector *self = CDN_SELECTOR (object);

	switch (prop_id)
	{
		case PROP_LINE_START:
			self->priv->line_start = g_value_get_int (value);
			break;
		case PROP_LINE_END:
			self->priv->line_end = g_value_get_int (value);
			break;
		case PROP_COLUMN_START:
			self->priv->column_start = g_value_get_int (value);
			break;
		case PROP_COLUMN_END:
			self->priv->column_end = g_value_get_int (value);
			break;
		case PROP_ROOT:
			self->priv->root = g_value_dup_object (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_selector_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
	CdnSelector *self = CDN_SELECTOR (object);

	switch (prop_id)
	{
		case PROP_LINE_START:
			g_value_set_int (value, self->priv->line_start);
			break;
		case PROP_LINE_END:
			g_value_set_int (value, self->priv->line_end);
			break;
		case PROP_COLUMN_START:
			g_value_set_int (value, self->priv->column_start);
			break;
		case PROP_COLUMN_END:
			g_value_set_int (value, self->priv->column_end);
			break;
		case PROP_ROOT:
			g_value_set_object (value, self->priv->root);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_selector_class_init (CdnSelectorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_selector_finalize;
	object_class->dispose = cdn_selector_dispose;

	object_class->get_property = cdn_selector_get_property;
	object_class->set_property = cdn_selector_set_property;

	signals[SELECT] =
		g_signal_new ("select",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__UINT,
		              G_TYPE_NONE,
		              1,
		              G_TYPE_UINT);

	g_type_class_add_private (object_class, sizeof (CdnSelectorPrivate));

	g_object_class_override_property (object_class,
	                                  PROP_LINE_START,
	                                  "line-start");

	g_object_class_override_property (object_class,
	                                  PROP_LINE_END,
	                                  "line-end");

	g_object_class_override_property (object_class,
	                                  PROP_COLUMN_START,
	                                  "column-start");

	g_object_class_override_property (object_class,
	                                  PROP_COLUMN_END,
	                                  "column-end");

	g_object_class_install_property (object_class,
	                                 PROP_ROOT,
	                                 g_param_spec_object ("root",
	                                                      "Root",
	                                                      "Root",
	                                                      CDN_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cdn_selector_init (CdnSelector *self)
{
	self->priv = CDN_SELECTOR_GET_PRIVATE (self);
}

CdnSelector *
cdn_selector_new (CdnObject *root)
{
	return g_object_new (CDN_TYPE_SELECTOR,
	                     "root", root,
	                     NULL);
}

/**
 * cdn_selector_parse:
 * @root: (allow-none): A #CdnObject
 * @s: The selector
 * @error: A #GError
 *
 * Parse a selector from a string.
 *
 * Returns: (transfer full): A #CdnSelector
 *
 **/
CdnSelector *
cdn_selector_parse (CdnObject    *root,
                    gchar const  *s,
                    GError      **error)
{
	CdnSelector *ret;
	CdnParserContext *ctx;
	GInputStream *stream;

	g_return_val_if_fail (root == NULL || CDN_IS_OBJECT (root), NULL);
	g_return_val_if_fail (s != NULL, NULL);

	ctx = cdn_parser_context_new (NULL);
	cdn_parser_context_set_start_token (ctx, T_START_SELECTOR);

	stream = g_memory_input_stream_new_from_data (s, strlen (s), NULL);

	cdn_parser_context_push_input (ctx, NULL, stream, NULL);
	g_object_unref (stream);

	if (!cdn_parser_context_parse (ctx, TRUE, error))
	{
		return NULL;
	}

	ret = cdn_parser_context_pop_selector (ctx);
	ret->priv->root = root ? g_object_ref (root) : NULL;

	g_object_unref (ctx);

	return ret;
}

static guint
add_selector (CdnSelector *selector,
              Selector    *sel)
{
	if (!selector->priv->has_selected)
	{
		selector->priv->selectors =
			g_slist_prepend (selector->priv->selectors, sel);
	}
	else
	{
		selector->priv->selectors =
			g_slist_append (selector->priv->selectors, sel);
	}

	return sel->id;
}

guint
cdn_selector_append (CdnSelector       *selector,
                     CdnEmbeddedString *identifier)
{
	g_return_val_if_fail (CDN_IS_SELECTOR (selector), 0);
	g_return_val_if_fail (CDN_IS_EMBEDDED_STRING (identifier), 0);

	return add_selector (selector, selector_identifier_new (selector,
	                                                        identifier,
	                                                        FALSE));
}

guint
cdn_selector_append_partial (CdnSelector       *selector,
                          CdnEmbeddedString *identifier)
{
	g_return_val_if_fail (CDN_IS_SELECTOR (selector), 0);
	g_return_val_if_fail (CDN_IS_EMBEDDED_STRING (identifier), 0);

	return add_selector (selector, selector_identifier_new (selector,
	                                                        identifier,
	                                                        TRUE));
}

guint
cdn_selector_append_pseudo (CdnSelector           *selector,
                            CdnSelectorPseudoType  type,
                            GSList                *arguments)
{
	g_return_val_if_fail (CDN_IS_SELECTOR (selector), 0);

	return add_selector (selector, selector_pseudo_new (selector,
	                                                    type,
	                                                    arguments));
}

guint
cdn_selector_append_regex (CdnSelector       *selector,
                           CdnEmbeddedString *regex)
{

	g_return_val_if_fail (CDN_IS_SELECTOR (selector), 0);
	g_return_val_if_fail (CDN_IS_EMBEDDED_STRING (regex), 0);

	return add_selector (selector, selector_regex_new (selector,
	                                                   regex,
	                                                   FALSE));
}

guint
cdn_selector_append_regex_partial (CdnSelector       *selector,
                                   CdnEmbeddedString *regex)
{

	g_return_val_if_fail (CDN_IS_SELECTOR (selector), 0);
	g_return_val_if_fail (CDN_IS_EMBEDDED_STRING (regex), 0);

	return add_selector (selector, selector_regex_new (selector,
	                                                   regex,
	                                                   TRUE));
}

static CdnSelection *
make_child_selection (CdnSelection *parent,
                      CdnExpansion *expansion,
                      gpointer      obj)
{
	GSList *expansions;
	CdnSelection *ret;

	expansions = g_slist_copy (cdn_selection_get_expansions (parent));

	if (expansion)
	{
		expansions = g_slist_prepend (expansions, expansion);
	}

	ret = cdn_selection_new_defines (obj,
	                                 expansions,
	                                 cdn_selection_get_defines (parent),
	                                 FALSE);

	g_slist_free (expansions);

	return ret;
}

static gboolean
identifier_match (Selector     *selector,
                  CdnExpansion *expansion,
                  gchar const  *name)
{
	gchar const *ex;

	ex = cdn_expansion_get (expansion, 0);

	if (!selector->identifier.partial)
	{
		return g_strcmp0 (ex, name) == 0;
	}
	else
	{
		return strstr (name, ex) != NULL;
	}
}

static gboolean
selector_select_identifier_name (Selector            *selector,
                                 CdnSelection        *parent,
                                 gchar const         *name,
                                 CdnEmbeddedContext  *context,
                                 CdnExpansion        *expansion,
                                 GSList             **ret)
{
	if (identifier_match (selector, expansion, name))
	{
		*ret = g_slist_prepend (*ret,
		                        make_child_selection (parent,
		                                              expansion,
		                                              cdn_selection_get_object (parent)));

		return TRUE;
	}

	return FALSE;
}

static gchar const *
name_from_selection (CdnSelection *selection)
{
	gchar const *ret;
	gpointer obj;

	ret = cdn_taggable_get_tag (CDN_TAGGABLE (selection),
	                            CDN_SELECTOR_KEY_OVERRIDE_NAME);

	if (ret)
	{
		return ret;
	}

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
selector_select_identifier (CdnSelector        *self,
                            Selector           *selector,
                            CdnSelection       *parent,
                            CdnEmbeddedContext *context)
{
	GSList *ret = NULL;
	GSList *exps;
	GSList *e;
	gchar const *name;

	exps = cdn_embedded_string_expand_multiple (selector->identifier.identifier,
	                                            context,
	                                            NULL);

	name = name_from_selection (parent);

	for (e = exps; e; e = g_slist_next (e))
	{
		if (selector_select_identifier_name (selector,
		                                     parent,
		                                     name,
		                                     context,
		                                     e->data,
		                                     &ret))
		{
			break;
		}
	}

	g_slist_foreach (exps, (GFunc)cdn_expansion_unref, NULL);
	g_slist_free (exps);

	return g_slist_reverse (ret);
}

static CdnExpansion *
expansion_from_match (GMatchInfo const *info)
{
	gchar **s;
	CdnExpansion *ret;

	s = g_match_info_fetch_all (info);
	ret = cdn_expansion_new ((gchar const * const *)s);
	g_strfreev (s);

	return ret;
}

static gboolean
regex_match_full (GRegex       *regex,
                  gchar const  *s,
                  GMatchInfo  **info,
                  gboolean      partial)
{
	if (g_regex_match (regex, s, 0, info))
	{
		gint startpos;
		gint endpos;

		if (partial)
		{
			return TRUE;
		}

		g_match_info_fetch_pos (*info, 0, &startpos, &endpos);

		return startpos == 0 && endpos == strlen (s);
	}

	return FALSE;
}

static CdnSelection *
make_child_selection_from_match (CdnSelection     *parent,
                                 GMatchInfo const *info,
                                 gpointer          obj)
{
	CdnExpansion *expansion;
	CdnSelection *ret;

	expansion = expansion_from_match (info);
	ret = make_child_selection (parent, expansion, obj);
	g_object_unref (expansion);

	return ret;
}

static GRegex *
regex_create (Selector            *selector,
              CdnEmbeddedContext  *context,
              GError             **error)
{
	gchar const *s;
	gchar *r;
	GRegex *ret;

	s = cdn_embedded_string_expand (selector->regex.regex, context, NULL);

	/* Manually anchor it if needed */
	if (!selector->regex.partial)
	{
		r = g_strconcat ("(?:", s, ")$", NULL);
	}
	else
	{
		r = g_strdup (s);
	}

	ret = g_regex_new (r,
	                   G_REGEX_CASELESS | (selector->regex.partial ? 0 : G_REGEX_ANCHORED),
	                   G_REGEX_MATCH_NOTEMPTY | (selector->regex.partial ? 0 : G_REGEX_MATCH_ANCHORED),
	                   error);

	g_free (r);
	return ret;
}

static GSList *
selector_select_regex_name (Selector           *selector,
                            CdnSelection       *parent,
                            gchar const        *name,
                            CdnEmbeddedContext *context,
                            GSList             *ret)
{
	GRegex *regex;
	GMatchInfo *info;

	regex = regex_create (selector, context, NULL);

	if (!regex)
	{
		return ret;
	}

	if (regex_match_full (regex, name, &info, selector->regex.partial))
	{
		CdnSelection *childsel;

		childsel = make_child_selection_from_match (parent,
		                                            info,
		                                            cdn_selection_get_object (parent));

		ret = g_slist_prepend (ret, childsel);
		g_match_info_free (info);
	}

	g_regex_unref (regex);
	return ret;
}

static GSList *
selector_select_regex (CdnSelector        *self,
                       Selector           *selector,
                       CdnSelection       *parent,
                       CdnEmbeddedContext *context)
{
	GSList *ret = NULL;
	gchar const *name;

	name = name_from_selection (parent);

	ret = selector_select_regex_name (selector,
	                                  parent,
	                                  name,
	                                  context,
	                                  ret);

	return g_slist_reverse (ret);
}

static gboolean
nth_match (Nth const *nth,
           gint       i)
{
	if (nth->a == 0)
	{
		return i == nth->b;
	}

	return (i - nth->b) % nth->a == 0 && (i - nth->b) / nth->a >= 0;
}

static GSList *
select_nth_reverse (GSList const    *children,
                    Nth const       *nth,
                    gint             offset)
{
	GSList *ret = NULL;
	gint i = 1 - offset;
	gint num = 0;

	while (children && (nth->max < 0 || num < nth->max))
	{
		CdnSelection *sel = children->data;

		if (nth_match (nth, i))
		{
			ret = g_slist_prepend (ret,
			                       cdn_selection_copy (sel));
			++num;
		}

		++i;

		children = g_slist_next (children);
	}

	return ret;
}

static GSList *
filter_list_reverse (GSList  *objs,
                     GType    gtype,
                     gboolean inherited)
{
	GSList *ret = NULL;

	while (objs)
	{
		CdnSelection *sel;
		GType tp;
		gpointer obj;

		sel = objs->data;
		obj = cdn_selection_get_object (sel);
		tp = G_OBJECT_TYPE (obj);

		if ((inherited && g_type_is_a (tp, gtype)) ||
		    (!inherited && tp == gtype))
		{
			ret = g_slist_prepend (ret,
			                       cdn_selection_copy (sel));
		}

		objs = g_slist_next (objs);
	}

	return ret;
}

static GSList *
copy_selections_reverse (GSList *selections)
{
	GSList *ret = NULL;

	while (selections)
	{
		ret = g_slist_prepend (ret, cdn_selection_copy (selections->data));
		selections = g_slist_next (selections);
	}

	return ret;
}


static GSList *
copy_selections (GSList *selections)
{
	return g_slist_reverse (copy_selections_reverse (selections));
}

static gpointer
pseudo_parent (gpointer obj)
{
	if (CDN_IS_OBJECT (obj))
	{
		return cdn_object_get_parent (obj);
	}
	else if (CDN_IS_VARIABLE (obj))
	{
		return cdn_variable_get_object (obj);
	}
	else if (CDN_IS_EDGE_ACTION (obj))
	{
		return cdn_edge_action_get_edge (obj);
	}

	return NULL;
}

static CdnObject *
top_parent (gpointer object)
{
	while (TRUE)
	{
		gpointer parent;

		parent = pseudo_parent (object);

		if (parent == NULL)
		{
			return object;
		}

		object = parent;
	}

	return NULL;
}

static CdnSelection *
expand_obj (CdnSelection *selection,
            gpointer      obj)
{
	return cdn_selection_new_defines (obj,
	                                  cdn_selection_get_expansions (selection),
	                                  cdn_selection_get_defines (selection),
	                                  FALSE);
}

static GSList *
expand_objs_reverse (CdnSelection *selection,
                     GSList const *objs)
{
	GSList *ret = NULL;

	while (objs)
	{
		ret = g_slist_prepend (ret, expand_obj (selection, objs->data));

		objs = g_slist_next (objs);
	}

	return ret;
}

static GSList *
annotate_names (GSList *selection)
{
	GSList *ret = NULL;

	while (selection)
	{
		CdnExpansion *ex;
		gchar const *s;
		CdnSelection *sel;
		GSList *expansions;
		CdnExpansion *expansion;

		s = name_from_selection (selection->data);
		ex = cdn_expansion_new_one (s);

		expansions = g_slist_copy (cdn_selection_get_expansions (selection->data));
		expansion = cdn_expansion_copy (ex);
		expansions = g_slist_prepend (expansions, expansion);

		sel = cdn_selection_new_defines (cdn_selection_get_object (selection->data),
		                                 expansions,
		                                 cdn_selection_get_defines (selection->data),
		                                 FALSE);

		g_slist_free (expansions);
		g_object_unref (ex);
		g_object_unref (expansion);

		ret = g_slist_prepend (ret, sel);

		selection = g_slist_next (selection);
	}

	return g_slist_reverse (ret);
}

static GSList *
count_selection (CdnEmbeddedContext *context,
                 GSList             *selection)
{
	GSList *ret = NULL;
	GSList *item;
	gint i = 0;
	gchar *s;
	CdnExpansion *ex;

	while (selection)
	{
		ret = g_slist_prepend (ret, selection->data);
		++i;

		selection = g_slist_next (selection);
	}

	s = g_strdup_printf ("%d", i);
	ex = cdn_expansion_new_one (s);
	g_free (s);

	for (item = ret; item; item = g_slist_next (item))
	{
		CdnSelection *sel;
		GSList *expansions;
		CdnExpansion *expansion;

		expansions = g_slist_copy (cdn_selection_get_expansions (item->data));
		expansion = cdn_expansion_copy (ex);
		expansions = g_slist_prepend (expansions, expansion);

		cdn_expansion_set_index (expansion, 0, --i);

		sel = cdn_selection_new_defines (cdn_selection_get_object (item->data),
		                                 expansions,
		                                 cdn_selection_get_defines (item->data),
		                                 FALSE);

		g_slist_free (expansions);
		g_object_unref (expansion);

		item->data = sel;
	}

	cdn_embedded_context_add_expansion (context, ex);

	g_object_unref (ex);

	return g_slist_reverse (ret);
}

static GSList *
selector_pseudo_from_to_name (CdnSelector        *self,
                              Selector           *selector,
                              gchar const        *name,
                              GSList             *ret,
                              CdnSelection       *sel,
                              CdnEmbeddedContext *context)
{
	CdnObject *obj;

	if (!CDN_IS_EDGE (cdn_selection_get_object (sel)))
	{
		return ret;
	}

	g_object_get (cdn_selection_get_object (sel), name, &obj, NULL);

	if (!obj)
	{
		return ret;
	}

	ret = g_slist_prepend (ret, make_child_selection (sel,
	                                                  cdn_expansion_new_one (cdn_object_get_id (obj)),
	                                                  cdn_selection_get_object (sel)));
	g_object_unref (obj);

	return ret;
}

static GSList *
selector_pseudo_from_to (CdnSelector        *self,
                         Selector           *selector,
                         gchar const        *name,
                         GSList             *ret,
                         CdnSelection       *sel,
                         CdnEmbeddedContext *context)
{
	CdnObject *obj;

	if (!CDN_IS_EDGE (cdn_selection_get_object (sel)))
	{
		return ret;
	}

	g_object_get (cdn_selection_get_object (sel), name, &obj, NULL);

	if (!obj)
	{
		return ret;
	}

	ret = g_slist_prepend (ret, make_child_selection (sel, NULL, obj));
	g_object_unref (obj);

	return ret;
}

static gboolean
has_all_templates (CdnObject *obj,
                   GSList    *templates)
{
	GSList *selitem;

	for (selitem = templates; selitem; selitem = g_slist_next (selitem))
	{
		gpointer t;
		GSList const *templ;
		gboolean found;

		t = cdn_selection_get_object (selitem->data);
		templ = cdn_object_get_applied_templates (obj);
		found = FALSE;

		while (templ)
		{
			if (templ->data == t || has_all_templates (templ->data, templates))
			{
				found = TRUE;
				break;
			}

			templ = g_slist_next (templ);
		}

		if (!found)
		{
			return FALSE;
		}
	}

	return TRUE;
}

static GSList *
selector_pseudo_has_tag (CdnSelector        *self,
                         Selector           *selector,
                         CdnSelection       *sel,
                         CdnEmbeddedContext *context,
                         GSList             *ret)
{
	gpointer obj;
	gboolean valid = TRUE;
	GSList *item;

	obj = cdn_selection_get_object (sel);

	if (!CDN_IS_TAGGABLE (obj))
	{
		return ret;
	}

	for (item = selector->pseudo.arguments; item; item = g_slist_next (item))
	{
		CdnEmbeddedString *s;
		GSList *ex;
		GSList *exitem;

		s = item->data;

		if (!CDN_IS_EMBEDDED_STRING (s))
		{
			continue;
		}

		ex = cdn_embedded_string_expand_multiple (s, context, NULL);

		for (exitem = ex; exitem; exitem = g_slist_next (exitem))
		{
			if (!cdn_taggable_has_tag (obj,
			                           cdn_expansion_get (exitem->data, 0)))
			{
				valid = FALSE;
				break;
			}
		}

		g_slist_foreach (ex, (GFunc)cdn_expansion_unref, NULL);
		g_slist_free (ex);

		if (!valid)
		{
			break;
		}
	}

	if (valid)
	{
		ret = g_slist_prepend (ret,
		                       cdn_selection_copy (sel));
	}

	return ret;
}

static GSList *
selector_pseudo_has_template (CdnSelector        *self,
                              Selector           *selector,
                              CdnSelection       *sel,
                              CdnEmbeddedContext *context,
                              GSList             *ret)
{
	CdnObject *obj;
	CdnNode *template_group;
	GSList *item;
	gboolean valid = TRUE;

	obj = cdn_selection_get_object (sel);

	if (!CDN_IS_OBJECT (obj))
	{
		return ret;
	}

	if (self->priv->root && CDN_IS_NETWORK (self->priv->root))
	{
		template_group = cdn_network_get_template_node (CDN_NETWORK (self->priv->root));
	}
	else
	{
		CdnObject *tp;

		tp = top_parent (obj);

		if (!CDN_IS_OBJECT (tp))
		{
			return ret;
		}

		template_group = cdn_network_get_template_node (CDN_NETWORK (tp));
	}

	cdn_embedded_context_save (context);
	cdn_embedded_context_add_selection (context, sel);

	for (item = selector->pseudo.arguments; item; item = g_slist_next (item))
	{
		CdnSelector *s = item->data;
		GSList *sub;
		GSList const *children;

		children = cdn_node_get_children (template_group);
		cdn_selector_set_self (s, self->priv->self);

		while (children)
		{
			sub = cdn_selector_select (s,
			                           G_OBJECT (children->data),
			                           CDN_SELECTOR_TYPE_OBJECT,
			                           context);

			if (!has_all_templates (obj, sub))
			{
				valid = FALSE;
			}

			g_slist_foreach (sub, (GFunc)g_object_unref, NULL);
			g_slist_free (sub);

			children = g_slist_next (children);

			if (!valid)
			{
				break;
			}
		}

		cdn_selector_set_self (s, NULL);

		if (!valid)
		{
			break;
		}
	}

	cdn_embedded_context_restore (context);

	if (valid)
	{
		ret = g_slist_prepend (ret, cdn_selection_copy (sel));
	}

	return ret;
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
selector_pseudo_if (CdnSelector        *self,
                    Selector           *selector,
                    GSList             *ret,
                    CdnSelection       *sel,
                    CdnEmbeddedContext *context,
                    gboolean            condition)
{
	gpointer obj;
	GSList *item;

	cdn_embedded_context_save (context);

	cdn_embedded_context_add_selection (context, sel);

	obj = cdn_selection_get_object (sel);

	for (item = selector->pseudo.arguments; item; item = g_slist_next (item))
	{
		GSList *sub = NULL;
		gboolean r = FALSE;

		if (CDN_IS_SELECTOR (item->data))
		{
			cdn_selector_set_self (item->data, self->priv->self);

			sub = cdn_selector_select (item->data,
			                           obj,
			                           CDN_SELECTOR_TYPE_ANY,
			                           context);

			cdn_selector_set_self (item->data, NULL);

			r = (sub != NULL);
		}
		else if (CDN_IS_EMBEDDED_STRING (item->data))
		{
			gchar const *ex;

			ex = cdn_embedded_string_expand (item->data, context, NULL);

			r = !test_string_empty (ex);
		}

		if (!condition && sub != NULL)
		{
			/* Remove if in selection */
			GSList *subitem;
			gboolean found = FALSE;

			for (subitem = sub; subitem; subitem = g_slist_next (subitem))
			{
				if (cdn_selection_get_object (subitem->data) == obj)
				{
					found = TRUE;
					break;
				}
			}

			if (!found)
			{
				r = TRUE;
			}
		}

		g_slist_foreach (sub, (GFunc)g_object_unref, NULL);
		g_slist_free (sub);

		if (condition == r)
		{
			ret = g_slist_prepend (ret,
			                       cdn_selection_copy (sel));
			break;
		}
	}

	cdn_embedded_context_restore (context);
	return ret;
}

static GSList *
selector_pseudo_isempty (CdnSelector        *self,
                         Selector           *selector,
                         GSList             *parent,
                         CdnEmbeddedContext *context)
{
	GSList *item;
	GSList *seli;
	gboolean isempty = TRUE;

	for (seli = parent; seli; seli = g_slist_next (seli))
	{
		CdnSelection *sel;

		sel = seli->data;

		cdn_embedded_context_save (context);

		cdn_embedded_context_add_selection (context, sel);

		for (item = selector->pseudo.arguments; item; item = g_slist_next (item))
		{
			gboolean r = FALSE;

			if (CDN_IS_SELECTOR (item->data))
			{
				GSList *sub;

				cdn_selector_set_self (item->data, self->priv->self);

				sub = cdn_selector_select (item->data,
				                           cdn_selection_get_object (sel),
				                           CDN_SELECTOR_TYPE_ANY,
				                           context);

				cdn_selector_set_self (item->data, NULL);

				r = (sub == NULL);

				g_slist_foreach (sub, (GFunc)g_object_unref, NULL);
				g_slist_free (sub);
			}
			else if (CDN_IS_EMBEDDED_STRING (item->data))
			{
				gchar const *ex;

				ex = cdn_embedded_string_expand (item->data, context, NULL);

				r = test_string_empty (ex);
			}

			if (!r)
			{
				isempty = FALSE;
				break;
			}
		}

		cdn_embedded_context_restore (context);

		if (!isempty)
		{
			break;
		}
	}

	if (isempty)
	{
		return g_slist_prepend (NULL,
		                        cdn_selection_copy (self->priv->self));
	}
	else
	{
		return NULL;
	}
}

static gchar const *
object_type_name (gpointer obj)
{
	if (CDN_IS_FUNCTION (obj))
	{
		return "function";
	}
	else if (CDN_IS_NETWORK (obj))
	{
		return "root";
	}
	else if (CDN_IS_NODE (obj))
	{
		return "node";
	}
	else if (CDN_IS_EDGE (obj))
	{
		return "edge";
	}
	else if (CDN_IS_VARIABLE (obj))
	{
		return "variable";
	}
	else if (CDN_IS_EDGE_ACTION (obj))
	{
		return "action";
	}
	else if (CDN_IS_OBJECT (obj))
	{
		return "object";
	}
	else
	{
		return "";
	}
}

static GSList *
selector_pseudo_type (CdnEmbeddedContext *context,
                      GSList             *selection)
{
	GSList *ret = NULL;
	gint numtypes = 0;
	GHashTable *types;
	GSList *item;

	types = g_hash_table_new_full (g_str_hash,
	                               g_str_equal,
	                               NULL,
	                               (GDestroyNotify)g_free);

	for (item = selection; item; item = g_slist_next (item))
	{
		gint *val;
		gchar const *tname;

		tname = object_type_name (cdn_selection_get_object (item->data));

		val = g_hash_table_lookup (types, tname);

		if (val)
		{
			++(val[1]);
		}
		else
		{
			val = g_new0 (gint, 2);

			val[0] = numtypes++;
			val[1] = 1;

			g_hash_table_insert (types, (gchar *)tname, val);
		}
	}

	while (selection)
	{
		GSList *expansions;
		CdnSelection *sel;
		gchar const *tname;
		gint *val;
		CdnExpansion *expansion;
		gchar const *ex[3];
		gchar *cnt;

		sel = selection->data;

		tname = object_type_name (cdn_selection_get_object (sel));

		val = g_hash_table_lookup (types, tname);

		expansions = g_slist_copy (cdn_selection_get_expansions (sel));

		cnt = g_strdup_printf ("%d", val[1]);

		ex[0] = tname;
		ex[1] = cnt;
		ex[2] = NULL;

		expansion = cdn_expansion_new (ex);
		g_free (cnt);

		expansions = g_slist_prepend (expansions, expansion);

		cdn_expansion_set_index (expansion, 0, val[0]);

		ret = g_slist_prepend (ret,
		                       cdn_selection_new_defines (cdn_selection_get_object (sel),
		                                                  expansions,
		                                                  cdn_selection_get_defines (sel),
		                                                  FALSE));

		g_slist_free (expansions);
		g_object_unref (expansion);

		selection = g_slist_next (selection);
	}

	g_hash_table_destroy (types);

	return g_slist_reverse (ret);
}

static gchar *
selector_until_as_string (CdnSelector *self,
                          Selector    *selector)
{
	GSList *item;
	GString *ret;

	ret = g_string_new ("");

	if (self->priv->implicit_children && !self->priv->prevent_implicit_children)
	{
		ret = g_string_append (ret, "children");
	}

	for (item = self->priv->selectors; item && item->data != selector; item = g_slist_next (item))
	{
		Selector *sel;
		gchar *s;

		sel = item->data;

		if (ret->len != 0)
		{
			g_string_append_c (ret, '|');
		}

		s = selector_to_string (sel);
		g_string_append (ret, s);
		g_free (s);
	}

	return g_string_free (ret, FALSE);
}

static gchar *
expansion_as_string (CdnExpansion *expansion)
{
	GString *ret;
	gint i;

	ret = g_string_new ("{");

	for (i = 0; i < cdn_expansion_num (expansion); ++i)
	{
		if (i != 0)
		{
			g_string_append (ret, ", ");
		}

		g_string_append (ret, cdn_expansion_get (expansion, i));
		g_string_append_printf (ret, ":%d", cdn_expansion_get_index (expansion, i));
	}

	g_string_append_c (ret, '}');

	return g_string_free (ret, FALSE);
}

static gchar *
expansions_as_string (GSList *expansions)
{
	GString *ret;

	ret = g_string_new ("[");

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

	g_string_append_c (ret, ']');
	return g_string_free (ret, FALSE);
}

static GSList *
debug_selections (CdnSelector        *self,
                  Selector           *selector,
                  GSList             *parent,
                  CdnEmbeddedContext *context)
{
	GSList *item;
	gchar *s;

	s = selector_until_as_string (self, selector);
	g_printerr ("[debug] Selector : %s\n", s);
	g_free (s);

	s = expansions_as_string (cdn_embedded_context_get_expansions (context));
	g_printerr ("[debug] Context  : %s\n", s);
	g_free (s);

	for (item = parent; item; item = g_slist_next (item))
	{
		CdnSelection *sel;
		gpointer obj;
		GSList *expansions;

		sel = item->data;
		obj = cdn_selection_get_object (sel);

		g_printerr ("[debug]   => ");

		if (CDN_IS_OBJECT (obj))
		{
			s = cdn_object_get_full_id (obj);
			g_printerr ("%s", s);
			g_free (s);
		}
		else if (CDN_IS_VARIABLE (obj))
		{
			s = cdn_variable_get_full_name (obj);
			g_printerr ("%s", s);
			g_free (s);
		}
		else if (CDN_IS_EDGE_ACTION (obj))
		{
			gchar const *target;
			gchar *id;

			target = cdn_edge_action_get_target (obj);
			id = cdn_object_get_full_id (CDN_OBJECT (cdn_edge_action_get_edge (obj)));

			g_printerr ("%s < %s", id, target);
			g_free (id);
		}

		expansions = cdn_selection_get_expansions (sel);
		s = expansions_as_string (expansions);

		g_printerr (" <= %s", s);
		g_free (s);

		if (selector->pseudo.arguments)
		{
			g_printerr (" with [");

			cdn_embedded_context_save (context);
			cdn_embedded_context_add_selection (context, sel);

			for (item = selector->pseudo.arguments; item; item = g_slist_next (item))
			{
				if (item != selector->pseudo.arguments)
				{
					g_printerr (", ");
				}

				g_printerr ("%s", cdn_embedded_string_expand (item->data, context, NULL));
			}

			cdn_embedded_context_restore (context);

			g_printerr ("]");
		}

		g_printerr ("\n");

	}

	g_printerr ("\n");

	return copy_selections (parent);
}

static GSList *
children_reverse (CdnSelection *selection,
                  gpointer      obj)
{
	GSList *ret = NULL;

	if (CDN_IS_NODE (obj))
	{
		GSList const *children;

		children = cdn_node_get_children (obj);

		ret = g_slist_concat (expand_objs_reverse (selection, children),
		                      ret);
	}

	if (CDN_IS_OBJECT (obj))
	{
		GSList *props;

		props = cdn_object_get_variables (obj);

		ret = g_slist_concat (expand_objs_reverse (selection, props),
		                      ret);

		g_slist_free (props);
	}

	if (CDN_IS_NODE (obj))
	{
		CdnVariableInterface *iface;
		gchar **names;
		gchar **ptr;

		iface = cdn_node_get_variable_interface (CDN_NODE (obj));
		names = cdn_variable_interface_get_names (iface);

		ptr = names;

		while (ptr && *ptr)
		{
			CdnSelection *s;

			s = expand_obj (selection,
			                cdn_variable_interface_lookup (iface, *ptr));

			cdn_taggable_add_tag (CDN_TAGGABLE (s),
			                      CDN_SELECTOR_KEY_OVERRIDE_NAME,
			                      *ptr);

			ret = g_slist_prepend (ret, s);

			++ptr;
		}

		g_strfreev (names);
	}

	if (CDN_IS_EDGE (obj))
	{
		GSList const *actions;

		actions = cdn_edge_get_actions (obj);

		ret = g_slist_concat (expand_objs_reverse (selection, actions),
		                      ret);
	}

	return ret;
}

static GSList *
descendants_reverse (CdnSelection *selection)
{
	GSList *ret = NULL;
	GQueue *queue;

	queue = g_queue_new ();
	g_queue_push_head (queue, selection);

	while (!g_queue_is_empty (queue))
	{
		CdnSelection *sel;
		GSList *children;
		GSList *item;
		gpointer obj;

		sel = g_queue_pop_head (queue);

		if (sel != selection)
		{
			ret = g_slist_prepend (ret, sel);
		}

		obj = cdn_selection_get_object (sel);
		children = g_slist_reverse (children_reverse (sel, obj));

		for (item = children; item; item = g_slist_next (item))
		{
			g_queue_push_tail (queue, item->data);
		}

		g_slist_free (children);
	}

	return ret;
}

static GSList *
ancestors_reverse (CdnSelection *selection)
{
	GSList *ret = NULL;
	gpointer obj;

	obj = cdn_selection_get_object (selection);

	while (TRUE)
	{
		obj = pseudo_parent (obj);

		if (!obj)
		{
			break;
		}

		ret = g_slist_prepend (ret,
		                       make_child_selection (selection, NULL, obj));
	}

	return ret;
}

static GSList *
unique_selections (GSList *parent)
{
	GSList *ret = NULL;
	GHashTable *objs;

	objs = g_hash_table_new (g_direct_hash,
	                         g_direct_equal);

	while (parent)
	{
		gpointer obj;

		obj = cdn_selection_get_object (parent->data);

		if (g_hash_table_lookup (objs, obj) == NULL)
		{
			g_hash_table_insert (objs, obj, GINT_TO_POINTER (1));

			ret = g_slist_prepend (ret,
			                       cdn_selection_copy_defines (parent->data,
			                                                   FALSE));
		}

		parent = g_slist_next (parent);
	}

	g_hash_table_destroy (objs);

	return g_slist_reverse (ret);
}

static GSList *
selector_pseudo_has_flag (CdnSelector        *self,
                          Selector           *selector,
                          CdnSelection       *selection,
                          CdnEmbeddedContext *context,
                          GSList             *ret)
{
	gpointer obj;
	GSList *arg;
	CdnVariableFlags flags;
	gboolean valid = TRUE;

	obj = cdn_selection_get_object (selection);

	if (!CDN_IS_VARIABLE (obj))
	{
		return ret;
	}

	flags = cdn_variable_get_flags (obj);

	for (arg = selector->pseudo.arguments; arg; arg = g_slist_next (arg))
	{
		CdnEmbeddedString *s;
		GSList *ret;
		GSList *item;

		if (!CDN_IS_EMBEDDED_STRING (arg->data))
		{
			continue;
		}

		s = CDN_EMBEDDED_STRING (arg->data);
		ret = cdn_embedded_string_expand_multiple (s, context, NULL);

		for (item = ret; item; item = g_slist_next (item))
		{
			CdnVariableFlags add = 0;

			cdn_variable_flags_from_string (cdn_expansion_get (item->data, 0),
			                                &add,
			                                NULL);

			if ((flags & add) == 0)
			{
				valid = FALSE;
				break;
			}
		}

		g_slist_foreach (ret, (GFunc)cdn_expansion_unref, NULL);
		g_slist_free (ret);

		if (!valid)
		{
			break;
		}
	}

	if (valid)
	{
		ret = g_slist_prepend (ret,
		                       cdn_selection_copy (selection));
	}

	return ret;
}

static CdnObject *
find_template_group (CdnSelector *selector,
                     gpointer     obj)
{
	if (!selector->priv->root || !CDN_IS_NETWORK (selector->priv->root))
	{
		CdnObject *p;

		if (!obj)
		{
			return NULL;
		}

		p = top_parent (obj);

		if (CDN_IS_NETWORK (p))
		{
			return CDN_OBJECT (cdn_network_get_template_node (CDN_NETWORK (p)));
		}
		else
		{
			return NULL;
		}
	}
	else
	{
		return CDN_OBJECT (cdn_network_get_template_node (CDN_NETWORK (selector->priv->root)));
	}
}

static gboolean
is_template (CdnSelector *selector,
             gpointer     obj)
{
	gpointer parent;
	CdnObject *template_group;

	template_group = find_template_group (selector, obj);

	if (!template_group)
	{
		return FALSE;
	}

	parent = pseudo_parent (obj);

	while (parent)
	{
		if (parent == template_group)
		{
			return TRUE;
		}

		parent = pseudo_parent (parent);
	}

	return FALSE;
}

static GSList *
selector_select_pseudo (CdnSelector        *self,
                        Selector           *selector,
                        GSList             *parent,
                        CdnEmbeddedContext *context)
{
	GSList *ret = NULL;
	GSList *item;
	CdnSelection *last = NULL;

	switch (selector->pseudo.type)
	{
		case CDN_SELECTOR_PSEUDO_TYPE_SUBSET:
		{
			Nth nth = parse_nth (selector->pseudo.arguments,
			                     context);

			return g_slist_reverse (select_nth_reverse (parent,
				                                    &nth,
				                                    0));
		}
		break;
		case CDN_SELECTOR_PSEUDO_TYPE_REVERSE:
			return copy_selections_reverse (parent);
		break;
		case CDN_SELECTOR_PSEUDO_TYPE_STATES:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CDN_TYPE_OBJECT,
				                                     FALSE));
		case CDN_SELECTOR_PSEUDO_TYPE_EDGES:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CDN_TYPE_EDGE,
				                                     FALSE));
		case CDN_SELECTOR_PSEUDO_TYPE_NODES:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CDN_TYPE_NODE,
				                                     FALSE));
		case CDN_SELECTOR_PSEUDO_TYPE_IMPORTS:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CDN_TYPE_IMPORT,
				                                     FALSE));
		case CDN_SELECTOR_PSEUDO_TYPE_VARIABLES:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CDN_TYPE_VARIABLE,
				                                     FALSE));
		case CDN_SELECTOR_PSEUDO_TYPE_ACTIONS:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CDN_TYPE_EDGE_ACTION,
				                                     FALSE));
		case CDN_SELECTOR_PSEUDO_TYPE_FUNCTIONS:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CDN_TYPE_FUNCTION,
				                                     FALSE));
		case CDN_SELECTOR_PSEUDO_TYPE_OBJECTS:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CDN_TYPE_OBJECT,
				                                     TRUE));
		break;
		case CDN_SELECTOR_PSEUDO_TYPE_COUNT:
			return count_selection (context, parent);
		break;
		case CDN_SELECTOR_PSEUDO_TYPE_NAME:
			return annotate_names (parent);
		break;
		case CDN_SELECTOR_PSEUDO_TYPE_TYPE:
			return selector_pseudo_type (context, parent);
		break;
		case CDN_SELECTOR_PSEUDO_TYPE_SELF:
			if (parent && parent->data)
			{
				/* Keep the context from the first parent */
				return g_slist_prepend (NULL,
				                        expand_obj (parent->data,
				                                    cdn_selection_get_object (self->priv->self)));
			}
			else
			{
				return g_slist_prepend (NULL,
				                        g_object_ref (self->priv->self));
			}
		break;
		case CDN_SELECTOR_PSEUDO_TYPE_DEBUG:
			return debug_selections (self, selector, parent, context);
		break;
		case CDN_SELECTOR_PSEUDO_TYPE_UNIQUE:
			return unique_selections (parent);
		break;
		case CDN_SELECTOR_PSEUDO_TYPE_IS_EMPTY:
			return selector_pseudo_isempty (self,
			                                selector,
			                                parent,
			                                context);
		break;
		case CDN_SELECTOR_PSEUDO_TYPE_FROM_SET:
			return copy_selections (self->priv->from_set);
		break;
		case CDN_SELECTOR_PSEUDO_TYPE_ROOT:
			if (!parent)
			{
				return g_slist_prepend (NULL,
				                        expand_obj (self->priv->self,
				                                    self->priv->root ?
				                                    self->priv->root :
				                                    top_parent (cdn_selection_get_object (self->priv->self))));
			}
		break;
		case CDN_SELECTOR_PSEUDO_TYPE_TEMPLATES_ROOT:
		{
			CdnObject *tg;

			tg = find_template_group (self, parent ? cdn_selection_get_object (parent->data) : NULL);

			if (tg)
			{
				return g_slist_prepend (NULL,
				                        expand_obj (self->priv->self,
				                                    tg));
			}
			else
			{
				return NULL;
			}
		}
		break;
		default:
		break;
	}

	for (item = parent; item; item = g_slist_next (item))
	{
		CdnSelection *sel = item->data;
		gpointer obj;
		Nth nth = {0,};

		obj = cdn_selection_get_object (sel);

		cdn_embedded_context_save (context);

		cdn_embedded_context_add_selection (context, sel);

		switch (selector->pseudo.type)
		{
			case CDN_SELECTOR_PSEUDO_TYPE_SIBLINGS:
				nth = parse_nth (selector->pseudo.arguments,
				                 context);
			break;
			default:
			break;
		}

		cdn_embedded_context_restore (context);

		switch (selector->pseudo.type)
		{
			case CDN_SELECTOR_PSEUDO_TYPE_FIRST:
				return g_slist_prepend (ret,
				                        cdn_selection_copy (sel));
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_LAST:
				last = sel;
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_ROOT:
			{
				CdnObject *top;

				top = top_parent (obj);

				if (top)
				{
					return g_slist_prepend (ret,
					                        expand_obj (sel, top));
				}
			}
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_CHILDREN:
			{
				ret = g_slist_concat (children_reverse (sel,
				                                        obj),
				                      ret);
			}
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_DESCENDANTS:
			{
				ret = g_slist_concat (descendants_reverse (sel),
				                      ret);
			}
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_ANCESTORS:
			{
				ret = g_slist_concat (ancestors_reverse (sel),
				                      ret);
			}
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_SIBLINGS:
			{
				gpointer parent;

				parent = pseudo_parent (obj);

				if (parent)
				{
					GSList *children;
					GSList *filtered;
					GSList *item;
					gint idx;

					children = children_reverse (sel,
					                             parent);

					filtered = filter_list_reverse (children,
					                                G_OBJECT_TYPE (obj),
					                                TRUE);

					g_slist_foreach (children, (GFunc)g_object_unref, NULL);
					g_slist_free (children);

					idx = 0;

					for (item = filtered; item; item = g_slist_next (item))
					{
						if (cdn_selection_get_object (item->data) == obj)
						{
							break;
						}

						++idx;
					}

					ret = g_slist_concat (select_nth_reverse (filtered,
					                                          &nth,
					                                          idx + 1),
					                      ret);

					g_slist_foreach (filtered, (GFunc)g_object_unref, NULL);
					g_slist_free (filtered);
				}
			}
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_PARENT:
			{
				gpointer parent;

				parent = pseudo_parent (obj);

				if (parent)
				{
					ret = g_slist_prepend (ret,
					                       expand_obj (sel,
					                                   parent));
				}
			}
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_FROM:
				ret = selector_pseudo_from_to (self,
				                               selector,
				                               "from",
				                               ret,
				                               sel,
				                               context);
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_TO:
				ret = selector_pseudo_from_to (self,
				                               selector,
				                               "to",
				                               ret,
				                               sel,
				                               context);
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_SOURCE_NAME:
				ret = selector_pseudo_from_to_name (self,
				                                    selector,
				                                    "from",
				                                    ret,
				                                    sel,
				                                    context);
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_SINK_NAME:
				ret = selector_pseudo_from_to_name (self,
				                                    selector,
				                                    "to",
				                                    ret,
				                                    sel,
				                                    context);
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_IF:
				ret = selector_pseudo_if (self,
				                          selector,
				                          ret,
				                          sel,
				                          context,
				                          TRUE);
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_REMOVE:
				ret = selector_pseudo_if (self,
				                          selector,
				                          ret,
				                          sel,
				                          context,
				                          FALSE);
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_HAS_FLAG:
				ret = selector_pseudo_has_flag (self,
				                                selector,
				                                sel,
				                                context,
				                                ret);
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_HAS_TEMPLATE:
				ret = selector_pseudo_has_template (self,
				                                    selector,
				                                    sel,
				                                    context,
				                                    ret);
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_HAS_TAG:
				ret = selector_pseudo_has_tag (self,
				                               selector,
				                               sel,
				                               context,
				                               ret);
			break;
			case CDN_SELECTOR_PSEUDO_TYPE_TEMPLATES:
				if (is_template (self, obj))
				{
					ret = g_slist_prepend (ret,
					                       cdn_selection_copy_defines (sel, FALSE));
				}
			break;
			default:
				g_assert_not_reached ();
			break;
		}
	}

	if (last)
	{
		ret = g_slist_prepend (ret,
		                       cdn_selection_copy (last));
	}

	return g_slist_reverse (ret);
}

static void
annotate_first_expansion (GSList *ret)
{
	GSList *expansions = NULL;
	GSList *ptr;
	gint i;

	while (ret)
	{
		GSList *exp;

		exp = cdn_selection_get_expansions (ret->data);

		expansions = g_slist_prepend (expansions,
		                              exp->data);

		ret = g_slist_next (ret);
	}

	expansions = g_slist_reverse (expansions);

	/* Annotate indices by string for the explicit groups in the regex */
	cdn_expansions_annotate_indices (expansions, 1);

	i = 0;

	/* Annotate the implicit match as a unique index */
	for (ptr = expansions; ptr; ptr = g_slist_next (ptr))
	{
		cdn_expansion_set_index (ptr->data, 0, i++);
	}

	g_slist_free (expansions);
}

static GSList *
selector_select (CdnSelector        *self,
                 Selector           *selector,
                 GSList             *parent,
                 CdnEmbeddedContext *context)
{
	GSList *ret = NULL;
	GSList *item;

	if (selector->type == SELECTOR_TYPE_PSEUDO)
	{
		ret = selector_select_pseudo (self,
		                              selector,
		                              parent,
		                              context);
	}
	else
	{
		for (item = parent; item; item = g_slist_next (item))
		{
			CdnSelection *sel = item->data;
			GSList *r = NULL;

			cdn_embedded_context_save (context);

			cdn_embedded_context_add_selection (context, sel);

			switch (selector->type)
			{
				case SELECTOR_TYPE_IDENTIFIER:
					r = selector_select_identifier (self,
					                                selector,
					                                sel,
					                                context);
				break;
				case SELECTOR_TYPE_REGEX:
					r = selector_select_regex (self,
					                           selector,
					                           sel,
					                           context);
				break;
				default:
					g_assert_not_reached ();
				break;
			}

			cdn_embedded_context_restore (context);

			ret = g_slist_concat (ret, r);
		}

		if (selector->type == SELECTOR_TYPE_REGEX)
		{
			annotate_first_expansion (ret);
		}
	}

	return ret;
}

static gboolean
selection_match_type (CdnSelector     *selector,
                      CdnSelection    *selection,
                      CdnSelectorType  type)
{
	gpointer obj;

	obj = cdn_selection_get_object (selection);

	if (type & CDN_SELECTOR_TYPE_ANY)
	{
		return TRUE;
	}

	if ((type & CDN_SELECTOR_TYPE_STATE) &&
	    G_OBJECT_TYPE (obj) == CDN_TYPE_OBJECT)
	{
		return TRUE;
	}

	if ((type & CDN_SELECTOR_TYPE_EDGE) &&
	    CDN_IS_EDGE (obj))
	{
		return TRUE;
	}

	if ((type & CDN_SELECTOR_TYPE_NODE) &&
	    CDN_IS_NODE (obj))
	{
		return TRUE;
	}

	if ((type & CDN_SELECTOR_TYPE_VARIABLE) &&
	    CDN_IS_VARIABLE (obj))
	{
		return TRUE;
	}

	if ((type & CDN_SELECTOR_TYPE_ACTION) &&
	    CDN_IS_EDGE_ACTION (obj))
	{
		return TRUE;
	}

	if ((type & CDN_SELECTOR_TYPE_FUNCTION) &&
	    CDN_IS_FUNCTION (obj))
	{
		return TRUE;
	}

	if ((type & CDN_SELECTOR_TYPE_TEMPLATE) &&
	     is_template (selector, obj))
	{
		return TRUE;
	}

	return FALSE;
}

static GSList *
filter_selection (CdnSelector     *selector,
                  GSList          *selection,
                  CdnSelectorType  type)
{
	GSList *item;
	GSList *ret = NULL;

	for (item = selection; item; item = g_slist_next (item))
	{
		if (!selection_match_type (selector, item->data, type))
		{
			g_object_unref (item->data);
		}
		else
		{
			ret = g_slist_prepend (ret,
			                       item->data);
		}
	}

	g_slist_free (selection);

	return g_slist_reverse (ret);
}

static GSList *
selector_select_all (CdnSelector        *selector,
                     GObject            *parent,
                     CdnSelectorType     type,
                     CdnEmbeddedContext *context)
{
	GSList *item;
	GSList *ctx = NULL;
	GHashTable *defines;
	gboolean release_self = FALSE;
	CdnSelection *sel;

	if (context == NULL)
	{
		context = cdn_embedded_context_new ();
	}
	else
	{
		cdn_embedded_context_save (context);
		g_object_ref (context);
	}

	if (!selector->priv->has_selected)
	{
		selector->priv->selectors =
			g_slist_reverse (selector->priv->selectors);

		selector->priv->has_selected = TRUE;
	}

	if (!selector->priv->selectors)
	{
		cdn_embedded_context_restore (context);
		g_object_unref (context);
		return NULL;
	}

	defines = context ? cdn_embedded_context_get_defines (context) : NULL;

	sel = cdn_selection_new_defines (parent,
	                                 NULL,
	                                 defines,
	                                 FALSE);

	if (!selector->priv->self)
	{
		release_self = TRUE;
		selector->priv->self = sel;
	}

	ctx = g_slist_prepend (NULL, g_object_ref (sel));

	if (selector->priv->implicit_children &&
	    !selector->priv->prevent_implicit_children)
	{
		g_slist_free (ctx);

		ctx = g_slist_reverse (children_reverse (sel, cdn_selection_get_object (sel)));
		g_object_unref (sel);
	}

	for (item = selector->priv->selectors; item; item = g_slist_next (item))
	{
		Selector *sel = item->data;
		GSList *item;

		free_current_selections (sel);

		sel->selections_in = ctx;

		ctx = selector_select (selector,
		                       sel,
		                       ctx,
		                       context);

		for (item = ctx; item; item = g_slist_next (item))
		{
			sel->selections_out =
				g_slist_prepend (sel->selections_out,
				                 g_object_ref (item->data));
		}

		sel->selections_out = g_slist_reverse (sel->selections_out);

		g_signal_emit (selector,
		               signals[SELECT],
		               0,
		               sel->id);
	}

	if (release_self)
	{
		g_object_unref (selector->priv->self);
		selector->priv->self = NULL;
	}

	ctx = filter_selection (selector, ctx, type);

	cdn_embedded_context_restore (context);
	g_object_unref (context);

	return ctx;
}

/**
 * cdn_selector_select:
 * @selector: A #CdnSelector
 * @parent: A #GObject
 * @type: A #CdnSelectorType
 * @context: A #CdnEmbeddedContext
 *
 * Select objects (from @parent) using the selector.
 *
 * Returns: (element-type CdnSelection) (transfer full): A #GSList of #CdnSelection
 *
 **/
GSList *
cdn_selector_select (CdnSelector        *selector,
                     GObject            *parent,
                     CdnSelectorType     type,
                     CdnEmbeddedContext *context)
{
	g_return_val_if_fail (CDN_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (G_IS_OBJECT (parent), NULL);
	g_return_val_if_fail (context == NULL || CDN_IS_EMBEDDED_CONTEXT (context), NULL);

	return selector_select_all (selector, parent, type, context);
}

gchar *
cdn_selector_as_string (CdnSelector *selector)
{
	g_return_val_if_fail (CDN_IS_SELECTOR (selector), NULL);

	return selector_until_as_string (selector, NULL);
}

void
cdn_selector_set_partial (CdnSelector *selector,
                          gboolean     partial)
{
	GSList *item;

	g_return_if_fail (CDN_IS_SELECTOR (selector));

	for (item = selector->priv->selectors; item; item = g_slist_next (item))
	{
		Selector *sel;

		sel = item->data;

		if (sel->type == SELECTOR_TYPE_IDENTIFIER)
		{
			sel->identifier.partial = partial;
		}
		else if (sel->type == SELECTOR_TYPE_REGEX)
		{
			sel->regex.partial = partial;
		}
	}
}

guint
cdn_selector_get_last_id (CdnSelector *selector)
{
	g_return_val_if_fail (CDN_IS_SELECTOR (selector), 0);

	return selector->priv->last_id;
}

static Selector *
find_selector_by_id (CdnSelector *selector,
                     guint        id)
{
	GSList *item;

	for (item = selector->priv->selectors; item; item = g_slist_next (item))
	{
		Selector *s;

		s = item->data;

		if (s->id == id)
		{
			return s;
		}
	}

	return NULL;
}

/**
 * cdn_selector_get_in_context:
 * @selector: A #CdnSelector
 * @id: The selector id
 *
 * Get the in-context for a particular selector.
 *
 * Returns: (element-type CdnSelection) (transfer none): A #GSList of #CdnSelection
 *
 **/
GSList const *
cdn_selector_get_in_context (CdnSelector *selector,
                             guint        id)
{
	Selector *sel;

	g_return_val_if_fail (CDN_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (id <= selector->priv->last_id, NULL);

	sel = find_selector_by_id (selector, id);

	return sel ? sel->selections_in : NULL;
}

/**
 * cdn_selector_get_out_context:
 * @selector: A #CdnSelector
 * @id: The selector id
 *
 * Get the out-context for a particular selector.
 *
 * Returns: (element-type CdnSelection) (transfer none): A #GSList of #CdnSelection
 *
 **/
GSList const *
cdn_selector_get_out_context (CdnSelector *selector,
                              guint        id)
{
	Selector *sel;

	g_return_val_if_fail (CDN_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (id <= selector->priv->last_id, NULL);

	sel = find_selector_by_id (selector, id);

	return sel ? sel->selections_out : NULL;
}

/**
 * cdn_selector_is_pseudo_name:
 * @name: the name
 *
 * Check whether a name is a pseudo selector.
 *
 * Returns: %TRUE if the name is a pseudo selector, %FALSE otherwise
 *
 **/
gboolean
cdn_selector_is_pseudo_name (gchar const *name)
{
	gint i;

	for (i = 0; i < CDN_SELECTOR_PSEUDO_NUM; ++i)
	{
		if (g_strcmp0 (selector_pseudo_names[i], name) == 0)
		{
			return TRUE;
		}
	}

	return FALSE;
}

/**
 * cdn_selector_escape_identifier:
 * @name: the name
 *
 * Escapes an identifier if needed.
 *
 * Returns: (transfer full): the escaped identifier
 *
 **/
gchar *
cdn_selector_escape_identifier (gchar const *name)
{
	GString *ret;

	if (!cdn_selector_is_pseudo_name (name) &&
	    strchr (name, '"') == NULL &&
	    strchr (name, '\\') == NULL)
	{
		return g_strdup (name);
	}

	ret = g_string_new ("\"");

	while (*name)
	{
		if (*name == '"' || *name == '\\')
		{
			g_string_append_c (ret, '\\');
		}

		g_string_append_c (ret, *name);

		++name;
	}

	g_string_append_c (ret, '"');

	return g_string_free (ret, FALSE);
}

void
cdn_selector_set_from_set (CdnSelector *selector,
                           GSList      *selections)
{
	g_return_if_fail (CDN_IS_SELECTOR (selector));

	g_slist_foreach (selector->priv->from_set, (GFunc)g_object_unref, NULL);
	g_slist_free (selector->priv->from_set);

	selector->priv->from_set = copy_selections (selections);
}

void
cdn_selector_set_self (CdnSelector  *selector,
                       CdnSelection *selection)
{
	g_return_if_fail (CDN_IS_SELECTOR (selector));
	g_return_if_fail (selection == NULL || CDN_IS_SELECTION (selection));

	if (selector->priv->self)
	{
		g_object_unref (selector->priv->self);
		selector->priv->self = NULL;
	}

	if (selection)
	{
		selector->priv->self = g_object_ref (selection);
	}
}

/**
 * cdn_selector_copy_with:
 * @selector: A #CdnSelector
 *
 * Copy a selector for a with block.
 *
 * Returns: (transfer full): A #CdnSelector
 *
 **/
CdnSelector *
cdn_selector_copy_with (CdnSelector *selector)
{
	CdnSelector *ret;
	GSList *item;
	gboolean explicit_children;
	Selector *children = NULL;

	g_return_val_if_fail (CDN_IS_SELECTOR (selector), NULL);

	ret = cdn_selector_new (selector->priv->root);

	explicit_children = selector->priv->implicit_children &&
	                    !selector->priv->prevent_implicit_children;

	if (explicit_children)
	{
		children = selector_pseudo_new (ret,
		                                CDN_SELECTOR_PSEUDO_TYPE_CHILDREN,
		                                NULL);

		if (ret->priv->has_selected)
		{
			ret->priv->selectors =
				g_slist_prepend (ret->priv->selectors,
				                 children);
		}
	}

	for (item = selector->priv->selectors; item; item = g_slist_next (item))
	{
		ret->priv->selectors =
			g_slist_prepend (ret->priv->selectors,
			                 selector_copy (ret, item->data));
	}

	if (explicit_children && !ret->priv->has_selected)
	{
		ret->priv->selectors =
			g_slist_prepend (ret->priv->selectors,
			                 children);
	}

	ret->priv->selectors = g_slist_reverse (ret->priv->selectors);
	ret->priv->has_selected = selector->priv->has_selected;

	ret->priv->prevent_implicit_children = TRUE;

	return ret;
}

void
cdn_selector_set_implicit_children (CdnSelector *selector,
                                    gboolean     isimplicit)
{
	g_return_if_fail (CDN_IS_SELECTOR (selector));

	selector->priv->implicit_children = isimplicit;
}
