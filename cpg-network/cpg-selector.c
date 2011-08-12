/*
 * cpg-selector.c
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

#include "cpg-selector.h"
#include "cpg-parser-context.h"
#include "cpg-parser.h"
#include "cpg-expansion.h"
#include "cpg-selection.h"
#include "cpg-statement.h"
#include "cpg-taggable.h"

#include <string.h>

#define CPG_SELECTOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_SELECTOR, CpgSelectorPrivate))

#define CPG_SELECTOR_KEY_OVERRIDE_NAME "CpgSelectorKeyOverrideName"

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
	gchar *as_string;
	GSList *selections_in;
	GSList *selections_out;

	union
	{
		struct
		{
			CpgEmbeddedString *identifier;
			gboolean partial;
		} identifier;

		struct
		{
			CpgEmbeddedString *regex;
			gboolean partial;
		} regex;

		struct
		{
			CpgSelectorPseudoType type;
			GSList *arguments;
		} pseudo;
	};
} Selector;

struct _CpgSelectorPrivate
{
	CpgSelection *self;

	GSList *selectors;
	gboolean has_selected;
	GString *as_string;

	GSList *from_set;

	guint last_id;

	gint line_start;
	gint line_end;
	gint column_start;
	gint column_end;
};

enum
{
	SELECT,
	NUM_SIGNALS
};

static gchar const *selector_pseudo_names[CPG_SELECTOR_PSEUDO_NUM] =
{
	"root",
	"children",
	"parent",
	"first",
	"last",
	"subset",
	"states",
	"links",
	"groups",
	"imports",
	"properties",
	"actions",
	"functions",
	"objects",
	"siblings",
	"templates",
	"count",
	"from",
	"to",
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
	"has-tag"
};

static guint signals[NUM_SIGNALS];

static void cpg_statement_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CpgSelector,
                         cpg_selector,
                         G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (CPG_TYPE_STATEMENT, cpg_statement_iface_init))

enum
{
	PROP_0,
	PROP_LINE_START,
	PROP_LINE_END,
	PROP_COLUMN_START,
	PROP_COLUMN_END
};

static void
cpg_statement_iface_init (gpointer iface)
{

}

static Selector *
selector_new (CpgSelector  *selector,
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

	g_free (selector->as_string);
	g_slice_free (Selector, selector);
}

static Selector *
selector_identifier_new (CpgSelector       *sel,
                         CpgEmbeddedString *identifier,
                         gboolean           partial)
{
	Selector *selector;
	gchar const *r;

	selector = selector_new (sel, SELECTOR_TYPE_IDENTIFIER);

	r = cpg_embedded_string_expand (identifier, NULL, NULL);

	selector->identifier.identifier = g_object_ref (identifier);
	selector->identifier.partial = partial;

	selector->as_string = g_strdup_printf ("\"%s\"", r);

	return selector;
}

static Selector *
selector_regex_new (CpgSelector       *sel,
                    CpgEmbeddedString *regex,
                    gboolean           partial)
{
	Selector *selector;
	gchar const *r;

	selector = selector_new (sel, SELECTOR_TYPE_REGEX);

	r = cpg_embedded_string_expand (regex, NULL, NULL);

	selector->regex.regex = g_object_ref (regex);
	selector->regex.partial = partial;

	selector->as_string = g_strdup_printf ("/%s/", r);

	return selector;
}

static Nth
parse_nth (GSList             *arguments,
           CpgEmbeddedContext *context)
{
	Nth ret = {1, 1, -1};
	GSList *maxpos = NULL;
	GSList *apos = NULL;
	gchar const *first;

	if (!arguments)
	{
		return ret;
	}

	first = cpg_embedded_string_expand (arguments->data, context, NULL);

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

		second = cpg_embedded_string_expand (arguments->next->data,
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
		ret.max = (gint)g_ascii_strtoll (cpg_embedded_string_expand (maxpos->data, context, NULL), NULL, 10);
	}

	if (apos)
	{
		ret.a = (gint)g_ascii_strtoll (cpg_embedded_string_expand (apos->data, context, NULL), NULL, 10);
	}

	return ret;
}

static gchar const *
pseudo_name (CpgSelectorPseudoType type)
{
	g_return_val_if_fail (type >= 0 && type < CPG_SELECTOR_PSEUDO_NUM, NULL);

	return selector_pseudo_names[type];
}

static Selector *
selector_pseudo_new (CpgSelector           *sel,
                     CpgSelectorPseudoType  type,
                     GSList                *arguments)
{
	Selector *selector;
	GString *args;

	selector = selector_new (sel, SELECTOR_TYPE_PSEUDO);

	selector->pseudo.type = type;
	selector->pseudo.arguments = g_slist_copy (arguments);

	g_slist_foreach (selector->pseudo.arguments, (GFunc)g_object_ref, NULL);

	args = g_string_new ("");

	while (arguments)
	{
		if (args->len != 0)
		{
			g_string_append (args, ", ");
		}

		if (CPG_IS_EMBEDDED_STRING (arguments->data))
		{
			g_string_append_printf (args,
			                        "\"%s\"",
			                        cpg_embedded_string_expand (arguments->data, NULL, NULL));
		}
		else if (CPG_IS_SELECTOR (arguments->data))
		{
			g_string_append (args, cpg_selector_as_string (arguments->data));
		}

		arguments = g_slist_next (arguments);
	}

	selector->as_string = g_strdup_printf ("%s%s%s%s",
	                                       pseudo_name (type),
	                                       args->len ? "(" : "",
	                                       args->str,
	                                       args->len ? ")" : "");

	g_string_free (args, TRUE);

	return selector;
}

static void
cpg_selector_finalize (GObject *object)
{
	CpgSelector *selector;

	selector = CPG_SELECTOR (object);

	g_slist_foreach (selector->priv->selectors, (GFunc)selector_free, NULL);
	g_slist_free (selector->priv->selectors);

	g_string_free (selector->priv->as_string, TRUE);

	g_slist_foreach (selector->priv->from_set, (GFunc)g_object_unref, NULL);
	g_slist_free (selector->priv->from_set);

	if (selector->priv->self)
	{
		g_object_unref (selector->priv->self);
	}

	G_OBJECT_CLASS (cpg_selector_parent_class)->finalize (object);
}

static void
cpg_selector_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
	CpgSelector *self = CPG_SELECTOR (object);

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
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_selector_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
	CpgSelector *self = CPG_SELECTOR (object);

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
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_selector_class_init (CpgSelectorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_selector_finalize;

	object_class->get_property = cpg_selector_get_property;
	object_class->set_property = cpg_selector_set_property;


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

	g_type_class_add_private (object_class, sizeof (CpgSelectorPrivate));

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
}

static void
cpg_selector_init (CpgSelector *self)
{
	self->priv = CPG_SELECTOR_GET_PRIVATE (self);
	self->priv->as_string = g_string_sized_new (255);
}

CpgSelector *
cpg_selector_new ()
{
	return g_object_new (CPG_TYPE_SELECTOR, NULL);
}

/**
 * cpg_selector_parse:
 * @s: Description
 * @error: A #GError
 *
 * Parse a selector from a string.
 *
 * Returns: (transfer full): A #CpgSelector
 *
 **/
CpgSelector *
cpg_selector_parse (gchar const *s,
                    GError      **error)
{
	CpgSelector *ret;
	CpgParserContext *ctx;
	GInputStream *stream;

	g_return_val_if_fail (s != NULL, NULL);

	ctx = cpg_parser_context_new (NULL);
	cpg_parser_context_set_start_token (ctx, T_START_SELECTOR);

	stream = g_memory_input_stream_new_from_data (s, strlen (s), NULL);

	cpg_parser_context_push_input (ctx, NULL, stream, NULL);
	g_object_unref (stream);

	if (!cpg_parser_context_parse (ctx, error))
	{
		return NULL;
	}

	ret = cpg_parser_context_pop_selector (ctx);
	g_object_unref (ctx);

	return ret;
}

static void
add_selector_to_string (CpgSelector *selector,
                        Selector    *sel,
                        gboolean     prepend)
{
	if (selector->priv->as_string->len != 0)
	{
		if (prepend)
		{
			g_string_prepend_c (selector->priv->as_string, '|');
		}
		else
		{
			g_string_append_c (selector->priv->as_string, '|');
		}
	}

	if (prepend)
	{
		g_string_prepend (selector->priv->as_string, sel->as_string);
	}
	else
	{
		g_string_append (selector->priv->as_string, sel->as_string);
	}
}

static guint
add_selector (CpgSelector *selector,
              Selector    *sel,
              gboolean     append)
{
	gboolean prepend;

	prepend = (!selector->priv->has_selected) == append;

	add_selector_to_string (selector, sel, !append);

	if (prepend)
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
cpg_selector_append (CpgSelector       *selector,
                     CpgEmbeddedString *identifier)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), 0);
	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (identifier), 0);

	return add_selector (selector, selector_identifier_new (selector,
	                                                        identifier,
	                                                        FALSE), TRUE);
}

guint
cpg_selector_append_partial (CpgSelector       *selector,
                          CpgEmbeddedString *identifier)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), 0);
	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (identifier), 0);

	return add_selector (selector, selector_identifier_new (selector,
	                                                        identifier,
	                                                        TRUE), TRUE);
}

guint
cpg_selector_prepend (CpgSelector       *selector,
                      CpgEmbeddedString *identifier)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), 0);
	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (identifier), 0);

	return add_selector (selector, selector_identifier_new (selector,
	                                                        identifier,
	                                                        FALSE), FALSE);
}


guint
cpg_selector_prepend_partial (CpgSelector       *selector,
                          CpgEmbeddedString *identifier)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), 0);
	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (identifier), 0);

	return add_selector (selector, selector_identifier_new (selector,
	                                                        identifier,
	                                                        TRUE), FALSE);
}

guint
cpg_selector_append_pseudo (CpgSelector           *selector,
                            CpgSelectorPseudoType  type,
                            GSList                *arguments)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), 0);

	return add_selector (selector, selector_pseudo_new (selector,
	                                                    type,
	                                                    arguments), TRUE);
}

guint
cpg_selector_prepend_pseudo (CpgSelector           *selector,
                             CpgSelectorPseudoType  type,
                             GSList                *arguments)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), 0);

	return add_selector (selector, selector_pseudo_new (selector,
	                                                    type,
	                                                    arguments), FALSE);
}

guint
cpg_selector_append_regex (CpgSelector       *selector,
                           CpgEmbeddedString *regex)
{

	g_return_val_if_fail (CPG_IS_SELECTOR (selector), 0);
	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (regex), 0);

	return add_selector (selector, selector_regex_new (selector,
	                                                   regex,
	                                                   FALSE), TRUE);
}

guint
cpg_selector_append_regex_partial (CpgSelector       *selector,
                                   CpgEmbeddedString *regex)
{

	g_return_val_if_fail (CPG_IS_SELECTOR (selector), 0);
	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (regex), 0);

	return add_selector (selector, selector_regex_new (selector,
	                                                   regex,
	                                                   TRUE), TRUE);
}

guint
cpg_selector_prepend_regex (CpgSelector       *selector,
                            CpgEmbeddedString *regex)
{

	g_return_val_if_fail (CPG_IS_SELECTOR (selector), 0);
	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (regex), 0);

	return add_selector (selector, selector_regex_new (selector,
	                                                   regex,
	                                                   FALSE), FALSE);
}

guint
cpg_selector_prepend_regex_partial (CpgSelector       *selector,
                                    CpgEmbeddedString *regex)
{

	g_return_val_if_fail (CPG_IS_SELECTOR (selector), 0);
	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (regex), 0);

	return add_selector (selector, selector_regex_new (selector,
	                                                   regex,
	                                                   TRUE), FALSE);
}

static CpgSelection *
make_child_selection (CpgSelection *parent,
                      CpgExpansion *expansion,
                      gpointer      obj)
{
	GSList *expansions;
	CpgSelection *ret;

	expansions = g_slist_copy (cpg_selection_get_expansions (parent));

	if (expansion)
	{
		expansions = g_slist_prepend (expansions, expansion);
	}

	ret = cpg_selection_new_defines (obj,
	                                 expansions,
	                                 cpg_selection_get_defines (parent),
	                                 FALSE);

	g_slist_free (expansions);

	return ret;
}

static gboolean
identifier_match (Selector     *selector,
                  CpgExpansion *expansion,
                  gchar const  *name)
{
	gchar const *ex;

	ex = cpg_expansion_get (expansion, 0);

	if (!selector->identifier.partial)
	{
		return g_strcmp0 (ex, name) == 0;
	}
	else
	{
		return strstr (name, ex) != NULL;
	}
}

static GSList *
selector_select_identifier_name (Selector           *selector,
                                 CpgSelection       *parent,
                                 gchar const        *name,
                                 CpgEmbeddedContext *context,
                                 CpgExpansion       *expansion,
                                 GSList             *ret)
{
	if (identifier_match (selector, expansion, name))
	{
		ret = g_slist_prepend (ret,
		                       make_child_selection (parent,
		                                             expansion,
		                                             cpg_selection_get_object (parent)));
	}

	return ret;
}

static gchar const *
name_from_selection (CpgSelection *selection)
{
	gchar const *ret;
	gpointer obj;

	ret = g_object_get_data (G_OBJECT (selection),
	                         CPG_SELECTOR_KEY_OVERRIDE_NAME);

	if (ret)
	{
		return ret;
	}

	obj = cpg_selection_get_object (selection);

	if (CPG_IS_OBJECT (obj))
	{
		return cpg_object_get_id (obj);
	}
	else if (CPG_IS_PROPERTY (obj))
	{
		return cpg_property_get_name (obj);
	}
	else if (CPG_IS_LINK_ACTION (obj))
	{
		return cpg_link_action_get_target (obj);
	}

	return NULL;
}

static GSList *
selector_select_identifier (CpgSelector        *self,
                            Selector           *selector,
                            CpgSelection       *parent,
                            CpgEmbeddedContext *context)
{
	GSList *ret = NULL;
	GSList *exps;
	GSList *e;
	gchar const *name;

	exps = cpg_embedded_string_expand_multiple (selector->identifier.identifier,
	                                            context,
	                                            NULL);

	name = name_from_selection (parent);

	for (e = exps; e; e = g_slist_next (e))
	{
		ret = selector_select_identifier_name (selector,
		                                       parent,
		                                       name,
		                                       context,
		                                       e->data,
		                                       ret);
	}

	g_slist_foreach (exps, (GFunc)g_object_unref, NULL);
	g_slist_free (exps);

	return g_slist_reverse (ret);
}

static CpgExpansion *
expansion_from_match (GMatchInfo const *info)
{
	gchar **s;
	CpgExpansion *ret;

	s = g_match_info_fetch_all (info);
	ret = cpg_expansion_new ((gchar const * const *)s);
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

static CpgSelection *
make_child_selection_from_match (CpgSelection     *parent,
                                 GMatchInfo const *info,
                                 gpointer          obj)
{
	CpgExpansion *expansion;
	CpgSelection *ret;

	expansion = expansion_from_match (info);
	ret = make_child_selection (parent, expansion, obj);
	g_object_unref (expansion);

	return ret;
}

static GRegex *
regex_create (Selector            *selector,
              CpgEmbeddedContext  *context,
              GError             **error)
{
	gchar const *s;
	gchar *r;
	GRegex *ret;

	s = cpg_embedded_string_expand (selector->regex.regex, context, NULL);

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
                            CpgSelection       *parent,
                            gchar const        *name,
                            CpgEmbeddedContext *context,
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
		CpgSelection *childsel;

		childsel = make_child_selection_from_match (parent,
		                                            info,
		                                            cpg_selection_get_object (parent));

		ret = g_slist_prepend (ret, childsel);
		g_match_info_free (info);
	}

	g_regex_unref (regex);
	return ret;
}

static GSList *
selector_select_regex (CpgSelector        *self,
                       Selector           *selector,
                       CpgSelection       *parent,
                       CpgEmbeddedContext *context)
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
		CpgSelection *sel = children->data;

		if (nth_match (nth, i))
		{
			ret = g_slist_prepend (ret,
			                       cpg_selection_copy (sel));
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
		CpgSelection *sel;
		GType tp;
		gpointer obj;

		sel = objs->data;
		obj = cpg_selection_get_object (sel);
		tp = G_OBJECT_TYPE (obj);

		if ((inherited && g_type_is_a (tp, gtype)) ||
		    (!inherited && tp == gtype))
		{
			ret = g_slist_prepend (ret,
			                       cpg_selection_copy (sel));
		}

		objs = g_slist_next (objs);
	}

	return ret;
}

static GSList *
copy_selections (GSList *selections)
{
	GSList *ret = NULL;

	while (selections)
	{
		ret = g_slist_prepend (ret, cpg_selection_copy (selections->data));
		selections = g_slist_next (selections);
	}

	return g_slist_reverse (ret);
}

static gpointer
pseudo_parent (gpointer obj)
{
	if (CPG_IS_OBJECT (obj))
	{
		return cpg_object_get_parent (obj);
	}
	else if (CPG_IS_PROPERTY (obj))
	{
		return cpg_property_get_object (obj);
	}
	else if (CPG_IS_LINK_ACTION (obj))
	{
		return cpg_link_action_get_link (obj);
	}

	return NULL;
}

static CpgObject *
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

static CpgSelection *
expand_obj (CpgSelection *selection,
            gpointer      obj)
{
	return cpg_selection_new_defines (obj,
	                                  cpg_selection_get_expansions (selection),
	                                  cpg_selection_get_defines (selection),
	                                  FALSE);
}

static GSList *
expand_objs_reverse (CpgSelection *selection,
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
		CpgExpansion *ex;
		gchar const *s;
		CpgSelection *sel;
		GSList *expansions;
		CpgExpansion *expansion;

		s = name_from_selection (selection->data);
		ex = cpg_expansion_new_one (s);

		expansions = g_slist_copy (cpg_selection_get_expansions (selection->data));
		expansion = cpg_expansion_copy (ex);
		expansions = g_slist_append (expansions, expansion);

		sel = cpg_selection_new_defines (cpg_selection_get_object (selection->data),
		                                 expansions,
		                                 cpg_selection_get_defines (selection->data),
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
count_selection (CpgEmbeddedContext *context,
                 GSList             *selection)
{
	GSList *ret = NULL;
	GSList *item;
	gint i = 0;
	gchar *s;
	CpgExpansion *ex;

	while (selection)
	{
		ret = g_slist_prepend (ret, selection->data);
		++i;

		selection = g_slist_next (selection);
	}

	s = g_strdup_printf ("%d", i);
	ex = cpg_expansion_new_one (s);
	g_free (s);

	i = 0;

	for (item = ret; item; item = g_slist_next (item))
	{
		CpgSelection *sel;
		GSList *expansions;
		CpgExpansion *expansion;

		expansions = g_slist_copy (cpg_selection_get_expansions (item->data));
		expansion = cpg_expansion_copy (ex);
		expansions = g_slist_append (expansions, expansion);

		cpg_expansion_set_index (expansion, 0, i++);

		sel = cpg_selection_new_defines (cpg_selection_get_object (item->data),
		                                 expansions,
		                                 cpg_selection_get_defines (item->data),
		                                 FALSE);

		g_slist_free (expansions);
		g_object_unref (expansion);

		item->data = sel;
	}

	cpg_embedded_context_add_expansion (context, ex);

	g_object_unref (ex);


	return g_slist_reverse (ret);
}

static GSList *
selector_pseudo_from_to (CpgSelector        *self,
                         Selector           *selector,
                         gchar const        *name,
                         GSList             *ret,
                         CpgSelection       *sel,
                         CpgEmbeddedContext *context)
{
	CpgObject *obj;
	GSList *item;

	if (!CPG_IS_LINK (cpg_selection_get_object (sel)))
	{
		return ret;
	}

	g_object_get (cpg_selection_get_object (sel), name, &obj, NULL);

	if (!obj)
	{
		return ret;
	}

	cpg_embedded_context_save (context);

	cpg_embedded_context_add_selection (context, sel);

	for (item = selector->pseudo.arguments; item; item = g_slist_next (item))
	{
		CpgSelector *s = item->data;
		GSList *sub;
		GSList *subitem;

		cpg_selector_set_self (s, self->priv->self);

		sub = cpg_selector_select (s,
		                           G_OBJECT (obj),
		                           CPG_SELECTOR_TYPE_OBJECT,
		                           context);

		cpg_selector_set_self (s, NULL);

		for (subitem = sub; subitem; subitem = g_slist_next (subitem))
		{
			CpgSelection *subsel = subitem->data;
			CpgSelection *childsel;
			GSList *expansions;

			if (cpg_selection_get_object (subsel) != obj)
			{
				continue;
			}

			expansions = g_slist_copy (cpg_selection_get_expansions (sel));
			expansions = g_slist_concat (g_slist_copy (cpg_selection_get_expansions (subsel)),
			                             expansions);

			childsel = cpg_selection_new_defines (cpg_selection_get_object (sel),
			                                      expansions,
			                                      cpg_selection_get_defines (sel),
			                                      FALSE);

			ret = g_slist_prepend (ret, childsel);

			g_slist_free (expansions);
		}

		g_slist_foreach (sub, (GFunc)g_object_unref, NULL);
		g_slist_free (sub);
	}

	g_object_unref (obj);

	cpg_embedded_context_restore (context);
	return ret;
}

static gboolean
has_all_templates (CpgObject *obj,
                   GSList    *templates)
{
	GSList *selitem;

	for (selitem = templates; selitem; selitem = g_slist_next (selitem))
	{
		gpointer t;
		GSList const *templ;
		gboolean found;

		t = cpg_selection_get_object (selitem->data);
		templ = cpg_object_get_applied_templates (obj);
		found = FALSE;

		while (templ)
		{
			if (templ->data == t)
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
selector_pseudo_has_tag (CpgSelector        *self,
                         Selector           *selector,
                         CpgSelection       *sel,
                         CpgEmbeddedContext *context,
                         GSList             *ret)
{
	gpointer obj;
	gboolean valid = TRUE;
	GSList *item;

	obj = cpg_selection_get_object (sel);

	if (!CPG_IS_TAGGABLE (obj))
	{
		return ret;
	}

	for (item = selector->pseudo.arguments; item; item = g_slist_next (item))
	{
		CpgEmbeddedString *s;
		GSList *ex;
		GSList *exitem;

		s = item->data;

		if (!CPG_IS_EMBEDDED_STRING (s))
		{
			continue;
		}

		ex = cpg_embedded_string_expand_multiple (s, context, NULL);

		for (exitem = ex; exitem; exitem = g_slist_next (exitem))
		{
			if (!cpg_taggable_has_tag (obj,
			                           cpg_expansion_get (exitem->data, 0)))
			{
				valid = FALSE;
				break;
			}
		}

		g_slist_foreach (ex, (GFunc)g_object_unref, NULL);
		g_slist_free (ex);

		if (!valid)
		{
			break;
		}
	}

	if (valid)
	{
		ret = g_slist_prepend (ret,
		                       cpg_selection_copy (sel));
	}

	return ret;
}

static GSList *
selector_pseudo_has_template (CpgSelector        *self,
                              Selector           *selector,
                              CpgSelection       *sel,
                              CpgEmbeddedContext *context,
                              GSList             *ret)
{
	CpgObject *obj;
	CpgObject *tp;
	CpgGroup *template_group;
	GSList *item;
	gboolean valid = TRUE;

	obj = cpg_selection_get_object (sel);

	if (!CPG_IS_OBJECT (obj))
	{
		return ret;
	}

	tp = top_parent (obj);

	if (!CPG_IS_NETWORK (tp))
	{
		return ret;
	}

	template_group = cpg_network_get_template_group (CPG_NETWORK (tp));

	cpg_embedded_context_save (context);
	cpg_embedded_context_add_selection (context, sel);

	for (item = selector->pseudo.arguments; item; item = g_slist_next (item))
	{
		CpgSelector *s = item->data;
		GSList *sub;
		GSList const *children;

		children = cpg_group_get_children (template_group);
		cpg_selector_set_self (s, self->priv->self);

		while (children)
		{
			sub = cpg_selector_select (s,
			                           G_OBJECT (children->data),
			                           CPG_SELECTOR_TYPE_OBJECT,
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

		cpg_selector_set_self (s, NULL);

		if (!valid)
		{
			break;
		}
	}

	cpg_embedded_context_restore (context);

	if (valid)
	{
		ret = g_slist_prepend (ret, cpg_selection_copy (sel));
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
selector_pseudo_if (CpgSelector        *self,
                    Selector           *selector,
                    GSList             *ret,
                    CpgSelection       *sel,
                    CpgEmbeddedContext *context,
                    gboolean            condition)
{
	gpointer obj;
	GSList *item;

	cpg_embedded_context_save (context);

	cpg_embedded_context_add_selection (context, sel);

	obj = cpg_selection_get_object (sel);

	for (item = selector->pseudo.arguments; item; item = g_slist_next (item))
	{
		GSList *sub = NULL;
		gboolean r = FALSE;

		if (CPG_IS_SELECTOR (item->data))
		{
			cpg_selector_set_self (item->data, self->priv->self);

			sub = cpg_selector_select (item->data,
			                           obj,
			                           CPG_SELECTOR_TYPE_ANY,
			                           context);

			cpg_selector_set_self (item->data, NULL);

			r = (sub != NULL);
		}
		else if (CPG_IS_EMBEDDED_STRING (item->data))
		{
			gchar const *ex;

			ex = cpg_embedded_string_expand (item->data, context, NULL);

			r = !test_string_empty (ex);
		}

		if (!condition && sub != NULL)
		{
			/* Remove if in selection */
			GSList *subitem;
			gboolean found = FALSE;

			for (subitem = sub; subitem; subitem = g_slist_next (subitem))
			{
				if (cpg_selection_get_object (subitem->data) == obj)
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
			                       cpg_selection_copy (sel));
			break;
		}
	}

	cpg_embedded_context_restore (context);
	return ret;
}

static GSList *
selector_pseudo_isempty (CpgSelector        *self,
                         Selector           *selector,
                         GSList             *parent,
                         CpgEmbeddedContext *context)
{
	GSList *item;
	GSList *seli;
	gboolean isempty = TRUE;

	for (seli = parent; seli; seli = g_slist_next (seli))
	{
		CpgSelection *sel;

		sel = seli->data;

		cpg_embedded_context_save (context);

		cpg_embedded_context_add_selection (context, sel);

		for (item = selector->pseudo.arguments; item; item = g_slist_next (item))
		{
			gboolean r = FALSE;

			if (CPG_IS_SELECTOR (item->data))
			{
				GSList *sub;

				cpg_selector_set_self (item->data, self->priv->self);

				sub = cpg_selector_select (item->data,
				                           cpg_selection_get_object (sel),
				                           CPG_SELECTOR_TYPE_ANY,
				                           context);

				cpg_selector_set_self (item->data, NULL);

				r = (sub == NULL);

				g_slist_foreach (sub, (GFunc)g_object_unref, NULL);
				g_slist_free (sub);
			}
			else if (CPG_IS_EMBEDDED_STRING (item->data))
			{
				gchar const *ex;

				ex = cpg_embedded_string_expand (item->data, context, NULL);

				r = test_string_empty (ex);
			}

			if (!r)
			{
				isempty = FALSE;
				break;
			}
		}

		cpg_embedded_context_restore (context);

		if (!isempty)
		{
			break;
		}
	}

	if (isempty)
	{
		return g_slist_prepend (NULL,
		                        cpg_selection_copy (self->priv->self));
	}
	else
	{
		return NULL;
	}
}

static gchar const *
object_type_name (gpointer obj)
{
	if (CPG_IS_FUNCTION (obj))
	{
		return "function";
	}
	else if (CPG_IS_NETWORK (obj))
	{
		return "network";
	}
	else if (CPG_IS_GROUP (obj))
	{
		return "group";
	}
	else if (CPG_IS_LINK (obj))
	{
		return "link";
	}
	else if (CPG_IS_PROPERTY (obj))
	{
		return "property";
	}
	else if (CPG_IS_LINK_ACTION (obj))
	{
		return "action";
	}
	else if (CPG_IS_OBJECT (obj))
	{
		return "state";
	}
	else
	{
		return "";
	}
}

static GSList *
selector_pseudo_type (CpgEmbeddedContext *context,
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

		tname = object_type_name (cpg_selection_get_object (item->data));

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
		CpgSelection *sel;
		gchar const *tname;
		gint *val;
		CpgExpansion *expansion;
		gchar const *ex[3];
		gchar *cnt;

		sel = selection->data;

		tname = object_type_name (cpg_selection_get_object (sel));

		val = g_hash_table_lookup (types, tname);

		expansions = g_slist_copy (cpg_selection_get_expansions (sel));

		cnt = g_strdup_printf ("%d", val[1]);

		ex[0] = tname;
		ex[1] = cnt;
		ex[2] = NULL;

		expansion = cpg_expansion_new (ex);
		g_free (cnt);

		expansions = g_slist_append (expansions, expansion);

		cpg_expansion_set_index (expansion, 0, val[0]);

		ret = g_slist_prepend (ret,
		                       cpg_selection_new_defines (cpg_selection_get_object (sel),
		                                                  expansions,
		                                                  cpg_selection_get_defines (sel),
		                                                  FALSE));

		g_slist_free (expansions);
		g_object_unref (expansion);

		selection = g_slist_next (selection);
	}

	g_hash_table_destroy (types);

	return g_slist_reverse (ret);
}

static gchar *
selector_until_as_string (CpgSelector *self,
                          Selector    *selector)
{
	GSList *item;
	GString *ret;

	ret = g_string_new ("");

	for (item = self->priv->selectors; item && item->data != selector; item = g_slist_next (item))
	{
		Selector *sel;

		sel = item->data;

		if (ret->len != 0)
		{
			g_string_append_c (ret, '|');
		}

		g_string_append (ret, sel->as_string);
	}

	return g_string_free (ret, FALSE);
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
			g_string_append (ret, ", ");
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
debug_selections (CpgSelector        *self,
                  Selector           *selector,
                  GSList             *parent,
                  CpgEmbeddedContext *context)
{
	GSList *item;
	gchar *s;

	s = selector_until_as_string (self, selector);
	g_printerr ("[debug] Selector : %s\n", s);
	g_free (s);

	s = expansions_as_string (cpg_embedded_context_get_expansions (context));
	g_printerr ("[debug] Context  : %s\n", s);
	g_free (s);

	for (item = parent; item; item = g_slist_next (item))
	{
		CpgSelection *sel;
		gpointer obj;
		GSList *expansions;

		sel = item->data;
		obj = cpg_selection_get_object (sel);

		g_printerr ("[debug]   => ");

		if (CPG_IS_OBJECT (obj))
		{
			s = cpg_object_get_full_id (obj);
			g_printerr ("%s", s);
			g_free (s);
		}
		else if (CPG_IS_PROPERTY (obj))
		{
			s = cpg_property_get_full_name (obj);
			g_printerr ("%s", s);
			g_free (s);
		}
		else if (CPG_IS_LINK_ACTION (obj))
		{
			gchar const *target;
			gchar *id;

			target = cpg_link_action_get_target (obj);
			id = cpg_object_get_full_id (CPG_OBJECT (cpg_link_action_get_link (obj)));

			g_printerr ("%s < %s", id, target);
			g_free (id);
		}

		expansions = cpg_selection_get_expansions (sel);
		s = expansions_as_string (expansions);

		g_printerr (" <= %s", s);
		g_free (s);

		if (selector->pseudo.arguments)
		{
			g_printerr (" with [");

			cpg_embedded_context_save (context);
			cpg_embedded_context_add_selection (context, sel);

			for (item = selector->pseudo.arguments; item; item = g_slist_next (item))
			{
				if (item != selector->pseudo.arguments)
				{
					g_printerr (", ");
				}

				g_printerr ("%s", cpg_embedded_string_expand (item->data, context, NULL));
			}

			cpg_embedded_context_restore (context);

			g_printerr ("]");
		}

		g_printerr ("\n");

	}

	g_printerr ("\n");

	return copy_selections (parent);
}

static GSList *
children_reverse (CpgSelection *selection,
                  gpointer      obj)
{
	GSList *ret = NULL;

	if (CPG_IS_GROUP (obj))
	{
		GSList const *children;

		children = cpg_group_get_children (obj);

		ret = g_slist_concat (expand_objs_reverse (selection, children),
		                      ret);
	}

	if (CPG_IS_OBJECT (obj))
	{
		GSList *props;

		props = cpg_object_get_properties (obj);

		ret = g_slist_concat (expand_objs_reverse (selection, props),
		                      ret);

		g_slist_free (props);
	}

	if (CPG_IS_GROUP (obj))
	{
		CpgPropertyInterface *iface;
		gchar **names;
		gchar **ptr;

		iface = cpg_group_get_property_interface (CPG_GROUP (obj));
		names = cpg_property_interface_get_names (iface);

		ptr = names;

		while (ptr && *ptr)
		{
			CpgSelection *s;

			s = expand_obj (selection,
			                cpg_property_interface_lookup (iface, *ptr));

			g_object_set_data_full (G_OBJECT (s),
			                        CPG_SELECTOR_KEY_OVERRIDE_NAME,
			                        g_strdup (*ptr),
			                        (GDestroyNotify)g_free);

			ret = g_slist_prepend (ret, s);

			++ptr;
		}

		g_strfreev (names);
	}

	if (CPG_IS_LINK (obj))
	{
		GSList const *actions;

		actions = cpg_link_get_actions (obj);

		ret = g_slist_concat (expand_objs_reverse (selection, actions),
		                      ret);
	}

	return ret;
}

static GSList *
descendants_reverse (CpgSelection *selection)
{
	GSList *ret = NULL;
	GQueue *queue;

	queue = g_queue_new ();
	g_queue_push_head (queue, selection);

	while (!g_queue_is_empty (queue))
	{
		CpgSelection *sel;
		GSList *children;
		GSList *item;
		gpointer obj;

		sel = g_queue_pop_head (queue);

		if (sel != selection)
		{
			ret = g_slist_prepend (ret, sel);
		}

		obj = cpg_selection_get_object (sel);
		children = g_slist_reverse (children_reverse (sel, obj));

		for (item = children; item; item = g_slist_next (item))
		{
			g_queue_push_tail (queue, item->data);
		}

		g_slist_free (item->data);
	}

	return ret;
}

static GSList *
ancestors_reverse (CpgSelection *selection)
{
	GSList *ret = NULL;
	gpointer obj;

	obj = cpg_selection_get_object (selection);

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
	GSList *objs = NULL;
	GSList *item;

	item = parent;

	while (item)
	{
		gpointer obj;

		obj = cpg_selection_get_object (item->data);

		if (g_slist_find (objs, obj) != NULL)
		{
			GSList *tmp;

			tmp = item->next;

			g_object_unref (item->data);
			parent = g_slist_delete_link (parent, item);

			item = tmp;
		}
		else
		{
			objs = g_slist_prepend (objs, obj);
		}
	}

	g_slist_free (objs);
	return parent;
}

static GSList *
selector_pseudo_has_flag (CpgSelector        *self,
                          Selector           *selector,
                          CpgSelection       *selection,
                          CpgEmbeddedContext *context,
                          GSList             *ret)
{
	gpointer obj;
	GSList *arg;
	CpgPropertyFlags flags;
	gboolean valid = TRUE;

	obj = cpg_selection_get_object (selection);

	if (!CPG_IS_PROPERTY (obj))
	{
		return ret;
	}

	flags = cpg_property_get_flags (obj);

	for (arg = selector->pseudo.arguments; arg; arg = g_slist_next (arg))
	{
		CpgEmbeddedString *s;
		GSList *ret;
		GSList *item;

		if (!CPG_IS_EMBEDDED_STRING (arg->data))
		{
			continue;
		}

		s = CPG_EMBEDDED_STRING (arg->data);
		ret = cpg_embedded_string_expand_multiple (s, context, NULL);

		for (item = ret; item; item = g_slist_next (item))
		{
			CpgPropertyFlags add = 0;

			cpg_property_flags_from_string (cpg_expansion_get (item->data, 0),
			                                &add,
			                                NULL);

			if ((flags & add) == 0)
			{
				valid = FALSE;
				break;
			}
		}

		g_slist_foreach (ret, (GFunc)g_object_unref, NULL);
		g_slist_free (ret);

		if (!valid)
		{
			break;
		}
	}

	if (valid)
	{
		ret = g_slist_prepend (ret,
		                       cpg_selection_copy (selection));
	}

	return ret;
}

static GSList *
selector_select_pseudo (CpgSelector        *self,
                        Selector           *selector,
                        GSList             *parent,
                        CpgEmbeddedContext *context)
{
	GSList *ret = NULL;
	GSList *item;
	CpgSelection *last = NULL;

	switch (selector->pseudo.type)
	{
		case CPG_SELECTOR_PSEUDO_TYPE_SUBSET:
		{
			Nth nth = parse_nth (selector->pseudo.arguments,
			                     context);

			return g_slist_reverse (select_nth_reverse (parent,
				                                    &nth,
				                                    0));
		}
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_STATES:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CPG_TYPE_OBJECT,
				                                     FALSE));
		case CPG_SELECTOR_PSEUDO_TYPE_LINKS:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CPG_TYPE_LINK,
				                                     FALSE));
		case CPG_SELECTOR_PSEUDO_TYPE_GROUPS:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CPG_TYPE_GROUP,
				                                     FALSE));
		case CPG_SELECTOR_PSEUDO_TYPE_IMPORTS:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CPG_TYPE_IMPORT,
				                                     FALSE));
		case CPG_SELECTOR_PSEUDO_TYPE_PROPERTIES:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CPG_TYPE_PROPERTY,
				                                     FALSE));
		case CPG_SELECTOR_PSEUDO_TYPE_ACTIONS:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CPG_TYPE_LINK_ACTION,
				                                     FALSE));
		case CPG_SELECTOR_PSEUDO_TYPE_FUNCTIONS:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CPG_TYPE_FUNCTION,
				                                     FALSE));
		case CPG_SELECTOR_PSEUDO_TYPE_OBJECTS:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     CPG_TYPE_OBJECT,
				                                     TRUE));
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_COUNT:
			return count_selection (context, parent);
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_NAME:
			return annotate_names (parent);
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_TYPE:
			return selector_pseudo_type (context, parent);
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_SELF:
			if (parent && parent->data)
			{
				/* Keep the context from the first parent */
				return g_slist_prepend (NULL,
				                        expand_obj (parent->data,
				                                    cpg_selection_get_object (self->priv->self)));
			}
			else
			{
				return g_slist_prepend (NULL,
				                        g_object_ref (self->priv->self));
			}
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_DEBUG:
			return debug_selections (self, selector, parent, context);
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_UNIQUE:
			return unique_selections (parent);
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_IS_EMPTY:
			return selector_pseudo_isempty (self,
			                                selector,
			                                parent,
			                                context);
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_FROM_SET:
			return copy_selections (self->priv->from_set);
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_ROOT:
			if (!parent)
			{
				return g_slist_prepend (NULL,
				                        expand_obj (self->priv->self,
				                                    top_parent (cpg_selection_get_object (self->priv->self))));
			}
		break;
		default:
		break;
	}

	for (item = parent; item; item = g_slist_next (item))
	{
		CpgSelection *sel = item->data;
		gpointer obj;
		Nth nth = {0,};

		obj = cpg_selection_get_object (sel);

		cpg_embedded_context_save (context);

		cpg_embedded_context_add_selection (context, sel);

		switch (selector->pseudo.type)
		{
			case CPG_SELECTOR_PSEUDO_TYPE_SIBLINGS:
				nth = parse_nth (selector->pseudo.arguments,
				                 context);
			break;
			default:
			break;
		}

		cpg_embedded_context_restore (context);

		switch (selector->pseudo.type)
		{
			case CPG_SELECTOR_PSEUDO_TYPE_FIRST:
				return g_slist_prepend (ret,
				                        cpg_selection_copy (sel));
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_LAST:
				last = sel;
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_ROOT:
			{
				CpgObject *top;

				top = top_parent (obj);

				if (top)
				{
					return g_slist_prepend (ret,
					                        expand_obj (sel, top));
				}
			}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_TEMPLATES:
			{
				CpgObject *top;

				top = top_parent (obj);

				if (top && CPG_IS_NETWORK (top))
				{
					CpgGroup *template_group;

					template_group = cpg_network_get_template_group (CPG_NETWORK (top));

					return g_slist_prepend (ret,
					                        expand_obj (sel, template_group));
				}
			}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_CHILDREN:
			{
				ret = g_slist_concat (children_reverse (sel,
				                                        obj),
				                      ret);
			}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_DESCENDANTS:
			{
				ret = g_slist_concat (descendants_reverse (sel),
				                      ret);
			}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_ANCESTORS:
			{
				ret = g_slist_concat (ancestors_reverse (sel),
				                      ret);
			}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_SIBLINGS:
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
						if (cpg_selection_get_object (item->data) == obj)
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
			case CPG_SELECTOR_PSEUDO_TYPE_PARENT:
			{
				gpointer parent;

				parent = pseudo_parent (sel);

				if (parent)
				{
					ret = g_slist_prepend (ret,
					                       expand_obj (sel,
					                                   parent));
				}
			}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_FROM:
				ret = selector_pseudo_from_to (self,
				                               selector,
				                               "from",
				                               ret,
				                               sel,
				                               context);
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_TO:
				ret = selector_pseudo_from_to (self,
				                               selector,
				                               "to",
				                               ret,
				                               sel,
				                               context);
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_IF:
				ret = selector_pseudo_if (self,
				                          selector,
				                          ret,
				                          sel,
				                          context,
				                          TRUE);
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_REMOVE:
				ret = selector_pseudo_if (self,
				                          selector,
				                          ret,
				                          sel,
				                          context,
				                          FALSE);
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_HAS_FLAG:
				ret = selector_pseudo_has_flag (self,
				                                selector,
				                                sel,
				                                context,
				                                ret);
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_HAS_TEMPLATE:
				ret = selector_pseudo_has_template (self,
				                                    selector,
				                                    sel,
				                                    context,
				                                    ret);
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_HAS_TAG:
				ret = selector_pseudo_has_tag (self,
				                               selector,
				                               sel,
				                               context,
				                               ret);
			break;
			default:
				g_assert_not_reached ();
			break;
		}
	}

	if (last)
	{
		ret = g_slist_prepend (ret,
		                       cpg_selection_copy (last));
	}

	return g_slist_reverse (ret);
}

static GSList *
selector_select (CpgSelector        *self,
                 Selector           *selector,
                 GSList             *parent,
                 CpgEmbeddedContext *context)
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
			CpgSelection *sel = item->data;
			GSList *r = NULL;

			cpg_embedded_context_save (context);

			cpg_embedded_context_add_selection (context, sel);

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

			cpg_embedded_context_restore (context);

			ret = g_slist_concat (ret, r);
		}
	}

	return ret;
}

static void
annotate_expansions (GSList *selections)
{
	GSList *ptrs = NULL;
	GSList *item;
	gboolean breakit = FALSE;

	for (item = selections; item; item = g_slist_next (item))
	{
		CpgSelection *sel;

		sel = item->data;

		ptrs = g_slist_prepend (ptrs, cpg_selection_get_expansions (sel));
	}

	ptrs = g_slist_reverse (ptrs);

	while (!breakit)
	{
		GSList *expansions = NULL;

		breakit = TRUE;

		for (item = ptrs; item; item = g_slist_next (item))
		{
			GSList *i;

			i = item->data;

			if (i)
			{
				expansions =
					g_slist_prepend (expansions,
					                 i ? i->data : NULL);

				item->data = g_slist_next (item->data);

				if (item->data)
				{
					breakit = FALSE;
				}
			}
		}

		expansions = g_slist_reverse (expansions);
		cpg_expansions_annotate_indices (expansions);

		g_slist_free (expansions);
	}

	g_slist_free (ptrs);
}

static gboolean
selection_match_type (CpgSelection    *selection,
                      CpgSelectorType  type)
{
	gpointer obj;

	obj = cpg_selection_get_object (selection);

	if (type & CPG_SELECTOR_TYPE_ANY)
	{
		return TRUE;
	}

	if ((type & CPG_SELECTOR_TYPE_STATE) &&
	    G_OBJECT_TYPE (obj) == CPG_TYPE_OBJECT)
	{
		return TRUE;
	}

	if ((type & CPG_SELECTOR_TYPE_LINK) &&
	    CPG_IS_LINK (obj))
	{
		return TRUE;
	}

	if ((type & CPG_SELECTOR_TYPE_GROUP) &&
	    CPG_IS_GROUP (obj))
	{
		return TRUE;
	}

	if ((type & CPG_SELECTOR_TYPE_PROPERTY) &&
	    CPG_IS_PROPERTY (obj))
	{
		return TRUE;
	}

	if ((type & CPG_SELECTOR_TYPE_ACTION) &&
	    CPG_IS_LINK_ACTION (obj))
	{
		return TRUE;
	}

	if ((type & CPG_SELECTOR_TYPE_FUNCTION) &&
	    CPG_IS_FUNCTION (obj))
	{
		return TRUE;
	}

	return FALSE;
}

static GSList *
filter_selection (GSList          *selection,
                  CpgSelectorType  type)
{
	GSList *item;
	GSList *ret = NULL;

	for (item = selection; item; item = g_slist_next (item))
	{
		if (!selection_match_type (item->data, type))
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
selector_select_all (CpgSelector        *selector,
                     GObject            *parent,
                     CpgSelectorType     type,
                     CpgEmbeddedContext *context)
{
	GSList *item;
	GSList *ctx = NULL;
	GHashTable *defines;
	gboolean release_self = FALSE;
	CpgSelection *sel;

	if (context == NULL)
	{
		context = cpg_embedded_context_new ();
	}
	else
	{
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
		g_object_unref (context);
		return NULL;
	}

	defines = context ? cpg_embedded_context_get_defines (context) : NULL;
	sel = cpg_selection_new_defines (parent,
	                                 NULL,
	                                 defines,
	                                 FALSE);

	if (!selector->priv->self)
	{
		release_self = TRUE;
		selector->priv->self = sel;
	}

	ctx = g_slist_prepend (NULL, g_object_ref (sel));

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

		annotate_expansions (ctx);

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

	ctx = filter_selection (ctx, type);

	g_object_unref (context);

	return ctx;
}

/**
 * cpg_selector_select:
 * @selector: A #CpgSelector
 * @parent: A #GObject
 * @type: A #CpgSelectorType
 * @context: A #CpgEmbeddedContext
 *
 * Select objects (from @parent) using the selector.
 *
 * Returns: (element-type CpgSelection) (transfer full): A #GSList of #CpgSelection
 *
 **/
GSList *
cpg_selector_select (CpgSelector        *selector,
                     GObject            *parent,
                     CpgSelectorType     type,
                     CpgEmbeddedContext *context)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (G_IS_OBJECT (parent), NULL);
	g_return_val_if_fail (context == NULL || CPG_IS_EMBEDDED_CONTEXT (context), NULL);

	return selector_select_all (selector, parent, type, context);
}

gchar const *
cpg_selector_as_string (CpgSelector *selector)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);

	return selector->priv->as_string->str;
}

void
cpg_selector_set_partial (CpgSelector *selector,
                          gboolean     partial)
{
	GSList *item;

	g_return_if_fail (CPG_IS_SELECTOR (selector));

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
cpg_selector_get_last_id (CpgSelector *selector)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), 0);

	return selector->priv->last_id;
}

static Selector *
find_selector_by_id (CpgSelector *selector,
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
 * cpg_selector_get_in_context:
 * @selector: A #CpgSelector
 * @id: The selector id
 *
 * Get the in-context for a particular selector.
 *
 * Returns: (element-type CpgSelection) (transfer none): A #GSList of #CpgSelection
 *
 **/
GSList const *
cpg_selector_get_in_context (CpgSelector *selector,
                             guint        id)
{
	Selector *sel;

	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (id <= selector->priv->last_id, NULL);

	sel = find_selector_by_id (selector, id);

	return sel ? sel->selections_in : NULL;
}

/**
 * cpg_selector_get_out_context:
 * @selector: A #CpgSelector
 * @id: The selector id
 *
 * Get the out-context for a particular selector.
 *
 * Returns: (element-type CpgSelection) (transfer none): A #GSList of #CpgSelection
 *
 **/
GSList const *
cpg_selector_get_out_context (CpgSelector *selector,
                              guint        id)
{
	Selector *sel;

	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (id <= selector->priv->last_id, NULL);

	sel = find_selector_by_id (selector, id);

	return sel ? sel->selections_out : NULL;
}

/**
 * cpg_selector_is_pseudo_name:
 * @name: the name
 *
 * Check whether a name is a pseudo selector.
 *
 * Returns: %TRUE if the name is a pseudo selector, %FALSE otherwise
 *
 **/
gboolean
cpg_selector_is_pseudo_name (gchar const *name)
{
	gint i;

	for (i = 0; i < CPG_SELECTOR_PSEUDO_NUM; ++i)
	{
		if (g_strcmp0 (selector_pseudo_names[i], name) == 0)
		{
			return TRUE;
		}
	}

	return FALSE;
}

/**
 * cpg_selector_escape_identifier:
 * @name: the name
 *
 * Escapes an identifier if needed.
 *
 * Returns: (transfer full): the escaped identifier
 *
 **/
gchar *
cpg_selector_escape_identifier (gchar const *name)
{
	GString *ret;

	if (!cpg_selector_is_pseudo_name (name) &&
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
cpg_selector_set_from_set (CpgSelector *selector,
                           GSList      *selections)
{
	g_return_if_fail (CPG_IS_SELECTOR (selector));

	g_slist_foreach (selector->priv->from_set, (GFunc)g_object_unref, NULL);
	g_slist_free (selector->priv->from_set);

	selector->priv->from_set = copy_selections (selections);
}

void
cpg_selector_set_self (CpgSelector  *selector,
                       CpgSelection *selection)
{
	g_return_if_fail (CPG_IS_SELECTOR (selector));
	g_return_if_fail (selection == NULL || CPG_IS_SELECTION (selection));

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

