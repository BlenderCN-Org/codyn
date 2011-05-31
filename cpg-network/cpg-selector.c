#include "cpg-selector.h"
#include "cpg-parser-context.h"
#include "cpg-parser.h"
#include "cpg-expansion.h"
#include "cpg-selection.h"

#include <string.h>

#define CPG_SELECTOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_SELECTOR, CpgSelectorPrivate))

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
	gchar *as_string;
	gboolean onset;

	union
	{
		CpgEmbeddedString *identifier;
		CpgEmbeddedString *regex;

		struct
		{
			CpgSelectorPseudoType type;
			GSList *arguments;
		} pseudo;
	};
} Selector;

struct _CpgSelectorPrivate
{
	GSList *selectors;
	gboolean has_selected;
	GString *as_string;
};

G_DEFINE_TYPE (CpgSelector, cpg_selector, G_TYPE_OBJECT)

static Selector *
selector_new (SelectorType type)
{
	Selector *ret;

	ret = g_slice_new0 (Selector);
	ret->type = type;

	return ret;
}

static void
selector_free (Selector *selector)
{
	switch (selector->type)
	{
		case SELECTOR_TYPE_IDENTIFIER:
			g_object_unref (selector->identifier);
		break;
		case SELECTOR_TYPE_REGEX:
			g_object_unref (selector->regex);
		break;
		case SELECTOR_TYPE_PSEUDO:
			g_slist_foreach (selector->pseudo.arguments, (GFunc)g_object_unref, NULL);
			g_slist_free (selector->pseudo.arguments);
		break;
	}

	g_free (selector->as_string);

	g_slice_free (Selector, selector);
}

static Selector *
selector_identifier_new (CpgEmbeddedString *identifier,
                         gboolean           onset)
{
	Selector *selector;
	gchar const *r;

	selector = selector_new (SELECTOR_TYPE_IDENTIFIER);

	r = cpg_embedded_string_expand (identifier, NULL);

	selector->identifier = g_object_ref (identifier);
	selector->as_string = g_strdup_printf ("\"%s\"", r);
	selector->onset = onset;

	return selector;
}

static Selector *
selector_regex_new (CpgEmbeddedString *regex,
                    gboolean           onset)
{
	Selector *selector;
	gchar const *r;

	selector = selector_new (SELECTOR_TYPE_REGEX);

	r = cpg_embedded_string_expand (regex, NULL);

	selector->regex = g_object_ref (regex);
	selector->as_string = g_strdup_printf ("/%s/", r);
	selector->onset = onset;

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

	first = cpg_embedded_string_expand (arguments->data, context);

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

		second = cpg_embedded_string_expand (arguments->next->data, context);

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
		ret.max = (gint)g_ascii_strtoll (cpg_embedded_string_expand (maxpos->data, context), NULL, 10);
	}

	if (apos)
	{
		ret.a = (gint)g_ascii_strtoll (cpg_embedded_string_expand (apos->data, context), NULL, 10);
	}

	return ret;
}

static gchar const *
pseudo_name (CpgSelectorPseudoType type)
{
	switch (type)
	{
		case CPG_SELECTOR_PSEUDO_TYPE_ROOT:
			return "root";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_CHILDREN:
			return "children";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_PARENT:
			return "parent";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_FIRST_CHILD:
			return "first-child";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_LAST_CHILD:
			return "last-child";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_FIRST:
			return "first";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_LAST:
			return "last";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_SUBSET:
			return "subset";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_STATES:
			return "states";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_LINKS:
			return "links";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_SIBLINGS:
			return "siblings";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_TEMPLATES:
			return "templates";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_COUNT:
			return "count";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_FROM:
			return "from";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_TO:
			return "to";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_SELF:
			return "self";
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_DEBUG:
			return "debug";
		break;
	}

	g_assert_not_reached ();
}

static Selector *
selector_pseudo_new (CpgSelectorPseudoType  type,
                     GSList                *arguments)
{
	Selector *selector;
	GString *args;

	selector = selector_new (SELECTOR_TYPE_PSEUDO);
	selector->onset = TRUE;

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
			                        cpg_embedded_string_expand (arguments->data, NULL));
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

	G_OBJECT_CLASS (cpg_selector_parent_class)->finalize (object);
}

static void
cpg_selector_class_init (CpgSelectorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_selector_finalize;

	g_type_class_add_private (object_class, sizeof (CpgSelectorPrivate));
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

	cpg_parser_context_push_input (ctx, NULL, stream);
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
                        Selector    *sel)
{
	switch (sel->type)
	{
		case SELECTOR_TYPE_IDENTIFIER:
		case SELECTOR_TYPE_REGEX:
			if (selector->priv->as_string->len != 0)
			{
				g_string_append_c (selector->priv->as_string, sel->onset ? '|' : '.');
			}
		break;
		case SELECTOR_TYPE_PSEUDO:
			if (selector->priv->as_string->len != 0)
			{
				g_string_append_c (selector->priv->as_string, '|');
			}
		break;
	}

	g_string_append (selector->priv->as_string, sel->as_string);
}

static void
add_selector (CpgSelector *selector,
              Selector    *sel)
{
	add_selector_to_string (selector, sel);

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
}

void
cpg_selector_add (CpgSelector       *selector,
                  CpgEmbeddedString *identifier,
                  gboolean           onset)
{
	g_return_if_fail (CPG_IS_SELECTOR (selector));
	g_return_if_fail (CPG_IS_EMBEDDED_STRING (identifier));

	add_selector (selector, selector_identifier_new (identifier, onset));
}

void
cpg_selector_add_pseudo (CpgSelector           *selector,
                         CpgSelectorPseudoType  type,
                         GSList                *arguments)
{
	g_return_if_fail (CPG_IS_SELECTOR (selector));

	add_selector (selector, selector_pseudo_new (type, arguments));
}

void
cpg_selector_add_regex (CpgSelector       *selector,
                        CpgEmbeddedString *regex,
                        gboolean           onset)
{

	g_return_if_fail (CPG_IS_SELECTOR (selector));
	g_return_if_fail (CPG_IS_EMBEDDED_STRING (regex));

	add_selector (selector, selector_regex_new (regex, onset));
}

static gboolean
check_type (gpointer        object,
            CpgSelectorType type)
{
	if ((type & CPG_SELECTOR_TYPE_STATE) &&
	         G_TYPE_FROM_INSTANCE (object) == CPG_TYPE_OBJECT)
	{
		return TRUE;
	}
	else if ((type & CPG_SELECTOR_TYPE_LINK) &&
	         CPG_IS_LINK (object))
	{
		return TRUE;
	}
	else if ((type & CPG_SELECTOR_TYPE_GROUP) &&
	         CPG_IS_GROUP (object))
	{
		return TRUE;
	}
	else if ((type & CPG_SELECTOR_TYPE_ACTION) &&
	         CPG_IS_LINK_ACTION (object))
	{
		return TRUE;
	}
	else if ((type & CPG_SELECTOR_TYPE_PROPERTY) &&
	         CPG_IS_PROPERTY (object))
	{
		return TRUE;
	}

	return FALSE;
}

static CpgSelection *
make_child_selection (CpgSelection *parent,
                      CpgExpansion *expansion,
                      gpointer      obj)
{
	GSList *expansions;
	CpgSelection *ret;

	expansions = g_slist_copy (cpg_selection_get_expansions (parent));
	expansions = g_slist_prepend (expansions, expansion);

	ret = cpg_selection_new (obj, expansions, cpg_selection_get_defines (parent));

	g_slist_free (expansions);

	return ret;
}

static GSList *
selector_select_identifier_object (Selector           *selector,
                                   CpgSelection       *parent,
                                   CpgSelectorType     type,
                                   CpgEmbeddedContext *context,
                                   CpgExpansion       *expansion,
                                   GSList             *ret)
{
	CpgObject *child;

	if (selector->onset)
	{
		gpointer obj;

		obj = cpg_selection_get_object (parent);

		if (g_strcmp0 (cpg_expansion_get (expansion, 0),
		               cpg_object_get_id (obj)) == 0 &&
		    check_type (obj, type))
		{
			ret = g_slist_prepend (ret,
			                       make_child_selection (parent,
			                                             expansion,
			                                             obj));
		}
	}
	else
	{
		if (!CPG_IS_GROUP (cpg_selection_get_object (parent)))
		{
			return ret;
		}

		child = cpg_group_get_child (CPG_GROUP (cpg_selection_get_object (parent)),
		                             cpg_expansion_get (expansion, 0));

		if (child && check_type (child, type))
		{
			ret = g_slist_prepend (ret,
			                       make_child_selection (parent,
			                                             expansion,
			                                             child));
		}
	}

	return ret;
}

static GSList *
selector_select_identifier_property (Selector           *selector,
                                     CpgSelection       *parent,
                                     CpgSelectorType     type,
                                     CpgEmbeddedContext *context,
                                     CpgExpansion       *expansion,
                                     GSList             *ret)
{
	CpgProperty *prop;

	if (selector->onset)
	{
		/* FIXME: make this work? */
		return ret;
	}

	prop = cpg_object_get_property (cpg_selection_get_object (parent),
	                                cpg_expansion_get (expansion, 0));

	if (prop)
	{
		ret = g_slist_prepend (ret,
		                       make_child_selection (parent,
		                                             expansion,
		                                             prop));
	}

	return ret;
}

static GSList *
selector_select_identifier_action (Selector           *selector,
                                   CpgSelection       *parent,
                                   CpgSelectorType     type,
                                   CpgEmbeddedContext *context,
                                   CpgExpansion       *expansion,
                                   GSList             *ret)
{
	GObject *obj;
	CpgLinkAction *action;

	if (selector->onset)
	{
		/* FIXME: make this work? */
		return ret;
	}

	obj = cpg_selection_get_object (parent);

	if (!CPG_IS_LINK (obj))
	{
		return ret;
	}

	action = cpg_link_get_action (CPG_LINK (obj),
	                              cpg_expansion_get (expansion, 0));

	if (action)
	{
		ret = g_slist_prepend (ret,
		                       make_child_selection (parent,
		                                             expansion,
		                                             action));
	}

	return ret;
}

static GSList *
selector_select_identifier (CpgSelector        *self,
                            Selector           *selector,
                            CpgSelection       *parent,
                            CpgSelectorType     type,
                            CpgEmbeddedContext *context)
{
	GSList *ret = NULL;
	GSList *exps;
	GSList *e;

	exps = cpg_embedded_string_expand_multiple (selector->identifier,
	                                            context);

	for (e = exps; e; e = g_slist_next (e))
	{
		if (type & CPG_SELECTOR_TYPE_OBJECT)
		{
			ret = selector_select_identifier_object (selector,
				                                 parent,
				                                 type,
				                                 context,
				                                 e->data,
				                                 ret);
		}

		if (type & CPG_SELECTOR_TYPE_PROPERTY)
		{
			ret = selector_select_identifier_property (selector,
				                                   parent,
				                                   type,
				                                   context,
				                                   e->data,
				                                   ret);
		}

		if (type & CPG_SELECTOR_TYPE_ACTION)
		{
			ret = selector_select_identifier_action (selector,
				                                 parent,
				                                 type,
				                                 context,
				                                 e->data,
				                                 ret);
		}
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
                  GMatchInfo  **info)
{
	if (g_regex_match (regex, s, 0, info))
	{
		gint startpos;
		gint endpos;

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

static GSList *
selector_select_regex_object (Selector           *selector,
                              CpgSelection       *parent,
                              CpgSelectorType     type,
                              CpgEmbeddedContext *context,
                              GSList             *ret)
{
	GSList const *children;
	GRegex *regex;

	regex = g_regex_new (cpg_embedded_string_expand (selector->regex, context),
	                     G_REGEX_CASELESS | G_REGEX_ANCHORED,
	                     G_REGEX_MATCH_ANCHORED | G_REGEX_MATCH_NOTEMPTY,
	                     NULL);

	if (!regex)
	{
		return NULL;
	}

	if (selector->onset)
	{
		CpgObject *obj;
		GMatchInfo *info;

		obj = cpg_selection_get_object (parent);

		if (check_type (obj, type) &&
		    regex_match_full (regex,
		                      cpg_object_get_id (obj),
		                      &info))
		{
			CpgSelection *childsel;

			childsel = make_child_selection_from_match (parent,
			                                            info,
			                                            obj);

			ret = g_slist_prepend (ret, childsel);
			g_match_info_free (info);
		}
	}
	else
	{
		if (!CPG_IS_GROUP (cpg_selection_get_object (parent)))
		{
			return NULL;
		}

		children = cpg_group_get_children (CPG_GROUP (cpg_selection_get_object (parent)));

		while (children)
		{
			CpgObject *child;
			GMatchInfo *info;

			child = children->data;

			if (check_type (child, type) &&
			    regex_match_full (regex,
			                      cpg_object_get_id (child),
			                      &info))
			{
				CpgSelection *childsel;

				childsel = make_child_selection_from_match (parent,
				                                            info,
				                                            child);

				ret = g_slist_prepend (ret, childsel);

				g_match_info_free (info);
			}

			children = g_slist_next (children);
		}
	}

	g_regex_unref (regex);
	return ret;
}

static GSList *
selector_select_regex_property (Selector           *selector,
                                CpgSelection       *parent,
                                CpgSelectorType     type,
                                CpgEmbeddedContext *context,
                                GSList             *ret)
{
	GSList *props;
	GSList *item;
	GRegex *regex;

	if (selector->onset)
	{
		/* FIXME: implement this? */
		return NULL;
	}

	regex = g_regex_new (cpg_embedded_string_expand (selector->regex, context),
	                     G_REGEX_CASELESS | G_REGEX_ANCHORED,
	                     G_REGEX_MATCH_ANCHORED | G_REGEX_MATCH_NOTEMPTY,
	                     NULL);

	if (!regex)
	{
		return NULL;
	}

	props = cpg_object_get_properties (cpg_selection_get_object (parent));

	for (item = props; item; item = g_slist_next (item))
	{
		GMatchInfo *info;
		CpgSelection *childsel;

		if (!regex_match_full (regex,
		                      cpg_property_get_name (item->data),
		                      &info))
		{
			continue;
		}

		childsel = make_child_selection_from_match (parent,
		                                            info,
		                                            item->data);

		ret = g_slist_prepend (ret, childsel);

		g_match_info_free (info);
	}

	g_regex_unref (regex);
	g_slist_free (props);
	return ret;
}

static GSList *
selector_select_regex_action (Selector           *selector,
                              CpgSelection       *parent,
                              CpgSelectorType     type,
                              CpgEmbeddedContext *context,
                              GSList             *ret)
{
	GSList const *actions;
	GRegex *regex;

	if (selector->onset)
	{
		/* FIXME: implement this? */
		return NULL;
	}

	if (!CPG_IS_LINK (cpg_selection_get_object (parent)))
	{
		return NULL;
	}

	regex = g_regex_new (cpg_embedded_string_expand (selector->regex, context),
	                     G_REGEX_CASELESS | G_REGEX_ANCHORED,
	                     G_REGEX_MATCH_ANCHORED | G_REGEX_MATCH_NOTEMPTY,
	                     NULL);

	if (!regex)
	{
		return NULL;
	}

	actions = cpg_link_get_actions (cpg_selection_get_object (parent));

	while (actions)
	{
		GMatchInfo *info;

		if (regex_match_full (regex,
		                      cpg_link_action_get_target (actions->data),
		                      &info))
		{
			CpgSelection *childsel;

			childsel = make_child_selection_from_match (parent,
			                                            info,
			                                            actions->data);
			ret = g_slist_prepend (ret, childsel);

			g_match_info_free (info);
		}

		actions = g_slist_next (actions);
	}

	g_regex_unref (regex);

	return ret;
}

static GSList *
selector_select_regex (CpgSelector        *self,
                       Selector           *selector,
                       CpgSelection       *parent,
                       CpgSelectorType     type,
                       CpgEmbeddedContext *context)
{
	GSList *ret = NULL;

	if (type & CPG_SELECTOR_TYPE_OBJECT)
	{
		ret = selector_select_regex_object (selector,
		                                    parent,
		                                    type,
		                                    context,
		                                    ret);
	}

	if (type & CPG_SELECTOR_TYPE_PROPERTY)
	{
		ret = selector_select_regex_property (selector,
		                                      parent,
		                                      type,
		                                      context,
		                                      ret);
	}

	if (type & CPG_SELECTOR_TYPE_ACTION)
	{
		ret = selector_select_regex_action (selector,
		                                    parent,
		                                    type,
		                                    context,
		                                    ret);
	}

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
                    gint             offset,
                    CpgSelectorType  type)
{
	GSList *ret = NULL;
	gint i = 1 - offset;
	gint num = 0;

	while (children && (nth->max < 0 || num < nth->max))
	{
		CpgSelection *sel = children->data;
		gboolean correcttype = check_type (cpg_selection_get_object (sel),
		                                   type);

		if (correcttype)
		{
			if (nth_match (nth, i))
			{
				ret = g_slist_prepend (ret,
				                       cpg_selection_copy (sel));
				++num;
			}

			++i;
		}

		children = g_slist_next (children);
	}

	return ret;
}

static GSList *
filter_list_reverse (GSList    *objs,
                     gboolean   states)
{
	GSList *ret = NULL;

	while (objs)
	{
		CpgSelection *sel;

		sel = objs->data;

		if (states == !CPG_IS_LINK (cpg_selection_get_object (sel)))
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

static CpgObject *
top_parent (CpgObject *object)
{
	while (TRUE)
	{
		CpgObject *parent;

		parent = cpg_object_get_parent (object);

		if (parent == NULL)
		{
			return object;
		}

		object = parent;
	}
}

static GSList *
expand_children (CpgSelection *selection,
                 GSList const *children)
{
	GSList *ret = NULL;

	while (children)
	{
		ret = g_slist_prepend (ret,
		                       cpg_selection_new (children->data,
		                                          cpg_selection_get_expansions (selection),
		                                          cpg_selection_get_defines (selection)));

		children = g_slist_next (children);
	}

	return g_slist_reverse (ret);
}

static GSList *
count_selection (GSList          *selection,
                 CpgSelectorType  type)
{
	GSList *ret = NULL;
	GSList *item;
	gint i = 0;
	gchar *s;
	CpgExpansion *ex;

	while (selection)
	{
		if (check_type (cpg_selection_get_object (selection->data), type))
		{
			ret = g_slist_prepend (ret, selection->data);
			++i;
		}

		selection = g_slist_next (selection);
	}

	s = g_strdup_printf ("%d", i);
	ex = cpg_expansion_new_one (s);
	g_free (s);

	for (item = ret; item; item = g_slist_next (item))
	{
		CpgSelection *sel;
		GSList *expansions;
		CpgExpansion *expansion;

		expansions = g_slist_copy (cpg_selection_get_expansions (item->data));
		expansion = cpg_expansion_copy (ex);
		expansions = g_slist_append (expansions, expansion);

		sel = cpg_selection_new (cpg_selection_get_object (item->data),
		                         expansions,
		                         cpg_selection_get_defines (item->data));

		g_slist_free (expansions);
		g_object_unref (ex);

		ret->data = sel;
	}

	g_object_unref (ex);

	return g_slist_reverse (ret);
}

static GSList *
selector_pseudo_from_to (Selector           *selector,
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

		sub = cpg_selector_select (s,
		                           obj,
		                           CPG_SELECTOR_TYPE_OBJECT,
		                           context);

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

			childsel = cpg_selection_new (cpg_selection_get_object (sel),
			                              expansions,
			                              cpg_selection_get_defines (sel));

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
			g_string_append_c (ret, sel->onset ? '|' : '.');
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

		g_printerr (" <= %s\n", s);
		g_free (s);
	}

	g_printerr ("\n");

	return copy_selections (parent);
}

static GSList *
selector_select_pseudo (CpgSelector        *self,
                        Selector           *selector,
                        GSList             *parent,
                        CpgSelectorType     type,
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
				                                    0,
				                                    type));
		}
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_STATES:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     TRUE));
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_LINKS:
			return g_slist_reverse (filter_list_reverse (parent,
				                                     FALSE));
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_COUNT:
			return count_selection (parent, type);
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_SELF:
			return copy_selections (parent);
		break;
		case CPG_SELECTOR_PSEUDO_TYPE_DEBUG:
			return debug_selections (self, selector, parent, context);
		break;
		default:
		break;
	}

	for (item = parent; item; item = g_slist_next (item))
	{
		CpgSelection *sel = item->data;
		Nth nth = {0,};

		cpg_embedded_context_save (context);

		cpg_embedded_context_add_selection (context, sel);

		switch (selector->pseudo.type)
		{
			case CPG_SELECTOR_PSEUDO_TYPE_CHILDREN:
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
				if (check_type (cpg_selection_get_object (sel), type))
				{
					return g_slist_prepend (ret,
					                        cpg_selection_copy (sel));
				}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_LAST:
				if (check_type (cpg_selection_get_object (sel), type))
				{
					last = sel;
				}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_ROOT:
			{
				CpgObject *top;

				top = top_parent (cpg_selection_get_object (sel));

				if (top && check_type (top, type))
				{
					return g_slist_prepend (ret,
					                        cpg_selection_new (top,
					                                           cpg_selection_get_expansions (sel),
					                                           cpg_selection_get_defines (sel)));
				}
			}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_TEMPLATES:
			{
				CpgObject *top;

				top = top_parent (cpg_selection_get_object (sel));

				if (top && CPG_IS_NETWORK (top))
				{
					CpgGroup *template_group;

					template_group = cpg_network_get_template_group (CPG_NETWORK (top));

					return g_slist_prepend (ret,
					                        cpg_selection_new (template_group,
					                                           cpg_selection_get_expansions (sel),
					                                           cpg_selection_get_defines (sel)));
				}
			}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_CHILDREN:
			{
				if (CPG_IS_GROUP (cpg_selection_get_object (sel)))
				{
					GSList *children;

					children = expand_children (sel,
					                            cpg_group_get_children (CPG_GROUP (cpg_selection_get_object (sel))));

					ret = g_slist_concat (select_nth_reverse (children,
					                                          &nth,
					                                          0,
					                                          type),
					                     ret);

					g_slist_foreach (children, (GFunc)g_object_unref, NULL);
					g_slist_free (children);
				}
			}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_SIBLINGS:
			{
				CpgObject *parent;

				parent = cpg_object_get_parent (cpg_selection_get_object (sel));

				if (parent)
				{
					GSList const *realchildren;
					GSList *children;
					gint idx;

					realchildren = cpg_group_get_children (CPG_GROUP (parent));
					idx = g_slist_index ((GSList *)realchildren, cpg_selection_get_object (sel));

					children = expand_children (sel,
					                            realchildren);

					ret = g_slist_concat (select_nth_reverse (children,
					                                          &nth,
					                                          idx + 1,
					                                          type),
					                      ret);

					g_slist_foreach (children, (GFunc)g_object_unref, NULL);
					g_slist_free (children);
				}
			}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_PARENT:
			{
				CpgObject *parent = cpg_object_get_parent (cpg_selection_get_object (sel));

				if (parent && check_type (parent, type))
				{
					ret = g_slist_prepend (ret,
					                       cpg_selection_new (parent,
					                                          cpg_selection_get_expansions (sel),
					                                          cpg_selection_get_defines (sel)));
				}
			}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_FIRST_CHILD:
			{
				if (CPG_IS_GROUP (cpg_selection_get_object (sel)))
				{
					GSList const *children;

					children = cpg_group_get_children (CPG_GROUP (cpg_selection_get_object (sel)));

					while (children)
					{
						if (check_type (children->data, type))
						{
							ret = g_slist_prepend (ret,
							                       cpg_selection_new (children->data,
							                                          cpg_selection_get_expansions (sel),
							                                          cpg_selection_get_defines (sel)));

							break;
						}

						children = g_slist_next (children);
					}
				}
			}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_LAST_CHILD:
			{
				if (CPG_IS_GROUP (cpg_selection_get_object (sel)))
				{
					GSList const *children;
					CpgObject *thelast = NULL;

					children = cpg_group_get_children (CPG_GROUP (cpg_selection_get_object (sel)));

					while (children)
					{
						if (check_type (children->data, type))
						{
							thelast = children->data;
						}

						children = g_slist_next (children);
					}

					if (thelast)
					{
						ret = g_slist_prepend (ret,
						                       cpg_selection_new (thelast,
						                                          cpg_selection_get_expansions (sel),
						                                          cpg_selection_get_defines (sel)));
					}
				}
			}
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_FROM:
				ret = selector_pseudo_from_to (selector,
				                               "from",
				                               ret,
				                               sel,
				                               context);
			break;
			case CPG_SELECTOR_PSEUDO_TYPE_TO:
				ret = selector_pseudo_from_to (selector,
				                               "to",
				                               ret,
				                               sel,
				                               context);
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
                 CpgSelectorType     type,
                 CpgEmbeddedContext *context)
{
	GSList *ret = NULL;
	GSList *item;

	if (selector->type == SELECTOR_TYPE_PSEUDO)
	{
		if (type & CPG_SELECTOR_TYPE_OBJECT)
		{
			ret = selector_select_pseudo (self,
			                              selector,
			                              parent,
			                              type,
			                              context);
		}
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
					                                type,
					                                context);
				break;
				case SELECTOR_TYPE_REGEX:
					r = selector_select_regex (self,
					                           selector,
					                           sel,
					                           type,
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

static GSList *
selector_select_all (CpgSelector        *selector,
                     CpgObject          *parent,
                     CpgSelectorType     type,
                     CpgEmbeddedContext *context)
{
	GSList *item;
	GSList *ctx = NULL;
	GHashTable *defines;

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

	ctx = g_slist_prepend (NULL,
	                       cpg_selection_new (parent, NULL, defines));

	for (item = selector->priv->selectors; item; item = g_slist_next (item))
	{
		GSList *tmp;

		tmp = ctx;
		ctx = selector_select (selector,
		                       item->data,
		                       ctx,
		                       item->next ? CPG_SELECTOR_TYPE_OBJECT : type,
		                       context);

		annotate_expansions (ctx);

		g_slist_foreach (tmp, (GFunc)g_object_unref, NULL);
		g_slist_free (tmp);

		if (ctx == NULL)
		{
			break;
		}
	}

	g_object_unref (context);

	return ctx;
}

GSList *
cpg_selector_select (CpgSelector        *selector,
                     CpgObject          *parent,
                     CpgSelectorType     type,
                     CpgEmbeddedContext *context)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (parent), NULL);
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
cpg_selector_set_first_onset (CpgSelector *selector,
                              gboolean     onset)
{
	Selector *sel;

	g_return_if_fail (CPG_IS_SELECTOR (selector));

	if (!selector->priv->selectors)
	{
		return;
	}

	sel = selector->priv->selectors->data;
	sel->onset = onset;
}

