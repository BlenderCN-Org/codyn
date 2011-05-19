#include "cpg-selector.h"
#include "cpg-parser-context.h"
#include "cpg-parser.h"

#include <string.h>

#define CPG_SELECTOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_SELECTOR, CpgSelectorPrivate))

struct _CpgExpansion
{
	GPtrArray *expansions;
};

struct _CpgSelection
{
	gpointer  object;
	GSList   *expansions;
};

typedef enum
{
	TYPE_ALL,
	TYPE_LINK,
	TYPE_PROPERTY,
	TYPE_STATE
} SelectType;

typedef enum
{
	SELECTOR_TYPE_IDENTIFIER,
	SELECTOR_TYPE_REGEX,
	SELECTOR_TYPE_PSEUDO
} SelectorType;

typedef enum
{
	SELECTOR_PSEUDO_TYPE_ROOT,
	SELECTOR_PSEUDO_TYPE_FROM,
	SELECTOR_PSEUDO_TYPE_NTH_CHILD,
	SELECTOR_PSEUDO_TYPE_PARENT,
	SELECTOR_PSEUDO_TYPE_FIRST_CHILD,
	SELECTOR_PSEUDO_TYPE_LAST_CHILD,
	SELECTOR_PSEUDO_TYPE_FIRST,
	SELECTOR_PSEUDO_TYPE_LAST,
	SELECTOR_PSEUDO_TYPE_NTH,
	SELECTOR_PSEUDO_TYPE_STATES,
	SELECTOR_PSEUDO_TYPE_LINKS,
	SELECTOR_PSEUDO_TYPE_NTH_SIBLING,
	SELECTOR_PSEUDO_TYPE_CHILDREN,
	SELECTOR_PSEUDO_TYPE_TEMPLATES,
	NUM_PSEUDO_SELECTORS
} SelectorPseudoType;

typedef struct
{
	SelectorPseudoType type;
	gchar const *name;
	gboolean has_argument;
} SelectorPseudoDefinition;

typedef struct
{
	gint a;
	gint b;
	gint max;
} Nth;

typedef struct
{
	SelectorPseudoDefinition const *definition;
	gchar **arguments;
	Nth nth;
} SelectorPseudo;

static SelectorPseudoDefinition const pseudo_selectors[] =
{
	{SELECTOR_PSEUDO_TYPE_ROOT, "root", FALSE},
	{SELECTOR_PSEUDO_TYPE_FROM, "from", FALSE},
	{SELECTOR_PSEUDO_TYPE_NTH_CHILD, "nth-child", TRUE},
	{SELECTOR_PSEUDO_TYPE_PARENT, "parent", FALSE},
	{SELECTOR_PSEUDO_TYPE_FIRST_CHILD, "first-child", FALSE},
	{SELECTOR_PSEUDO_TYPE_LAST_CHILD, "last-child", FALSE},
	{SELECTOR_PSEUDO_TYPE_FIRST, "first", FALSE},
	{SELECTOR_PSEUDO_TYPE_LAST, "last", FALSE},
	{SELECTOR_PSEUDO_TYPE_NTH, "nth", TRUE},
	{SELECTOR_PSEUDO_TYPE_STATES, "states", FALSE},
	{SELECTOR_PSEUDO_TYPE_LINKS, "links", FALSE},
	{SELECTOR_PSEUDO_TYPE_NTH_SIBLING, "nth-sibling", TRUE},
	{SELECTOR_PSEUDO_TYPE_CHILDREN, "children", FALSE},
	{SELECTOR_PSEUDO_TYPE_TEMPLATES, "templates", FALSE}
};

typedef struct
{
	SelectorType type;
	gchar *as_string;

	union
	{
		gchar *identifier;
		GRegex *regex;
		SelectorPseudo pseudo;
	};
} Selector;

struct _CpgSelectorPrivate
{
	GSList *selectors;
	gboolean has_selected;
	GString *as_string;
};

G_DEFINE_TYPE (CpgSelector, cpg_selector, G_TYPE_OBJECT)

static GSList *
copy_expansions (GSList *list)
{
	GSList *ret = NULL;

	while (list)
	{
		ret = g_slist_prepend (ret,
		                       cpg_expansion_copy (list->data));

		list = g_slist_next (list);
	}

	return g_slist_reverse (ret);
}

CpgSelection *
cpg_selection_new (gpointer  object,
                   GSList   *expansions)
{
	CpgSelection *ret;

	ret = g_slice_new0 (CpgSelection);

	ret->object = g_object_ref (object);
	ret->expansions = copy_expansions (expansions);

	return ret;
}

CpgSelection *
cpg_selection_copy (CpgSelection *selection)
{
	return cpg_selection_new (selection->object, selection->expansions);
}

void
cpg_selection_free (CpgSelection *selection)
{
	g_object_unref (selection->object);

	g_slist_foreach (selection->expansions, (GFunc)cpg_expansion_free, NULL);
	g_slist_free (selection->expansions);

	g_slice_free (CpgSelection, selection);
}

CpgObject *
cpg_selection_get_object (CpgSelection *selection)
{
	g_return_val_if_fail (CPG_IS_OBJECT (selection->object), NULL);

	return CPG_OBJECT (selection->object);
}

CpgProperty *
cpg_selection_get_property (CpgSelection *selection)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (selection->object), NULL);

	return CPG_PROPERTY (selection->object);
}

GSList *
cpg_selection_get_expansions (CpgSelection *selection)
{
	return selection->expansions;
}

static void
selector_pseudo_free (SelectorPseudo *selector)
{
	if (selector->arguments)
	{
		g_strfreev (selector->arguments);
	}
}

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
			g_free (selector->identifier);
		break;
		case SELECTOR_TYPE_REGEX:
			g_regex_unref (selector->regex);
		break;
		case SELECTOR_TYPE_PSEUDO:
			selector_pseudo_free (&(selector->pseudo));
		break;
	}

	g_free (selector->as_string);

	g_slice_free (Selector, selector);
}

static Selector *
selector_identifier_new (gchar const *identifier)
{
	Selector *selector;

	selector = selector_new (SELECTOR_TYPE_IDENTIFIER);
	selector->identifier = g_strdup (identifier);
	selector->as_string = g_strdup_printf ("\"%s\"", identifier);

	return selector;
}

static Selector *
selector_regex_new (gchar const *regex)
{
	Selector *selector;

	selector = selector_new (SELECTOR_TYPE_REGEX);
	selector->as_string = g_strdup (regex);
	selector->regex = g_regex_new (regex,
	                               G_REGEX_CASELESS |
	                               G_REGEX_ANCHORED,
	                               G_REGEX_MATCH_ANCHORED |
	                               G_REGEX_MATCH_NOTEMPTY,
	                               NULL);

	return selector;
}

static Nth
parse_nth (gchar const * const *arguments)
{
	Nth ret = {0, 0, -1};
	gint maxpos = 2;

	if (!arguments)
	{
		return ret;
	}

	if (g_strcmp0 (arguments[0], "odd"))
	{
		ret.a = 2;
		ret.b = 1;

		maxpos = 1;
	}
	else if (g_strcmp0 (arguments[0], "even"))
	{
		ret.a = 2;
		ret.b = 0;

		maxpos = 1;
	}
	else if (arguments[1])
	{
		ret.a = (gint)g_ascii_strtoll (arguments[0], NULL, 10);
		ret.b = (gint)g_ascii_strtoll (arguments[1], NULL, 10);

		maxpos = 2;
	}
	else
	{
		ret.a = (gint)g_ascii_strtoll (arguments[0], NULL, 10);
		maxpos = 1;
	}

	if (arguments[maxpos])
	{
		ret.max = (gint)g_ascii_strtoll (arguments[maxpos], NULL, 10);
	}

	return ret;
}

static gchar **
strcopyv (gchar const * const *s)
{
	GPtrArray *ptr;

	if (!s)
	{
		return NULL;
	}

	ptr = g_ptr_array_new ();

	while (*s)
	{
		g_ptr_array_add (ptr, g_strdup (*s));
		++s;
	}

	g_ptr_array_add (ptr, NULL);

	return (gchar **)g_ptr_array_free (ptr, FALSE);
}

static Selector *
selector_pseudo_new (SelectorPseudoDefinition const *definition,
                     gchar const * const            *arguments)
{
	Selector *selector;
	GString *args;

	selector = selector_new (SELECTOR_TYPE_PSEUDO);
	selector->pseudo.definition = definition;

	if (definition->has_argument)
	{
		switch (definition->type)
		{
			case SELECTOR_PSEUDO_TYPE_NTH:
			case SELECTOR_PSEUDO_TYPE_NTH_CHILD:
			case SELECTOR_PSEUDO_TYPE_NTH_SIBLING:
				selector->pseudo.nth = parse_nth (arguments);
			break;
			default:
				g_assert_not_reached ();
			break;
		}
	}

	selector->pseudo.arguments = strcopyv (arguments);

	args = g_string_new ("");

	while (arguments && *arguments)
	{
		if (args->len != 0)
		{
			g_string_append (args, ", ");
		}

		g_string_append_printf (args, "\"%s\"", *arguments);

		++arguments;
	}

	selector->as_string = g_strdup_printf ("%s%s%s%s",
	                                       definition->name,
	                                       args->len ? "(" : "",
	                                       args->str,
	                                       args->len ? "(" : "");

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
				g_string_append_c (selector->priv->as_string, '.');
			}
		break;
		case SELECTOR_TYPE_PSEUDO:
			g_string_append_c (selector->priv->as_string, ':');
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
cpg_selector_add (CpgSelector *selector,
                  gchar const *identifier)
{
	g_return_if_fail (CPG_IS_SELECTOR (selector));
	g_return_if_fail (identifier != NULL);

	add_selector (selector, selector_identifier_new (identifier));
}

void
cpg_selector_add_pseudo (CpgSelector *selector,
                         gchar const *pseudo,
                         gchar const * const *arguments)
{
	gint i;

	g_return_if_fail (CPG_IS_SELECTOR (selector));
	g_return_if_fail (pseudo != NULL);
	g_return_if_fail (arguments != NULL);

	for (i = 0; i < NUM_PSEUDO_SELECTORS; ++i)
	{
		SelectorPseudoDefinition const *def;

		def = &pseudo_selectors[i];

		if (g_strcmp0 (def->name, pseudo) == 0)
		{
			add_selector (selector,
			              selector_pseudo_new (def, arguments));
			break;
		}
	}
}

void
cpg_selector_add_regex (CpgSelector *selector,
                        gchar const *regex)
{

	g_return_if_fail (CPG_IS_SELECTOR (selector));
	g_return_if_fail (regex != NULL);

	add_selector (selector, selector_regex_new (regex));
}

static gboolean
check_type (CpgObject  *object,
            SelectType  type)
{
	switch (type)
	{
		case TYPE_ALL:
			return TRUE;
		break;
		case TYPE_LINK:
			return CPG_IS_LINK (object);
		break;
		case TYPE_PROPERTY:
			return FALSE;
		break;
		case TYPE_STATE:
			return !CPG_IS_LINK (object);
		break;
	}

	return FALSE;
}

static GSList *
selector_select_identifier (Selector   *selector,
                            CpgSelection  *parent,
                            SelectType  type)
{
	GSList *ret = NULL;

	if (type != TYPE_PROPERTY)
	{
		CpgObject *child;

		if (!CPG_IS_GROUP (parent->object))
		{
			return NULL;
		}

		child = cpg_group_get_child (CPG_GROUP (parent->object),
		                             selector->identifier);

		if (child && check_type (child, type))
		{
			ret = g_slist_prepend (NULL,
			                       cpg_selection_new (child, parent->expansions));
		}
	}
	else
	{
		CpgProperty *prop;

		prop = cpg_object_get_property (parent->object,
		                                selector->identifier);

		if (prop)
		{
			ret = g_slist_prepend (NULL,
			                       cpg_selection_new (prop, parent->expansions));
		}
	}

	return g_slist_reverse (ret);
}

static CpgExpansion *
expansion_from_match (GMatchInfo *info)
{
	gchar **s;
	CpgExpansion *ret;

	s = g_match_info_fetch_all (info);
	ret = cpg_expansion_new ((gchar const * const *)s);
	g_strfreev (s);

	return ret;
}

static GSList *
selector_select_regex (Selector    *selector,
                       CpgSelection   *parent,
                       SelectType   type)
{
	GSList *ret = NULL;

	if (type != TYPE_PROPERTY)
	{
		GSList const *children;

		if (!CPG_IS_GROUP (parent->object))
		{
			return NULL;
		}

		children = cpg_group_get_children (CPG_GROUP (parent->object));

		while (children)
		{
			CpgObject *child;
			GMatchInfo *info;

			child = children->data;

			if (check_type (child, type) &&
			    g_regex_match (selector->regex,
			                   cpg_object_get_id (child),
			                   0,
			                   &info))
			{
				CpgSelection *childsel;

				childsel = cpg_selection_new (child, parent->expansions);
				
				childsel->expansions =
					g_slist_append (childsel->expansions,
					                expansion_from_match (info));

				ret = g_slist_prepend (ret, childsel);

				g_match_info_free (info);
			}

			children = g_slist_next (children);
		}
	}
	else
	{
		GSList *props;
		GSList *item;

		props = cpg_object_get_properties (parent->object);

		for (item = props; item; item = g_slist_next (item))
		{
			GMatchInfo *info;

			if (g_regex_match (selector->regex,
			                   cpg_property_get_name (item->data),
			                   0,
			                   &info))
			{
				CpgSelection *childsel;

				childsel = cpg_selection_new (item->data, parent->expansions);
				childsel->expansions =
					g_slist_append (childsel->expansions,
					                expansion_from_match (info));

				ret = g_slist_prepend (ret, childsel);

				g_match_info_free (info);
			}
		}

		g_slist_free (props);
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
select_nth_reverse (GSList const *children,
                    Nth const    *nth,
                    gint          offset,
                    SelectType    type)
{
	GSList *ret = NULL;
	gint i = 1 - offset;
	gint num = 0;

	while (children && (nth->max < 0 || num < nth->max))
	{
		CpgSelection *sel = children->data;
		gboolean correcttype = check_type (sel->object, type);

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

		if (states == !CPG_IS_LINK (sel->object))
		{
			ret = g_slist_prepend (ret,
			                       cpg_selection_copy (sel));
		}

		objs = g_slist_next (objs);
	}

	return ret;
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
		                                      selection->expansions));

		children = g_slist_next (children);
	}

	return g_slist_reverse (ret);
}

static GSList *
selector_select_pseudo (Selector    *selector,
                        GSList      *parent,
                        CpgObject   *from,
                        SelectType   type)
{
	GSList *ret = NULL;
	GSList *item;
	CpgSelection *last = NULL;

	switch (selector->pseudo.definition->type)
	{
		case SELECTOR_PSEUDO_TYPE_FROM:
			if (from != NULL && check_type (from, type))
			{
				return g_slist_prepend (NULL,
				                        cpg_selection_new (from, NULL));
			}
			else
			{
				return NULL;
			}
		break;
		case SELECTOR_PSEUDO_TYPE_NTH:
			return g_slist_reverse (select_nth_reverse (parent,
			                                            &(selector->pseudo.nth),
			                                            0,
			                                            type));
		break;
		case SELECTOR_PSEUDO_TYPE_STATES:
			return g_slist_reverse (filter_list_reverse (parent,
			                                             TRUE));
		break;
		case SELECTOR_PSEUDO_TYPE_LINKS:
			return g_slist_reverse (filter_list_reverse (parent,
			                                             FALSE));
		break;
		default:
		break;
	}

	for (item = parent; item; item = g_slist_next (item))
	{
		CpgSelection *sel = item->data;

		switch (selector->pseudo.definition->type)
		{
			case SELECTOR_PSEUDO_TYPE_FIRST:
				if (check_type (sel->object, type))
				{
					return g_slist_prepend (ret,
					                        cpg_selection_copy (sel));
				}
			break;
			case SELECTOR_PSEUDO_TYPE_LAST:
				if (check_type (sel->object, type))
				{
					last = sel;
				}
			break;
			case SELECTOR_PSEUDO_TYPE_ROOT:
			{
				CpgObject *top;

				top = top_parent (sel->object);

				if (top && check_type (top, type))
				{
					return g_slist_prepend (ret,
					                        cpg_selection_new (top, sel->expansions));
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_TEMPLATES:
			{
				CpgObject *top;

				top = top_parent (sel->object);

				if (top && CPG_IS_NETWORK (top))
				{
					CpgGroup *template_group;

					template_group = cpg_network_get_template_group (CPG_NETWORK (top));

					return g_slist_prepend (ret,
					                        cpg_selection_new (template_group,
					                                       sel->expansions));
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_NTH_CHILD:
			{
				if (CPG_IS_GROUP (sel->object))
				{
					GSList *children;

					children = expand_children (sel,
					                            cpg_group_get_children (CPG_GROUP (sel->object)));

					ret = g_slist_concat (select_nth_reverse (children,
					                                          &(selector->pseudo.nth),
					                                          0,
					                                          type),
					                     ret);

					g_slist_foreach (children, (GFunc)cpg_selection_free, NULL);
					g_slist_free (children);
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_NTH_SIBLING:
			{
				CpgObject *parent;

				parent = cpg_object_get_parent (sel->object);

				if (parent)
				{
					GSList const *realchildren;
					GSList *children;
					gint idx;

					realchildren = cpg_group_get_children (CPG_GROUP (parent));
					idx = g_slist_index ((GSList *)realchildren, sel->object);

					children = expand_children (sel,
					                            realchildren);

					ret = g_slist_concat (select_nth_reverse (children,
					                                          &(selector->pseudo.nth),
					                                          idx,
					                                          type == TYPE_ALL ? (CPG_IS_LINK (sel->object) ? TYPE_LINK : TYPE_STATE) : type),
					                      ret);

					g_slist_foreach (children, (GFunc)cpg_selection_free, NULL);
					g_slist_free (children);
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_PARENT:
			{
				CpgObject *parent = cpg_object_get_parent (sel->object);

				if (parent && check_type (parent, type))
				{
					ret = g_slist_prepend (ret,
					                       cpg_selection_new (parent,
					                                      sel->expansions));
				}
			}
			case SELECTOR_PSEUDO_TYPE_CHILDREN:
			{
				if (CPG_IS_GROUP (sel->object))
				{
					GSList const *children;
					GSList *copy = NULL;

					children = cpg_group_get_children (CPG_GROUP (sel->object));

					while (children)
					{
						copy = g_slist_prepend (copy,
						                        cpg_selection_new (children->data,
						                                       sel->expansions));

						children = g_slist_next (children);
					}

					ret = g_slist_concat (copy, ret);
				}
			}
			break;
			break;
			case SELECTOR_PSEUDO_TYPE_FIRST_CHILD:
			{
				if (CPG_IS_GROUP (sel->object))
				{
					GSList const *children;

					children = cpg_group_get_children (CPG_GROUP (sel->object));

					while (children)
					{
						if (check_type (children->data, type))
						{
							ret = g_slist_prepend (ret,
							                       cpg_selection_new (children->data,
							                                      sel->expansions));

							break;
						}

						children = g_slist_next (children);
					}
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_LAST_CHILD:
			{
				if (CPG_IS_GROUP (sel->object))
				{
					GSList const *children;
					CpgObject *thelast = NULL;

					children = cpg_group_get_children (CPG_GROUP (sel->object));

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
						                                      sel->expansions));
					}
				}
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
		                       cpg_selection_copy (last));
	}

	return g_slist_reverse (ret);
}

static GSList *
selector_select (Selector    *selector,
                 GSList      *parent,
                 CpgObject   *from,
                 SelectType   type)
{
	GSList *ret = NULL;
	GSList *item;

	if (selector->type == SELECTOR_TYPE_PSEUDO)
	{
		if (type != TYPE_PROPERTY)
		{
			ret = selector_select_pseudo (selector,
			                              parent,
			                              from,
			                              type);
		}
	}
	else
	{
		for (item = parent; item; item = g_slist_next (item))
		{
			CpgSelection *sel = item->data;
			GSList *r = NULL;

			switch (selector->type)
			{
				case SELECTOR_TYPE_IDENTIFIER:
					r = selector_select_identifier (selector,
					                                sel,
					                                type);
				break;
				case SELECTOR_TYPE_REGEX:
					r = selector_select_regex (selector,
					                           sel,
					                           type);
				break;
				default:
					g_assert_not_reached ();
				break;
			}

			ret = g_slist_concat (ret, r);
		}
	}

	return ret;
}

static GSList *
selector_select_all (CpgSelector  *selector,
                     CpgObject    *parent,
                     CpgObject    *from,
                     SelectType    type)
{
	GSList *item;
	GSList *ctx = NULL;

	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (parent), NULL);
	g_return_val_if_fail (from == NULL || CPG_IS_OBJECT (from), NULL);

	if (!selector->priv->has_selected)
	{
		selector->priv->selectors =
			g_slist_reverse (selector->priv->selectors);

		selector->priv->has_selected = TRUE;
	}

	if (!selector->priv->selectors)
	{
		return NULL;
	}

	ctx = g_slist_prepend (NULL,
	                       cpg_selection_new (parent, NULL));

	for (item = selector->priv->selectors; item; item = g_slist_next (item))
	{
		GSList *tmp;

		tmp = ctx;
		ctx = selector_select (item->data,
		                       ctx,
		                       from,
		                       item->next ? TYPE_ALL : type);

		g_slist_foreach (tmp, (GFunc)cpg_selection_free, NULL);
		g_slist_free (tmp);

		if (ctx == NULL)
		{
			break;
		}
	}

	return ctx;
}

GSList *
cpg_selector_select (CpgSelector  *selector,
                     CpgObject    *parent)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (parent), NULL);

	return selector_select_all (selector, parent, NULL, TYPE_ALL);
}

GSList *
cpg_selector_select_states (CpgSelector  *selector,
                            CpgObject    *parent)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);

	return selector_select_all (selector, parent, NULL, TYPE_STATE);
}

GSList *
cpg_selector_select_links (CpgSelector  *selector,
                           CpgObject    *parent)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);

	return selector_select_all (selector, parent, NULL, TYPE_LINK);
}

GSList *
cpg_selector_select_link_to (CpgSelector  *selector,
                             CpgObject    *parent,
                             CpgObject    *from)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (from), NULL);

	return selector_select_all (selector, parent, from, TYPE_STATE);
}

GSList *
cpg_selector_select_properties (CpgSelector  *selector,
                                CpgObject    *parent)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);

	return selector_select_all (selector, parent, NULL, TYPE_PROPERTY);
}

gchar const *
cpg_selector_as_string (CpgSelector *selector)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);

	return selector->priv->as_string->str;
}

typedef struct
{
	CpgSelector *selector;
	GRegex *regex;
	CpgSelectorExpandFunc func;
	gpointer userdata;
} ExpandInfo;

static gchar *
expand_expansion (CpgSelector      *selector,
                  GMatchInfo const *info,
                  GSList           *expansions)
{
	gchar *parent;
	gchar *idx;
	gchar const *num;
	CpgExpansion *ex;
	gchar *ret = NULL;

	parent = g_match_info_fetch_named (info, "parent");
	idx = g_match_info_fetch_named (info, "index");

	if (idx && *idx)
	{
		ex = g_slist_nth_data (expansions,
		                       (gint)g_ascii_strtoll (parent, NULL, 10));

		num = idx;
	}
	else
	{
		ex = expansions ? expansions->data : NULL;
		num = parent;
	}

	if (ex)
	{
		gint idd = (gint)g_ascii_strtoll (num, NULL, 10);
		gchar const *ss;

		ss = cpg_expansion_get (ex, idd);

		if (ss)
		{
			ret = g_strdup (ss);
		}
	}

	g_free (parent);
	g_free (idx);

	return ret;
}

static gboolean
expand_string_eval (GMatchInfo const *info,
                    GString          *result,
                    ExpandInfo       *exinfo)
{
	gchar *ret;

	if (!exinfo->func)
	{
		return TRUE;
	}

	ret = exinfo->func (exinfo->selector, info, exinfo->userdata);

	if (ret)
	{
		g_string_append (result, ret);
	}

	g_free (ret);

	return TRUE;
}

static gchar *
expand_string (ExpandInfo  *info,
               gchar const *s)
{
	return g_regex_replace_eval (info->regex,
	                             s,
	                             -1,
	                             0,
	                             0,
	                             (GRegexEvalCallback)expand_string_eval,
	                             info,
	                             NULL);
}

static Selector *
expand_selector_identifier (ExpandInfo *info,
                            Selector   *selector)
{
	gchar *expanded;
	Selector *ret;

	expanded = expand_string (info, selector->identifier);
	ret = selector_identifier_new (expanded);
	g_free (expanded);

	return ret;
}

static Selector *
expand_selector_regex (ExpandInfo *info,
                       Selector *selector)
{
	gchar *expanded;
	Selector *ret;

	expanded = expand_string (info, selector->as_string);
	ret = selector_regex_new (expanded);
	g_free (expanded);

	return ret;
}

static Selector *
expand_selector_pseudo (ExpandInfo *info,
                        Selector *selector)
{
	GPtrArray *args;
	Selector *ret;
	gchar **ptr;

	if (!selector->pseudo.arguments)
	{
		return selector_pseudo_new (selector->pseudo.definition, NULL);
	}

	args = g_ptr_array_new ();

	for (ptr = selector->pseudo.arguments; *ptr; ++ptr)
	{
		g_ptr_array_add (args, expand_string (info,
		                                      *ptr));
	}

	g_ptr_array_add (args, NULL);
	ptr = (gchar **)g_ptr_array_free (args, FALSE);

	ret = selector_pseudo_new (selector->pseudo.definition,
	                           (gchar const * const *)ptr);

	g_strfreev (ptr);

	return ret;
}

static Selector *
expand_selector (ExpandInfo *info,
                 Selector *selector)
{
	switch (selector->type)
	{
		case SELECTOR_TYPE_IDENTIFIER:
			return expand_selector_identifier (info,
			                                   selector);
		break;
		case SELECTOR_TYPE_REGEX:
			return expand_selector_regex (info, selector);
		break;
		case SELECTOR_TYPE_PSEUDO:
			return expand_selector_pseudo (info, selector);
		break;
		default:
			g_assert_not_reached ();
		break;
	}
}

CpgSelector *
cpg_selector_expand (CpgSelector *selector,
                     GSList      *expansions,
                     GRegex      *regex)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (regex != NULL, NULL);

	return cpg_selector_expand_func (selector,
	                                 regex,
	                                 (CpgSelectorExpandFunc)expand_expansion,
	                                 expansions,
	                                 NULL);
}

CpgSelector *
cpg_selector_expand_func (CpgSelector           *selector,
                          GRegex                *regex,
                          CpgSelectorExpandFunc  func,
                          gpointer               userdata,
                          GDestroyNotify         destroy_func)
{
	CpgSelector *sel;
	ExpandInfo info;
	GSList *item;

	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (regex != NULL, NULL);

	sel = cpg_selector_new ();

	info.selector = selector;
	info.regex = regex;
	info.func = func;
	info.userdata = userdata;

	for (item = selector->priv->selectors; item; item = g_slist_next (item))
	{
		add_selector (sel,
		              expand_selector (&info,
		                               item->data));
	}

	if (destroy_func)
	{
		destroy_func (userdata);
	}

	return sel;
}

CpgExpansion *
cpg_expansion_new (gchar const * const *items)
{
	CpgExpansion *ret;

	ret = g_slice_new (CpgExpansion);

	ret->expansions = g_ptr_array_new ();

	while (items && *items)
	{
		g_ptr_array_add (ret->expansions, g_strdup (*items));
		++items;
	}

	if (ret->expansions->len == 0)
	{
		g_ptr_array_add (ret->expansions, g_strdup (""));
	}

	return ret;
}

gint
cpg_expansion_num (CpgExpansion *id)
{
	return id->expansions->len;
}

gchar const *
cpg_expansion_get (CpgExpansion *id,
                   gint           idx)
{
	if (idx < 0 || idx >= id->expansions->len)
	{
		return NULL;
	}

	return (gchar const *)g_ptr_array_index (id->expansions, idx);
}

void
cpg_expansion_set (CpgExpansion *id,
                   gint           idx,
                   gchar const   *val)
{
	if (idx >= 0 && idx < id->expansions->len)
	{
		g_free (g_ptr_array_index (id->expansions, idx));
		id->expansions->pdata[idx] = g_strdup (val);
	}
}

void
cpg_expansion_free (CpgExpansion *id)
{
	gint i;

	if (!id)
	{
		return;
	}

	for (i = 0; i < id->expansions->len; ++i)
	{
		g_free (g_ptr_array_index (id->expansions, i));
	}

	g_slice_free (CpgExpansion, id);
}

CpgExpansion *
cpg_expansion_copy (CpgExpansion *id)
{
	CpgExpansion *ret;
	GPtrArray *ptr;
	gint i;

	if (id == NULL)
	{
		return NULL;
	}

	ptr = g_ptr_array_sized_new (cpg_expansion_num (id) + 1);

	for (i = 0; i < cpg_expansion_num (id); ++i)
	{
		g_ptr_array_add (ptr,
		                 g_strdup (cpg_expansion_get (id, i)));
	}

	ret = g_slice_new (CpgExpansion);
	ret->expansions = ptr;

	return ret;
}

void
cpg_expansion_add (CpgExpansion *id,
                   gchar const  *item)
{
	if (!id || !item)
	{
		return;
	}

	g_ptr_array_add (id->expansions, g_strdup (item));
}

gchar *
cpg_expansions_expand (GSList      *expansions,
                       gchar const *s,
                       GRegex      *regex)
{
	ExpandInfo info;

	info.selector = NULL;
	info.func = (CpgSelectorExpandFunc)expand_expansion;
	info.userdata = expansions;
	info.regex = regex;

	return expand_string (&info, s);
}
