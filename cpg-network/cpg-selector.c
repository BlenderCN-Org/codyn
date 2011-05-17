#include "cpg-selector.h"
#include "cpg-parser-context.h"
#include "cpg-parser.h"

#include <string.h>

#define CPG_SELECTOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_SELECTOR, CpgSelectorPrivate))

struct _CpgExpansion
{
	GPtrArray *expansions;
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
                            CpgObject  *parent,
                            SelectType  type)
{
	GSList *ret = NULL;

	if (type != TYPE_PROPERTY)
	{
		CpgObject *child;

		if (!CPG_IS_GROUP (parent))
		{
			return NULL;
		}

		child = cpg_group_get_child (CPG_GROUP (parent), selector->identifier);

		if (child && check_type (child, type))
		{
			ret = g_slist_prepend (NULL, child);
		}
	}
	else
	{
		CpgProperty *prop;

		prop = cpg_object_get_property (parent, selector->identifier);

		if (prop)
		{
			ret = g_slist_prepend (NULL, prop);
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
                       CpgObject   *parent,
                       SelectType   type,
                       GSList     **expansions)
{
	GSList *ret = NULL;

	if (type != TYPE_PROPERTY)
	{
		GSList const *children;

		if (!CPG_IS_GROUP (parent))
		{
			return NULL;
		}

		children = cpg_group_get_children (CPG_GROUP (parent));

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
				ret = g_slist_prepend (ret, child);

				if (expansions)
				{
					*expansions = g_slist_prepend (*expansions,
					                               expansion_from_match (info));
				}

				g_match_info_free (info);
			}

			children = g_slist_next (children);
		}
	}
	else
	{
		GSList *props;
		GSList *item;

		props = cpg_object_get_properties (parent);

		for (item = props; item; item = g_slist_next (item))
		{
			GMatchInfo *info;

			if (g_regex_match (selector->regex,
			                   cpg_property_get_name (item->data),
			                   0,
			                   &info))
			{
				ret = g_slist_prepend (ret, item->data);

				if (expansions)
				{
					*expansions = g_slist_prepend (*expansions,
					                               expansion_from_match (info));
				}

				g_match_info_free (info);
			}
		}

		g_slist_free (props);
	}

	if (expansions)
	{
		*expansions = g_slist_reverse (*expansions);
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
select_nth (GSList const *children,
            Nth const    *nth,
            gint          offset,
            SelectType    type,
            GSList       **expansions)
{
	GSList *ret = NULL;
	gint i = 1 - offset;
	gint num = 0;

	while (children && (nth->max < 0 || num < nth->max))
	{
		gboolean correcttype = check_type (children->data, type);

		if (correcttype)
		{
			if (nth_match (nth, i))
			{
				ret = g_slist_prepend (ret, children->data);
				++num;
			}

			++i;
		}

		children = g_slist_next (children);
	}

	return ret;
}

static GSList *
filter_list (GSList    *objs,
             gboolean   states,
             GSList   **expansions)
{
	GSList *ret = NULL;
	GSList *expret = NULL;
	GSList *expitem = expansions ? *expansions : NULL;

	while (objs)
	{
		if (states == !CPG_IS_LINK (objs->data))
		{
			ret = g_slist_prepend (ret, objs->data);

			if (expansions)
			{
				expret = g_slist_prepend (expret, expitem->data);
			}
		}
		else if (expansions)
		{
			g_slist_foreach (expitem->data, (GFunc)cpg_expansion_free, NULL);
			g_slist_free (expitem->data);
		}

		objs = g_slist_next (objs);
		expitem = g_slist_next (expitem);
	}

	if (expansions)
	{
		g_slist_free (*expansions);
		*expansions = expret;
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

static void
empty_expansions (CpgSelector *selector,
                  GSList **expansions)
{
	if (!expansions)
	{
		return;
	}

	cpg_selector_free_expansions (selector, *expansions);
	*expansions = g_slist_prepend (NULL, NULL);
}

static void
copy_expansions (CpgSelector *selector,
                 GSList **expansions,
                 GSList  *item)
{
	GList *ret = NULL;

	if (!expansions)
	{
		return;
	}

	while (item)
	{
		ret = g_slist_prepend (ret, cpg_expansion_copy (item->data));
		item = g_slist_next (item);
	}

	cpg_selector_free_expansions (selector, *expansions);
	*expansions = g_slist_prepend (NULL, g_slist_reverse (ret));
}

static void
clear_expansions (CpgSelector  *selector,
                  GSList      **expansions)
{
	if (!expansions)
	{
		return;
	}

	cpg_selector_free_expansions (selector, *expansions);
	*expansions = NULL;
}

static GSList *
selector_select_pseudo (Selector    *selector,
                        GSList      *parent,
                        CpgObject   *from,
                        SelectType   type,
                        GSList     **expansions)
{
	GSList *ret = NULL;
	GSList *item;

	switch (selector->pseudo.definition->type)
	{
		case SELECTOR_PSEUDO_TYPE_FROM:
			if (from != NULL && check_type (from, type))
			{
				empty_expansions (selector, expansions);
				return g_slist_prepend (NULL, from);
			}
			else
			{
				clear_expansions (selector, expansions);
				return NULL;
			}
		break;
		case SELECTOR_PSEUDO_TYPE_FIRST:
			if (parent && check_type (parent->data, type))
			{
				if (expansions)
				{
					copy_expansions (selector,
					                 expansions,
					                 (*expansions)->data);
				}

				return g_slist_prepend (NULL, parent->data);
			}
			else
			{
				clear_expansions (selector, expansions);
				return NULL;
			}
		break;
		case SELECTOR_PSEUDO_TYPE_LAST:
			if (parent && check_type (parent->data, type))
			{
				if (expansions)
				{
					copy_expansions (selector,
					                 expansions,
					                 g_slist_last (*expansions)->data);
				}

				return g_slist_prepend (NULL, g_slist_last (parent)->data);
			}
			else
			{
				clear_expansions (selector, expansions);
				return NULL;
			}
		break;
		case SELECTOR_PSEUDO_TYPE_NTH:
			return g_slist_reverse (select_nth (parent,
			                                    &(selector->pseudo.nth),
			                                    0,
			                                    type,
			                                    expansions));
		break;
		case SELECTOR_PSEUDO_TYPE_STATES:
			return filter_list (parent, TRUE, expansions);
		break;
		case SELECTOR_PSEUDO_TYPE_LINKS:
			return filter_list (parent, FALSE, expansions);
		break;
		default:
		break;
	}

	for (item = parent; item; item = g_slist_next (item))
	{
		CpgObject *obj = item->data;

		switch (selector->pseudo.definition->type)
		{
			case SELECTOR_PSEUDO_TYPE_ROOT:
			{
				CpgObject *top;

				top = top_parent (obj);

				if (top && check_type (top, type))
				{
					return g_slist_prepend (ret, top);
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_TEMPLATES:
			{
				CpgObject *top;

				top = top_parent (obj);

				if (top && CPG_IS_NETWORK (top))
				{
					return g_slist_prepend (ret,
					                        cpg_network_get_template_group (CPG_NETWORK (top)));
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_NTH_CHILD:
			{
				if (CPG_IS_GROUP (obj))
				{
					GSList const *children;

					children = cpg_group_get_children (CPG_GROUP (obj));

					ret = g_slist_concat (select_nth (children,
					                                 &(selector->pseudo.nth),
					                                 0,
					                                 type),
					                     ret);
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_NTH_SIBLING:
			{
				CpgObject *parent;

				parent = cpg_object_get_parent (obj);

				if (parent)
				{
					GSList const *children;
					gint idx;

					children = cpg_group_get_children (CPG_GROUP (parent));
					idx = g_slist_index ((GSList *)children, obj);

					ret = g_slist_concat (select_nth (children,
					                                  &(selector->pseudo.nth),
					                                  idx,
					                                  type == TYPE_ALL ? (CPG_IS_LINK (obj) ? TYPE_LINK : TYPE_STATE) : type),
					                      ret);
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_PARENT:
			{
				CpgObject *parent = cpg_object_get_parent (obj);

				if (parent && check_type (parent, type))
				{
					ret = g_slist_prepend (ret, parent);
				}
			}
			case SELECTOR_PSEUDO_TYPE_CHILDREN:
			{
				if (CPG_IS_GROUP (obj))
				{
					GSList const *children;
					GSList *copy;

					children = cpg_group_get_children (CPG_GROUP (obj));

					copy = g_slist_reverse (g_slist_copy ((GSList *)children));

					ret = g_slist_concat (copy, ret);
				}
			}
			break;
			break;
			case SELECTOR_PSEUDO_TYPE_FIRST_CHILD:
			{
				if (CPG_IS_GROUP (obj))
				{
					GSList const *children;

					children = cpg_group_get_children (CPG_GROUP (obj));

					if (children && check_type (children->data, type))
					{
						ret = g_slist_prepend (ret,
						                      children->data);
					}
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_LAST_CHILD:
			{
				if (CPG_IS_GROUP (obj))
				{
					GSList const *children;

					children = cpg_group_get_children (CPG_GROUP (obj));

					if (children)
					{
						CpgObject *last;

						last = g_slist_last ((GSList *)children)->data;

						if (check_type (last, type))
						{
							ret = g_slist_prepend (ret,
							                       last);
						}
					}
				}
			}
			break;
			default:
				g_assert_not_reached ();
			break;
		}
	}

	return g_slist_reverse (ret);
}

static GSList *
copy_expansions_reverse (GSList *expansions)
{
	GSList *ret = NULL;

	while (expansions)
	{
		ret = g_slist_prepend (ret,
		                       cpg_expansion_copy (expansions->data));

		expansions = g_slist_next (expansions);
	}

	return ret;
}

static GSList *
selector_select (Selector    *selector,
                 GSList      *parent,
                 CpgObject   *from,
                 SelectType   type,
                 GSList     **expansions)
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
			                              type,
			                              expansions);
		}
	}
	else
	{
		GSList *newex = NULL;
		GSList *curex = expansions ? *expansions : NULL;

		for (item = parent; item; item = g_slist_next (item))
		{
			CpgObject *obj = item->data;

			GSList *r = NULL;
			GSList *expp = NULL;
			GSList *original;

			switch (selector->type)
			{
				case SELECTOR_TYPE_IDENTIFIER:
					r = selector_select_identifier (selector,
					                                obj,
					                                type);
				break;
				case SELECTOR_TYPE_REGEX:
					r = selector_select_regex (selector,
					                           obj,
					                           type,
					                           expansions ? &expp : NULL);
				break;
				default:
					g_assert_not_reached ();
				break;
			}

			original = curex->data;

			if (expp)
			{
				GSList *ex = NULL;
				GSList *it;

				for (it = expp; it; it = g_slist_next (it))
				{
					GSList *copied;

					copied = copy_expansions_reverse (original);
					copied = g_slist_prepend (copied, it->data);

					ex = g_slist_prepend (ex, g_slist_reverse (copied));
				}

				g_slist_free (expp);
				newex = g_slist_concat (newex, g_slist_reverse (ex));
			}

			ret = g_slist_concat (ret, r);

			if (curex)
			{
				curex = g_slist_next (curex);
			}
		}

		if (expansions)
		{
			*expansions = newex;
		}
	}

	return ret;
}

static GSList *
selector_select_all (CpgSelector  *selector,
                     CpgObject    *parent,
                     CpgObject    *from,
                     SelectType    type,
                     GSList      **expansions)
{
	GSList *item;
	GSList *ctx = NULL;

	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (parent), NULL);
	g_return_val_if_fail (from == NULL || CPG_IS_OBJECT (from), NULL);

	if (expansions)
	{
		*expansions = NULL;
	}

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

	ctx = g_slist_prepend (NULL, parent);

	for (item = selector->priv->selectors; item; item = g_slist_next (item))
	{
		GSList *tmp;

		tmp = ctx;
		ctx = selector_select (item->data,
		                       ctx,
		                       from,
		                       item->next ? TYPE_ALL : type,
		                       expansions);

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
                     CpgObject    *parent,
                     GSList      **expansions)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (parent), NULL);

	return selector_select_all (selector, parent, NULL, TYPE_ALL, expansions);
}

GSList *
cpg_selector_select_states (CpgSelector  *selector,
                            CpgObject    *parent,
                            GSList      **expansions)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);

	return selector_select_all (selector, parent, NULL, TYPE_STATE, expansions);
}

GSList *
cpg_selector_select_links (CpgSelector  *selector,
                           CpgObject    *parent,
                           GSList      **expansions)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);

	return selector_select_all (selector, parent, NULL, TYPE_LINK, expansions);
}

GSList *
cpg_selector_select_link_to (CpgSelector  *selector,
                             CpgObject    *parent,
                             CpgObject    *from,
                             GSList      **expansions)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (from), NULL);

	return selector_select_all (selector, parent, from, TYPE_STATE, expansions);
}

GSList *
cpg_selector_select_properties (CpgSelector  *selector,
                                CpgObject    *parent,
                                GSList      **expansions)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);

	return selector_select_all (selector, parent, NULL, TYPE_PROPERTY, expansions);
}

void
cpg_selector_free_expansions (CpgSelector *selector,
                              GSList      *expansions)
{
	g_return_if_fail (CPG_IS_SELECTOR (selector));

	while (expansions)
	{
		g_slist_foreach (expansions->data, (GFunc)cpg_expansion_free, NULL);
		g_slist_free (expansions->data);

		expansions = g_slist_delete_link (expansions, expansions);
	}
}

gchar const *
cpg_selector_as_string (CpgSelector *selector)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);

	return selector->priv->as_string->str;
}

typedef struct
{
	CpgSelectorExpandFunc func;
	gpointer userdata;
} ExpanderInfo;

static gboolean
expand_string_eval (GMatchInfo const *info,
                    GString          *result,
                    GSList           *expansions)
{
	gchar *first;
	gchar *second;
	gchar const *num;
	CpgExpansions *ex;

	first = g_match_info_fetch (info, 1);
	second = g_match_info_fetch (info, 2);

	if (second && *second)
	{
		ex = g_slist_nth_data (expansions,
		                       (gint)g_ascii_strtoll (first, NULL, 10));

		num = second;
	}
	else
	{
		ex = expansions ? expansions->data : NULL;
		num = first;
	}

	if (ex)
	{
		gint idx = (gint)g_ascii_strtoll (num, NULL, 10);
		gchar const *ss;

		ss = cpg_expansion_get (ex, idx);

		if (ss)
		{
			g_string_append (result, ss);
		}
	}

	g_free (first);
	g_free (second);

	return TRUE;
}

static gchar *
expand_string (gchar const *s,
               GSList      *expansions)
{
	static GRegex *expander = NULL;
	ExpanderInfo info = {func, userdata};

	if (!expander)
	{
		expander = g_regex_new ("@([0-9]+)(:[0-9]+)?",
		                        0,
		                        0,
		                        NULL);
	}

	return g_regex_replace_eval (expander,
	                             s,
	                             -1,
	                             0,
	                             0,
	                             (GRegexEvalCallback)expand_string_eval,
	                             expansions,
	                             NULL);
}

static Selector *
expand_selector_identifier (Selector *selector,
                            GSList   *expansions)
{
	gchar *expanded;
	Selector *ret;

	expanded = expand_string (selector->identifier, expansions);
	ret = selector_identifier_new (expanded);
	g_free (expanded);

	return ret;
}

static Selector *
expand_selector_regex (Selector *selector,
                       GSList   *expansions)
{
	gchar *expanded;
	Selector *ret;

	expanded = expand_string (selector->as_string, expansions);
	ret = selector_regex_new (expanded);
	g_free (expanded);

	return ret;
}

static Selector *
expand_selector_pseudo (Selector *selector,
                        GSList   *expansions)
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
		g_ptr_array_add (args, expand_string (*ptr,
		                                      expansions));
	}

	g_ptr_array_add (args, NULL);
	ptr = (gchar **)g_ptr_array_free (args, FALSE);

	ret = selector_pseudo_new (selector->pseudo.definition,
	                           (gchar const * const *)ptr);

	g_strfreev (ptr);

	return ret;
}

static Selector *
expand_selector (Selector *selector,
                 GSList   *expansions)
{
	switch (selector->type)
	{
		case SELECTOR_TYPE_IDENTIFIER:
			return expand_selector_identifier (selector,
			                                   expansions);
		break;
		case SELECTOR_TYPE_REGEX:
			return expand_selector_regex (selector,
			                              expansions);
		break;
		case SELECTOR_TYPE_PSEUDO:
			return expand_selector_pseudo (selector,
			                               expansions);
		break;
		default:
			g_assert_not_reached ();
		break;
	}
}

CpgSelector *
cpg_selector_expand (CpgSelector *selector,
                     GSList      *expansions)
{
	CpgSelector *sel;
	GSList *item;

	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);

	sel = cpg_selector_new ();

	for (item = selector->priv->selectors; item; item = g_slist_next (item))
	{
		add_selector (sel,
		              expand_selector (item->data,
		                               expansions));
	}

	return sel;
}

CpgExpansion *
cpg_expansion_new (gchar const * const *items)
{
	CpgExpandedId *ret;

	ret = g_slice_new (CpgExpandedId);

	ret->expansions = g_ptr_array_new ();

	while (items && *items)
	{
		g_ptr_array_add (ret->expansions, g_strdup (*items));
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
cpg_expansion_set (CpgExpandedId *id,
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
cpg_expanded_id_free (CpgExpansion *id)
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

static gboolean
expand_replace (GMatchInfo const *info,
                GString          *result,
                CpgExpansion     *id)
{
	gchar *n;
	gint num;
	gchar const *rep;

	n = g_match_info_fetch (info, 1);
	num = g_ascii_strtoll (n, NULL, 10);
	g_free (n);

	rep = cpg_expansion_get (id, num - 1);

	if (rep != NULL)
	{
		g_string_append (result, rep);
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

gchar *
cpg_expansion_expand (CpgExpansion *id,
                      gchar const  *s)
{
	static GRegex *reg = NULL;

	if (!reg)
	{
		reg = g_regex_new ("@([0-9]+)",
		                   0,
		                   0,
		                   NULL);
	}

	return g_regex_replace_eval (reg,
	                             s,
	                             -1,
	                             0,
	                             0,
	                             (GRegexEvalCallback)expand_replace,
	                             id,
	                             NULL);
}

gchar **
cpg_expansion_expand_all (CpgExpansion        *id,
                          gchar const * const *s)
{
	GPtrArray *ret;

	ret = g_ptr_array_new ();

	while (s && *s)
	{
		g_ptr_array_add (ret, cpg_expansion_expand (id, *s));
		++s;
	}

	g_ptr_array_add (ret, NULL);
	return (gchar **)g_ptr_array_free (ret, FALSE);
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
