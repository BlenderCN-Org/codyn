#include "cpg-selector.h"
#include <string.h>

#define CPG_SELECTOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_SELECTOR, CpgSelectorPrivate))

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
	int a;
	int b;
} Nth;

typedef struct
{
	SelectorPseudoDefinition const *definition;
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
	{SELECTOR_PSEUDO_TYPE_NTH_SIBLING, "nth-sibling", TRUE}
};

typedef struct
{
	SelectorType type;

	union
	{
		gchar *identifier;
		GRegex *regex;
		SelectorPseudo pseudo;
	};
} Selector;

static GRegex *nth_regex = NULL;

struct _CpgSelectorPrivate
{
	GSList *selectors;
	gboolean has_selected;
};

G_DEFINE_TYPE (CpgSelector, cpg_selector, G_TYPE_OBJECT)

static void
selector_pseudo_free (SelectorPseudo *selector)
{
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

	g_slice_free (Selector, selector);
}

static Selector *
selector_identifier_new (gchar const *identifier)
{
	Selector *selector;

	selector = selector_new (SELECTOR_TYPE_IDENTIFIER);
	selector->identifier = g_strdup (identifier);

	return selector;
}

static Selector *
selector_regex_new (gchar const *regex)
{
	Selector *selector;

	selector = selector_new (SELECTOR_TYPE_REGEX);
	selector->regex = g_regex_new (regex,
	                               G_REGEX_CASELESS |
	                               G_REGEX_ANCHORED,
	                               G_REGEX_MATCH_ANCHORED |
	                               G_REGEX_MATCH_NOTEMPTY,
	                               NULL);

	return selector;
}

static int
parse_nth_num (GMatchInfo *info,
               gint        num)
{
	gchar *s;
	gint ret;

	s = g_match_info_fetch (info, num);

	if (!s || !*s)
	{
		ret = 0;
	}
	else
	{
		ret = (gint)g_ascii_strtoll (s, NULL, 10);
	}

	g_free (s);

	return ret;
}

static Nth
parse_nth (gchar const *argument)
{
	Nth ret = {0, 0};
	GMatchInfo *info;

	if (!argument)
	{
		return ret;
	}

	if (nth_regex == NULL)
	{
		nth_regex = g_regex_new ("(([+-]?[0-9]+)n)?([+-]?[0-9]+)?",
		                         G_REGEX_CASELESS |
		                         G_REGEX_ANCHORED,
		                         G_REGEX_MATCH_ANCHORED |
		                         G_REGEX_MATCH_NOTEMPTY,
		                         NULL);
	}

	if (g_strcmp0 (argument, "odd") == 0)
	{
		ret.a = 2;
		ret.b = 1;
	}
	else if (g_strcmp0 (argument, "even") == 0)
	{
		ret.a = 2;
		ret.b = 0;
	}
	else if (g_regex_match (nth_regex, argument, 0, &info))
	{
		ret.a = parse_nth_num (info, 2);
		ret.b = parse_nth_num (info, 3);

		g_match_info_free (info);
	}

	return ret;
}

static Selector *
selector_pseudo_new (SelectorPseudoDefinition const *definition,
                     gchar const                    *argument)
{
	Selector *selector;

	selector = selector_new (SELECTOR_TYPE_PSEUDO);
	selector->pseudo.definition = definition;

	if (definition->has_argument)
	{
		selector->pseudo.nth = parse_nth (argument);
	}

	return selector;
}

static void
cpg_selector_finalize (GObject *object)
{
	CpgSelector *selector;

	selector = CPG_SELECTOR (object);

	g_slist_foreach (selector->priv->selectors, (GFunc)selector_free, NULL);
	g_slist_free (selector->priv->selectors);

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
}

CpgSelector *
cpg_selector_new ()
{
	return g_object_new (CPG_TYPE_SELECTOR, NULL);
}

static gchar *
parse_string (gchar const **s)
{
	gchar *ret = g_new (gchar, strlen (*s));
	gchar *ptr = ret;

	while (**s && **s != '"')
	{
		if (**s == '\\' && *(*s + 1) == '"')
		{
			++*s;
		}

		*ptr++ = *(*s++);
	}

	*ptr = '\0';

	return ret;
}

static void
parse_pseudo (CpgSelector  *selector,
              gchar const **s)
{
	GString *identifier;
	GString *nth;
	gboolean innth = FALSE;

	identifier = g_string_new ("");
	nth = g_string_new ("");

	while (**s && **s != '.' && **s != ':' && (!innth || **s != ')'))
	{
		switch (**s)
		{
			case '(':
				innth = TRUE;
			break;
			default:
				if (innth)
				{
					g_string_append_c (nth, **s);
				}
				else
				{
					g_string_append_c (identifier, **s);
				}
			break;
		}

		++*s;
	}

	cpg_selector_add_pseudo (selector, identifier->str, nth->str);

	g_string_free (identifier, TRUE);
	g_string_free (nth, TRUE);
}

CpgSelector *
cpg_selector_parse (gchar const *s)
{
	CpgSelector *ret;
	GString *identifier;

	ret = cpg_selector_new ();
	identifier = g_string_new ("");

	while (*s)
	{
		switch (*s)
		{
			case ':':
			case '.':
				if (identifier->len != 0)
				{
					cpg_selector_add (ret, identifier->str);
					g_string_assign (identifier, "");
				}

				if (*s == ':')
				{
					parse_pseudo (ret, &s);
				}
			break;
			case '"':
			{
				gchar *id;

				id = parse_string (&s);
				g_string_append (identifier, id);
				g_free (id);
			}
			break;
			default:
				g_string_append_c (identifier, *s);
			break;
		}

		if (*s)
		{
			++s;
		}
	}

	if (identifier->len != 0)
	{
		cpg_selector_add (ret, identifier->str);
	}

	g_string_free (identifier, TRUE);

	return ret;
}

static void
add_selector (CpgSelector *selector,
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
                         gchar const *argument)
{
	gint i;

	g_return_if_fail (CPG_IS_SELECTOR (selector));
	g_return_if_fail (pseudo != NULL);

	for (i = 0; i < NUM_PSEUDO_SELECTORS; ++i)
	{
		SelectorPseudoDefinition const *def;

		def = &pseudo_selectors[i];

		if (g_strcmp0 (def->name, pseudo) == 0)
		{
			add_selector (selector,
			              selector_pseudo_new (def, argument));
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

static GSList *
selector_select_regex (Selector   *selector,
                       CpgObject  *parent,
                       SelectType  type)
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

			child = children->data;

			if (check_type (child, type) &&
			    g_regex_match (selector->regex,
			                   cpg_object_get_id (child),
			                   0,
			                   NULL))
			{
				ret = g_slist_prepend (ret, child);
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
			if (g_regex_match (selector->regex,
			                   cpg_property_get_name (item->data),
			                   0,
			                   NULL))
			{
				ret = g_slist_prepend (ret, item->data);
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
		return nth->b == nth->a;
	}

	return (i - nth->b) % nth->a == 0 && (i - nth->b) / nth->a >= 0;
}

static GSList *
select_nth (GSList const *children,
            Nth const    *nth,
            gint          offset,
            SelectType    type)
{
	GSList *ret = NULL;
	gint i = 1 - offset;

	while (children)
	{
		if (nth_match (nth, i) && check_type (children->data, type))
		{
			ret = g_slist_prepend (ret, children->data);
		}

		++i;
		children = g_slist_next (children);
	}

	return ret;
}

static GSList *
filter_list (GSList   *objs,
             gboolean  states)
{
	GSList *ret = NULL;

	while (objs)
	{
		if (states == !CPG_IS_LINK (objs->data))
		{
			ret = g_slist_prepend (ret, objs->data);
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
selector_select_pseudo (Selector   *selector,
                        GSList     *parent,
                        CpgObject  *from,
                        SelectType  type)
{
	GSList *ret = NULL;
	GSList *item;

	switch (selector->pseudo.definition->type)
	{
		case SELECTOR_PSEUDO_TYPE_FROM:
			if (from != NULL && check_type (from, type))
			{
				return g_slist_prepend (NULL, from);
			}
			else
			{
				return NULL;
			}
		break;
		case SELECTOR_PSEUDO_TYPE_FIRST:
			if (parent && check_type (parent->data, type))
			{
				return g_slist_prepend (NULL, parent->data);
			}
			else
			{
				return NULL;
			}
		break;
		case SELECTOR_PSEUDO_TYPE_LAST:
			if (parent && check_type (parent->data, type))
			{
				return g_slist_prepend (NULL, g_slist_last (parent)->data);
			}
			else
			{
				return NULL;
			}
		break;
		case SELECTOR_PSEUDO_TYPE_NTH:
			return g_slist_reverse (select_nth (parent,
			                                    &(selector->pseudo.nth),
			                                    0,
			                                    type));
		break;
		case SELECTOR_PSEUDO_TYPE_STATES:
			return filter_list (parent, TRUE);
		break;
		case SELECTOR_PSEUDO_TYPE_LINKS:
			return filter_list (parent, FALSE);
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
					                                  type),
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
selector_select (Selector   *selector,
                 GSList     *parent,
                 CpgObject  *from,
                 SelectType  type)
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
			CpgObject *obj = item->data;
			GSList *r = NULL;

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
selector_select_all (CpgSelector *selector,
                     CpgObject   *parent,
                     CpgObject   *from,
                     SelectType   type)
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

	ctx = g_slist_prepend (NULL, parent);

	for (item = selector->priv->selectors; item; item = g_slist_next (item))
	{
		GSList *tmp;

		tmp = ctx;
		ctx = selector_select (item->data,
		                       ctx,
		                       from,
		                       item->next ? TYPE_ALL : type);

		g_slist_free (tmp);

		if (ctx == NULL)
		{
			break;
		}
	}

	return ctx;
}

GSList *
cpg_selector_select (CpgSelector *selector,
                     CpgObject   *parent)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (parent), NULL);

	return selector_select_all (selector, parent, NULL, TYPE_ALL);
}

GSList *
cpg_selector_select_states (CpgSelector *selector,
                            CpgObject   *parent)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);

	return selector_select_all (selector, parent, NULL, TYPE_STATE);
}

GSList *
cpg_selector_select_links (CpgSelector *selector,
                           CpgObject   *parent)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);

	return selector_select_all (selector, parent, NULL, TYPE_LINK);
}

GSList *
cpg_selector_select_link_to (CpgSelector *selector,
                             CpgObject   *parent,
                             CpgObject   *from)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (from), NULL);

	return selector_select_all (selector, parent, from, TYPE_STATE);
}

GSList *
cpg_selector_select_properties (CpgSelector *selector,
                                CpgObject   *parent)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);

	return selector_select_all (selector, parent, NULL, TYPE_PROPERTY);
}
