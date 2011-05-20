#include "cpg-selector.h"
#include "cpg-parser-context.h"
#include "cpg-parser.h"
#include "cpg-expansion.h"
#include "cpg-selection.h"

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
	SELECTOR_PSEUDO_TYPE_CHILDREN,
	SELECTOR_PSEUDO_TYPE_PARENT,
	SELECTOR_PSEUDO_TYPE_FIRST_CHILD,
	SELECTOR_PSEUDO_TYPE_LAST_CHILD,
	SELECTOR_PSEUDO_TYPE_FIRST,
	SELECTOR_PSEUDO_TYPE_LAST,
	SELECTOR_PSEUDO_TYPE_SUBSET,
	SELECTOR_PSEUDO_TYPE_STATES,
	SELECTOR_PSEUDO_TYPE_LINKS,
	SELECTOR_PSEUDO_TYPE_SIBLINGS,
	SELECTOR_PSEUDO_TYPE_TEMPLATES,
	SELECTOR_PSEUDO_TYPE_COUNT,
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

static SelectorPseudoDefinition const pseudo_selectors[] =
{
	{SELECTOR_PSEUDO_TYPE_ROOT, "root", FALSE},
	{SELECTOR_PSEUDO_TYPE_CHILDREN, "children", TRUE},
	{SELECTOR_PSEUDO_TYPE_PARENT, "parent", FALSE},
	{SELECTOR_PSEUDO_TYPE_FIRST_CHILD, "first-child", FALSE},
	{SELECTOR_PSEUDO_TYPE_LAST_CHILD, "last-child", FALSE},
	{SELECTOR_PSEUDO_TYPE_FIRST, "first", FALSE},
	{SELECTOR_PSEUDO_TYPE_LAST, "last", FALSE},
	{SELECTOR_PSEUDO_TYPE_SUBSET, "subset", TRUE},
	{SELECTOR_PSEUDO_TYPE_STATES, "states", FALSE},
	{SELECTOR_PSEUDO_TYPE_LINKS, "links", FALSE},
	{SELECTOR_PSEUDO_TYPE_SIBLINGS, "siblings", TRUE},
	{SELECTOR_PSEUDO_TYPE_TEMPLATES, "templates", FALSE},
	{SELECTOR_PSEUDO_TYPE_COUNT, "count", FALSE}
};

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
			CpgEmbeddedString *identifier;
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
			g_object_unref (selector->pseudo.identifier);

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

static Selector *
selector_pseudo_new (CpgEmbeddedString *identifier,
                     GSList            *arguments)
{
	Selector *selector;
	GString *args;

	selector = selector_new (SELECTOR_TYPE_PSEUDO);

	selector->pseudo.identifier = g_object_ref (identifier);

	selector->pseudo.arguments = g_slist_copy (arguments);
	g_slist_foreach (selector->pseudo.arguments, (GFunc)g_object_ref, NULL);

	args = g_string_new ("");

	while (arguments)
	{
		if (args->len != 0)
		{
			g_string_append (args, ", ");
		}

		g_string_append_printf (args,
		                        "\"%s\"",
		                        cpg_embedded_string_expand (arguments->data, NULL));

		arguments = g_slist_next (arguments);
	}

	selector->as_string = g_strdup_printf ("%s%s%s%s",
	                                       cpg_embedded_string_expand (selector->pseudo.identifier, NULL),
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
cpg_selector_add (CpgSelector       *selector,
                  CpgEmbeddedString *identifier,
                  gboolean           onset)
{
	g_return_if_fail (CPG_IS_SELECTOR (selector));
	g_return_if_fail (CPG_IS_EMBEDDED_STRING (identifier));

	add_selector (selector, selector_identifier_new (identifier, onset));
}

void
cpg_selector_add_pseudo (CpgSelector       *selector,
                         CpgEmbeddedString *pseudo,
                         GSList            *arguments)
{
	g_return_if_fail (CPG_IS_SELECTOR (selector));
	g_return_if_fail (CPG_IS_EMBEDDED_STRING (pseudo));

	add_selector (selector, selector_pseudo_new (pseudo, arguments));
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
selector_select_identifier (Selector           *selector,
                            CpgSelection       *parent,
                            SelectType          type,
                            CpgEmbeddedContext *context)
{
	GSList *ret = NULL;

	if (type != TYPE_PROPERTY)
	{
		CpgObject *child;

		if (selector->onset)
		{
			if (g_strcmp0 (cpg_embedded_string_expand (selector->identifier,
			                                           context),
			               cpg_object_get_id (cpg_selection_get_object (parent))) == 0 &&
			    check_type (cpg_selection_get_object (parent), type))
			{
				ret = g_slist_prepend (NULL,
				                        cpg_selection_copy (parent));
			}

		}
		else
		{
			if (!CPG_IS_GROUP (cpg_selection_get_object (parent)))
			{
				return NULL;
			}

			child = cpg_group_get_child (CPG_GROUP (cpg_selection_get_object (parent)),
			                             cpg_embedded_string_expand (selector->identifier,
			                                                         context));

			if (child && check_type (child, type))
			{
				ret = g_slist_prepend (NULL,
				                       cpg_selection_new (child,
				                                          cpg_selection_get_expansions (parent)));
			}
		}
	}
	else
	{
		CpgProperty *prop;

		if (selector->onset)
		{
			/* FIXME: make this work? */
			return NULL;
		}

		prop = cpg_object_get_property (cpg_selection_get_object (parent),
		                                cpg_embedded_string_expand (selector->identifier,
		                                                            context));

		if (prop)
		{
			ret = g_slist_prepend (NULL,
			                       cpg_selection_new (prop, cpg_selection_get_expansions (parent)));
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

static GSList *
selector_select_regex (Selector           *selector,
                       CpgSelection       *parent,
                       SelectType          type,
                       CpgEmbeddedContext *context)
{
	GSList *ret = NULL;
	GRegex *regex = NULL;

	if (type != TYPE_PROPERTY)
	{
		GSList const *children;

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
				GSList *expansions;
				CpgExpansion *expansion;

				expansions = g_slist_copy (cpg_selection_get_expansions (parent));

				expansion = expansion_from_match (info);
				expansions = g_slist_append (expansions, expansion);

				childsel = cpg_selection_new (obj, expansions);
				g_slist_free (expansions);

				cpg_expansion_free (expansion);

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
					GSList *expansions;
					CpgExpansion *expansion;

					expansions = g_slist_copy (cpg_selection_get_expansions (parent));

					expansion = expansion_from_match (info);
					expansions = g_slist_append (expansions, expansion);

					childsel = cpg_selection_new (child, expansions);
					g_slist_free (expansions);

					cpg_expansion_free (expansion);

					ret = g_slist_prepend (ret, childsel);

					g_match_info_free (info);
				}

				children = g_slist_next (children);
			}
		}
	}
	else
	{
		GSList *props;
		GSList *item;

		if (selector->onset)
		{
			/* FIXME: implement this? */
			return NULL;
		}

		regex = g_regex_new (cpg_embedded_string_expand (selector->regex, context),
		                     G_REGEX_CASELESS | G_REGEX_ANCHORED,
		                     G_REGEX_MATCH_ANCHORED | G_REGEX_MATCH_NOTEMPTY,
		                     NULL);

		props = cpg_object_get_properties (cpg_selection_get_object (parent));

		for (item = props; item; item = g_slist_next (item))
		{
			GMatchInfo *info;

			if (regex_match_full (regex,
			                      cpg_property_get_name (item->data),
			                      &info))
			{
				CpgSelection *childsel;
				GSList *expansions;
				CpgExpansion *expansion;

				expansions = g_slist_copy (cpg_selection_get_expansions (parent));
				expansion = expansion_from_match (info);
				expansions = g_slist_append (expansions, expansion);

				childsel = cpg_selection_new (item->data, expansions);

				g_slist_free (expansions);
				cpg_expansion_free (expansion);

				ret = g_slist_prepend (ret, childsel);

				g_match_info_free (info);
			}
		}

		g_slist_free (props);
	}

	if (regex)
	{
		g_regex_unref (regex);
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
		                                          cpg_selection_get_expansions (selection)));

		children = g_slist_next (children);
	}

	return g_slist_reverse (ret);
}

static SelectorPseudoDefinition const *
find_pseudo_definition (Selector           *pseudo,
                        CpgEmbeddedContext *context)
{
	gint i;
	gchar const *id;

	id = cpg_embedded_string_expand (pseudo->pseudo.identifier, context);

	for (i = 0; i < NUM_PSEUDO_SELECTORS; ++i)
	{
		if (g_strcmp0 (pseudo_selectors[i].name, id) == 0)
		{
			return &(pseudo_selectors[i]);
		}
	}

	return NULL;
}

static GSList *
count_selection (GSList     *selection,
                 SelectType  type)
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
		                         expansions);

		g_slist_free (expansions);
		cpg_expansion_free (ex);

		ret->data = sel;
	}

	cpg_expansion_free (ex);

	return g_slist_reverse (ret);
}

static GSList *
selector_select_pseudo (Selector    *selector,
                        GSList      *parent,
                        SelectType   type,
                        CpgEmbeddedContext *context)
{
	GSList *ret = NULL;
	GSList *item;
	CpgSelection *last = NULL;
	SelectorPseudoDefinition const *def;

	def = find_pseudo_definition (selector, context);

	if (def)
	{
		switch (def->type)
		{
			case SELECTOR_PSEUDO_TYPE_SUBSET:
			{
				Nth nth = parse_nth (selector->pseudo.arguments,
				                     context);

				return g_slist_reverse (select_nth_reverse (parent,
					                                    &nth,
					                                    0,
					                                    type));
			}
			break;
			case SELECTOR_PSEUDO_TYPE_STATES:
				return g_slist_reverse (filter_list_reverse (parent,
					                                     TRUE));
			break;
			case SELECTOR_PSEUDO_TYPE_LINKS:
				return g_slist_reverse (filter_list_reverse (parent,
					                                     FALSE));
			break;
			case SELECTOR_PSEUDO_TYPE_COUNT:
				return count_selection (parent, type);
			break;
			default:
			break;
		}
	}

	for (item = parent; item; item = g_slist_next (item))
	{
		CpgSelection *sel = item->data;
		Nth nth = {0,};

		cpg_embedded_context_push_expansions (context,
		                                      cpg_selection_get_expansions (sel));

		def = find_pseudo_definition (selector, context);

		if (!def)
		{
			g_warning ("Could not find pseudo selector: %s",
			           cpg_embedded_string_expand (selector->pseudo.identifier, context));

			cpg_embedded_context_pop_expansions (context);
			continue;
		}

		switch (def->type)
		{
			case SELECTOR_PSEUDO_TYPE_CHILDREN:
			case SELECTOR_PSEUDO_TYPE_SIBLINGS:
				nth = parse_nth (selector->pseudo.arguments,
				                 context);
			break;
			default:
			break;
		}

		cpg_embedded_context_pop_expansions (context);

		switch (def->type)
		{
			case SELECTOR_PSEUDO_TYPE_FIRST:
				if (check_type (cpg_selection_get_object (sel), type))
				{
					return g_slist_prepend (ret,
					                        cpg_selection_copy (sel));
				}
			break;
			case SELECTOR_PSEUDO_TYPE_LAST:
				if (check_type (cpg_selection_get_object (sel), type))
				{
					last = sel;
				}
			break;
			case SELECTOR_PSEUDO_TYPE_ROOT:
			{
				CpgObject *top;

				top = top_parent (cpg_selection_get_object (sel));

				if (top && check_type (top, type))
				{
					return g_slist_prepend (ret,
					                        cpg_selection_new (top, cpg_selection_get_expansions (sel)));
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_TEMPLATES:
			{
				CpgObject *top;

				top = top_parent (cpg_selection_get_object (sel));

				if (top && CPG_IS_NETWORK (top))
				{
					CpgGroup *template_group;

					template_group = cpg_network_get_template_group (CPG_NETWORK (top));

					return g_slist_prepend (ret,
					                        cpg_selection_new (template_group,
					                                       cpg_selection_get_expansions (sel)));
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_CHILDREN:
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

					g_slist_foreach (children, (GFunc)cpg_selection_free, NULL);
					g_slist_free (children);
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_SIBLINGS:
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
					                                          idx,
					                                          type == TYPE_ALL ? (CPG_IS_LINK (cpg_selection_get_object (sel)) ? TYPE_LINK : TYPE_STATE) : type),
					                      ret);

					g_slist_foreach (children, (GFunc)cpg_selection_free, NULL);
					g_slist_free (children);
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_PARENT:
			{
				CpgObject *parent = cpg_object_get_parent (cpg_selection_get_object (sel));

				if (parent && check_type (parent, type))
				{
					ret = g_slist_prepend (ret,
					                       cpg_selection_new (parent,
					                                          cpg_selection_get_expansions (sel)));
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_FIRST_CHILD:
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
							                                          cpg_selection_get_expansions (sel)));

							break;
						}

						children = g_slist_next (children);
					}
				}
			}
			break;
			case SELECTOR_PSEUDO_TYPE_LAST_CHILD:
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
						                                          cpg_selection_get_expansions (sel)));
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
selector_select (Selector           *selector,
                 GSList             *parent,
                 SelectType          type,
                 CpgEmbeddedContext *context)
{
	GSList *ret = NULL;
	GSList *item;

	if (selector->type == SELECTOR_TYPE_PSEUDO)
	{
		if (type != TYPE_PROPERTY)
		{
			ret = selector_select_pseudo (selector,
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

			cpg_embedded_context_push_expansions (context,
			                                      cpg_selection_get_expansions (sel));

			switch (selector->type)
			{
				case SELECTOR_TYPE_IDENTIFIER:
					r = selector_select_identifier (selector,
					                                sel,
					                                type,
					                                context);
				break;
				case SELECTOR_TYPE_REGEX:
					r = selector_select_regex (selector,
					                           sel,
					                           type,
					                           context);
				break;
				default:
					g_assert_not_reached ();
				break;
			}

			cpg_embedded_context_pop_expansions (context);

			ret = g_slist_concat (ret, r);
		}
	}

	return ret;
}

static GSList *
selector_select_all (CpgSelector        *selector,
                     CpgObject          *parent,
                     SelectType          type,
                     CpgEmbeddedContext *context)
{
	GSList *item;
	GSList *ctx = NULL;

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
		                       item->next ? TYPE_ALL : type,
		                       context);

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
cpg_selector_select (CpgSelector        *selector,
                     CpgObject          *parent,
                     CpgEmbeddedContext *context)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (parent), NULL);
	g_return_val_if_fail (context == NULL || CPG_IS_EMBEDDED_CONTEXT (context), NULL);

	return selector_select_all (selector, parent, TYPE_ALL, context);
}

GSList *
cpg_selector_select_states (CpgSelector        *selector,
                            CpgObject          *parent,
                            CpgEmbeddedContext *context)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);
	g_return_val_if_fail (context == NULL || CPG_IS_EMBEDDED_CONTEXT (context), NULL);

	return selector_select_all (selector, parent, TYPE_STATE, context);
}

GSList *
cpg_selector_select_links (CpgSelector        *selector,
                           CpgObject          *parent,
                           CpgEmbeddedContext *context)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);
	g_return_val_if_fail (context == NULL || CPG_IS_EMBEDDED_CONTEXT (context), NULL);

	return selector_select_all (selector, parent, TYPE_LINK, context);
}

GSList *
cpg_selector_select_properties (CpgSelector        *selector,
                                CpgObject          *parent,
                                CpgEmbeddedContext *context)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_OBJECT (parent), NULL);
	g_return_val_if_fail (context == NULL || CPG_IS_EMBEDDED_CONTEXT (context), NULL);

	return selector_select_all (selector, parent, TYPE_PROPERTY, context);
}

gchar const *
cpg_selector_as_string (CpgSelector *selector)
{
	g_return_val_if_fail (CPG_IS_SELECTOR (selector), NULL);

	return selector->priv->as_string->str;
}
