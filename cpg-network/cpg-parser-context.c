#include "cpg-parser-context.h"
#include "cpg-network-parser-utils.h"
#include "cpg-integrators.h"
#include "cpg-parser.h"

#include <string.h>

#define CPG_PARSER_CONTEXT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_PARSER_CONTEXT, CpgParserContextPrivate))

void cpg_parser_lex_destroy (gpointer scanner);
void cpg_parser_lex_init_extra (gpointer context, gpointer *scanner);
int cpg_parser_parse (gpointer context);

typedef struct
{
	gchar *name;
	CpgSelector *target;
} InterfaceItem;

typedef struct
{
	GFile *file;
	GInputStream *stream;
} InputItem;

static InputItem *
input_item_new (GFile        *file,
                GInputStream *stream)
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
		                  NULL);

		if (is)
		{
			ret->stream = G_INPUT_STREAM (is);
		}
	}

	return ret;
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
		g_object_unref (self->stream);
	}

	g_slice_free (InputItem, self);
}

typedef struct
{
	gchar *id;
	GSList *ids;

	CpgParserContextLinkFlags link_flags;

	CpgSelector *link_from;
	CpgSelector *link_to;

	GSList *templates;

	CpgParserContextScope scope;
	GSList *objects;

	GSList *properties;
	GSList *interface_items;
	GSList *actions;
} Context;

struct _CpgParserContextPrivate
{
	CpgNetwork *network;
	GSList *inputs;
	gpointer scanner;
	GHashTable *defines;
	gint start_token;

	gint lineno;
	gint cstart;
	gint cend;

	gchar *line;
	gchar *token;

	/* Stack of Context */
	GSList *context_stack;
	CpgSelector *selector;

	gboolean is_template;

	GError *error;
	gboolean error_occurred;
};

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

static gboolean ensure_parents (CpgParserContext *context, GSList *item);

static InterfaceItem *
interface_item_new (gchar const *name,
                    CpgSelector *target)
{
	InterfaceItem *item;

	item = g_slice_new (InterfaceItem);

	item->name = g_strdup (name);
	item->target = g_object_ref (target);

	return item;
}

static void
interface_item_free (InterfaceItem *item)
{
	g_free (item->name);
	g_object_unref (item->target);

	g_slice_free (InterfaceItem, item);
}

static Context *
context_new (CpgParserContextScope scope)
{
	Context *ret;

	ret = g_slice_new0 (Context);

	ret->scope = scope;

	return ret;
}

static void
context_free (Context *context)
{
	g_slist_foreach (context->properties, (GFunc)g_object_unref, NULL);
	g_slist_free (context->properties);

	g_slist_foreach (context->interface_items, (GFunc)interface_item_free, NULL);
	g_slist_free (context->interface_items);

	g_slist_foreach (context->actions, (GFunc)g_object_unref, NULL);
	g_slist_free (context->actions);

	g_free (context->id);
	g_slist_foreach (context->ids, (GFunc)cpg_expansion_free, NULL);
	g_slist_free (context->ids);

	g_slist_foreach (context->templates, (GFunc)g_object_unref, NULL);
	g_slist_free (context->templates);

	g_slice_free (Context, context);
}

static void
cpg_parser_context_finalize (GObject *object)
{
	CpgParserContext *self;

	self = CPG_PARSER_CONTEXT (object);

	while (self->priv->context_stack)
	{
		cpg_parser_context_pop_scope (self);
	}

	g_object_unref (self->priv->network);

	g_slist_foreach (self->priv->inputs, (GFunc)input_item_free, NULL);
	g_slist_free (self->priv->inputs);

	g_free (self->priv->line);
	g_free (self->priv->token);

	cpg_parser_lex_destroy (self->priv->scanner);

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

	cpg_parser_context_push_scope (self,
	                               CPG_PARSER_CONTEXT_SCOPE_NONE);

	cpg_parser_context_set_id (self, NULL, NULL);

	CURRENT_CONTEXT (self)->objects = g_slist_prepend (NULL, CPG_OBJECT (self->priv->network));

	self->priv->defines = g_hash_table_new_full (g_str_hash,
	                                             g_str_equal,
	                                             (GDestroyNotify)g_free,
	                                             (GDestroyNotify)g_free);

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
}

static void
cpg_parser_context_init (CpgParserContext *self)
{
	self->priv = CPG_PARSER_CONTEXT_GET_PRIVATE (self);

	self->priv->start_token = T_START_DOCUMENT;
}

CpgParserContext *
cpg_parser_context_new (CpgNetwork *network)
{
	return g_object_new (CPG_TYPE_PARSER_CONTEXT,
	                     "network", network,
	                     NULL);
}

CpgProperty *
cpg_parser_context_add_property (CpgParserContext *context,
                                 gchar const      *name,
                                 gchar const      *expression)
{
	CpgProperty *property;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (name != NULL, NULL);
	g_return_val_if_fail (expression != NULL, NULL);

	property = cpg_property_new (name, expression, CPG_PROPERTY_FLAG_NONE);

	CURRENT_CONTEXT (context)->properties =
		g_slist_prepend (CURRENT_CONTEXT (context)->properties,
		                 g_object_ref_sink (property));

	return property;
}

CpgLinkAction *
cpg_parser_context_add_action (CpgParserContext *context,
                               gchar const      *target,
                               gchar const      *expression)
{
	CpgLinkAction *action;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (target != NULL, NULL);
	g_return_val_if_fail (expression != NULL, NULL);

	action = cpg_link_action_new (target, cpg_expression_new (expression));

	CURRENT_CONTEXT (context)->actions =
		g_slist_prepend (CURRENT_CONTEXT (context)->actions,
		                 g_object_ref_sink (action));

	return action;
}

CpgFunction *
cpg_parser_context_add_function (CpgParserContext *context,
                                 gchar const      *name,
                                 gchar const      *expression,
                                 GArray           *arguments)
{
	CpgFunction *function;
	gint i;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (name != NULL, NULL);
	g_return_val_if_fail (expression != NULL, NULL);

	function = cpg_function_new (name, expression);

	for (i = 0; i < arguments->len; ++i)
	{
		CpgFunctionArgument *argument;

		argument = g_array_index (arguments, CpgFunctionArgument *, i);
		cpg_function_add_argument (function, argument);

		g_object_unref (argument);
	}

	cpg_group_add (cpg_network_get_function_group (context->priv->network),
	               CPG_OBJECT (function),
	               NULL);

	return function;
}

CpgFunctionPolynomial *
cpg_parser_context_add_polynomial (CpgParserContext *context,
                                   gchar const      *name,
                                   GArray           *pieces)
{
	CpgFunctionPolynomial *function;
	gint i;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	function = cpg_function_polynomial_new (name);

	for (i = 0; i < pieces->len; ++i)
	{
		CpgFunctionPolynomialPiece *piece;

		piece = g_array_index (pieces, CpgFunctionPolynomialPiece *, i);

		cpg_function_polynomial_add (function, piece);
	}

	cpg_group_add (cpg_network_get_function_group (context->priv->network),
	               CPG_OBJECT (function),
	               NULL);

	return function;

}

void
cpg_parser_context_add_interface (CpgParserContext *context,
                                  gchar const      *name,
                                  CpgSelector      *target)
{
	InterfaceItem *item;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (CPG_IS_SELECTOR (target));

	item = interface_item_new (name, target);

	CURRENT_CONTEXT (context)->interface_items =
		g_slist_prepend (CURRENT_CONTEXT (context)->interface_items,
		                 item);
}

static gboolean
parser_failed_error (CpgParserContext *context,
                     GError           *error)
{
	context->priv->error = error;
	return FALSE;
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

void
cpg_parser_context_error (CpgParserContext *context,
                          gchar const      *message)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	parser_failed (context,
	               CPG_NETWORK_LOAD_ERROR_SYNTAX,
	               "Unexpected token `%s' on line %d",
	               context->priv->token,
	               context->priv->lineno);
}

GError *
cpg_parser_context_get_error (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	return context->priv->error;
}

void
cpg_parser_context_push_scope (CpgParserContext      *context,
                               CpgParserContextScope  scope)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	context->priv->context_stack =
		g_slist_prepend (context->priv->context_stack,
		                 context_new (scope));
}

void
cpg_parser_context_set_id (CpgParserContext *context,
                           gchar const      *id,
                           GArray           *templates)
{
	Context *ctx;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	ctx = CURRENT_CONTEXT (context);

	g_free (ctx->id);

	g_slist_foreach (ctx->ids, (GFunc)cpg_expansion_free, NULL);
	g_slist_free (ctx->ids);

	if (id)
	{
		ctx->id = g_strdup (id);
		ctx->ids = cpg_network_parser_utils_expand_id (ctx->id);
	}
	else
	{
		ctx->id = NULL;
		ctx->ids = NULL;
	}

	if (ctx->templates)
	{
		g_slist_foreach (ctx->templates, (GFunc)g_object_unref, NULL);
		g_slist_free (ctx->templates);

		ctx->templates = NULL;
	}

	if (templates)
	{
		gint i;

		for (i = 0; i < templates->len; ++i)
		{
			ctx->templates =
				g_slist_prepend (ctx->templates,
				                 g_object_ref (g_array_index (templates, CpgSelector *, i)));
		}
	}

	ctx->templates = g_slist_reverse (ctx->templates);
}

static gboolean
add_properties (CpgParserContext *context,
                GSList           *context_item,
                CpgObject        *object)
{
	Context *ctx;
	GSList *item;
	GSList *copy;

	ctx = context_item->data;
	copy = g_slist_reverse (g_slist_copy (ctx->properties));

	for (item = copy; item; item = g_slist_next (item))
	{
		CpgProperty *property;
		GError *error = NULL;

		property = cpg_property_copy (item->data);

		if (!cpg_object_add_property (object, property, &error))
		{
			g_slist_free (copy);
			g_object_unref (property);

			return parser_failed_error (context,
			                            error);
		}

		property = cpg_object_get_property (object,
		                                    cpg_property_get_name (item->data));

		cpg_modifiable_set_modified (CPG_MODIFIABLE (property), FALSE);
	}

	g_slist_free (copy);

	return TRUE;
}

static GSList *
context_get_parent (CpgParserContext *context,
                    GSList           *context_item)
{
	GSList *parent;
	GSList *item;

	ensure_parents (context, context_item->next);

	parent = g_slist_copy (CONTEXT (context_item->next->data)->objects);

	if (context->priv->is_template)
	{
		for (item = parent; item; item = g_slist_next (item))
		{
			if ((gpointer)context->priv->network == item->data)
			{
				item->data = cpg_network_get_template_group (context->priv->network);
			}
		}
	}

	return parent;
}

static CpgObject *
parse_object_single_id (CpgParserContext *context,
                        GSList           *context_item,
                        CpgExpansion     *id,
                        CpgGroup         *parent,
                        GType             gtype,
                        gboolean         *new_object)
{
	GSList *templates;
	gchar *missing;
	GSList *item;
	CpgObject *child;
	gboolean ret = TRUE;
	GError *error = NULL;
	Context *ctx;
	GSList *expanded = NULL;
	GSList *expansions;

	expansions = g_slist_prepend (NULL, id);

	ctx = context_item->data;

	for (item = ctx->templates; item; item = g_slist_next (item))
	{
		expanded =
			g_slist_prepend (expanded,
			                 cpg_selector_expand (item->data,
			                                      expansions));
	}

	g_slist_free (expansions);
	expanded = g_slist_reverse (expanded);

	if (!cpg_network_parser_utils_get_templates (context->priv->network,
	                                             parent,
	                                             context->priv->is_template,
	                                             expanded,
	                                             &missing,
	                                             &templates))
	{
		parser_failed (context,
		               CPG_NETWORK_LOAD_ERROR_OBJECT,
		               "Could not find template `%s'",
		               missing);

		g_free (missing);

		g_slist_foreach (templates, (GFunc)cpg_selection_free, NULL);
		g_slist_free (templates);

		g_slist_foreach (expanded, (GFunc)g_object_unref, NULL);
		g_slist_free (expanded);

		return NULL;
	}

	g_slist_foreach (expanded, (GFunc)g_object_unref, NULL);
	g_slist_free (expanded);

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

			g_slist_foreach (templates, (GFunc)cpg_selection_free, NULL);
			g_slist_free (templates);

			return NULL;
		}
	}

	child = cpg_group_get_child (parent, cpg_expansion_get (id, 0));

	if (!child)
	{
		/* Just construct a new object with the right type */
		child = g_object_new (gtype, "id", cpg_expansion_get (id, 0), NULL);
		*new_object = TRUE;
	}
	else if (!g_type_is_a (gtype, G_TYPE_FROM_INSTANCE (child)))
	{
		/* This means the object already existed (this can happen
		   because existing objects created by other templates can be
		   extended) and the type is incorrect */
		parser_failed (context,
		               CPG_NETWORK_LOAD_ERROR_OBJECT,
		               "Cannot extend type %s with type %s",
		               g_type_name (G_TYPE_FROM_INSTANCE (child)),
		               g_type_name (gtype));

		g_slist_free (templates);
		return NULL;
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

			ret = FALSE;
			break;
		}
	}

	g_slist_foreach (templates, (GFunc)cpg_selection_free, NULL);
	g_slist_free (templates);

	if (!ret || !add_properties (context, context_item, child))
	{
		if (*new_object)
		{
			g_object_unref (child);
		}

		return NULL;
	}

	return child;
}

static GSList *
link_pairs (CpgParserContext *context,
            GSList           *context_item,
            CpgExpansion     *id,
            CpgGroup         *parent)
{
	Context *ctx;
	GSList *from;
	GSList *fromobj;
	CpgSelector *fromsel;
	GSList *ret = NULL;
	GSList *expansions;

	expansions = g_slist_prepend (NULL, id);

	ctx = CONTEXT (context_item->data);

	fromsel = cpg_selector_expand (ctx->link_from,
	                               expansions);

	from = cpg_selector_select_states (fromsel,
	                                   CPG_OBJECT (parent));

	if (!from)
	{
		g_slist_free (expansions);
		g_object_unref (fromsel);

		return NULL;
	}

	g_slist_free (expansions);

	for (fromobj = from; fromobj; fromobj = g_slist_next (fromobj))
	{
		CpgObject *fobj;
		CpgSelector *tosel;
		GSList *to;

		fobj = cpg_selection_get_object (fromobj->data);

		expansions = g_slist_copy (cpg_selection_get_expansions (fromobj->data));
		expansions = g_slist_prepend (expansions, id);

		/* Select TO states */
		tosel = cpg_selector_expand (ctx->link_to,
		                             expansions);

		to = cpg_selector_select_link_to (tosel,
		                                  CPG_OBJECT (parent),
		                                  fobj);

		if (!(ctx->link_flags & CPG_PARSER_CONTEXT_LINK_FLAG_ALL) && to)
		{
			ret = g_slist_prepend (ret, fobj);
			ret = g_slist_prepend (ret, cpg_selection_get_object (to->data));

			if (ctx->link_flags & CPG_PARSER_CONTEXT_LINK_FLAG_BIDIRECTIONAL)
			{
				ret = g_slist_prepend (ret, cpg_selection_get_object (to->data));
				ret = g_slist_prepend (ret, fobj);
			}
		}
		else if (ctx->link_flags & CPG_PARSER_CONTEXT_LINK_FLAG_ALL)
		{
			GSList *toitem;

			for (toitem = to; toitem; toitem = g_slist_next (toitem))
			{
				ret = g_slist_prepend (ret, fobj);
				ret = g_slist_prepend (ret, cpg_selection_get_object (toitem->data));

				if (ctx->link_flags & CPG_PARSER_CONTEXT_LINK_FLAG_BIDIRECTIONAL)
				{
					ret = g_slist_prepend (ret, cpg_selection_get_object (toitem->data));
					ret = g_slist_prepend (ret, fobj);
				}
			}
		}

		g_slist_free (expansions);
	}

	g_slist_foreach (from, (GFunc)cpg_selection_free, NULL);
	g_slist_free (from);

	return g_slist_reverse (ret);
}

static GSList *
parse_link_and_connect (CpgParserContext *context,
                        GSList           *context_item,
                        CpgExpansion     *id,
                        CpgGroup         *parent,
                        GType             gtype,
                        GSList          **new_object)
{
	GSList *pairs;
	GSList *item;
	GSList *ret = NULL;
	gboolean multiple;
	gint num = 1;

	/* For each pair FROM -> TO generate a link */
	pairs = link_pairs (context, context_item, id, parent);
	item = pairs;

	multiple = pairs && pairs->next && pairs->next->next;

	while (item)
	{
		CpgObject *from;
		CpgObject *to;
		gboolean no;
		CpgExpansion *realid;
		CpgObject *obj;

		from = item->data;
		item = g_slist_next (item);

		to = item->data;
		item = g_slist_next (item);

		if (cpg_object_get_parent (from) != cpg_object_get_parent (to))
		{
			continue;
		}

		if (multiple)
		{
			/* Alter id to be numeric incremental */
			gchar *newid;

			realid = cpg_expansion_copy (id);
			newid = g_strdup_printf ("%s_%d",
			                         cpg_expansion_get (id, 0),
			                         num);

			cpg_expansion_set (realid, 0, newid);
			g_free (newid);
		}
		else
		{
			realid = id;
		}

		obj = parse_object_single_id (context,
		                              context_item,
		                              realid,
		                              parent,
		                              gtype,
		                              &no);

		if (multiple)
		{
			cpg_expansion_free (realid);
		}

		if (!obj)
		{
			continue;
		}

		++num;

		cpg_link_attach (CPG_LINK (obj), from, to);

		if (new_object)
		{
			*new_object = g_slist_prepend (*new_object,
			                               GINT_TO_POINTER (no));
		}
	}

	if (new_object)
	{
		*new_object = g_slist_reverse (*new_object);
	}

	g_slist_free (pairs);

	return g_slist_reverse (ret);
}

static GSList *
parse_object_single (CpgParserContext  *context,
                     GSList            *context_item,
                     CpgGroup          *parent,
                     GType              gtype,
                     GSList           **new_object)
{
	Context *ctx;
	GSList *ids;
	GSList *ret = NULL;
	gboolean islink;

	ctx = CONTEXT (context_item->data);

	*new_object = NULL;

	islink = g_type_is_a (gtype, CPG_TYPE_LINK) &&
	         ctx->link_from != NULL && ctx->link_to != NULL;

	for (ids = ctx->ids; ids; ids = g_slist_next (ids))
	{
		if (islink)
		{
			GSList *no = NULL;
			GSList *links;

			links = parse_link_and_connect (context,
			                                context_item,
			                                ids->data,
			                                parent,
			                                gtype,
			                                &no);

			*new_object = g_slist_concat (no, *new_object);
			ret = g_slist_prepend (links, ret);
		}
		else
		{
			gboolean no;
			CpgObject *obj;

			obj = parse_object_single_id (context,
			                              context_item,
			                              ids->data,
			                              parent,
			                              gtype,
			                              &no);

			if (!obj)
			{
				g_slist_free (ret);
				g_slist_free (*new_object);

				return NULL;
			}

			ret = g_slist_prepend (ret, obj);

			*new_object = g_slist_prepend (*new_object,
			                               GINT_TO_POINTER (no));
		}
	}

	*new_object = g_slist_reverse (*new_object);
	return g_slist_reverse (ret);
}

static GSList *
parse_object (CpgParserContext  *context,
              GSList            *context_item,
              GType              gtype,
              GSList           **new_object)
{
	GSList *parents;
	GSList *parent;
	GSList *ret = NULL;

	*new_object = NULL;

	parents = context_get_parent (context, context_item);

	for (parent = parents; parent; parent = g_slist_next (parent))
	{
		GSList *objs;
		GSList *nos = NULL;

		objs = parse_object_single (context,
		                            context_item,
		                            parent->data,
		                            gtype,
		                            &nos);

		if (!objs)
		{
			g_slist_free (ret);
			g_slist_free (*new_object);

			*new_object = NULL;

			return NULL;
		}

		ret = g_slist_concat (ret, objs);
		*new_object = g_slist_concat (*new_object, nos);
	}

	return ret;
}

static GSList *
new_object (CpgParserContext *context,
            GSList           *context_item,
            GType             gtype)
{
	GSList *objects;
	GSList *new_object = NULL;
	GSList *i1;
	GSList *i2;

	GSList *parents;
	GSList *parent;

	parents = context_get_parent (context, context_item);
	objects = parse_object (context, context_item, gtype, &new_object);

	i1 = objects;
	i2 = new_object;

	while (i1 && i2)
	{
		CpgObject *obj = i1->data;
		gboolean no = GPOINTER_TO_INT (i2->data);

		if (no)
		{
			for (parent = parents; parent; parent = g_slist_next (parent))
			{
				GError *error = NULL;

				if (!cpg_group_add (parent->data, obj, &error))
				{
					parser_failed_error (context, error);

					g_slist_foreach (objects, (GFunc)g_object_unref, NULL);
					g_slist_free (objects);

					g_slist_free (parents);

					return NULL;
				}
			}
		}

		i1 = g_slist_next (i1);
		i2 = g_slist_next (i2);
	}

	g_slist_free (new_object);
	g_slist_free (parents);

	return objects;
}

static gboolean
ensure_parents (CpgParserContext *context,
                GSList           *item)
{
	Context *ctx;

	ctx = CONTEXT (item->data);

	if (ctx->objects != NULL)
	{
		return TRUE;
	}

	if (!ensure_parents (context, item->next))
	{
		return FALSE;
	}

	/* Create a group */
	ctx->objects = new_object (context, item, CPG_TYPE_GROUP);

	if (!ctx->objects)
	{
		return FALSE;
	}

	return TRUE;
}

static gboolean
parse_group_single (CpgParserContext *context,
                    GSList           *context_item,
                    CpgObject        *object)
{
	Context *ctx;
	CpgGroup *grp;
	CpgPropertyInterface *iface;
	GSList *item;
	GSList *copy;

	if (!CPG_IS_GROUP (object))
	{
		return TRUE;
	}

	ctx = context_item->data;
	grp = CPG_GROUP (object);

	/* TODO: proxy */

	iface = cpg_group_get_property_interface (grp);
	copy = g_slist_reverse (g_slist_copy (ctx->interface_items));

	for (item = copy; item; item = g_slist_next (item))
	{
		InterfaceItem *it;
		GSList *properties;
		GError *error = NULL;
		CpgProperty *property;

		it = item->data;

		properties = cpg_selector_select_properties (it->target,
		                                             CPG_OBJECT (grp));

		if (!properties)
		{
			g_slist_free (copy);

			return parser_failed (context,
			                      CPG_NETWORK_LOAD_ERROR_INTERFACE,
			                      "Could not find interface target for interface property `%s' on `%s'",
			                      it->name,
			                      cpg_object_get_id (CPG_OBJECT (grp)));
		}

		property = cpg_selection_get_property (properties->data);

		g_slist_foreach (properties, (GFunc)cpg_selection_free, NULL);
		g_slist_free (properties);

		if (!cpg_property_interface_add (iface,
		                                 it->name,
		                                 property,
		                                 &error))
		{
			parser_failed_error (context,
			                     error);

			return FALSE;
		}
	}

	g_slist_free (copy);

	return TRUE;
}

static gboolean
parse_group (CpgParserContext *context,
             GSList           *context_item)
{
	Context *ctx;
	GSList *item;

	ctx = context_item->data;

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		if (!parse_group_single (context, context_item, item->data))
		{
			return FALSE;
		}
	}

	return TRUE;
}

static GSList *
create_state (CpgParserContext *context)
{
	Context *ctx;

	ctx = CURRENT_CONTEXT (context);

	if (ctx->objects == NULL)
	{
		ctx->objects = new_object (context,
		                           context->priv->context_stack,
		                           CPG_TYPE_OBJECT);
	}

	if (ctx->objects == NULL)
	{
		return NULL;
	}

	if (!parse_group (context, context->priv->context_stack))
	{
		return NULL;
	}

	return ctx->objects;
}

static gboolean
parse_link_single (CpgParserContext *context,
                   GSList           *context_item,
                   CpgObject        *object)
{
	GSList *item;
	Context *ctx;
	CpgLink *link;

	if (!CPG_IS_LINK (object))
	{
		return TRUE;
	}

	ctx = context_item->data;
	link = CPG_LINK (object);

	for (item = ctx->actions; item; item = g_slist_next (item))
	{
		CpgLinkAction *action;

		action = item->data;

		cpg_link_add_action (link, cpg_link_action_copy (action));
	}

	return TRUE;
}

static gboolean
parse_link (CpgParserContext *context,
            GSList           *context_item)
{
	Context *ctx;
	GSList *item;

	ctx = context_item->data;

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		if (!parse_link_single (context, context_item, item->data))
		{
			return FALSE;
		}
	}

	return TRUE;
}

static GSList *
create_link (CpgParserContext *context)
{
	Context *ctx;

	ctx = CURRENT_CONTEXT (context);

	if (ctx->objects == NULL)
	{
		ctx->objects = new_object (context,
		                           context->priv->context_stack,
		                           CPG_TYPE_LINK);
	}

	if (ctx->objects == NULL)
	{
		return NULL;
	}

	if (parse_link (context, context->priv->context_stack))
	{
		return ctx->objects;
	}
	else
	{
		return NULL;
	}
}

CpgObject *
cpg_parser_context_pop_scope (CpgParserContext *context)
{
	Context *ctx;
	GSList *rett = NULL;
	CpgObject *ret = NULL;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (context->priv->context_stack, NULL);

	ctx = CURRENT_CONTEXT (context);

	switch (ctx->scope)
	{
		case CPG_PARSER_CONTEXT_SCOPE_NETWORK:
			add_properties (context,
			                context->priv->context_stack,
			                CPG_OBJECT (context->priv->network));

			ret = CPG_OBJECT (context->priv->network);
		break;
		case CPG_PARSER_CONTEXT_SCOPE_STATE:
			rett = create_state (context);
		break;
		case CPG_PARSER_CONTEXT_SCOPE_LINK:
			rett = create_link (context);
		break;
		default:
		break;
	}

	context_free (context->priv->context_stack->data);

	context->priv->context_stack =
		g_slist_delete_link (context->priv->context_stack,
		                     context->priv->context_stack);

	return ret ? ret : (rett ? rett->data : NULL);
}

void
cpg_parser_context_pop_template (CpgParserContext *context)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	context->priv->is_template = FALSE;
}

void
cpg_parser_context_push_template (CpgParserContext *context)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	context->priv->is_template = TRUE;
}

static gboolean
is_template (CpgParserContext *context,
             CpgObject        *obj)
{
	CpgObject *template_group;

	template_group = CPG_OBJECT (cpg_network_get_template_group (context->priv->network));
	obj = cpg_object_get_parent (obj);

	while (obj)
	{
		if (obj == template_group)
		{
			return TRUE;
		}

		obj = cpg_object_get_parent (obj);
	}

	return FALSE;
}

static CpgObject *
create_from_template (CpgGroup  *parent,
                      CpgObject *temp,
                      GType      gtype)
{
	CpgObject *obj;

	obj = g_object_new (gtype, "id", cpg_object_get_id (temp), NULL);
	cpg_object_apply_template (obj, temp, NULL);

	if (parent)
	{
		cpg_group_add (parent, obj, NULL);
	}

	return obj;
}

gboolean
cpg_parser_context_link_one (CpgParserContext *context,
                             CpgLink          *link,
                             CpgSelector      *from,
                             CpgSelector      *to,
                             CpgParserContextLinkFlags flags)
{
	GSList *fromobjs;
	GSList *fromobj;
	GSList *parent;
	gboolean link_is_template;
	gint numfrom = 0;
	gboolean all_to_all;
	gboolean bidi;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), FALSE);
	g_return_val_if_fail (CPG_IS_LINK (link), FALSE);
	g_return_val_if_fail (CPG_IS_SELECTOR (from), FALSE);
	g_return_val_if_fail (CPG_IS_SELECTOR (to), FALSE);

	ensure_parents (context, context->priv->context_stack);
	parent = CURRENT_CONTEXT (context)->objects;

	link_is_template = !context->priv->is_template &&
	                   is_template (context, CPG_OBJECT (link));

	all_to_all = (flags & CPG_PARSER_CONTEXT_LINK_FLAG_ALL);
	bidi = (flags & CPG_PARSER_CONTEXT_LINK_FLAG_BIDIRECTIONAL);

	while (parent)
	{
		fromobjs = cpg_selector_select_states (from, parent->data);

		for (fromobj = fromobjs; fromobj; fromobj = g_slist_next (fromobj))
		{
			GSList *toobjs;
			GSList *tostart;
			GSList *toobj;
			CpgObject *fromparent;

			fromparent = cpg_object_get_parent (cpg_selection_get_object (fromobj->data));

			toobjs = cpg_selector_select_link_to (to,
			                                      parent->data,
			                                      cpg_selection_get_object (fromobj->data));

			if (!all_to_all)
			{
				tostart = g_slist_nth (toobjs, numfrom);
			}
			else
			{
				tostart = toobjs;
			}

			for (toobj = tostart; toobj; toobj = g_slist_next (toobj))
			{
				CpgLink *reallink;

				if (link_is_template)
				{
					reallink = CPG_LINK (create_from_template (CPG_GROUP (fromparent),
					                                           CPG_OBJECT (link),
					                                           CPG_TYPE_LINK));
				}
				else
				{
					reallink = link;
				}

				cpg_link_attach (reallink,
				                 cpg_selection_get_object (fromobj->data),
				                 cpg_selection_get_object (toobj->data));

				if (bidi)
				{
					if (link_is_template)
					{
						reallink = CPG_LINK (create_from_template (CPG_GROUP (fromparent),
						                                           CPG_OBJECT (link),
						                                           CPG_TYPE_LINK));
					}

					cpg_link_attach (reallink,
					                 cpg_selection_get_object (toobj->data),
					                 cpg_selection_get_object (fromobj->data));
				}
				else
				{
					reallink = link;
				}

				if (!all_to_all)
				{
					break;
				}
			}

			++numfrom;

			g_slist_foreach (toobjs, (GFunc)cpg_selection_free, NULL);
			g_slist_free (toobjs);
		}

		g_slist_foreach (fromobjs, (GFunc)cpg_selection_free, NULL);
		g_slist_free (fromobjs);

		parent = g_slist_next (parent);
	}

	return TRUE;
}

gboolean
cpg_parser_context_link (CpgParserContext *context,
                         CpgSelector      *link,
                         CpgSelector      *from,
                         CpgSelector      *to,
                         CpgParserContextLinkFlags flags)
{
	GSList *linkobjs;
	GSList *linkobj;
	GSList *parent;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), FALSE);
	g_return_val_if_fail (CPG_IS_SELECTOR (link), FALSE);
	g_return_val_if_fail (CPG_IS_SELECTOR (from), FALSE);
	g_return_val_if_fail (CPG_IS_SELECTOR (to), FALSE);

	ensure_parents (context, context->priv->context_stack);
	parent = CURRENT_CONTEXT (context)->objects;

	while (parent)
	{
		linkobjs = cpg_selector_select_links (link,
		                                      parent->data);

		if (!linkobjs)
		{
			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_LINK,
			               "Could not find link `%s'",
			               cpg_selector_as_string (link));

			return FALSE;
		}

		for (linkobj = linkobjs; linkobj; linkobj = g_slist_next (linkobj))
		{
			if (!cpg_parser_context_link_one (context,
			                                  CPG_LINK (cpg_selection_get_object (linkobj->data)),
			                                  from,
			                                  to,
			                                  flags))
			{
				return FALSE;
			}
		}

		g_slist_free (linkobjs);

		parent = g_slist_next (parent);
	}

	return TRUE;
}

void
cpg_parser_context_import (CpgParserContext *context,
                           gchar const      *id,
                           gchar const      *path)
{
	GFile *file = NULL;
	GSList *parent;
	CpgImport *import;
	GError *error = NULL;
	GFile *curfile;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (id != NULL);
	g_return_if_fail (path != NULL);

	curfile = cpg_parser_context_get_file (context);

	if (curfile)
	{
		file = cpg_network_parser_utils_resolve_import (curfile,
		                                                path);
		g_object_unref (curfile);
	}

	if (!file)
	{
		parser_failed (context,
		               CPG_NETWORK_LOAD_ERROR_IMPORT,
		               "File `%s' for import `%s' could not be found",
		               path,
		               id);

		return;
	}

	parent = context_get_parent (context, context->priv->context_stack);

	while (parent)
	{
		if (context->priv->is_template)
		{
			CpgGroup *template_group;

			template_group = cpg_network_get_template_group (context->priv->network);
			import = cpg_network_parser_utils_find_template_import (CPG_OBJECT (template_group), file);

			if (import)
			{
				CpgImportAlias *alias;

				alias = cpg_import_alias_new (import);

				if (!cpg_group_add (parent->data,
				                    CPG_OBJECT (alias),
				                    &error))
				{
					parser_failed_error (context, error);
				}

				g_object_unref (alias);
				return;
			}
		}

		import = cpg_import_new (context->priv->network,
		                         parent->data,
		                         id,
		                         file,
		                         &error);

		if (!import)
		{
			parser_failed_error (context, error);
			return;
		}

		g_object_unref (import);
		parent = g_slist_next (parent);
	}
}

static CpgSelector *
ensure_selector (CpgParserContext *context)
{
	if (!context->priv->selector)
	{
		context->priv->selector = cpg_selector_new ();
	}

	return context->priv->selector;
}

void
cpg_parser_context_push_selector (CpgParserContext *context,
                                  gchar const      *identifier)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (identifier != NULL);

	cpg_selector_add (ensure_selector (context), identifier);
}

void
cpg_parser_context_push_selector_regex (CpgParserContext *context,
                                        gchar const      *regex)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (regex != NULL);

	cpg_selector_add_regex (ensure_selector (context), regex);
}

void
cpg_parser_context_push_selector_pseudo (CpgParserContext *context,
                                         gchar const      *identifier,
                                         GArray           *argument)
{
	gchar **args;
	GPtrArray *ptr;
	gint i;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (identifier != NULL);

	ptr = g_ptr_array_new ();

	if (argument)
	{
		for (i = 0; i < argument->len; ++i)
		{
			g_ptr_array_add (ptr, g_strdup (g_array_index (argument, gchar const *, i)));
		}
	}

	g_ptr_array_add (ptr, NULL);
	args = (gchar **)g_ptr_array_free (ptr, FALSE);

	cpg_selector_add_pseudo (ensure_selector (context),
	                         identifier,
	                         (gchar const * const *)args);

	g_strfreev (args);
}

CpgSelector *
cpg_parser_context_pop_selector (CpgParserContext *context)
{
	CpgSelector *ret;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (context->priv->selector != NULL, NULL);

	ret = context->priv->selector;
	context->priv->selector = NULL;

	return ret;
}

void
cpg_parser_context_apply_template (CpgParserContext *context,
                                   CpgSelector      *objects,
                                   CpgSelector      *templates)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CPG_IS_SELECTOR (objects));
	g_return_if_fail (CPG_IS_SELECTOR (templates));

	/* TODO */
}

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
                          GError           **error)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), FALSE);
	g_return_val_if_fail (context->priv->inputs, FALSE);

	if (cpg_parser_parse (context) == 0)
	{
		return TRUE;
	}
	else
	{
		if (error && context->priv->error)
		{
			*error = g_error_copy (context->priv->error);
		}

		return FALSE;
	}
}

void
cpg_parser_context_set_line (CpgParserContext *context,
                             gchar const      *line,
                             gint              lineno)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	g_free (context->priv->line);

	context->priv->line = g_strdup (line);
	context->priv->lineno = lineno;
}

void
cpg_parser_context_set_column (CpgParserContext *context,
                               gint              start,
                               gint              end)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	context->priv->cstart = start;
	context->priv->cend = end;
}

gchar const *
cpg_parser_context_get_line (CpgParserContext *context,
                             gint             *lineno)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	if (lineno)
	{
		*lineno = context->priv->lineno;
	}

	return context->priv->line;
}

void
cpg_parser_context_get_column (CpgParserContext *context,
                               gint             *start,
                               gint             *end)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (start)
	{
		*start = context->priv->cstart;
	}

	if (end)
	{
		*end = context->priv->cend;
	}
}

void
cpg_parser_context_set_token (CpgParserContext *context,
                              gchar const      *token)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	g_free (context->priv->token);
	context->priv->token = g_strdup (token);
}

gchar const *
cpg_parser_context_get_token (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	return context->priv->token;
}

void
cpg_parser_context_define (CpgParserContext *context,
                           gchar const      *name,
                           gchar const      *define)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (define != NULL);

	g_hash_table_insert (context->priv->defines,
	                     g_strdup (name),
	                     g_strdup (define));
}

gchar const *
cpg_parser_context_lookup_define (CpgParserContext *context,
                                  gchar const      *define)
{
	gchar const *ret;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (define != NULL, NULL);

	ret = g_hash_table_lookup (context->priv->defines, define);

	return ret ? ret : "";
}

static gboolean
remove_selector (CpgParserContext *context,
                 CpgSelector      *selector)
{
	GSList *ret;
	GSList *item;
	gboolean val = TRUE;

	ret = cpg_selector_select (selector,
	                           CPG_OBJECT (context->priv->network));

	for (item = ret; item; item = g_slist_next (item))
	{
		CpgObject *parent;
		GError *error = NULL;

		parent = cpg_object_get_parent (cpg_selection_get_object (item->data));

		if (!parent)
		{
			val = FALSE;
			break;
		}

		if (!cpg_group_remove (CPG_GROUP (parent),
		                       cpg_selection_get_object (item->data),
		                       &error))
		{
			parser_failed_error (context, error);
			val = FALSE;

			break;
		}
	}

	g_slist_foreach (ret, (GFunc)cpg_selection_free, NULL);
	g_slist_free (ret);

	return val;
}

void
cpg_parser_context_remove (CpgParserContext *context,
                           GArray           *selectors)
{
	gint i;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (selectors != NULL);

	for (i = 0; i < selectors->len; ++i)
	{
		if (!remove_selector (context,
		                      g_array_index (selectors,
		                                     CpgSelector *,
		                                     i)))
		{
			break;
		}
	}
}

void
cpg_parser_context_set_integrator (CpgParserContext *context,
                                   gchar const      *integrator)
{
	GType type;
	CpgIntegrator *it;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (integrator != NULL);

	type = cpg_integrators_find (integrator);

	if (type == G_TYPE_INVALID)
	{
		parser_failed (context,
		               CPG_NETWORK_LOAD_ERROR_SYNTAX,
		               "Could not find integrator `%s'",
		               integrator);

		return;
	}

	it = g_object_new (type, NULL);
	cpg_network_set_integrator (context->priv->network, it);
	g_object_unref (it);
}

void
cpg_parser_context_push_input (CpgParserContext *context,
                               GFile            *file,
                               GInputStream     *stream)
{
	InputItem *item;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (file != NULL || stream != NULL);

	item = input_item_new (file, stream);

	context->priv->inputs = g_slist_prepend (context->priv->inputs,
	                                         item);
}

void
cpg_parser_context_push_input_from_path (CpgParserContext *context,
                                         gchar const      *filename)
{
	GFile *file = NULL;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (filename != NULL);

	if (g_path_is_absolute (filename))
	{
		file = g_file_new_for_path (filename);
	}
	else
	{
		GSList *item;

		for (item = context->priv->inputs; item; item = g_slist_next (item))
		{
			InputItem *ip = item->data;

			if (!ip->file)
			{
				continue;
			}

			file = g_file_resolve_relative_path (ip->file, filename);
			break;
		}

		if (!file)
		{
			file = g_file_new_for_commandline_arg (filename);
		}
	}

	cpg_parser_context_push_input (context, file, NULL);
	g_object_unref (file);
}

void
cpg_parser_context_pop_input (CpgParserContext *context)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->inputs)
	{
		input_item_free (context->priv->inputs->data);

		context->priv->inputs = g_slist_delete_link (context->priv->inputs,
		                                             context->priv->inputs);
	}
}

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

static void
import_define (gchar const *key,
               gchar const *value,
               CpgObject   *ret)
{
	cpg_object_add_property (ret,
	                         cpg_property_new (key,
	                                           value,
	                                           CPG_PROPERTY_FLAG_NONE),
	                         NULL);
}

static CpgObject *
create_context (CpgParserContext *context)
{
	CpgObject *ret;

	ret = cpg_object_new ("s");

	g_hash_table_foreach (context->priv->defines,
	                      (GHFunc)import_define,
	                      ret);


	if (!cpg_object_compile (ret, NULL, NULL))
	{
		return NULL;
	}

	return ret;
}

gdouble
cpg_parser_context_calculate (CpgParserContext *context,
                              gchar const      *expression)
{
	CpgObject *obj;
	CpgExpression *expr;
	gdouble ret;
	CpgCompileContext *ctx;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), 0.0);
	g_return_val_if_fail (expression != NULL, 0.0);

	obj = create_context (context);

	expr = cpg_expression_new (expression);

	ctx = cpg_compile_context_new ();
	cpg_compile_context_prepend_object (ctx, obj);

	if (!cpg_expression_compile (expr, ctx, NULL))
	{
		ret = 0.0;
	}
	else
	{
		ret = cpg_expression_evaluate (expr);
	}

	g_object_unref (obj);
	g_object_unref (expr);
	g_object_unref (ctx);

	return ret;
}

gchar *
cpg_parser_context_calculate_str (CpgParserContext *context,
                                  gchar const      *expression)
{
	gdouble val;
	gchar buf[G_ASCII_DTOSTR_BUF_SIZE];

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (expression != NULL, NULL);

	val = cpg_parser_context_calculate (context, expression);

	g_ascii_dtostr (buf, G_ASCII_DTOSTR_BUF_SIZE, val);
	return g_strdup (buf);
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
cpg_parser_context_set_link (CpgParserContext *context,
                             CpgParserContextLinkFlags flags,
                             GArray           *fromto)
{
	Context *ctx;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	ctx = CURRENT_CONTEXT (context);

	if (fromto == NULL)
	{
		ctx->link_from = NULL;
		ctx->link_to = NULL;

		return;
	}

	ctx->link_flags = flags;

	ctx->link_from = g_array_index (fromto, CpgSelector *, 0);
	ctx->link_to = g_array_index (fromto, CpgSelector *, 1);
}
