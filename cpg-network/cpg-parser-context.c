#include "cpg-parser-context.h"
#include "cpg-network-parser-utils.h"

#define CPG_PARSER_CONTEXT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_PARSER_CONTEXT, CpgParserContextPrivate))

typedef struct
{
	gchar *name;
	gchar *target;
} InterfaceItem;

typedef struct
{
	gchar *id;
	CpgParserContextScope scope;
	CpgObject *object;

	GSList *properties;
	GPtrArray *identifiers;
	GSList *interface_items;
	GSList *actions;
} Context;

struct _CpgParserContextPrivate
{
	CpgNetwork *network;
	GFile *file;

	/* Stack of Context */
	GSList *context_stack;

	gboolean is_template;

	GError *error;
	gboolean error_occurred;
};

G_DEFINE_TYPE (CpgParserContext, cpg_parser_context, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NETWORK,
	PROP_FILE
};

#define CONTEXT(x) ((Context *)x)
#define CURRENT_CONTEXT(context) CONTEXT(context->priv->context_stack->data)

static gboolean ensure_parents (CpgParserContext *context, GSList *item);

static InterfaceItem *
interface_item_new (gchar const *name,
                    gchar const *target)
{
	InterfaceItem *item;

	item = g_slice_new (InterfaceItem);

	item->name = g_strdup (name);
	item->target = g_strdup (target);

	return item;
}

static void
interface_item_free (InterfaceItem *item)
{
	g_free (item->name);
	g_free (item->target);

	g_slice_free (InterfaceItem, item);
}

static Context *
context_new (gchar const           *id,
             CpgParserContextScope  scope)
{
	Context *ret;

	ret = g_slice_new0 (Context);

	ret->id = g_strdup (id);
	ret->scope = scope;

	return ret;
}

static void
context_free (Context *context)
{
	g_slist_foreach (context->properties, (GFunc)g_object_unref, NULL);
	g_slist_free (context->properties);

	g_ptr_array_free (context->identifiers, TRUE);

	g_slist_foreach (context->interface_items, (GFunc)interface_item_free, NULL);
	g_slist_free (context->interface_items);

	g_slist_foreach (context->actions, (GFunc)g_object_unref, NULL);
	g_slist_free (context->actions);

	g_free (context->id);

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

	if (self->priv->file)
	{
		g_object_unref (self->priv->file);
	}

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
		case PROP_FILE:
		{
			GFile *file = g_value_get_object (value);

			if (file)
			{
				self->priv->file = g_file_dup (file);
			}

			break;
		}
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
		case PROP_FILE:
			g_value_take_object (value, g_file_dup (self->priv->file));
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_parser_context_class_init (CpgParserContextClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_parser_context_finalize;

	object_class->get_property = cpg_parser_context_get_property;
	object_class->set_property = cpg_parser_context_set_property;

	g_type_class_add_private (object_class, sizeof(CpgParserContextPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_NETWORK,
	                                 g_param_spec_object ("network",
	                                                      "Network",
	                                                      "Network",
	                                                      CPG_TYPE_NETWORK,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (object_class,
	                                 PROP_FILE,
	                                 g_param_spec_object ("file",
	                                                      "File",
	                                                      "File",
	                                                      G_TYPE_FILE,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cpg_parser_context_init (CpgParserContext *self)
{
	self->priv = CPG_PARSER_CONTEXT_GET_PRIVATE (self);

	cpg_parser_context_push_scope (self,
	                               NULL,
	                               CPG_PARSER_CONTEXT_SCOPE_NONE);

	CURRENT_CONTEXT (self)->object = CPG_OBJECT (self->priv->network);
}

CpgParserContext *
cpg_parser_context_new (CpgNetwork *network, GFile *file)
{
	return g_object_new (CPG_TYPE_PARSER_CONTEXT,
	                     "network", network,
	                     "file", file,
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

void
cpg_parser_context_add_identifier (CpgParserContext *context,
                                   gchar const      *name)
{
	Context *ctx;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);

	ctx = CURRENT_CONTEXT (context);

	g_ptr_array_index (ctx->identifiers, ctx->identifiers->len - 1) = g_strdup (name);
	g_ptr_array_add (ctx->identifiers, NULL);
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
                                  gchar const      *target)
{
	InterfaceItem *item;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (target != NULL);

	item = interface_item_new (name, target);

	CURRENT_CONTEXT (context)->interface_items =
		g_slist_prepend (CURRENT_CONTEXT (context)->interface_items,
		                 item);
}

void
cpg_parser_context_error (CpgParserContext *context,
                          gint              lineno,
                          gchar const      *message)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	context->priv->error_occurred = TRUE;
}

void
cpg_parser_context_push_scope (CpgParserContext      *context,
                               gchar const           *id,
                               CpgParserContextScope  scope)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	context->priv->context_stack =
		g_slist_prepend (context->priv->context_stack,
		                 context_new (id, scope));
}

static gboolean
parser_failed_error (CpgParserContext *context,
                     GError           *error)
{
	context->priv->error = error;

	/* TODO */

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

		g_error_free (error);

		va_end (ap);
	}

	return FALSE;
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

		property = item->data;

		if (!cpg_object_add_property (object, property, &error))
		{
			g_slist_free (copy);

			return parser_failed_error (context,
			                            error);
		}

		property = cpg_object_get_property (object,
		                                    cpg_property_get_name (item->data));

		cpg_modifiable_set_modified (CPG_MODIFIABLE (property), FALSE);
	}

	g_slist_foreach (ctx->properties, (GFunc)g_object_unref, NULL);
	g_slist_free (ctx->properties);

	ctx->properties = NULL;

	g_slist_free (copy);

	return TRUE;
}

static CpgGroup *
context_get_parent (CpgParserContext *context,
                    GSList           *context_item)
{
	CpgGroup *parent;

	ensure_parents (context, context_item->next);

	parent = CPG_GROUP (CONTEXT (context_item->next)->object);

	if (context->priv->is_template && CPG_GROUP (context->priv->network) == parent)
	{
		parent = cpg_network_get_template_group (context->priv->network);
	}

	return parent;
}

static CpgObject *
parse_object (CpgParserContext *context,
              GSList           *context_item,
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
	CpgGroup *parent;

	ctx = context_item->data;
	parent = context_get_parent (context, context_item);

	if (!cpg_network_parser_utils_get_templates (context->priv->network,
	                                             parent,
	                                             context->priv->is_template,
	                                             (gchar const * const *)ctx->identifiers->pdata,
	                                             &missing,
	                                             &templates))
	{
		parser_failed (context,
		               CPG_NETWORK_LOAD_ERROR_OBJECT,
		               "Could not find template `%s'",
		               missing);

		g_free (missing);
		g_slist_free (templates);

		return NULL;
	}

	gtype = cpg_network_parser_utils_type_from_templates (gtype, templates);

	/* Check if the template types can actually be applied to the
	   object type that we are constructing. Only template types
	   which are superclasses of the new object type can be
	   applied */
	for (item = templates; item; item = g_slist_next (item))
	{
		GType template_type = G_TYPE_FROM_INSTANCE (item->data);

		if (!g_type_is_a (gtype, template_type))
		{
			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_OBJECT,
			               "Referenced template is of incorrect type %s (need %s)",
			               g_type_name (template_type),
			               g_type_name (gtype));

			g_slist_free (templates);

			return NULL;
		}
	}

	child = cpg_group_get_child (parent, ctx->id);

	if (!child)
	{
		/* Just construct a new object with the right type */
		child = g_object_new (gtype, "id", ctx->id, NULL);
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
		if (!cpg_object_apply_template (child, item->data, &error))
		{
			parser_failed_error (context,
			                     error);

			ret = FALSE;
			break;
		}
	}

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

static CpgObject *
new_object (CpgParserContext *context,
            GSList           *context_item,
            GType             gtype)
{
	CpgObject *object;
	gboolean new_object;
	CpgGroup *parent;

	parent = context_get_parent (context, context_item);
	object = parse_object (context, context_item, gtype, &new_object);

	if (object)
	{
		if (new_object)
		{
			if (!cpg_group_add (parent, object, NULL))
			{
				g_object_unref (object);
				return NULL;
			}

			g_object_unref (object);
		}

		return object;
	}
	else
	{
		return NULL;
	}
}

static gboolean
ensure_parents (CpgParserContext *context,
                GSList           *item)
{
	Context *ctx;

	ctx = CONTEXT (item->data);

	if (ctx->object != NULL)
	{
		return TRUE;
	}

	if (!ensure_parents (context, item->next))
	{
		return FALSE;
	}

	/* Create a group */
	ctx->object = new_object (context, item, CPG_TYPE_GROUP);

	if (!ctx->object)
	{
		return FALSE;
	}

	return TRUE;
}

static gboolean
parse_group (CpgParserContext *context,
             GSList           *context_item)
{
	Context *ctx;
	CpgGroup *grp;
	CpgPropertyInterface *iface;
	GSList *item;
	GSList *copy;

	ctx = context_item->data;
	grp = CPG_GROUP (ctx->object);

	/* TODO: proxy */

	iface = cpg_group_get_property_interface (grp);
	copy = g_slist_reverse (g_slist_copy (ctx->interface_items));

	for (item = copy; item; item = g_slist_next (item))
	{
		InterfaceItem *it;
		CpgProperty *property;
		GError *error = NULL;

		it = item->data;

		property = cpg_group_find_property (grp, it->target);

		if (!property)
		{
			g_slist_free (copy);

			return parser_failed (context,
			                      CPG_NETWORK_LOAD_ERROR_INTERFACE,
			                      "Could not find interface target `%s' for interface property `%s' on `%s'",
			                      it->target,
			                      it->name,
			                      cpg_object_get_id (CPG_OBJECT (grp)));
		}

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

	g_slist_foreach (ctx->interface_items, (GFunc)interface_item_free, NULL);
	g_slist_free (ctx->interface_items);
	ctx->interface_items = NULL;

	g_slist_free (copy);

	return TRUE;
}

static CpgObject *
create_state (CpgParserContext *context)
{
	Context *ctx;

	/* Create a group if necessary */
	ctx = CURRENT_CONTEXT (context);

	if (ctx->object == NULL)
	{
		ctx->object = new_object (context,
		                          context->priv->context_stack,
		                          CPG_TYPE_OBJECT);
	}

	if (ctx->object == NULL)
	{
		return NULL;
	}

	if (CPG_IS_GROUP (ctx->object))
	{
		if (!parse_group (context, context->priv->context_stack))
		{
			return NULL;
		}
	}

	return ctx->object;
}

static gboolean
parse_link (CpgParserContext *context,
            GSList           *context_item)
{
	GSList *item;
	Context *ctx;
	CpgLink *link;

	ctx = context_item->data;
	link = CPG_LINK (ctx->object);

	for (item = ctx->actions; item; item = g_slist_next (item))
	{
		CpgLinkAction *action;

		action = item->data;

		cpg_link_add_action (link, action);

		g_object_unref (action);
	}

	g_slist_free (ctx->actions);
	ctx->actions = NULL;

	return FALSE;
}

static CpgObject *
create_link (CpgParserContext *context)
{
	Context *ctx;

	ctx = CURRENT_CONTEXT (context);

	ctx->object = new_object (context,
	                          context->priv->context_stack,
	                          CPG_TYPE_LINK);

	if (ctx->object == NULL)
	{
		return FALSE;
	}

	if (parse_link (context, context->priv->context_stack))
	{
		return ctx->object;
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
			ret = create_state (context);
		break;
		case CPG_PARSER_CONTEXT_SCOPE_LINK:
			ret = create_link (context);
		break;
		default:
		break;
	}

	context_free (context->priv->context_stack->data);

	context->priv->context_stack =
		g_slist_delete_link (context->priv->context_stack,
		                     context->priv->context_stack);

	return ret;
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

void
cpg_parser_context_link (CpgParserContext *context,
                         gchar const      *linkid,
                         gchar const      *from,
                         gchar const      *to)
{
	CpgObject *link;
	CpgObject *fromobj;
	CpgGroup *parent;
	CpgObject *toobj;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (linkid != NULL);
	g_return_if_fail (from != NULL);
	g_return_if_fail (to != NULL);

	parent = context_get_parent (context, context->priv->context_stack);
	link = cpg_group_get_child (parent, linkid);

	if (!link)
	{
		parser_failed (context,
		               CPG_NETWORK_LOAD_ERROR_LINK,
		               "Could not find link `%s'",
		               linkid);

		return;
	}

	fromobj = cpg_group_get_child (parent, from);

	if (!fromobj)
	{
		parser_failed (context,
		               CPG_NETWORK_LOAD_ERROR_LINK,
		               "The `from' object `%s' could not be found for link `%s'",
		               from,
		               linkid);

		return;
	}
	else if (CPG_IS_LINK (fromobj))
	{
		parser_failed (context,
		               CPG_NETWORK_LOAD_ERROR_LINK,
		               "The `from' object `%s' cannot be a link for link `%s'",
		               from,
		               linkid);

		return;
	}

	toobj = cpg_group_get_child (parent, to);

	if (!toobj)
	{
		parser_failed (context,
		               CPG_NETWORK_LOAD_ERROR_LINK,
		               "The `to' object `%s' could not be found for link `%s'",
		               to,
		               linkid);

		return;
	}
	else if (CPG_IS_LINK (toobj))
	{
		parser_failed (context,
		               CPG_NETWORK_LOAD_ERROR_LINK,
		               "The `to' object `%s' cannot be a link for link `%s'",
		               to,
		               linkid);

		return;
	}

	cpg_link_attach (CPG_LINK (link), fromobj, toobj);
}

void
cpg_parser_context_import (CpgParserContext *context,
                           gchar const      *id,
                           gchar const      *path,
                           gboolean          is_template)
{
	GFile *file;
	CpgGroup *parent;
	CpgImport *import;
	GError *error = NULL;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (id != NULL);
	g_return_if_fail (path != NULL);

	file = cpg_network_parser_utils_resolve_import (context->priv->file,
	                                                path);

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

	if (is_template || context->priv->is_template)
	{
		CpgGroup *template_group;

		template_group = cpg_network_get_template_group (context->priv->network);
		import = cpg_network_parser_utils_find_template_import (CPG_OBJECT (template_group), file);

		if (import)
		{
			CpgImportAlias *alias;

			alias = cpg_import_alias_new (import);

			if (!cpg_group_add (parent,
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
	                         parent,
	                         id,
	                         file,
	                         &error);

	if (!import)
	{
		parser_failed_error (context, error);
		return;
	}

	g_object_unref (import);
}
