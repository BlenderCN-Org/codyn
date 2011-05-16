#include "cpg-parser-context.h"
#include "cpg-network-parser-utils.h"
#include "cpg-integrators.h"

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
	gchar *id;
	GSList *ids;

	GPtrArray *templates;

	CpgParserContextScope scope;
	GSList *objects;

	GSList *properties;
	GSList *interface_items;
	GSList *actions;
} Context;

struct _CpgParserContextPrivate
{
	CpgNetwork *network;
	GFile *file;
	GInputStream *stream;
	gpointer scanner;
	GHashTable *defines;

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
	g_slist_foreach (context->ids, (GFunc)cpg_expanded_id_free, NULL);
	g_slist_free (context->ids);

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

	g_free (self->priv->line);

	if (self->priv->stream)
	{
		g_object_unref (self->priv->stream);
	}

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
		case PROP_FILE:
		{
			GFile *file = g_value_get_object (value);

			if (file)
			{
				self->priv->file = g_file_dup (file);
			}

			break;
		}
		case PROP_STREAM:
			self->priv->stream = g_value_dup_object (value);
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
		case PROP_FILE:
			g_value_take_object (value, g_file_dup (self->priv->file));
			break;
		case PROP_STREAM:
			g_value_set_object (value, self->priv->stream);
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

	g_object_class_install_property (object_class,
	                                 PROP_FILE,
	                                 g_param_spec_object ("file",
	                                                      "File",
	                                                      "File",
	                                                      G_TYPE_FILE,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (object_class,
	                                 PROP_STREAM,
	                                 g_param_spec_object ("stream",
	                                                      "Stream",
	                                                      "Stream",
	                                                      G_TYPE_INPUT_STREAM,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cpg_parser_context_init (CpgParserContext *self)
{
	self->priv = CPG_PARSER_CONTEXT_GET_PRIVATE (self);
}

CpgParserContext *
cpg_parser_context_new (CpgNetwork   *network,
                        GFile        *file)
{
	return g_object_new (CPG_TYPE_PARSER_CONTEXT,
	                     "network", network,
	                     "file", file,
	                     NULL);
}

CpgParserContext *
cpg_parser_context_new_for_path (CpgNetwork   *network,
                                 gchar const  *path)
{
	GFile *file;
	CpgParserContext *ret;

	file = g_file_new_for_path (path);
	ret = cpg_parser_context_new (network, file);
	g_object_unref (file);

	return ret;
}

CpgParserContext *
cpg_parser_context_new_for_stream (CpgNetwork   *network,
                                   GInputStream *stream)
{
	return g_object_new (CPG_TYPE_PARSER_CONTEXT,
	                     "network", network,
	                     "stream", stream,
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

	g_slist_foreach (ctx->ids, (GFunc)cpg_expanded_id_free, NULL);
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
		g_ptr_array_free (ctx->templates, TRUE);
	}

	ctx->templates = g_ptr_array_new ();

	if (templates)
	{
		gint i;

		for (i = 0; i < templates->len; ++i)
		{
			g_ptr_array_add (ctx->templates, g_array_index (templates, gchar *, i));
		}
	}

	g_ptr_array_add (ctx->templates, NULL);
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
                        CpgExpandedId    *id,
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
	gchar **expanded_templates;

	ctx = context_item->data;
	expanded_templates = cpg_expanded_id_expand_all (id,
	                                                 (gchar const * const *)ctx->templates->pdata);

	if (!cpg_network_parser_utils_get_templates (context->priv->network,
	                                             parent,
	                                             context->priv->is_template,
	                                             (gchar const * const *)expanded_templates,
	                                             &missing,
	                                             &templates))
	{
		parser_failed (context,
		               CPG_NETWORK_LOAD_ERROR_OBJECT,
		               "Could not find template `%s'",
		               missing);

		g_free (missing);
		g_slist_free (templates);
		g_strfreev (expanded_templates);

		return NULL;
	}

	g_strfreev (expanded_templates);

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

	child = cpg_group_get_child (parent, cpg_expanded_id_get_id (id));

	if (!child)
	{
		/* Just construct a new object with the right type */
		child = g_object_new (gtype, "id", cpg_expanded_id_get_id (id), NULL);
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

	ctx = CONTEXT (context_item->data);

	*new_object = NULL;

	for (ids = ctx->ids; ids; ids = g_slist_next (ids))
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

		property = properties->data;
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
                             gboolean          bidirectional,
                             gboolean          all_to_all)
{
	GSList *fromobjs;
	GSList *fromobj;
	GSList *parent;
	gboolean link_is_template;
	gint numfrom = 0;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), FALSE);
	g_return_val_if_fail (CPG_IS_LINK (link), FALSE);
	g_return_val_if_fail (CPG_IS_SELECTOR (from), FALSE);
	g_return_val_if_fail (CPG_IS_SELECTOR (to), FALSE);

	ensure_parents (context, context->priv->context_stack);
	parent = CURRENT_CONTEXT (context)->objects;

	link_is_template = !context->priv->is_template &&
	                   is_template (context, CPG_OBJECT (link));

	while (parent)
	{
		fromobjs = cpg_selector_select_states (from, parent->data);

		for (fromobj = fromobjs; fromobj; fromobj = g_slist_next (fromobj))
		{
			GSList *toobjs;
			GSList *tostart;
			GSList *toobj;
			CpgObject *fromparent;

			fromparent = cpg_object_get_parent (fromobj->data);

			toobjs = cpg_selector_select_link_to (to,
			                                      parent->data,
			                                      fromobj->data);

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
				                 fromobj->data,
				                 toobj->data);

				if (bidirectional)
				{
					if (link_is_template)
					{
						reallink = CPG_LINK (create_from_template (CPG_GROUP (fromparent),
						                                           CPG_OBJECT (link),
						                                           CPG_TYPE_LINK));
					}

					cpg_link_attach (reallink,
					                 toobj->data,
					                 fromobj->data);
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

			g_slist_free (toobjs);
		}

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
                         gboolean          bidirectional,
                         gboolean          all_to_all)
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
			               "Could not find link");

			return FALSE;
		}

		for (linkobj = linkobjs; linkobj; linkobj = g_slist_next (linkobj))
		{
			if (!cpg_parser_context_link_one (context,
			                                  linkobj->data,
			                                  from,
			                                  to,
			                                  bidirectional,
			                                  all_to_all))
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
	GFile *file;
	GSList *parent;
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
                                         gchar const      *argument)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (identifier != NULL);

	cpg_selector_add_pseudo (ensure_selector (context), identifier, argument);
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
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), EOF);
	g_return_val_if_fail (buffer != NULL, EOF);

	return g_input_stream_read (context->priv->stream,
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
	g_return_val_if_fail (context->priv->file || context->priv->stream, FALSE);

	if (!context->priv->stream)
	{
		context->priv->stream = G_INPUT_STREAM (g_file_read (context->priv->file,
		                                                     NULL,
		                                                     NULL));
	}

	if (!context->priv->stream)
	{
		return FALSE;
	}

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
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (define != NULL, NULL);

	return g_hash_table_lookup (context->priv->defines, define);
}

static gboolean
expand_define (GMatchInfo const *info,
               GString          *result,
               CpgParserContext *context)
{
	gchar *esc;

	esc = g_match_info_fetch (info, 1);

	if (esc && strlen (esc) % 2 != 0)
	{
		gchar *rest;

		if (strlen (esc) > 1)
		{
			gchar *es;

			es = g_strnfill ((strlen (esc) - 1) / 2, '\\');
			g_string_append (result, es);
			g_free (es);
		}

		rest = g_match_info_fetch (info, 2);
		g_string_append (result, rest);

		g_free (rest);
	}
	else
	{
		gchar *def;
		gchar const *ret;

		if (esc && *esc)
		{
			gchar *es;

			es = g_strnfill (strlen (esc) / 2, '\\');
			g_string_append (result, es);
			g_free (es);
		}

		def = g_match_info_fetch (info, 3);
		ret = cpg_parser_context_lookup_define (context, def);

		if (ret)
		{
			g_string_append (result, ret);
		}

		g_free (def);
	}

	g_free (esc);
	return TRUE;
}

gchar *
cpg_parser_context_expand_defines (CpgParserContext *context,
                                   gchar const      *s)
{
	static GRegex *reg = NULL;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	if (s == NULL)
	{
		return NULL;
	}

	if (reg == NULL)
	{
		reg = g_regex_new ("(\\\\*)(@{?([a-z_][a-z0-9_-]*)}?)",
		                   G_REGEX_CASELESS,
		                   0,
		                   NULL);
	}

	return g_regex_replace_eval (reg,
	                             s,
	                             -1,
	                             0,
	                             0,
	                             (GRegexEvalCallback)expand_define,
	                             context,
	                             NULL);
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

		parent = cpg_object_get_parent (item->data);

		if (!parent)
		{
			val = FALSE;
			break;
		}

		if (!cpg_group_remove (CPG_GROUP (parent), item->data, &error))
		{
			parser_failed_error (context, error);
			val = FALSE;

			break;
		}
	}

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
