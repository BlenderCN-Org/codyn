#include "cpg-parser-context.h"
#include "cpg-network-parser-utils.h"
#include "cpg-integrators.h"
#include "cpg-parser.h"
#include "cpg-annotatable.h"
#include "cpg-embedded-context.h"
#include "cpg-selection.h"
#include "cpg-expansion.h"
#include "cpg-layoutable.h"

#include <string.h>

#define CPG_PARSER_CONTEXT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_PARSER_CONTEXT, CpgParserContextPrivate))

void cpg_parser_lex_destroy (gpointer scanner);
void cpg_parser_lex_init_extra (gpointer context, gpointer *scanner);
int cpg_parser_parse (gpointer context);

typedef struct
{
	GFile *file;
	GInputStream *stream;
} InputItem;

typedef struct
{
	GSList *objects;
} Context;

struct _CpgParserContextPrivate
{
	CpgNetwork *network;
	GSList *inputs;
	gpointer scanner;
	gint start_token;

	gint lineno;
	gint cstart;
	gint cend;

	gint previous_annotation;
	GString *annotation;

	gchar *line;
	gchar *token;

	/* Stack of Context */
	GSList *context_stack;
	CpgSelector *selector;

	Context *is_template;
	CpgEmbeddedContext *embedded;

	GSList *strings;
	GSList *equations;

	GError *error;
	gboolean error_occurred;

	CpgLayout *layout;
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

static Context *
context_new (GSList *objects)
{
	Context *ret;

	ret = g_slice_new0 (Context);

	ret->objects = g_slist_copy (objects);

	return ret;
}

static void
context_free (Context *context)
{
	g_slist_foreach (context->objects, (GFunc)g_object_unref, NULL);
	g_slist_free (context->objects);

	g_slice_free (Context, context);
}

static void
cpg_parser_context_finalize (GObject *object)
{
	CpgParserContext *self;

	self = CPG_PARSER_CONTEXT (object);

	while (self->priv->context_stack)
	{
		cpg_parser_context_pop (self);
	}

	if (self->priv->network)
	{
		g_object_unref (self->priv->network);
	}

	g_slist_foreach (self->priv->inputs, (GFunc)input_item_free, NULL);
	g_slist_free (self->priv->inputs);

	g_free (self->priv->line);
	g_free (self->priv->token);

	cpg_parser_lex_destroy (self->priv->scanner);
	g_string_free (self->priv->annotation, TRUE);

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

	cpg_parser_context_push_network (self);

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
	self->priv->annotation = g_string_new ("");
	self->priv->embedded = cpg_embedded_context_new ();
}

CpgParserContext *
cpg_parser_context_new (CpgNetwork *network)
{
	return g_object_new (CPG_TYPE_PARSER_CONTEXT,
	                     "network", network,
	                     NULL);
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

static gchar *
steal_annotation (CpgParserContext *context)
{
	gchar *ret;

	if (context->priv->annotation->len == 0)
	{
		return NULL;
	}

	ret = g_string_free (context->priv->annotation, FALSE);
	context->priv->annotation = g_string_new ("");

	return ret;
}

#if 0
static void
print_expansions (GSList *expansions)
{
	while (expansions)
	{
		CpgExpansion *e = expansions->data;
		gint i;

		g_printerr ("\t");

		for (i = 0; i < cpg_expansion_num (e); ++i)
		{
			if (i != 0)
			{
				g_printerr (", ");
			}

			g_printerr ("{%s}", cpg_expansion_get (e, i));
		}

		g_printerr ("\n");

		expansions = g_slist_next (expansions);
	}
}

static void
print_selection (CpgSelection *selection)
{
	g_printerr ("%s:\n", cpg_object_get_full_id (cpg_selection_get_object (selection)));
	print_expansions (cpg_selection_get_expansions (selection));
	g_printerr ("\n");
}

#if 0
static void
print_selections (GSList *list)
{
	while (list)
	{
		print_selection (list->data);

		list = g_slist_next (list);
	}
}
#endif
#endif

void
cpg_parser_context_add_property (CpgParserContext  *context,
                                 CpgEmbeddedString *name,
                                 CpgEmbeddedString *expression,
                                 CpgPropertyFlags  flags)
{
	Context *ctx;
	GSList *item;
	gchar *annotation;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (expression != NULL);

	ctx = CURRENT_CONTEXT (context);

	annotation = steal_annotation (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		GError *error = NULL;
		CpgObject *obj;
		CpgProperty *property;
		gchar const *exname;
		gchar const *exexpression;

		obj = cpg_selection_get_object (item->data);

		cpg_embedded_context_push_expansions (context->priv->embedded,
		                                      cpg_selection_get_expansions (item->data));

		exname = cpg_embedded_string_expand (name, context->priv->embedded);
		exexpression = cpg_embedded_string_expand (expression, context->priv->embedded);

		cpg_embedded_context_pop_expansions (context->priv->embedded);

		if (!cpg_object_add_property (obj,
		                              cpg_property_new (exname, exexpression, flags),
		                              &error))
		{
			parser_failed_error (context, error);
			break;
		}

		property = cpg_object_get_property (obj, exname);
		cpg_modifiable_set_modified (CPG_MODIFIABLE (property), FALSE);

		if (annotation)
		{
			cpg_annotatable_set_annotation (CPG_ANNOTATABLE (property),
			                                annotation);
		}
	}

	g_free (annotation);

	g_object_unref (name);
	g_object_unref (expression);
}

void
cpg_parser_context_add_action (CpgParserContext  *context,
                               CpgEmbeddedString *target,
                               CpgEmbeddedString *expression)
{
	Context *ctx;
	GSList *item;
	gchar *annotation;


	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (target != NULL);
	g_return_if_fail (expression != NULL);

	ctx = CURRENT_CONTEXT (context);
	annotation = steal_annotation (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		CpgLinkAction *action;
		gchar const *extarget;
		gchar const *exexpression;

		cpg_embedded_context_push_expansions (context->priv->embedded,
		                                      cpg_selection_get_expansions (item->data));

		extarget = cpg_embedded_string_expand (target, context->priv->embedded);
		exexpression = cpg_embedded_string_expand (expression, context->priv->embedded);

		cpg_embedded_context_pop_expansions (context->priv->embedded);

		action = cpg_link_action_new (extarget,
		                              cpg_expression_new (exexpression));

		cpg_link_add_action (CPG_LINK (cpg_selection_get_object (item->data)),
		                     action);

		if (annotation)
		{
			cpg_annotatable_set_annotation (CPG_ANNOTATABLE (action),
			                                annotation);
		}
	}

	g_object_unref (target);
	g_object_unref (expression);

	g_free (annotation);
}

CpgFunction *
cpg_parser_context_add_function (CpgParserContext  *context,
                                 CpgEmbeddedString *name,
                                 CpgEmbeddedString *expression,
                                 GArray            *arguments)
{
	CpgFunction *function;
	gint i;
	gchar *annotation;
	gchar const *exname;
	gchar const *exexpression;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (name != NULL, NULL);
	g_return_val_if_fail (expression != NULL, NULL);

	exname = cpg_embedded_string_expand (name, context->priv->embedded);
	exexpression = cpg_embedded_string_expand (expression, context->priv->embedded);

	function = cpg_function_new (exname, exexpression);
	annotation = steal_annotation (context);

	g_object_unref (name);
	g_object_unref (expression);

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

	if (annotation)
	{
		cpg_annotatable_set_annotation (CPG_ANNOTATABLE (function),
		                                annotation);
	}

	g_free (annotation);

	return function;
}

CpgFunctionPolynomial *
cpg_parser_context_add_polynomial (CpgParserContext  *context,
                                   CpgEmbeddedString *name,
                                   GArray            *pieces)
{
	CpgFunctionPolynomial *function;
	gint i;
	gchar *annotation;
	gchar const *exname;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	annotation = steal_annotation (context);

	exname = cpg_embedded_string_expand (name, context->priv->embedded);
	function = cpg_function_polynomial_new (exname);
	g_object_unref (name);

	for (i = 0; i < pieces->len; ++i)
	{
		CpgFunctionPolynomialPiece *piece;

		piece = g_array_index (pieces, CpgFunctionPolynomialPiece *, i);

		cpg_function_polynomial_add (function, piece);
	}

	cpg_group_add (cpg_network_get_function_group (context->priv->network),
	               CPG_OBJECT (function),
	               NULL);

	if (annotation)
	{
		cpg_annotatable_set_annotation (CPG_ANNOTATABLE (function),
		                                annotation);
	}

	g_free (annotation);

	return function;
}

void
cpg_parser_context_add_interface (CpgParserContext  *context,
                                  CpgEmbeddedString *name,
                                  CpgSelector       *target)
{
	Context *ctx;
	GSList *item;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (CPG_IS_SELECTOR (target));

	ctx = CURRENT_CONTEXT (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		CpgPropertyInterface *iface;
		CpgGroup *parent;
		GSList *props;
		gboolean ret = TRUE;
		GError *error = NULL;
		gchar const *exname;

		parent = CPG_GROUP (cpg_selection_get_object (item->data));

		iface = cpg_group_get_property_interface (parent);

		cpg_embedded_context_push_expansions (context->priv->embedded,
		                                      cpg_selection_get_expansions (item->data));

		props = cpg_selector_select (target,
		                             CPG_OBJECT (parent),
		                             CPG_SELECTOR_TYPE_PROPERTY,
		                             context->priv->embedded);

		exname = cpg_embedded_string_expand (name, context->priv->embedded);

		if (!props)
		{
			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_INTERFACE,
			               "Could not find target property `%s' for `%s'",
			               cpg_selector_as_string (target),
			               name);

			ret = FALSE;
		}
		else if (!cpg_property_interface_add (iface,
		                                      exname,
		                                      cpg_selection_get_object (props->data),
		                                      &error))
		{
			parser_failed_error (context, error);
			ret = FALSE;
		}

		g_slist_foreach (props, (GFunc)g_object_unref, NULL);
		g_slist_free (props);

		if (!ret)
		{
			break;
		}
	}

	g_object_unref (name);
}

void
cpg_parser_context_set_error (CpgParserContext *context,
                              gchar const      *message)
{
	gchar *fname = NULL;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (context->priv->inputs)
	{
		InputItem *item = context->priv->inputs->data;

		if (item->file)
		{
			fname = g_file_get_basename (item->file);
		}
	}

	parser_failed (context,
	               CPG_NETWORK_LOAD_ERROR_SYNTAX,
	               "Unexpected token `%s' at %s%s%d.%d",
	               context->priv->token,
	               fname ? fname : "",
	               fname ? ":" : "",
	               context->priv->lineno,
	               context->priv->cstart);

	g_free (fname);
}

GError *
cpg_parser_context_get_error (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	return context->priv->error;
}

static CpgSelection *
parse_object_single_id (CpgParserContext *context,
                        CpgExpansion     *id,
                        GSList           *temps,
                        CpgSelection     *parent,
                        GType             gtype)
{
	GSList *templates;
	CpgSelection *sel = NULL;
	gchar *missing;
	GSList *item;
	CpgObject *child = NULL;
	GError *error = NULL;
	CpgGroup *parent_group;

	parent_group = CPG_GROUP (cpg_selection_get_object (parent));

	if (!cpg_network_parser_utils_get_templates (context->priv->network,
	                                             parent_group,
	                                             context->priv->is_template ? TRUE : FALSE,
	                                             temps,
	                                             context->priv->embedded,
	                                             &missing,
	                                             &templates))
	{
		parser_failed (context,
		               CPG_NETWORK_LOAD_ERROR_OBJECT,
		               "Could not find template `%s'",
		               missing);

		g_free (missing);

		goto cleanup;
	}

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

			goto cleanup;
		}
	}

	child = cpg_group_get_child (parent_group,
	                             cpg_expansion_get (id, 0));

	if (!child)
	{
		/* Just construct a new object with the right type */
		child = g_object_new (gtype, "id", cpg_expansion_get (id, 0), NULL);

		if (!cpg_group_add (parent_group,
		                    child,
		                    &error))
		{
			parser_failed_error (context, error);

			goto cleanup;
		}
	}
	else
	{
		g_object_ref (child);

		if (!g_type_is_a (gtype, G_TYPE_FROM_INSTANCE (child)))
		{
			/* This means the object already existed (this can happen
			   because existing objects created by other templates can be
			   extended) and the type is incorrect */
			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_OBJECT,
			               "Cannot extend type %s with type %s",
			               g_type_name (G_TYPE_FROM_INSTANCE (child)),
			               g_type_name (gtype));

			goto cleanup;
		}
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

			goto cleanup;
		}
	}

	sel = cpg_selection_new (child,
	                         cpg_embedded_context_get_expansions (context->priv->embedded));

cleanup:
	g_slist_foreach (templates, (GFunc)g_object_unref, NULL);
	g_slist_free (templates);

	if (child)
	{
		g_object_unref (child);
	}

	return sel;
}

static GSList *
parse_object_single (CpgParserContext  *context,
                     GSList            *ids,
                     GSList            *templates,
                     CpgSelection      *parent,
                     GType              gtype)
{
	GSList *ret = NULL;

	while (ids)
	{
		CpgSelection *sel;

		cpg_embedded_context_push_expansion (context->priv->embedded,
		                                     ids->data);

		sel = parse_object_single_id (context,
		                              ids->data,
		                              templates,
		                              parent,
		                              gtype);

		cpg_embedded_context_pop_expansions (context->priv->embedded);

		if (!sel)
		{
			g_slist_free (ret);
			return NULL;
		}

		ret = g_slist_prepend (ret, sel);
		ids = g_slist_next (ids);
	}

	return g_slist_reverse (ret);
}

static GSList *
parse_objects (CpgParserContext  *context,
               CpgEmbeddedString *id,
               GSList            *templates,
               GType              gtype)
{
	GSList *parents;
	GSList *parent;
	GSList *ret = NULL;

	parents = CURRENT_CONTEXT (context)->objects;

	for (parent = parents; parent; parent = g_slist_next (parent))
	{
		GSList *objs;
		GSList *ids;

		cpg_embedded_context_push_expansions (context->priv->embedded,
		                                      cpg_selection_get_expansions (parent->data));

		ids = cpg_embedded_string_expand_multiple (id, context->priv->embedded);

		objs = parse_object_single (context,
		                            ids,
		                            templates,
		                            parent->data,
		                            gtype);

		g_slist_foreach (ids, (GFunc)g_object_unref, NULL);
		g_slist_free (ids);

		if (!objs)
		{
			g_slist_free (ret);
			return NULL;
		}

		ret = g_slist_concat (ret, objs);
	}

	return ret;
}

static CpgAttribute *
find_attribute (GSList *attributes,
                gchar const *name)
{
	while (attributes)
	{
		if (g_strcmp0 (name, cpg_attribute_get_id (attributes->data)) == 0)
		{
			return attributes->data;
		}

		attributes = g_slist_next (attributes);
	}

	return NULL;
}

static GSList *
link_pairs (CpgParserContext *context,
            CpgExpansion     *id,
            gboolean          autoid,
            CpgSelection     *parent,
            GSList           *attributes,
            CpgSelector      *from,
            CpgSelector      *to)
{
	GSList *fromobjs;
	GSList *fromobj;
	GSList *ret = NULL;
	CpgAttribute *bidi;

	bidi = find_attribute (attributes, "bidirectional");

	if (!autoid)
	{
		cpg_embedded_context_push_expansion (context->priv->embedded, id);
	}

	fromobjs = cpg_selector_select (from,
	                                cpg_selection_get_object (parent),
	                                CPG_SELECTOR_TYPE_STATE |
	                                CPG_SELECTOR_TYPE_GROUP,
	                                context->priv->embedded);

	if (!fromobjs)
	{
		if (!autoid)
		{
			cpg_embedded_context_pop_expansions (context->priv->embedded);
		}

		return NULL;
	}

	for (fromobj = fromobjs; fromobj; fromobj = g_slist_next (fromobj))
	{
		GSList *toobjs;

		cpg_embedded_context_push_expansions (context->priv->embedded,
		                                      cpg_selection_get_expansions (fromobj->data));

		/* Select TO states */
		toobjs = cpg_selector_select (to,
		                              cpg_selection_get_object (parent),
		                              CPG_SELECTOR_TYPE_STATE |
		                              CPG_SELECTOR_TYPE_GROUP,
		                              context->priv->embedded);

		cpg_embedded_context_pop_expansions (context->priv->embedded);

		GSList *toobj;

		for (toobj = toobjs; toobj; toobj = g_slist_next (toobj))
		{
			ret = g_slist_prepend (ret,
			                       cpg_selection_copy (fromobj->data));

			ret = g_slist_prepend (ret,
			                       cpg_selection_copy (toobj->data));

			if (bidi != NULL)
			{
				ret = g_slist_prepend (ret,
				                       cpg_selection_copy (toobj->data));

				ret = g_slist_prepend (ret,
				                       cpg_selection_copy (fromobj->data));
			}
		}

		g_slist_foreach (toobjs, (GFunc)g_object_unref, NULL);
		g_slist_free (toobjs);
	}

	g_slist_foreach (fromobjs, (GFunc)g_object_unref, NULL);
	g_slist_free (fromobjs);

	if (!autoid)
	{
		cpg_embedded_context_pop_expansions (context->priv->embedded);
	}

	return g_slist_reverse (ret);
}

static void
store_annotation_objects (CpgParserContext *context,
                          GSList           *objects)
{
	gchar *s;

	s = steal_annotation (context);

	if (!s)
	{
		return;
	}

	while (objects)
	{
		CpgObject *obj;

		obj = cpg_selection_get_object (objects->data);

		cpg_annotatable_set_annotation (CPG_ANNOTATABLE (obj),
		                                s);

		objects = g_slist_next (objects);
	}

	g_free (s);
}

void
cpg_parser_context_push_object (CpgParserContext *context,
                                GSList           *objects)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	store_annotation_objects (context, objects);

	context->priv->context_stack =
		g_slist_prepend (context->priv->context_stack,
		                 context_new (objects));

	cpg_embedded_context_push_define (context->priv->embedded);
}

static GSList *
convert_templates (GArray *array)
{
	GSList *temps = NULL;
	gint i;

	if (array)
	{
		for (i = 0; i < array->len; ++i)
		{
			temps = g_slist_prepend (temps,
			                         g_array_index (array, CpgSelector *, i));
		}
	}

	return g_slist_reverse (temps);
}

static GSList *
create_objects (CpgParserContext  *context,
                CpgEmbeddedString *id,
                GArray            *templates,
                GType              type)
{
	GSList *temps;
	GSList *ret;

	temps = convert_templates (templates);

	ret = parse_objects (context, id, temps, type);

	g_slist_free (temps);

	return ret;
}

static gchar *
unique_id (CpgGroup    *parent,
           gchar const *prefix)
{
	gint i = 1;

	if (!cpg_group_get_child (parent, prefix))
	{
		return g_strdup (prefix);
	}

	while (TRUE)
	{
		gchar *id = g_strdup_printf ("%s_%d", prefix, i++);

		if (!cpg_group_get_child (parent, id))
		{
			return id;
		}

		g_free (id);
	}
}

static GSList *
create_links_single (CpgParserContext          *context,
                     CpgExpansion              *id,
                     gboolean                   autoid,
                     GSList                    *templates,
                     CpgSelection              *parent,
                     GSList                    *attributes,
                     CpgSelector               *from,
                     CpgSelector               *to)
{
	GSList *pairs;
	GSList *item;
	GSList *ret = NULL;
	gboolean multiple;
	gint num = 1;
	gint idx = 0;
	CpgAttribute *bidi;

	/* For each pair FROM -> TO generate a link */
	pairs = link_pairs (context,
	                    id,
	                    autoid,
	                    parent,
	                    attributes,
	                    from,
	                    to);
	item = pairs;

	bidi = find_attribute (attributes, "bidirectional");

	multiple = pairs && pairs->next && pairs->next->next;

	while (item)
	{
		CpgSelection *fromsel;
		CpgSelection *tosel;
		CpgExpansion *realid;
		CpgSelection *obj;
		CpgSelection *firstsel;
		CpgSelection *secondsel;
		gboolean argswitch;
		gboolean popone;
		gchar *newid;
		gchar *uniq;

		++idx;

		fromsel = item->data;
		item = g_slist_next (item);

		tosel = item->data;
		item = g_slist_next (item);

		if (bidi == NULL || idx % 2 == 1)
		{
			firstsel = fromsel;
			secondsel = tosel;

			argswitch = FALSE;
		}
		else
		{
			firstsel = tosel;
			secondsel = fromsel;

			argswitch = TRUE;
		}

		if (cpg_object_get_parent (cpg_selection_get_object (fromsel)) !=
		    cpg_object_get_parent (cpg_selection_get_object (tosel)))
		{
			continue;
		}

		/* Alter id to be numeric incremental */

		realid = cpg_expansion_copy (id);

		if (multiple)
		{
			newid = g_strdup_printf ("%s_%d",
			                         cpg_expansion_get (id, 0),
			                         num);
		}
		else
		{
			newid = g_strdup (cpg_expansion_get (id, 0));
		}

		uniq = unique_id (CPG_GROUP (cpg_selection_get_object (parent)),
		                  newid);

		g_free (newid);

		cpg_expansion_set (realid, 0, uniq);
		g_free (uniq);

		popone = FALSE;

		if (bidi != NULL)
		{
			gint widx = argswitch ? 1 : 0;
			CpgEmbeddedString *s;

			s = cpg_attribute_get_argument (bidi, widx);

			if (s)
			{
				gchar const *ex;
				CpgExpansion *expansion;

				ex = cpg_embedded_string_expand (s,
				                                 context->priv->embedded);

				expansion = cpg_expansion_new_one (ex);
				cpg_embedded_context_push_expansion (context->priv->embedded,
				                                     expansion);
				g_object_unref (expansion);

				popone = TRUE;
			}
		}

		if (!autoid)
		{
			cpg_embedded_context_push_expansion (context->priv->embedded,
			                                     realid);
		}

		cpg_embedded_context_push_expansions (context->priv->embedded,
		                                      cpg_selection_get_expansions (firstsel));

		cpg_embedded_context_push_expansions (context->priv->embedded,
		                                      cpg_selection_get_expansions (secondsel));

		obj = parse_object_single_id (context,
		                              realid,
		                              templates,
		                              parent,
		                              CPG_TYPE_LINK);

		if (!autoid)
		{
			cpg_embedded_context_pop_expansions (context->priv->embedded);
		}

		cpg_embedded_context_pop_expansions (context->priv->embedded);
		cpg_embedded_context_pop_expansions (context->priv->embedded);

		if (popone)
		{
			cpg_embedded_context_pop_expansions (context->priv->embedded);
		}

		g_object_unref (realid);

		if (!obj)
		{
			continue;
		}

		ret = g_slist_prepend (ret, obj);

		++num;

		cpg_link_attach (CPG_LINK (cpg_selection_get_object (obj)),
		                 cpg_selection_get_object (fromsel),
		                 cpg_selection_get_object (tosel));
	}

	g_slist_foreach (pairs, (GFunc)g_object_unref, NULL);
	g_slist_free (pairs);

	return g_slist_reverse (ret);
}

static GSList *
create_links (CpgParserContext          *context,
              CpgEmbeddedString         *id,
              gboolean                   autoid,
              GArray                    *templates,
              GSList                    *attributes,
              GArray                    *fromto)
{
	GSList *temps;
	GSList *ids;
	GSList *ret = NULL;
	GSList *item;
	Context *ctx;
	CpgSelector *from;
	CpgSelector *to;

	ctx = CURRENT_CONTEXT (context);
	temps = convert_templates (templates);

	from = g_array_index (fromto, CpgSelector *, 0);

	if (fromto->len == 1)
	{
		to = from;
	}
	else
	{
		to = g_array_index (fromto, CpgSelector *, 1);
	}

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		GSList *it;

		/* Expand the id with the parent expansions */
		cpg_embedded_context_push_expansions (context->priv->embedded,
		                                      cpg_selection_get_expansions (item->data));

		ids = cpg_embedded_string_expand_multiple (id,
		                                           context->priv->embedded);

		for (it = ids; it; it = g_slist_next (it))
		{
			ret = g_slist_concat (ret,
			                      create_links_single (context,
			                                           it->data,
			                                           autoid,
			                                           temps,
			                                           item->data,
			                                           attributes,
			                                           from,
			                                           to));
		}

		cpg_embedded_context_pop_expansions (context->priv->embedded);

		g_slist_foreach (ids, (GFunc)g_object_unref, NULL);
		g_slist_free (ids);
	}

	g_slist_free (temps);

	return ret;
}

void
cpg_parser_context_push_state (CpgParserContext  *context,
                               CpgEmbeddedString *id,
                               GArray            *templates)
{
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (id != NULL);

	objects = create_objects (context,
	                          id,
	                          templates,
	                          CPG_TYPE_OBJECT);

	cpg_parser_context_push_object (context, objects);
	g_slist_free (objects);
}

void
cpg_parser_context_push_group (CpgParserContext  *context,
                               CpgEmbeddedString *id,
                               GArray            *templates)
{
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (id != NULL);

	objects = create_objects (context,
	                          id,
	                          templates,
	                          CPG_TYPE_GROUP);

	cpg_parser_context_push_object (context, objects);
	g_slist_free (objects);
}

void
cpg_parser_context_push_link (CpgParserContext          *context,
                              CpgEmbeddedString         *id,
                              GArray                    *templates,
                              GSList                    *attributes,
                              GArray                    *fromto)
{
	GSList *objects;
	gboolean autoid;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	autoid = id == NULL;

	if (id == NULL)
	{
		id = cpg_embedded_string_new_from_string ("link");
	}

	if (!fromto)
	{
		objects = create_objects (context,
		                          id,
		                          templates,
		                          CPG_TYPE_LINK);
	}
	else
	{
		objects = create_links (context,
		                        id,
		                        autoid,
		                        templates,
		                        attributes,
		                        fromto);
	}

	cpg_parser_context_push_object (context, objects);
	g_slist_free (objects);

	g_object_unref (id);
}

void
cpg_parser_context_push_network (CpgParserContext *context)
{
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	objects = g_slist_prepend (NULL,
	                           cpg_selection_new (context->priv->network,
	                                              NULL));

	cpg_parser_context_push_object (context, objects);
	g_slist_free (objects);
}

void
cpg_parser_context_push_integrator (CpgParserContext *context)
{
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	objects = g_slist_prepend (NULL,
	                           cpg_selection_new (cpg_network_get_integrator (context->priv->network),
	                                              NULL));

	cpg_parser_context_push_object (context, objects);

	g_slist_free (objects);
}

void
cpg_parser_context_push_templates (CpgParserContext *context)
{
	GSList *objects;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	objects = g_slist_prepend (NULL,
	                           cpg_selection_new (cpg_network_get_template_group (context->priv->network),
	                                              NULL));

	cpg_parser_context_push_object (context, objects);
	context->priv->is_template = CURRENT_CONTEXT (context);

	g_slist_free (objects);
}

GSList *
cpg_parser_context_pop (CpgParserContext *context)
{
	Context *ctx;
	GSList *ret = NULL;
	GSList *item;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (context->priv->context_stack, NULL);

	ctx = CURRENT_CONTEXT (context);

	if (context->priv->is_template == ctx)
	{
		context->priv->is_template = NULL;
	}

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		ret = g_slist_prepend (ret,
		                       cpg_selection_get_object (item->data));
	}

	ret = g_slist_reverse (ret);

	context_free (ctx);

	context->priv->context_stack =
		g_slist_delete_link (context->priv->context_stack,
		                     context->priv->context_stack);

	cpg_embedded_context_pop_define (context->priv->embedded);

	return ret;
}

void
cpg_parser_context_import (CpgParserContext  *context,
                           CpgEmbeddedString *id,
                           CpgEmbeddedString *path)
{
	Context *ctx;
	GSList *item;
	gchar *annotation;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (id != NULL);
	g_return_if_fail (path != NULL);

	ctx = CURRENT_CONTEXT (context);
	annotation = steal_annotation (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		GSList *ids;
		GSList *idi;

		cpg_embedded_context_push_expansions (context->priv->embedded,
		                                      cpg_selection_get_expansions (item->data));

		ids = cpg_embedded_string_expand_multiple (id, context->priv->embedded);

		for (idi = ids; idi; idi = g_slist_next (idi))
		{
			gchar const *expath;
			GFile *file = NULL;
			CpgImport *import;
			GError *error = NULL;
			GFile *curfile;
			CpgGroup *parent_group;
			gchar const *exid;

			exid = cpg_expansion_get (idi->data, 0);

			cpg_embedded_context_push_expansion (context->priv->embedded,
			                                     idi->data);

			expath = cpg_embedded_string_expand (path, context->priv->embedded);

			cpg_embedded_context_pop_expansions (context->priv->embedded);
			curfile = cpg_parser_context_get_file (context);

			if (curfile)
			{
				file = cpg_network_parser_utils_resolve_import (curfile,
				                                                expath);
				g_object_unref (curfile);
			}

			if (!file)
			{
				parser_failed (context,
				               CPG_NETWORK_LOAD_ERROR_IMPORT,
				               "File `%s' for import `%s' could not be found",
				               expath,
				               exid);

				break;
			}

			parent_group = CPG_GROUP (cpg_selection_get_object (item->data));

			if (context->priv->is_template)
			{
				CpgGroup *template_group;

				template_group = cpg_network_get_template_group (context->priv->network);
				import = cpg_network_parser_utils_find_template_import (CPG_OBJECT (template_group), file);

				if (import)
				{
					CpgImportAlias *alias;

					alias = cpg_import_alias_new (import);

					if (!cpg_group_add (parent_group,
					                    CPG_OBJECT (alias),
					                    &error))
					{
						parser_failed_error (context, error);
						cpg_embedded_context_pop_expansions (context->priv->embedded);

						g_slist_foreach (ids,
						                 (GFunc)g_object_unref,
						                 NULL);

						g_slist_free (ids);
						goto cleanup;
					}

					if (annotation)
					{
						cpg_annotatable_set_annotation (CPG_ANNOTATABLE (alias),
						                                annotation);
					}

					g_object_unref (alias);
					g_object_unref (file);

					continue;
				}
			}

			import = cpg_import_new (context->priv->network,
			                         parent_group,
			                         cpg_expansion_get (idi->data, 0),
			                         file,
			                         &error);

			if (!import)
			{
				parser_failed_error (context, error);
				cpg_embedded_context_pop_expansions (context->priv->embedded);

				g_slist_foreach (ids,
				                 (GFunc)g_object_unref,
				                 NULL);

				g_slist_free (ids);

				goto cleanup;
			}

			if (annotation)
			{
				cpg_annotatable_set_annotation (CPG_ANNOTATABLE (import),
				                                annotation);
			}

			g_object_unref (import);
		}

		g_slist_foreach (ids, (GFunc)g_object_unref, NULL);
		g_slist_free (ids);

		ids = NULL;

		cpg_embedded_context_pop_expansions (context->priv->embedded);
	}

cleanup:
	g_free (annotation);

	g_object_unref (id);
	g_object_unref (path);
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
cpg_parser_context_push_selector (CpgParserContext  *context,
                                  CpgEmbeddedString *identifier,
                                  gboolean           onset)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (identifier != NULL);

	cpg_selector_add (ensure_selector (context), identifier, onset);
	g_object_unref (identifier);
}

void
cpg_parser_context_push_selector_regex (CpgParserContext  *context,
                                        CpgEmbeddedString *regex,
                                        gboolean           onset)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (regex != NULL);

	cpg_selector_add_regex (ensure_selector (context), regex, onset);
	g_object_unref (regex);
}

void
cpg_parser_context_push_selector_pseudo (CpgParserContext  *context,
                                         CpgEmbeddedString *identifier,
                                         GArray            *argument)
{
	GSList *ptr = NULL;
	gint i;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (identifier != NULL);

	if (argument)
	{
		for (i = 0; i < argument->len; ++i)
		{
			ptr = g_slist_prepend (ptr,
			                       g_array_index (argument, CpgEmbeddedString *, i));
		}

		ptr = g_slist_reverse (ptr);
	}

	cpg_selector_add_pseudo (ensure_selector (context),
	                         identifier,
	                         ptr);

	g_slist_free (ptr);
	g_object_unref (identifier);
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
cpg_parser_context_define (CpgParserContext  *context,
                           CpgEmbeddedString *name,
                           GSList            *defines,
                           gboolean           expand)
{
	gchar const *exname;
	gchar const *exdefine;
	CpgEmbeddedString *define = NULL;
	GSList *item;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (defines != NULL);

	exname = cpg_embedded_string_expand (name, context->priv->embedded);

	for (item = defines; item; item = g_slist_next (item))
	{
		gchar const *s;

		s = cpg_embedded_string_expand (item->data,
		                                context->priv->embedded);

		if (s && *s)
		{
			define = g_object_ref (item->data);
			break;
		}
	}

	if (!define)
	{
		define = g_object_ref (defines->data);
	}

	g_slist_foreach (defines, (GFunc)g_object_unref, NULL);
	g_slist_free (defines);

	exdefine = cpg_embedded_string_expand (define, context->priv->embedded);

	if (expand)
	{
		GSList *items;
		GSList *item;
		gint num = 0;
		gchar *cntname;
		gchar *cntval;
		gchar *s0;

		s0 = g_strdup_printf ("%s0", exname);

		cpg_embedded_context_define (context->priv->embedded,
		                             s0,
		                             exdefine);

		g_free (s0);

		items = cpg_embedded_string_expand_multiple (define, context->priv->embedded);

		for (item = items; item; item = g_slist_next (item))
		{
			CpgExpansion *ex = item->data;
			gint i;
			gchar *name;

			name = g_strdup_printf ("%s%d", exname, ++num);

			cpg_embedded_context_define (context->priv->embedded,
			                             name, cpg_expansion_get (ex, 0));

			for (i = 0; i < cpg_expansion_num (ex); ++i)
			{
				gchar *sub;

				sub = g_strdup_printf ("%s,%d", name, i);

				cpg_embedded_context_define (context->priv->embedded,
				                             sub,
				                             cpg_expansion_get (ex, i));

				g_free (sub);
			}

			g_free (name);
		}

		g_slist_foreach (items, (GFunc)g_object_unref, NULL);
		g_slist_free (items);

		cntname = g_strconcat (exname, "~", NULL);
		cntval = g_strdup_printf ("%d", num);

		cpg_embedded_context_define (context->priv->embedded,
		                             cntname,
		                             cntval);

		g_free (cntname);
		g_free (cntval);
	}
	else
	{
		cpg_embedded_context_define (context->priv->embedded,
		                             exname,
		                             exdefine);
	}

	g_object_unref (name);
	g_object_unref (define);
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
cpg_parser_context_push_input_from_path (CpgParserContext  *context,
                                         CpgEmbeddedString *filename)
{
	GFile *file = NULL;
	gchar const *res;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (filename != NULL);

	res = cpg_embedded_string_expand (filename, context->priv->embedded);

	if (g_path_is_absolute (res))
	{
		file = g_file_new_for_path (res);
	}
	else
	{
		GSList *item;

		for (item = context->priv->inputs; item; item = g_slist_next (item))
		{
			InputItem *ip = item->data;
			GFile *dir;

			if (!ip->file)
			{
				continue;
			}

			dir = g_file_get_parent (ip->file);

			if (!dir)
			{
				continue;
			}

			file = g_file_resolve_relative_path (dir, res);
			g_object_unref (dir);

			if (file && !g_file_query_exists (file, NULL))
			{
				g_object_unref (file);
				file = NULL;
			}
			else
			{
				break;
			}
		}

		if (!file)
		{
			file = g_file_new_for_commandline_arg (res);
		}
	}

	cpg_parser_context_push_input (context, file, NULL);
	g_object_unref (file);

	g_object_unref (filename);
}

void
cpg_parser_context_push_input_from_string (CpgParserContext *context,
                                           gchar const      *s)
{
	GInputStream *stream;
	gchar *ret;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (s != NULL);

	ret = g_strdup (s);

	stream = g_memory_input_stream_new_from_data (ret,
	                                              strlen (ret),
	                                              (GDestroyNotify)g_free);

	cpg_parser_context_push_input (context, NULL, stream);

	g_object_unref (stream);
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
cpg_parser_context_push_annotation (CpgParserContext  *context,
                                    CpgEmbeddedString *annotation)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (annotation != NULL);

	if (context->priv->previous_annotation != context->priv->lineno + 1)
	{
		g_string_assign (context->priv->annotation,
		                 cpg_embedded_string_expand (annotation,
		                                               context->priv->embedded));
	}
	else
	{
		g_string_append (context->priv->annotation,
		                 cpg_embedded_string_expand (annotation,
		                                               context->priv->embedded));

		g_string_append_c (context->priv->annotation, '\n');
	}

	context->priv->previous_annotation = context->priv->lineno;
	g_object_unref (annotation);
}

void
cpg_parser_context_push_layout (CpgParserContext *context)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	if (!context->priv->layout)
	{
		context->priv->layout = cpg_layout_new (context->priv->network);
	}

	cpg_parser_context_push_object (context,
	                                NULL);
}

void
cpg_parser_context_pop_layout (CpgParserContext *context)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	cpg_parser_context_pop (context);
}

void
cpg_parser_context_add_layout (CpgParserContext *context,
                               CpgLayoutRelation relation,
                               CpgSelector      *left,
                               CpgSelector      *right)
{
	GSList *leftobjs;
	GSList *leftobj;
	GSList *objs;
	Context *ctx;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CPG_IS_SELECTOR (left));
	g_return_if_fail (CPG_IS_SELECTOR (right));

	ctx = context->priv->context_stack->next->data;

	for (objs = ctx->objects; objs; objs = g_slist_next (objs))
	{
		CpgSelection *sel;

		sel = objs->data;

		cpg_embedded_context_push_expansions (context->priv->embedded,
		                                      cpg_selection_get_expansions (sel));

		leftobjs = cpg_selector_select (left,
		                                CPG_OBJECT (cpg_selection_get_object (sel)),
		                                CPG_SELECTOR_TYPE_STATE |
		                                CPG_SELECTOR_TYPE_GROUP,
		                                context->priv->embedded);

		if (!leftobjs)
		{
			cpg_embedded_context_pop_expansions (context->priv->embedded);
			return;
		}

		for (leftobj = leftobjs; leftobj; leftobj = g_slist_next (leftobj))
		{
			CpgSelection *leftsel = leftobj->data;
			GSList *rightobjs;

			cpg_embedded_context_push_expansions (context->priv->embedded,
			                                      cpg_selection_get_expansions (leftsel));

			rightobjs = cpg_selector_select (right,
			                                 CPG_OBJECT (cpg_selection_get_object (sel)),
			                                 CPG_SELECTOR_TYPE_STATE |
			                                 CPG_SELECTOR_TYPE_GROUP,
			                                 context->priv->embedded);

			cpg_embedded_context_pop_expansions (context->priv->embedded);

			if (!rightobjs)
			{

				continue;
			}

			GSList *rightobj;

			for (rightobj = rightobjs; rightobj; rightobj = g_slist_next (rightobj))
			{
				cpg_layout_add (context->priv->layout,
				                CPG_LAYOUTABLE (cpg_selection_get_object (leftsel)),
				                CPG_LAYOUTABLE (cpg_selection_get_object (rightobj->data)),
				                relation);
			}

			g_slist_foreach (rightobjs, (GFunc)g_object_unref, NULL);
			g_slist_free (rightobjs);
		}

		g_slist_foreach (leftobjs, (GFunc)g_object_unref, NULL);
		g_slist_free (leftobjs);

		cpg_embedded_context_pop_expansions (context->priv->embedded);
	}
}

void
cpg_parser_context_add_layout_position (CpgParserContext  *context,
                                        CpgSelector       *selector,
                                        CpgEmbeddedString *x,
                                        CpgEmbeddedString *y,
                                        CpgSelector       *of)
{
	GSList *objs;
	GSList *cobjs;
	GSList *obj;
	Context *ctx;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CPG_IS_SELECTOR (selector));
	g_return_if_fail (x != NULL);
	g_return_if_fail (y != NULL);
	g_return_if_fail (of == NULL || CPG_IS_SELECTOR (of));

	ctx = context->priv->context_stack->next->data;

	for (cobjs = ctx->objects; cobjs; cobjs = g_slist_next (cobjs))
	{
		CpgSelection *sel;

		sel = cobjs->data;

		cpg_embedded_context_push_expansions (context->priv->embedded,
		                                      cpg_selection_get_expansions (sel));

		objs = cpg_selector_select (selector,
		                            cpg_selection_get_object (sel),
		                            CPG_SELECTOR_TYPE_OBJECT,
		                            context->priv->embedded);

		for (obj = objs; obj; obj = g_slist_next (obj))
		{
			gchar const *exx;
			gchar const *exy;
			gint xx;
			gint yy;

			if (!CPG_IS_LAYOUTABLE (cpg_selection_get_object (obj->data)) ||
			    !cpg_layoutable_supports_location (CPG_LAYOUTABLE (cpg_selection_get_object (obj->data))))
			{
				continue;
			}

			cpg_embedded_context_push_expansions (context->priv->embedded,
			                                      cpg_selection_get_expansions (obj->data));

			exx = cpg_embedded_string_expand (x, context->priv->embedded);
			exy = cpg_embedded_string_expand (y, context->priv->embedded);

			xx = (gint)g_ascii_strtoll (exx, NULL, 10);
			yy = (gint)g_ascii_strtoll (exy, NULL, 10);

			if (of)
			{
				GSList *ofobjs;
				GSList *ofobj;
				gint mx = 0;
				gint my = 0;
				gint num = 0;

				ofobjs = cpg_selector_select (of,
				                              cpg_selection_get_object (sel),
				                              CPG_SELECTOR_TYPE_OBJECT,
				                              context->priv->embedded);

				for (ofobj = ofobjs; ofobj; ofobj = g_slist_next (ofobj))
				{
					CpgObject *o;
					gint ox;
					gint oy;

					o = cpg_selection_get_object (ofobj->data);

					if (CPG_IS_LAYOUTABLE (o) &&
					    cpg_layoutable_supports_location (CPG_LAYOUTABLE (o)))
					{
						cpg_layoutable_get_location (CPG_LAYOUTABLE (o),
						                             &ox,
						                             &oy);

						mx += ox;
						my += oy;

						++num;
					}
				}

				if (num > 0)
				{
					mx /= num;
					my /= num;
				}

				xx += mx;
				yy += my;
			}

			cpg_layoutable_set_location (CPG_LAYOUTABLE (cpg_selection_get_object (obj->data)),
			                             xx,
			                             yy);

			cpg_embedded_context_pop_expansions (context->priv->embedded);
		}

		cpg_embedded_context_pop_expansions (context->priv->embedded);
	}
}

void
cpg_parser_context_set_proxy (CpgParserContext *context,
                              GSList           *objects)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	while (objects)
	{
		CpgObject *obj = objects->data;
		CpgObject *parent;

		objects = g_slist_next (objects);

		if (CPG_IS_LINK (obj))
		{
			continue;
		}

		parent = cpg_object_get_parent (obj);

		if (!parent)
		{
			continue;
		}

		cpg_group_set_proxy (CPG_GROUP (parent), obj);
	}
}

void
cpg_parser_context_add_integrator_property (CpgParserContext  *context,
                                            CpgEmbeddedString *name,
                                            CpgEmbeddedString *value)
{
	gchar const *exname;
	gchar const *exval;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (value != NULL);

	exname = cpg_embedded_string_expand (name, context->priv->embedded);
	exval = cpg_embedded_string_expand (value, context->priv->embedded);

	if (g_strcmp0 (exname, "method") == 0)
	{
		GType type;
		CpgIntegrator *it;

		type = cpg_integrators_find (exval);

		if (type == G_TYPE_INVALID)
		{
			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_SYNTAX,
			               "Could not find integrator `%s'",
			               exval);

			g_object_unref (name);
			g_object_unref (value);

			return;
		}

		it = g_object_new (type, NULL);
		cpg_network_set_integrator (context->priv->network, it);

		g_object_unref (it);
	}
	else
	{
		/* General purpose */
		GObjectClass *klass;
		CpgIntegrator *it;
		GParamSpec *spec;
		GValue v = {0,};
		GValue dest = {0,};

		it = cpg_network_get_integrator (context->priv->network);
		klass = G_OBJECT_GET_CLASS (it);

		spec = g_object_class_find_property (klass, exname);

		if (spec == NULL)
		{
			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_SYNTAX,
			               "Invalid integrator property `%s' for `%s'",
			               exname,
			               cpg_integrator_get_name (it));

			g_object_unref (name);
			g_object_unref (value);

			return;
		}

		if (!g_value_type_transformable (G_TYPE_STRING, spec->value_type))
		{
			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_SYNTAX,
			               "Could not convert `%s' to `%s'",
			               exval,
			               g_type_name (spec->value_type));

			g_object_unref (name);
			g_object_unref (value);

			return;
		}

		g_value_init (&v, G_TYPE_STRING);
		g_value_set_string (&v, exval);

		g_value_init (&dest, spec->value_type);

		if (!g_value_transform (&v, &dest))
		{
			g_value_unset (&v);

			parser_failed (context,
			               CPG_NETWORK_LOAD_ERROR_SYNTAX,
			               "Could not convert `%s' to `%s'",
			               exval,
			               g_type_name (spec->value_type));

			g_object_unref (name);
			g_object_unref (value);

			return;
		}

		g_object_set_property (G_OBJECT (it), exname, &v);
		g_value_unset (&v);
	}

	g_object_unref (name);
	g_object_unref (value);
}

CpgEmbeddedString *
cpg_parser_context_push_string (CpgParserContext *context)
{
	CpgEmbeddedString *s;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	s = cpg_embedded_string_new ();

	context->priv->strings =
		g_slist_prepend (context->priv->strings, s);

	return s;
}

CpgEmbeddedString *
cpg_parser_context_peek_string (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (context->priv->strings, NULL);

	return context->priv->strings->data;
}

CpgEmbeddedString *
cpg_parser_context_pop_string (CpgParserContext *context)
{
	CpgEmbeddedString *s;

	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);
	g_return_val_if_fail (context->priv->strings, NULL);

	s = context->priv->strings->data;

	context->priv->strings =
		g_slist_delete_link (context->priv->strings,
		                     context->priv->strings);

	return s;
}

void
cpg_parser_context_push_equation (CpgParserContext *context)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));

	context->priv->equations =
		g_slist_prepend (context->priv->equations,
		                 GINT_TO_POINTER (1));
}

void
cpg_parser_context_push_equation_depth (CpgParserContext *context)
{
	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (context->priv->equations);

	context->priv->equations->data =
		GINT_TO_POINTER (GPOINTER_TO_INT (context->priv->equations->data) + 1);
}

gboolean
cpg_parser_context_pop_equation_depth (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), FALSE);
	g_return_val_if_fail (context->priv->equations, FALSE);

	context->priv->equations->data =
		GINT_TO_POINTER (GPOINTER_TO_INT (context->priv->equations->data) - 1);

	if (GPOINTER_TO_INT (context->priv->equations->data) == 0)
	{
		context->priv->equations =
			g_slist_delete_link (context->priv->equations,
			                     context->priv->equations);

		return TRUE;
	}

	return FALSE;
}

CpgEmbeddedContext *
cpg_parser_context_get_embedded (CpgParserContext *context)
{
	g_return_val_if_fail (CPG_IS_PARSER_CONTEXT (context), NULL);

	return context->priv->embedded;
}

static void
debug_selector (CpgParserContext *context,
                CpgSelection     *selection,
                GSList           *objects)
{
	gchar *fullid;
	GSList *orig = objects;

	fullid = cpg_object_get_full_id (cpg_selection_get_object (selection));

	while (objects)
	{
		gchar *msg;
		CpgSelection *sel;

		sel = objects->data;

		if (CPG_IS_OBJECT (cpg_selection_get_object (sel)))
		{
			msg = cpg_object_get_full_id (cpg_selection_get_object (sel));
		}
		else
		{
			msg = cpg_property_get_full_name (cpg_selection_get_object (sel));
		}

		g_printerr ("[debug] (%d): {%s} => %s\n",
		            context->priv->lineno,
		            fullid,
		            msg);

		g_free (msg);
		objects = g_slist_next (objects);
	}

	g_free (fullid);

	g_slist_foreach (orig, (GFunc)g_object_unref, NULL);
	g_slist_free (orig);
}

void
cpg_parser_context_debug_selector (CpgParserContext *context,
                                   CpgSelectorType   type,
                                   CpgSelector      *selector)
{
	GSList *item;
	Context *ctx;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CPG_IS_SELECTOR (selector));

	ctx = CURRENT_CONTEXT (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		debug_selector (context,
		                item->data,
		                cpg_selector_select (selector,
		                                     cpg_selection_get_object (item->data),
		                                     type,
		                                     context->priv->embedded));
	}
}

void
cpg_parser_context_debug_string (CpgParserContext  *context,
                                 CpgEmbeddedString *s)
{
	gchar const *ret;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CPG_IS_EMBEDDED_STRING (s));

	ret = cpg_embedded_string_expand (s, context->priv->embedded);

	g_printerr ("[debug] (%d): %s\n", context->priv->lineno, ret);
}

void
cpg_parser_context_delete_selector (CpgParserContext *context,
                                    CpgSelectorType   type,
                                    CpgSelector      *selector)
{
	GSList *ret;
	GSList *item;

	g_return_if_fail (CPG_IS_PARSER_CONTEXT (context));
	g_return_if_fail (CPG_IS_SELECTOR (selector));

	ret = cpg_selector_select (selector,
	                           CPG_OBJECT (context->priv->network),
	                           type,
	                           context->priv->embedded);

	for (item = ret; item; item = g_slist_next (item))
	{
		CpgSelection *sel = item->data;
		gpointer obj = cpg_selection_get_object (sel);
		GError *error = NULL;

		if (CPG_IS_PROPERTY (obj))
		{
			CpgObject *parent;

			parent = cpg_property_get_object (obj);

			if (!cpg_object_remove_property (parent, obj, &error))
			{
				parser_failed_error (context, error);
				break;
			}
		}
		else if (CPG_IS_LINK_ACTION (obj))
		{
			CpgLink *link;

			link = cpg_link_action_get_link (obj);

			cpg_link_remove_action (link, obj);
		}
		else if (CPG_IS_OBJECT (obj))
		{
			CpgObject *parent;

			parent = cpg_object_get_parent (obj);

			if (!cpg_group_remove (CPG_GROUP (parent), obj, &error))
			{
				parser_failed_error (context, error);
				break;
			}
		}
	}

	g_slist_foreach (ret, (GFunc)g_object_unref, NULL);
	g_slist_free (ret);
}

