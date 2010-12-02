#include "cpg-network-deserializer.h"

#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include <libxml/xinclude.h>
#include <libxml/xmlreader.h>

#include <errno.h>
#include <string.h>

#include "cpg-network.h"
#include "cpg-integrators.h"
#include "cpg-function-polynomial.h"
#include "cpg-enum-types.h"
#include "cpg-network-xml.h"
#include "cpg-import.h"
#include "cpg-import-alias.h"
#include "cpg-input-file.h"

/**
 * SECTION:cpg-network-deserializer
 * @short_description: XML to Network deserializer
 *
 * Use this to deserialize an XML description of a network to a #CpgNetwork.
 *
 */
#define CPG_NETWORK_DESERIALIZER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_NETWORK_DESERIALIZER, CpgNetworkDeserializerPrivate))

struct _CpgNetworkDeserializerPrivate
{
	CpgNetwork  *network;
	CpgGroup    *root;
	GInputStream *stream;

	xmlDocPtr    doc;
	GError     **error;
	CpgObject   *object;
	GSList      *parents;

	GFile *file;
};

static gboolean parse_all (CpgNetworkDeserializer *deserializer,
                           xmlNodePtr              root,
                           gpointer                parent);

typedef gboolean (*XPathResultFunc)(CpgNetworkDeserializer *deserializer, GList *nodes, gpointer data);

G_DEFINE_TYPE (CpgNetworkDeserializer, cpg_network_deserializer, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NETWORK,
	PROP_ROOT
};

static void
cpg_network_deserializer_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_network_deserializer_parent_class)->finalize (object);
}

static void
cpg_network_deserializer_dispose (GObject *object)
{
	CpgNetworkDeserializer *deserializer = CPG_NETWORK_DESERIALIZER (object);

	if (deserializer->priv->network)
	{
		g_object_unref (deserializer->priv->network);
		deserializer->priv->network = NULL;
	}

	if (deserializer->priv->root)
	{
		g_object_unref (deserializer->priv->root);
		deserializer->priv->root = NULL;
	}

	G_OBJECT_CLASS (cpg_network_deserializer_parent_class)->dispose (object);
}

static void
cpg_network_deserializer_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
	CpgNetworkDeserializer *self = CPG_NETWORK_DESERIALIZER (object);

	switch (prop_id)
	{
		case PROP_NETWORK:
			self->priv->network = g_value_dup_object (value);
		break;
		case PROP_ROOT:
			self->priv->root = g_value_dup_object (value);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_network_deserializer_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
	CpgNetworkDeserializer *self = CPG_NETWORK_DESERIALIZER (object);

	switch (prop_id)
	{
		case PROP_NETWORK:
			g_value_set_object (value, self->priv->network);
		break;
		case PROP_ROOT:
			g_value_set_object (value, self->priv->root);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_network_deserializer_constructed (GObject *object)
{
	CpgNetworkDeserializer *deserializer = CPG_NETWORK_DESERIALIZER (object);

	if (deserializer->priv->root == NULL && deserializer->priv->network)
	{
		deserializer->priv->root = g_object_ref (deserializer->priv->network);
	}
}

static void
cpg_network_deserializer_class_init (CpgNetworkDeserializerClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_network_deserializer_finalize;

	object_class->get_property = cpg_network_deserializer_get_property;
	object_class->set_property = cpg_network_deserializer_set_property;

	object_class->dispose = cpg_network_deserializer_dispose;
	object_class->constructed = cpg_network_deserializer_constructed;

	g_type_class_add_private (object_class, sizeof(CpgNetworkDeserializerPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_NETWORK,
	                                 g_param_spec_object ("network",
	                                                      "Network",
	                                                      "Network",
	                                                      CPG_TYPE_NETWORK,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (object_class,
	                                 PROP_ROOT,
	                                 g_param_spec_object ("root",
	                                                      "Root",
	                                                      "Root",
	                                                      CPG_TYPE_GROUP,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cpg_network_deserializer_init (CpgNetworkDeserializer *self)
{
	self->priv = CPG_NETWORK_DESERIALIZER_GET_PRIVATE (self);
}

static void
save_comment (xmlNodePtr  node,
              GObject    *object)
{
	xmlNodePtr prev = node->prev;

	if (prev == NULL || prev->type != XML_COMMENT_NODE)
	{
		return;
	}

	g_object_set_data_full (object,
	                        CPG_NETWORK_XML_COMMENT_DATA_KEY,
	                        g_strdup ((gchar const *)prev->content),
	                        (GDestroyNotify)g_free);
}

static gboolean
xml_xpath (CpgNetworkDeserializer *deserializer,
           xmlNodePtr              root,
           gchar const            *expr,
           xmlElementType          type,
           XPathResultFunc         func,
           gpointer                data)
{
	xmlXPathContextPtr ctx;
	xmlXPathObjectPtr obj;
	GList *set = NULL;

	if (!expr)
	{
		xmlNodePtr child = root->children;

		while (child)
		{
			if (child->type == type)
			{
				set = g_list_prepend (set, child);
			}

			child = child->next;
		}
	}
	else
	{
		ctx = xmlXPathNewContext (deserializer->priv->doc);
		ctx->node = root;

		if (!ctx)
		{
			g_warning ("Could not create XPath context");
			return FALSE;
		}

		obj = xmlXPathEvalExpression ((xmlChar *)expr, ctx);

		if (!obj)
		{
			g_warning ("Failed to evaluate xpath expression '%s'", expr);
			xmlXPathFreeContext (ctx);
			return FALSE;
		}

		int i;

		for (i = 0; i < obj->nodesetval->nodeNr; ++i)
		{
			if (type == 0 || obj->nodesetval->nodeTab[i]->type == type)
			{
				set = g_list_prepend (set, obj->nodesetval->nodeTab[i]);
			}
		}

		/* Free up */
		xmlXPathFreeObject (obj);
		xmlXPathFreeContext (ctx);
	}

	set = g_list_reverse (set);
	gboolean ret = func (deserializer, set, data);
	g_list_free (set);

	return ret;
}

static gboolean
xpath_first (CpgNetworkDeserializer *deserializer,
             GList                  *nodes,
             gpointer                data)
{
	xmlNodePtr *first = (xmlNodePtr *)data;

	if (nodes)
	{
		*first = nodes->data;
		return TRUE;
	}
	else
	{
		*first = NULL;
		return FALSE;
	}
}

static xmlNodePtr
xml_xpath_first (CpgNetworkDeserializer *deserializer,
                 xmlNodePtr              root,
                 gchar const            *expr,
                 xmlElementType          type)
{
	xmlNodePtr first = NULL;

	xml_xpath (deserializer, root, expr, type, xpath_first, &first);
	return first;
}

static gboolean
parser_failed (CpgNetworkDeserializer *deserializer,
               xmlNodePtr              node,
               gint                    code,
               gchar const            *format,
               ...)
{
	if (deserializer->priv->error != NULL)
	{
		va_list ap;
		va_start (ap, format);

		gchar *message = g_strdup_vprintf (format, ap);
		va_end (ap);

		if (*deserializer->priv->error)
		{
			g_error_free (*deserializer->priv->error);
			*deserializer->priv->error = NULL;
		}

		g_warning ("XML load error: %s", message);

		g_set_error (deserializer->priv->error,
		             CPG_NETWORK_LOAD_ERROR,
		             code,
		             "%s (line %d)",
		             message,
		             node ? node->line : 0);

		g_free (message);
	}

	return FALSE;
}

static gboolean
extract_flags (CpgNetworkDeserializer *deserializer,
               xmlNodePtr              node,
               gchar const            *name,
               CpgPropertyFlags       *flags,
               gboolean               *flags_attr)
{
	GFlagsClass *klass;
	guint i;

	*flags = CPG_PROPERTY_FLAG_NONE;
	klass = g_type_class_ref (CPG_TYPE_PROPERTY_FLAGS);

	for (i = 0; i < klass->n_values; ++i)
	{
		xmlChar *prop = xmlGetProp (node, (xmlChar *)klass->values[i].value_nick);

		if (prop && g_ascii_strcasecmp ((gchar const *)prop, "yes") == 0)
		{
			*flags |= klass->values[i].value;
		}

		xmlFree (prop);
	}

	g_type_class_unref (klass);

	xmlChar *prop = xmlGetProp (node, (xmlChar *)"flags");
	gboolean ret = TRUE;

	*flags_attr = FALSE;

	if (prop)
	{
		*flags |= cpg_property_flags_from_string ((gchar const *)prop);
	}

	xmlFree (prop);

	return ret;
}

static gboolean
parse_properties (CpgNetworkDeserializer *deserializer,
                  GList                  *nodes)
{
	GList *item;

	for (item = nodes; item; item = g_list_next (item))
	{
		xmlNodePtr node = (xmlNodePtr)item->data;
		xmlChar *name = xmlGetProp (node, (xmlChar *)"name");

		if (!name)
		{
			xmlFree (name);

			return parser_failed (deserializer,
			                      node,
			                      CPG_NETWORK_LOAD_ERROR_PROPERTY,
			                      "Property on %s has no name",
			                      cpg_object_get_id (deserializer->priv->object));
		}

		xmlChar const *expression = (xmlChar *)"";

		if (node->children && node->children->type == XML_TEXT_NODE)
		{
			expression = node->children->content;
		}

		CpgProperty *property;
		CpgPropertyFlags flags;
		gboolean flags_attr;

		if (!extract_flags (deserializer,
		                    node,
		                    (gchar const *)name,
		                    &flags,
		                    &flags_attr))
		{
			return FALSE;
		}

		property = cpg_property_new ((const gchar *)name,
		                             (const gchar *)expression,
		                             flags);

		save_comment (node, G_OBJECT (property));

		cpg_object_add_property (deserializer->priv->object,
		                         property);

		property = cpg_object_get_property (deserializer->priv->object,
		                                    (const gchar *)name);

		cpg_modifiable_set_modified (CPG_MODIFIABLE (property), FALSE);

		g_object_set_data (G_OBJECT (property),
		                   CPG_NETWORK_XML_PROPERTY_FLAGS_ATTRIBUTE,
		                   GINT_TO_POINTER (flags_attr));

		xmlFree (name);
	}

	return TRUE;
}

static gboolean
parse_object_properties (CpgNetworkDeserializer *deserializer,
                         xmlNodePtr              node,
                         CpgObject              *object)
{
	deserializer->priv->object = object;

	if (!xml_xpath (deserializer,
	                node,
	                "property",
	                XML_ELEMENT_NODE,
	                (XPathResultFunc)parse_properties,
	                NULL))
	{
		g_warning ("Could not parse object properties for: %s",
		                 cpg_object_get_id (object));
		return FALSE;
	}

	return TRUE;
}

static GType
type_from_templates (GType   orig,
                     GSList *templates)
{
	/* This only needs to be checked for states since in the XML groups
	   can be defined with a <state> to make it nicer to write the XML */
	if (orig != CPG_TYPE_OBJECT)
	{
		return orig;
	}

	/* Check here if any of the templates applied are actually groups,
	   if so, the resulting type for the new object should also be a
	   group */
	while (templates)
	{
		if (G_TYPE_FROM_INSTANCE (templates->data) == CPG_TYPE_GROUP)
		{
			return CPG_TYPE_GROUP;
		}

		templates = g_slist_next (templates);
	}

	return orig;
}

static gboolean
get_templates (CpgNetworkDeserializer  *deserializer,
               xmlNodePtr               node,
               gchar const             *id,
               GSList                 **templates)
{
	xmlChar *ref = xmlGetProp (node, (xmlChar *)"ref");
	*templates = NULL;

	if (!ref)
	{
		return TRUE;
	}

	CpgGroup *template_group = cpg_network_get_template_group (deserializer->priv->network);

	gchar **parts;
	gchar **ptr;

	parts = g_strsplit_set ((gchar const *)ref, ", ", -1);
	gboolean ret = TRUE;

	/* Multiple templates are allowed, iterate over all the template ids
	   and resolve them from the template group */
	for (ptr = parts; *ptr; ++ptr)
	{
		CpgObject *template = NULL;

		if (!**ptr)
		{
			continue;
		}

		/* Find the reference relative to the current parent if we
		   are parsing templates, since they can reference other
		   templates */
		if (g_slist_last (deserializer->priv->parents)->data ==
		    (gpointer)template_group)
		{
			template = cpg_group_find_object (deserializer->priv->parents->data,
			                                  *ptr);
		}

		if (!template)
		{
			/* Find template in the root template group */
			template = cpg_group_find_object (template_group,
			                                  *ptr);
		}

		if (!template)
		{
			ret = parser_failed (deserializer,
			                     node,
			                     CPG_NETWORK_LOAD_ERROR_OBJECT,
			                     "Could not find template `%s' for object `%s'",
			                     *ptr,
			                     id);
		}
		else
		{
			*templates = g_slist_prepend (*templates, template);
		}

		if (!ret)
		{
			break;
		}
	}

	if (ret)
	{
		*templates = g_slist_reverse (*templates);
	}
	else
	{
		g_slist_free (*templates);
	}

	g_strfreev (parts);
	return ret;
}

static CpgObject *
parse_object (CpgNetworkDeserializer *deserializer,
              GType                   gtype,
              xmlNodePtr              node,
              gboolean               *new_object)
{
	xmlChar *id = xmlGetProp (node, (xmlChar *)"id");
	*new_object = FALSE;

	if (!id)
	{
		parser_failed (deserializer,
		               node,
		               CPG_NETWORK_LOAD_ERROR_OBJECT,
		               "One of the objects does not have an id");

		return NULL;
	}

	GSList *templates;

	if (!get_templates (deserializer, node, (gchar const *)id, &templates))
	{
		return NULL;
	}

	/* Get the final type by inspecting the template types. This is only
	   needed because groups can be defined in the XML using the <state>
	   tag (which makes it easier for the user, but a bit more effort
	   to parse :)) */
	gtype = type_from_templates (gtype, templates);

	GSList *item;

	/* Check if the template types can actually be applied to the
	   object type that we are constructing. Only template types
	   which are superclasses of the new object type can be
	   applied */
	for (item = templates; item; item = g_slist_next (item))
	{
		GType template_type = G_TYPE_FROM_INSTANCE (item->data);

		if (!g_type_is_a (gtype, template_type))
		{
			parser_failed (deserializer,
			               node,
			               CPG_NETWORK_LOAD_ERROR_OBJECT,
			               "Referenced template is of incorrect type %s (need %s)",
			               g_type_name (template_type),
			               g_type_name (gtype));

			g_slist_free (templates);
			xmlFree (id);

			return NULL;
		}
	}

	CpgGroup *parent = deserializer->priv->parents->data;
	CpgObject *child;

	child = cpg_group_get_child (parent, (gchar const *)id);

	if (!child)
	{
		/* Just construct a new object with the right type */
		child = g_object_new (gtype, "id", (gchar const *)id, NULL);
		*new_object = TRUE;

		save_comment (node, G_OBJECT (child));
	}
	else if (!g_type_is_a (gtype, G_TYPE_FROM_INSTANCE (child)))
	{
		/* This means the object already existed (this can happen
		   because existing objects created by other templates can be
		   extended) and the type is incorrect */
		parser_failed (deserializer,
		               node,
		               CPG_NETWORK_LOAD_ERROR_OBJECT,
		               "Cannot extend type %s with type %s",
		               g_type_name (G_TYPE_FROM_INSTANCE (child)),
		               g_type_name (gtype));

		xmlFree (id);
		return NULL;
	}

	gboolean ret = TRUE;

	/* Apply all the templates */
	for (item = templates; item; item = g_slist_next (item))
	{
		cpg_object_apply_template (child,
		                           item->data);
	}

	g_slist_free (templates);

	xmlFree (id);

	if (!ret || !parse_object_properties (deserializer, node, child))
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
new_object (CpgNetworkDeserializer *deserializer,
            GType                   gtype,
            xmlNodePtr              node)
{
	CpgObject *object;
	gboolean new_object;

	object = parse_object (deserializer, gtype, node, &new_object);

	if (object)
	{
		if (new_object)
		{
			if (!cpg_group_add (CPG_GROUP (deserializer->priv->parents->data),
			                    object,
			                    NULL))
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
parse_globals (CpgNetworkDeserializer *deserializer,
               xmlNodePtr              node)
{
	return parse_object_properties (deserializer,
	                                node,
	                                CPG_OBJECT (deserializer->priv->network));
}

static gboolean
get_function_expression (CpgNetworkDeserializer  *deserializer,
                         GList                   *nodes,
                         xmlNodePtr              *ret)
{
	if (nodes == NULL)
	{
		return FALSE;
	}

	xmlNodePtr node = (xmlNodePtr)nodes->data;

	if (!(node->children && node->children->type == XML_TEXT_NODE))
	{
		return FALSE;
	}

	*ret = node;
	return TRUE;
}

static gboolean
parse_function_arguments (CpgNetworkDeserializer *deserializer,
                          GList                  *nodes,
                          CpgFunction            *function)
{
	GList *item;

	for (item = nodes; item; item = g_list_next (item))
	{
		xmlNode *node = (xmlNode *)item->data;

		if (!(node->children && node->children->type == XML_TEXT_NODE))
		{
			continue;
		}

		gchar const *name = (gchar const *)node->children->content;

		if (cpg_object_get_property (CPG_OBJECT (function), name))
		{
			continue;
		}

		xmlChar *opt = xmlGetProp (node, (xmlChar *)"optional");
		gboolean optional = opt ? g_ascii_strcasecmp ((gchar const *)opt, "yes") == 0 : FALSE;
		xmlFree (opt);

		xmlChar *def = xmlGetProp (node, (xmlChar *)"default");
		gdouble default_value = 0;

		if (def)
		{
			default_value = g_ascii_strtod ((gchar const *)def, NULL);
			xmlFree (def);
		}

		CpgFunctionArgument *argument =
			cpg_function_argument_new (name,
			                           optional,
			                           default_value);

		save_comment (node, G_OBJECT (argument));

		cpg_function_add_argument (function, argument);
	}

	return TRUE;
}

static gboolean
parse_function (CpgNetworkDeserializer *deserializer,
                xmlNodePtr              node)
{
	xmlChar *name = xmlGetProp (node, (xmlChar *)"name");

	if (!name)
	{
		return parser_failed (deserializer,
		                      node,
		                      CPG_NETWORK_LOAD_ERROR_FUNCTION,
		                      "One of the functions does not have a name");
	}

	CpgGroup *function_group = cpg_network_get_function_group (deserializer->priv->network);

	if (cpg_group_get_child (function_group, (gchar const *)name))
	{
		parser_failed (deserializer,
		               node,
		               CPG_NETWORK_LOAD_ERROR_FUNCTION,
		               "The function `%s' is already defined",
		               name);

		xmlFree (name);
		return FALSE;
	}

	gchar *expression = NULL;
	xmlNodePtr expressionNode;

	if (!xml_xpath (deserializer,
	                node,
	                "expression",
	                XML_ELEMENT_NODE,
	                (XPathResultFunc)get_function_expression,
	                &expressionNode))
	{
		parser_failed (deserializer,
		               node,
		               CPG_NETWORK_LOAD_ERROR_FUNCTION,
		               "Expression not set for function %s",
		               name);

		xmlFree (name);
		return FALSE;
	}

	expression = g_strdup ((gchar const *)expressionNode->children->content);

	CpgFunction *function = cpg_function_new ((gchar const *)name, expression);
	g_free (expression);
	xmlFree (name);

	save_comment (expressionNode, G_OBJECT (cpg_function_get_expression (function)));

	save_comment (node, G_OBJECT (function));

	if (!xml_xpath (deserializer,
	                node,
	                "argument",
	                XML_ELEMENT_NODE,
	                (XPathResultFunc)parse_function_arguments,
	                function))
	{
		parser_failed (deserializer,
		               node,
		               CPG_NETWORK_LOAD_ERROR_FUNCTION,
		               "Failed to parse function arguments for %s",
		               cpg_object_get_id (CPG_OBJECT (function)));

		g_object_unref (function);
		return FALSE;
	}

	gboolean ret = cpg_group_add (function_group,
	                              CPG_OBJECT (function),
	                              NULL);
	g_object_unref (function);

	return ret;
}

static gboolean
parse_polynomial_pieces (CpgNetworkDeserializer *deserializer,
                         GList                  *nodes,
                         CpgFunctionPolynomial  *function)
{
	GList *item;

	for (item = nodes; item; item = g_list_next (item))
	{
		xmlNode *node = (xmlNode *)item->data;

		xmlChar *beginPtr = xmlGetProp (node, (xmlChar *)"begin");

		if (!beginPtr)
		{
			g_warning ("Piece does not define a begin");
			return FALSE;
		}

		xmlChar *endPtr = xmlGetProp (node, (xmlChar *)"end");

		if (!endPtr)
		{
			g_warning ("Piece does not define an end");
			xmlFree (beginPtr);
			return FALSE;
		}

		gdouble begin = g_ascii_strtod ((gchar const *)beginPtr, NULL);
		gdouble end = g_ascii_strtod ((gchar const *)endPtr, NULL);

		xmlFree (beginPtr);
		xmlFree (endPtr);

		if (begin >= end)
		{
			g_warning ("Begin of piece should be smaller than end");
			return FALSE;
		}

		if (!(node->children && node->children->type == XML_TEXT_NODE))
		{
			g_warning ("No coefficients are specified for polynomial piece");
			return FALSE;
		}

		gchar const *coefs = (gchar const *)node->children->content;
		gchar **ptrs = g_strsplit_set (coefs, ", ", -1);

		if (!ptrs || !*ptrs)
		{
			g_warning ("No coefficients are specified for polynomial piece");
			g_strfreev (ptrs);

			return FALSE;
		}

		guint num = g_strv_length (ptrs);
		guint num_coefficients = 0;
		gdouble *coefficients = g_new (gdouble, num);
		guint i;

		for (i = 0; i < num; ++i)
		{
			if (!*ptrs[i])
			{
				continue;
			}

			coefficients[num_coefficients++] = g_ascii_strtod (ptrs[i], NULL);
		}

		g_strfreev (ptrs);

		CpgFunctionPolynomialPiece *piece =
				cpg_function_polynomial_piece_new (begin,
				                                   end,
				                                   coefficients,
				                                   num_coefficients);

		save_comment (node, G_OBJECT (piece));

		cpg_function_polynomial_add (function, piece);

		g_free (coefficients);
	}

	return TRUE;
}

static gboolean
parse_polynomial (CpgNetworkDeserializer  *deserializer,
                  xmlNodePtr               node)
{
	xmlChar *name = xmlGetProp (node, (xmlChar *)"name");

	if (!name)
	{
		return parser_failed (deserializer,
		                      node,
		                      CPG_NETWORK_LOAD_ERROR_FUNCTION,
		                      "One of the polynomials does not have a name");
	}

	CpgGroup *function_group = cpg_network_get_function_group (deserializer->priv->network);

	if (cpg_group_get_child (function_group, (gchar const *)name))
	{
		parser_failed (deserializer,
		               node,
		               CPG_NETWORK_LOAD_ERROR_FUNCTION,
		               "The polynomial `%s' is already defined",
		               name);

		xmlFree (name);
		return FALSE;
	}

	CpgFunctionPolynomial *function = cpg_function_polynomial_new ((gchar const *)name);
	xmlFree (name);

	save_comment (node, G_OBJECT (function));

	if (!xml_xpath (deserializer,
	                node,
	                "piece",
	                XML_ELEMENT_NODE,
	                (XPathResultFunc)parse_polynomial_pieces,
	                function))
	{
		parser_failed (deserializer,
		               node,
		               CPG_NETWORK_LOAD_ERROR_FUNCTION,
		               "Failed to parse polynomial pieces for: %s",
		               cpg_object_get_id (CPG_OBJECT (function)));

		g_object_unref (function);
		return FALSE;
	}

	gboolean ret = cpg_group_add (function_group,
	                              CPG_OBJECT (function),
	                              NULL);
	g_object_unref (function);

	return ret;
}

static gboolean
parse_actions (CpgNetworkDeserializer *deserializer,
               GList                  *nodes)
{
	GList *item;
	CpgLink *link = CPG_LINK (deserializer->priv->object);

	for (item = nodes; item; item = g_list_next (item))
	{
		xmlNodePtr node = (xmlNodePtr)item->data;

		xmlChar *target = xmlGetProp (node, (xmlChar *)"target");

		if (!target)
		{
			return parser_failed (deserializer,
			                      node,
			                      CPG_NETWORK_LOAD_ERROR_LINK,
			                      "Missing target for action on %s",
			                      cpg_object_get_id (CPG_OBJECT (link)));
		}

		gchar const *expr = "";

		if (node->children && node->children->type == XML_TEXT_NODE)
		{
			expr = (gchar const *)node->children->content;
		}

		CpgLinkAction *action = cpg_link_action_new ((gchar const *)target,
		                                              cpg_expression_new (expr));

		save_comment (node, G_OBJECT (action));

		cpg_link_add_action (link, action);

		xmlFree (target);
	}

	return TRUE;
}

static gboolean
parse_link (CpgNetworkDeserializer *deserializer,
            xmlNodePtr              node)
{
	CpgObject *object;
	gboolean new_object;

	object = parse_object (deserializer, CPG_TYPE_LINK, node, &new_object);

	if (!object)
	{
		return FALSE;
	}

	/* Fill in from and to */
	xmlChar *from = xmlGetProp (node, (xmlChar *)"from");

	if (from)
	{
		CpgObject *fromobj = cpg_group_get_child (CPG_GROUP (deserializer->priv->parents->data),
		                                          (gchar const *)from);
		gboolean ret = TRUE;

		if (!fromobj)
		{
			parser_failed (deserializer,
			               node,
			               CPG_NETWORK_LOAD_ERROR_LINK,
			               "The `from' object `%s' could not be found for link `%s'",
			               from,
			               cpg_object_get_id (object));
			ret = FALSE;
		}
		else if (CPG_IS_LINK (fromobj))
		{
			g_warning ("The `from` object can not be a link (%s)",
			                 cpg_object_get_id (object));
			ret = FALSE;
		}

		xmlFree (from);

		if (!ret)
		{
			if (new_object)
			{
				g_object_unref (object);
			}

			return FALSE;
		}

		g_object_set (object, "from", fromobj, NULL);
	}

	xmlChar *to = xmlGetProp (node, (xmlChar *)"to");

	if (to)
	{
		CpgObject *toobj = cpg_group_get_child (CPG_GROUP (deserializer->priv->parents->data),
		                                       (gchar const *)to);
		gboolean ret = TRUE;

		if (!toobj)
		{
			parser_failed (deserializer,
			               node,
			               CPG_NETWORK_LOAD_ERROR_LINK,
			               "The `to' object `%s' could not be found for link `%s'",
			               to,
			               cpg_object_get_id (object));
			ret = FALSE;
		}
		else if (CPG_IS_LINK (toobj))
		{
			g_warning ("The `to' object can not be a link (%s)",
			                 cpg_object_get_id (object));
			ret = FALSE;
		}

		xmlFree (to);

		if (!ret)
		{
			if (new_object)
			{
				g_object_unref (object);
			}

			return FALSE;
		}

		g_object_set (object, "to", toobj, NULL);
	}

	deserializer->priv->object = object;

	if (!xml_xpath (deserializer,
	                node,
	                "action",
	                XML_ELEMENT_NODE,
	                (XPathResultFunc)parse_actions,
	                NULL))
	{
		g_warning ("Could not parse actions successfully");

		if (new_object)
		{
			g_object_unref (object);
		}

		return FALSE;
	}

	gboolean ret = TRUE;

	if (new_object)
	{
		ret = cpg_group_add (CPG_GROUP (deserializer->priv->parents->data),
		                     object,
		                     NULL);

		g_object_unref (object);
	}

	return ret;
}

static gboolean
parse_group (CpgNetworkDeserializer *deserializer,
             xmlNodePtr              node)
{
	CpgObject *object;

	object = new_object (deserializer, CPG_TYPE_GROUP, node);

	if (!object)
	{
		return FALSE;
	}

	/* Recurse into the group members */
	if (!parse_all (deserializer, node, object))
	{
		return FALSE;
	}

	xmlChar *proxy = xmlGetProp (node, (xmlChar *)"proxy");

	if (proxy)
	{
		CpgObject *child = cpg_group_get_child (CPG_GROUP (object),
		                                        (gchar const *)proxy);

		if (!child)
		{
			parser_failed (deserializer,
			               node,
			               CPG_NETWORK_LOAD_ERROR_OBJECT,
			               "Could not find proxy `%s' for group `%s'",
			               proxy,
			               cpg_object_get_id (object));

			xmlFree (proxy);
			return FALSE;
		}

		cpg_group_set_proxy (CPG_GROUP (object), child);

		xmlFree (proxy);
	}

	return TRUE;
}

static CpgImport *
find_template_import (CpgObject *child,
                      GFile     *file)
{
	if (CPG_IS_IMPORT (child))
	{
		GFile *f = cpg_import_get_file (CPG_IMPORT (child));
		gboolean equal = g_file_equal (file, f);

		g_object_unref (f);

		if (equal)
		{
			return CPG_IMPORT (child);
		}
	}

	if (CPG_IS_GROUP (child))
	{
		GSList const *children = cpg_group_get_children (CPG_GROUP (child));

		while (children)
		{
			CpgImport *ret = find_template_import (children->data, file);

			if (ret)
			{
				return ret;
			}

			children = g_slist_next (children);
		}
	}

	return NULL;
}

static gboolean
parse_input_file (CpgNetworkDeserializer *deserializer,
                  xmlNodePtr              node)
{
	CpgObject *object;

	object = new_object (deserializer, CPG_TYPE_INPUT_FILE, node);

	if (!object)
	{
		return FALSE;
	}

	xmlChar *prop;
	gchar *filename = NULL;

	prop = xmlGetProp (node, (xmlChar *)"filename");

	if (prop)
	{
		filename = g_strdup ((gchar const *)prop);
		xmlFree (prop);
	}

	if (!filename)
	{
		xmlNodePtr child = node->children;

		while (child)
		{
			if (child->type == XML_TEXT_NODE)
			{
				filename = g_strdup ((gchar const *)child->content);
				break;
			}

			child = child->next;
		}
	}

	GFile *file = NULL;

	if (filename)
	{
		if (!g_path_is_absolute (filename))
		{
			GFile *parent = NULL;

			if (deserializer->priv->file)
			{
				parent = g_file_get_parent (deserializer->priv->file);
			}
			else
			{
				GFile *f;

				f = cpg_network_get_file (deserializer->priv->network);
				parent = g_file_get_parent (f);
				g_object_unref (f);
			}

			if (!parent)
			{
				parser_failed (deserializer,
					       node,
					       CPG_NETWORK_LOAD_ERROR_INPUT_FILE,
					       "Input file node `%s' cannot find file `%s'",
					       cpg_object_get_id (object),
					       filename);

				g_object_unref (object);
				g_free (filename);

				return FALSE;
			}

			/* Relative to file being imported */
			file = g_file_get_child (parent, filename);
			g_object_unref (parent);
		}
		else
		{
			file = g_file_new_for_path (filename);
		}

		g_free (filename);
	}

	if (file && !g_file_query_exists (file, NULL))
	{
		gchar *path;

		path = g_file_get_path (file);
		g_object_unref (file);

		parser_failed (deserializer,
		               node,
		               CPG_NETWORK_LOAD_ERROR_INPUT_FILE,
		               "Input file node `%s' cannot find file `%s'",
		               cpg_object_get_id (object),
		               path);

		g_free (path);
		g_object_unref (object);
		return FALSE;
	}

	cpg_input_file_set_file (CPG_INPUT_FILE (object), file);

	if (file)
	{
		g_object_unref (file);
	}

	gchar const *tc = (gchar const *)xmlGetProp (node, (xmlChar *)"time-column");

	if (tc)
	{
		gint column = atoi (tc);

		if (column >= 0)
		{
			cpg_input_file_set_time_column (CPG_INPUT_FILE (object),
			                                column);
		}
		else
		{
			parser_failed (deserializer,
			               node,
			               CPG_NETWORK_LOAD_ERROR_INPUT_FILE,
			               "Input file node `%s' invalid column: %d",
			               cpg_object_get_id (object),
			               column);

			g_object_unref (object);
			return FALSE;
		}
	}

	gchar const *repeat = (gchar const *)xmlGetProp (node, (xmlChar *)"repeat");

	if (repeat)
	{
		gboolean r = g_ascii_strcasecmp (repeat, "yes");

		cpg_input_file_set_repeat (CPG_INPUT_FILE (object), r);
	}

	return TRUE;
}

static gboolean
parse_import (CpgNetworkDeserializer *deserializer,
              xmlNodePtr              node)
{
	xmlChar *id = xmlGetProp (node, (xmlChar *)"id");

	if (!id)
	{
		return parser_failed (deserializer,
		                      node,
		                      CPG_NETWORK_LOAD_ERROR_IMPORT,
		                      "Some import nodes do not have an id");
	}

	gchar const *filename = NULL;

	if (node->children && node->children->type == XML_TEXT_NODE)
	{
		filename = (gchar const *)node->children->content;
	}

	if (!filename)
	{
		parser_failed (deserializer,
		               node,
		               CPG_NETWORK_LOAD_ERROR_IMPORT,
		               "Import node `%s' does not have a filename",
		               id);

		xmlFree (id);
		return FALSE;
	}

	GFile *file = NULL;

	if (g_path_is_absolute (filename))
	{
		file = g_file_new_for_path (filename);
	}
	else
	{
		GFile *parent = NULL;

		if (deserializer->priv->file)
		{
			parent = g_file_get_parent (deserializer->priv->file);
		}
		else
		{
			GFile *f;

			f = cpg_network_get_file (deserializer->priv->network);
			parent = g_file_get_parent (f);
			g_object_unref (f);
		}

		if (parent)
		{
			/* Relative to file being imported */
			file = g_file_get_child (parent, filename);
			g_object_unref (parent);

			if (!g_file_query_exists (file, NULL))
			{
				g_object_unref (file);
				file = NULL;
			}
		}

		if (!file)
		{
			/* Current working directory maybe? */
			file = g_file_new_for_path (filename);

			if (!g_file_query_exists (file, NULL))
			{
				g_object_unref (file);
				file = NULL;
			}
		}

		if (!file)
		{
			/* Search directories */
			const gchar * const *dirs = cpg_import_get_search_path ();

			while (dirs && *dirs)
			{
				gchar *path = g_build_filename (*dirs, filename, NULL);
				file = g_file_new_for_path (path);
				g_free (path);

				if (g_file_query_exists (file, NULL))
				{
					break;
				}

				g_object_unref (file);
				file = NULL;

				++dirs;
			}
		}
	}

	if (!file)
	{
		parser_failed (deserializer,
		               node,
		               CPG_NETWORK_LOAD_ERROR_IMPORT,
		               "File `%s' for import `%s' could not be found",
		               filename,
		               id);

		xmlFree (id);
		return FALSE;
	}

	/* Check if we already imported something like that for templates */
	if (g_slist_last (deserializer->priv->parents)->data !=
	    (gpointer)deserializer->priv->network)
	{
		CpgGroup *template_group = cpg_network_get_template_group (deserializer->priv->network);
		CpgImport *import = find_template_import (CPG_OBJECT (template_group), file);

		if (import)
		{
			CpgImportAlias *alias = cpg_import_alias_new (import);
			gboolean ret = cpg_group_add (deserializer->priv->parents->data,
			                              CPG_OBJECT (alias),
			                              NULL);
			g_object_unref (alias);

			save_comment (node, G_OBJECT (alias));

			return ret;
		}
	}

	CpgImport *imp = cpg_import_new (deserializer->priv->network,
	                                 deserializer->priv->parents->data,
	                                 (gchar const *)id,
	                                 file,
	                                 deserializer->priv->error);

	g_object_unref (file);
	xmlFree (id);

	if (!imp)
	{
		return FALSE;
	}

	save_comment (node, G_OBJECT (imp));
	g_object_unref (imp);

	return TRUE;
}

static gboolean
parse_network (CpgNetworkDeserializer *deserializer,
               GList                  *nodes)
{
	GList *item;
	gboolean ret = TRUE;

	gboolean atroot = deserializer->priv->root && CPG_IS_NETWORK (deserializer->priv->parents->data);

	for (item = nodes; item; item = g_list_next (item))
	{
		xmlNodePtr node = item->data;

		gboolean has_child = xml_xpath_first (deserializer,
		                                      node,
		                                      "state | link",
		                                      XML_ELEMENT_NODE) != NULL;

		gchar const *nodename = (gchar const *)node->name;

		if (g_strcmp0 (nodename, "state") == 0)
		{
			if (has_child)
			{
				ret = parse_group (deserializer, node);
			}
			else
			{
				ret = new_object (deserializer, CPG_TYPE_OBJECT, node) != NULL;
			}
		}
		else if (g_strcmp0 (nodename, "link") == 0)
		{
			ret = parse_link (deserializer, node);
		}
		else if (g_strcmp0 (nodename, "import") == 0)
		{
			ret = parse_import (deserializer, node);
		}
		else if (g_strcmp0 (nodename, "function") == 0)
		{
			ret = parse_function (deserializer, node);
		}
		else if (g_strcmp0 (nodename, "polynomial") == 0)
		{
			ret = parse_polynomial (deserializer, node);
		}
		else if (g_strcmp0 (nodename, "input-file") == 0)
		{
			ret = parse_input_file (deserializer, node);
		}
		else if (atroot)
		{
			if (g_strcmp0 (nodename, "globals") == 0)
			{
				ret = parse_globals (deserializer, node);
			}
			else if (g_strcmp0 (nodename, "functions") == 0)
			{
				ret = parse_all (deserializer, node, NULL);
			}
			else if (g_strcmp0 (nodename, "function") == 0)
			{
				ret = parse_function (deserializer, node);
			}
			else if (g_strcmp0 (nodename, "polynomial") == 0)
			{
				ret = parse_polynomial (deserializer, node);
			}
			else if (g_strcmp0 (nodename, "templates") == 0)
			{
				/* Ignore */
				ret = TRUE;
			}
			else
			{
				g_warning ("Unknown element: %s", node->name);
			}
		}
		else
		{
			g_warning ("Unknown element: %s", node->name);
		}

		if (!ret)
		{
			break;
		}
	}

	return ret;
}

static gboolean
parse_all (CpgNetworkDeserializer *deserializer,
           xmlNodePtr              root,
           gpointer                parent)
{
	gboolean ret;
	deserializer->priv->parents = g_slist_prepend (deserializer->priv->parents,
	                                               parent);

	ret = xml_xpath (deserializer,
	                 root,
	                 "state | link | templates | functions | function | globals | polynomial | import | input-file",
	                 XML_ELEMENT_NODE,
	                 (XPathResultFunc)parse_network,
	                 NULL);

	deserializer->priv->parents = g_slist_remove (deserializer->priv->parents,
	                                              parent);
	return ret;
}

static gboolean
parse_objects (CpgNetworkDeserializer *deserializer,
               gchar const            *root_path,
               CpgGroup               *root)
{
	xmlNodePtr root_node = xml_xpath_first (deserializer,
	                                        NULL,
	                                        root_path,
	                                        XML_ELEMENT_NODE);

	if (!root_node)
	{
		return TRUE;
	}

	save_comment (root_node, G_OBJECT (root));

	return parse_all (deserializer,
	                  root_node,
	                  root);
}

static gboolean
parse_templates (CpgNetworkDeserializer *deserializer)
{
	return parse_objects (deserializer,
	                      "/cpg/network/templates",
	                      cpg_network_get_template_group (deserializer->priv->network));
}

static gboolean
parse_instances (CpgNetworkDeserializer *deserializer)
{
	return parse_objects (deserializer,
	                      "/cpg/network",
	                      deserializer->priv->root);
}

static gboolean
parse_network_config (CpgNetworkDeserializer *deserializer,
                      GList                  *nodes)
{
	if (!nodes)
	{
		return TRUE;
	}

	xmlNodePtr net = nodes->data;
	xmlChar *it = xmlGetProp (net, (xmlChar *)"integrator");

	if (it)
	{
		GType inttype = cpg_integrators_find ((gchar const *)it);

		if (inttype != G_TYPE_INVALID)
		{
			CpgIntegrator *integrator = CPG_INTEGRATOR (g_object_new (inttype, NULL));

			cpg_network_set_integrator (deserializer->priv->network,
			                            integrator);
			g_object_unref (integrator);
		}

		xmlFree (it);
	}

	return TRUE;
}

static gboolean
parse_config (CpgNetworkDeserializer *deserializer)
{
	return xml_xpath (deserializer,
	                  NULL,
	                  "/cpg/network",
	                  XML_ELEMENT_NODE,
	                  (XPathResultFunc)parse_network_config,
	                  NULL);
}

static gboolean
reader_xml (CpgNetworkDeserializer *deserializer)
{
	if (!parse_templates (deserializer))
	{
		return FALSE;
	}

	if (!parse_instances (deserializer))
	{
		return FALSE;
	}

	if (!parse_config (deserializer))
	{
		return FALSE;
	}

	return TRUE;
}

/**
 * cpg_network_deserializer_new:
 * @network: A #CpgNetwork
 * @root: A #CpgGroup
 *
 * Create a new deserializer for a given @network. When calling
 * #cpg_network_deserializer_deserialize, objects will be deserialized into
 * @root. If @root is %NULL, the objects will be deserialized in the root
 * of the network.
 *
 * Returns: A #CpgNetworkDeserializer
 *
 **/
CpgNetworkDeserializer *
cpg_network_deserializer_new (CpgNetwork *network,
                              CpgGroup   *root)
{
	return g_object_new (CPG_TYPE_NETWORK_DESERIALIZER,
	                     "network", network,
	                     "root", root,
	                     NULL);
}

static int
xml_ioread (CpgNetworkDeserializer *deserializer,
            char                   *buffer,
            int                     len)
{
	gboolean ret;
	gsize bytes_read;

	ret = g_input_stream_read_all (deserializer->priv->stream,
	                               buffer,
	                               len,
	                               &bytes_read,
	                               NULL,
	                               deserializer->priv->error);

	if (!ret)
	{
		return -1;
	}

	return bytes_read;
}

static void
xml_ioclose (CpgNetworkDeserializer *deserializer)
{
	g_input_stream_close (deserializer->priv->stream, NULL, NULL);
}

/**
 * cpg_network_deserializer_deserialize:
 * @deserializer: A #CpgNetworkDeserializer
 * @stream: A #GInputStream
 * @error: A #GError
 *
 * Deserialize a network from an input stream.
 *
 * Returns: %TRUE if the deserialization was successful, %FALSE otherwise.
 *
 **/
gboolean
cpg_network_deserializer_deserialize (CpgNetworkDeserializer  *deserializer,
                                      GInputStream            *stream,
                                      GError                 **error)
{
	g_return_val_if_fail (CPG_IS_NETWORK_DESERIALIZER (deserializer), FALSE);
	g_return_val_if_fail (G_INPUT_STREAM (stream), FALSE);

	xmlTextReaderPtr reader;

	deserializer->priv->stream = stream;
	deserializer->priv->error = error;

	reader = xmlReaderForIO ((xmlInputReadCallback)xml_ioread,
	                         (xmlInputCloseCallback)xml_ioclose,
	                         deserializer,
	                         NULL,
	                         NULL,
	                         0);

	if (reader == NULL)
	{
		return FALSE;
	}

	gint ret;

	do
	{
		ret = xmlTextReaderRead(reader);
		xmlTextReaderPreserve(reader);
	} while (ret == 1);

	if (ret != 0)
	{
		xmlFreeTextReader (reader);

		return parser_failed (deserializer,
		                      NULL,
		                      CPG_NETWORK_LOAD_ERROR_XML,
		                      "Failed parsing xml at %d:%d: %s",
		                      xmlLastError.line,
		                      xmlLastError.int2,
		                      xmlLastError.message);
	}
	else
	{
		xmlDocPtr doc = xmlTextReaderCurrentDoc(reader);

		deserializer->priv->doc = doc;
		reader_xml (deserializer);

		xmlFreeDoc (doc);
	}

	xmlFreeTextReader (reader);
	return TRUE;
}

/**
 * cpg_network_deserializer_deserialize_file:
 * @deserializer: A #CpgNetworkDeserializer
 * @file: A #GFile
 * @error: A #GError
 *
 * Convenience function to deserialize a network from a file.
 *
 * Returns: %TRUE if the deserialization was successful, %FALSE otherwise.
 *
 **/
gboolean
cpg_network_deserializer_deserialize_file (CpgNetworkDeserializer  *deserializer,
                                           GFile                   *file,
                                           GError                 **error)
{
	g_return_val_if_fail (CPG_IS_NETWORK_DESERIALIZER (deserializer), FALSE);
	g_return_val_if_fail (G_IS_FILE (file), FALSE);

	GFileInputStream *stream = g_file_read (file, NULL, error);

	if (!stream)
	{
		return FALSE;
	}

	deserializer->priv->file = g_file_dup (file);

	gboolean ret;

	ret = cpg_network_deserializer_deserialize (deserializer,
	                                            G_INPUT_STREAM (stream),
	                                            error);

	g_object_unref (deserializer->priv->file);
	deserializer->priv->file = NULL;

	g_object_unref (stream);

	return ret;
}

/**
 * cpg_network_deserializer_deserialize_path:
 * @deserializer: A #CpgNetworkDeserializer
 * @path: The file path
 * @error: A #GError
 * 
 * Convenience function to deserialize a network from a file path.
 *
 * Returns: %TRUE if the deserialization was successful, %FALSE otherwise
 *
 **/
gboolean
cpg_network_deserializer_deserialize_path (CpgNetworkDeserializer  *deserializer,
                                           gchar const             *path,
                                           GError                 **error)
{
	g_return_val_if_fail (CPG_IS_NETWORK_DESERIALIZER (deserializer), FALSE);
	g_return_val_if_fail (path != NULL, FALSE);

	GFile *file = g_file_new_for_path (path);

	gboolean ret;

	ret = cpg_network_deserializer_deserialize_file (deserializer,
	                                                 file,
	                                                 error);

	g_object_unref (file);

	return ret;
}
