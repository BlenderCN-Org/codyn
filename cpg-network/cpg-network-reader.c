#include "cpg-network-reader.h"

#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include <libxml/xinclude.h>

#include <errno.h>
#include <string.h>

#include "cpg-network.h"
#include "cpg-debug.h"
#include "cpg-integrators.h"
#include "cpg-function-polynomial.h"

typedef struct
{
	CpgNetwork *network;
	GError **error;
	gboolean template;
	CpgObject *object;
	GSList *parents;
} ParseInfo;

static gboolean parse_all (xmlDocPtr   doc,
                           xmlNodePtr  root,
                           gpointer    parent,
                           ParseInfo  *info);

typedef gboolean (*XPathResultFunc)(xmlDocPtr doc, GList *nodes, gpointer data);

static gboolean
xml_xpath (xmlDocPtr        doc,
           xmlNodePtr       root,
           gchar const     *expr,
           xmlElementType   type,
           XPathResultFunc  func,
           gpointer         data)
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
		ctx = xmlXPathNewContext (doc);
		ctx->node = root;

		if (!ctx)
		{
			cpg_debug_error ("Could not create XPath context");
			return FALSE;
		}

		obj = xmlXPathEvalExpression ((xmlChar *)expr, ctx);

		if (!obj)
		{
			cpg_debug_error ("Failed to evaluate xpath expression '%s'", expr);
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
	gboolean ret = func (doc, set, data);
	g_list_free (set);

	return ret;
}

static gboolean
xpath_first (xmlDocPtr doc, GList *nodes, gpointer data)
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
xml_xpath_first (xmlDocPtr        doc,
                 xmlNodePtr       root,
                 gchar const     *expr,
                 xmlElementType   type)
{
	xmlNodePtr first = NULL;

	xml_xpath (doc, root, expr, type, xpath_first, &first);
	return first;
}

static gboolean
parser_failed (ParseInfo   *info,
               xmlNodePtr   node,
               gint         code,
               gchar const *format,
               ...)
{
	if (info->error != NULL)
	{
		va_list ap;
		va_start (ap, format);

		gchar *message = g_strdup_vprintf (format, ap);
		va_end (ap);

		if (*info->error)
		{
			g_error_free (*info->error);
			*info->error = NULL;
		}

		cpg_debug_error ("XML load error: %s", message);

		g_set_error (info->error,
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
attribute_true (xmlNodePtr node, gchar const *name)
{
	xmlChar *prop = xmlGetProp (node, (xmlChar *)name);
	gboolean ret = (prop && (g_ascii_strcasecmp ((const gchar *)prop, "true") == 0 ||
	                         g_ascii_strcasecmp ((const gchar *)prop, "yes") == 0 ||
	                         g_ascii_strcasecmp ((const gchar *)prop, "1") == 0));
	xmlFree (prop);

	return ret;
}

static gboolean
parse_properties (xmlDocPtr  doc,
                  GList     *nodes,
                  ParseInfo *info)
{
	GList *item;

	for (item = nodes; item; item = g_list_next (item))
	{
		xmlNodePtr node = (xmlNodePtr)item->data;
		xmlChar *name = xmlGetProp (node, (xmlChar *)"name");

		if (!name)
		{
			xmlFree (name);

			return parser_failed (info,
			                      node,
			                      CPG_NETWORK_LOAD_ERROR_PROPERTY,
			                      "Property on %s has no name",
			                      cpg_object_get_id (info->object));
		}

		xmlChar const *expression = (xmlChar *)"";

		if (node->children && node->children->type == XML_TEXT_NODE)
		{
			expression = node->children->content;
		}

		CpgProperty *property;

		property = cpg_object_add_property (info->object,
		                                    (const gchar *)name,
		                                    (const gchar *)expression,
		                                    attribute_true (node, "integrated") ||
		                                    attribute_true (node, "integrate"));

		if (property)
		{
			if (attribute_true (node, "in"))
			{
				cpg_property_add_hint (property,
				                       CPG_PROPERTY_HINT_IN);
			}

			if (attribute_true (node, "out"))
			{
				cpg_property_add_hint (property,
				                       CPG_PROPERTY_HINT_OUT);
			}

			if (attribute_true (node, "once"))
			{
				cpg_property_add_hint (property,
				                       CPG_PROPERTY_HINT_ONCE);
			}
		}

		xmlFree (name);
	}

	return TRUE;
}

static gboolean
parse_object_properties (CpgObject  *object,
                         xmlNodePtr  node,
                         ParseInfo  *info)
{
	info->object = object;

	if (!xml_xpath (node->doc,
	                node,
	                "property",
	                XML_ELEMENT_NODE,
	                (XPathResultFunc)parse_properties,
	                info))
	{
		cpg_debug_error ("Could not parse object properties for: %s",
		                 cpg_object_get_id (object));
		return FALSE;
	}

	return TRUE;
}

static CpgObject *
parse_object (GType       gtype,
              xmlNodePtr  node,
              ParseInfo  *info,
              gboolean   *new_object)
{
	xmlChar *id = xmlGetProp (node, (xmlChar *)"id");
	*new_object = FALSE;

	if (!id)
	{
		parser_failed (info,
		               node,
		               CPG_NETWORK_LOAD_ERROR_OBJECT,
		               "One of the objects does not have an id");

		return NULL;
	}

	/* Check if the object already exists, if so, extend the existing object
	   instead of creating a new one */
	CpgGroup *parent = info->parents->data;
	CpgObject *child;

	child = cpg_group_get_child (parent, (gchar const *)id);

	if (!child)
	{
		child = g_object_new (gtype, "id", (gchar const *)id, NULL);
		*new_object = TRUE;
	}
	else if (!g_type_is_a (gtype, G_TYPE_FROM_INSTANCE (child)))
	{
		parser_failed (info,
		               node,
		               CPG_NETWORK_LOAD_ERROR_OBJECT,
		               "Cannot extend type %s with type %s",
		               g_type_name (G_TYPE_FROM_INSTANCE (child)),
		               g_type_name (gtype));

		xmlFree (id);
		return NULL;
	}

	xmlChar *ref = xmlGetProp (node, (xmlChar *)"ref");
	gboolean ret = TRUE;

	if (ref)
	{
		gchar **parts;
		gchar **ptr;

		parts = g_strsplit_set ((gchar const *)ref, ", ", 0);

		for (ptr = parts; *ptr; ++ptr)
		{
			CpgObject *template = cpg_network_get_template (info->network,
			                                                *ptr);

			if (!template)
			{
				ret = parser_failed (info,
				                     node,
				                     CPG_NETWORK_LOAD_ERROR_OBJECT,
				                     "Could not find template %s for object %s",
				                     ref,
				                     id);
			}
			else
			{
				GType template_type = G_TYPE_FROM_INSTANCE (template);
			
				if (!g_type_is_a (gtype, template_type))
				{
					ret = parser_failed (info,
					                     node,
					                     CPG_NETWORK_LOAD_ERROR_OBJECT,
					                     "Referenced template is of incorrect type %s (need %s)",
					                     g_type_name (template_type),
					                     g_type_name (gtype));
				}
				else
				{
					_cpg_object_apply_template (child,
					                            template);
				}
			}

			if (!ret)
			{
				break;
			}
		}

		g_strfreev (parts);
	}

	xmlFree (id);
	xmlFree (ref);

	if (!ret || !parse_object_properties (child, node, info))
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
new_object (GType       gtype,
            xmlNodePtr  node,
            ParseInfo  *info)
{
	CpgObject *object;
	gboolean new_object;

	object = parse_object (gtype, node, info, &new_object);

	if (object)
	{
		if (new_object)
		{
			if (info->template)
			{
				cpg_network_add_template (info->network,
				                          cpg_object_get_id (object),
				                          object);
			}
			else
			{
				cpg_group_add (CPG_GROUP (info->parents->data),
				               object);
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
parse_globals (xmlDocPtr   doc,
               xmlNodePtr  node,
               ParseInfo  *info)
{
	return parse_object_properties (CPG_OBJECT (info->network),
	                                node,
	                                info);
}

static gboolean
get_function_expression (xmlDocPtr   doc,
                         GList      *nodes,
                         gchar     **ret)
{
	if (nodes == NULL)
	{
		return FALSE;
	}

	xmlNode *node = (xmlNode *)nodes->data;

	if (!(node->children && node->children->type == XML_TEXT_NODE))
	{
		return FALSE;
	}

	*ret = g_strdup ((gchar const *)node->children->content);
	return TRUE;
}

static gboolean
parse_function_arguments (xmlDocPtr    doc,
                          GList       *nodes,
                          CpgFunction *function)
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

		gboolean optional = attribute_true (node, "optional");
		xmlChar *def = xmlGetProp (node, (xmlChar *)"default");
		gdouble default_value = 0;

		if (def)
		{
			default_value = g_ascii_strtod ((gchar const *)def, NULL);
			xmlFree (def);
		}

		CpgFunctionArgument *argument = cpg_function_argument_new (name, optional, default_value);
		cpg_function_add_argument (function, argument);
		cpg_ref_counted_unref (argument);
	}

	return TRUE;
}

static gboolean
parse_function (xmlDocPtr   doc,
                xmlNodePtr  node,
                ParseInfo  *info)
{
	xmlChar *name = xmlGetProp (node, (xmlChar *)"name");

	if (!name)
	{
		return parser_failed (info,
		                      node,
		                      CPG_NETWORK_LOAD_ERROR_FUNCTION,
		                      "One of the functions does not have a name");
	}

	if (cpg_network_get_function (info->network, (gchar const *)name))
	{
		parser_failed (info,
		               node,
		               CPG_NETWORK_LOAD_ERROR_FUNCTION,
		               "The function `%s' is already defined",
		               name);

		xmlFree (name);
		return FALSE;
	}

	gchar *expression = NULL;

	if (!xml_xpath (node->doc,
	                node,
	                "expression",
	                XML_ELEMENT_NODE,
	                (XPathResultFunc)get_function_expression,
	                &expression))
	{
		parser_failed (info,
		               node,
		               CPG_NETWORK_LOAD_ERROR_FUNCTION,
		               "Expression not set for function %s",
		               name);

		xmlFree (name);
		return FALSE;
	}

	CpgFunction *function = cpg_function_new ((gchar const *)name, expression);
	g_free (expression);
	xmlFree (name);

	if (!xml_xpath (node->doc,
	                node,
	                "argument",
	                XML_ELEMENT_NODE,
	                (XPathResultFunc)parse_function_arguments,
	                function))
	{
		parser_failed (info,
		               node,
		               CPG_NETWORK_LOAD_ERROR_FUNCTION,
		               "Failed to parse function arguments for %s",
		               cpg_object_get_id (CPG_OBJECT (function)));

		g_object_unref (function);
		return FALSE;
	}

	cpg_network_add_function (info->network, function);
	g_object_unref (function);

	return TRUE;
}

static gboolean
parse_polynomial_pieces (xmlDocPtr              doc,
                         GList                 *nodes,
                         CpgFunctionPolynomial *function)
{
	GList *item;

	for (item = nodes; item; item = g_list_next (item))
	{
		xmlNode *node = (xmlNode *)item->data;

		xmlChar *beginPtr = xmlGetProp (node, (xmlChar *)"begin");

		if (!beginPtr)
		{
			cpg_debug_error ("Piece does not define a begin");
			return FALSE;
		}

		xmlChar *endPtr = xmlGetProp (node, (xmlChar *)"end");

		if (!endPtr)
		{
			cpg_debug_error ("Piece does not define an end");
			xmlFree (beginPtr);
			return FALSE;
		}

		gdouble begin = g_ascii_strtod ((gchar const *)beginPtr, NULL);
		gdouble end = g_ascii_strtod ((gchar const *)endPtr, NULL);

		xmlFree (beginPtr);
		xmlFree (endPtr);

		if (begin >= end)
		{
			cpg_debug_error ("Begin of piece should be smaller than end");
			return FALSE;
		}

		if (!(node->children && node->children->type == XML_TEXT_NODE))
		{
			cpg_debug_error ("No coefficients are specified for polynomial piece");
			return FALSE;
		}

		gchar const *coefs = (gchar const *)node->children->content;
		gchar **ptrs = g_strsplit_set (coefs, ", ", -1);

		if (!ptrs || !*ptrs)
		{
			cpg_debug_error ("No coefficients are specified for polynomial piece");
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

		cpg_function_polynomial_add (function, piece);

		g_free (coefficients);
	}

	return TRUE;
}

static gboolean
parse_polynomial (xmlDocPtr   doc,
                  xmlNodePtr  node,
                  ParseInfo  *info)
{
	xmlChar *name = xmlGetProp (node, (xmlChar *)"name");

	if (!name)
	{
		return parser_failed (info,
		                      node,
		                      CPG_NETWORK_LOAD_ERROR_FUNCTION,
		                      "One of the polynomials does not have a name");
	}

	if (cpg_network_get_function (info->network, (gchar const *)name))
	{
		parser_failed (info,
		               node,
		               CPG_NETWORK_LOAD_ERROR_FUNCTION,
		               "The polynomial `%s' is already defined",
		               name);

		xmlFree (name);
		return FALSE;
	}

	CpgFunctionPolynomial *function = cpg_function_polynomial_new ((gchar const *)name);
	xmlFree (name);

	if (!xml_xpath (node->doc,
	                node,
	                "piece",
	                XML_ELEMENT_NODE,
	                (XPathResultFunc)parse_polynomial_pieces,
	                function))
	{
		parser_failed (info,
		               node,
		               CPG_NETWORK_LOAD_ERROR_FUNCTION,
		               "Failed to parse polynomial pieces for: %s",
		               cpg_object_get_id (CPG_OBJECT (function)));

		g_object_unref (function);
		return FALSE;
	}

	cpg_network_add_function (info->network, CPG_FUNCTION (function));
	g_object_unref (function);

	return TRUE;
}

static gboolean
parse_actions (xmlDocPtr doc,
               GList     *nodes,
               ParseInfo *info)
{
	GList *item;
	CpgLink *link = CPG_LINK (info->object);
	CpgObject *to = cpg_link_get_to (link);

	for (item = nodes; item; item = g_list_next (item))
	{
		xmlNodePtr node = (xmlNodePtr)item->data;

		xmlChar *target = xmlGetProp (node, (xmlChar *)"target");

		if (!target)
		{
			return parser_failed (info,
			                      node,
			                      CPG_NETWORK_LOAD_ERROR_LINK,
			                      "Missing target for action on %s",
			                      cpg_object_get_id (CPG_OBJECT (link)));
		}

		/* Find target property in link.to */
		CpgProperty *property;

		if (to)
		{
			property = cpg_object_get_property (to, (gchar const *)target);

			if (!property)
			{
				parser_failed (info,
				               node,
				               CPG_NETWORK_LOAD_ERROR_LINK,
				               "Target property %s not found for action on %s",
				               target,
				               cpg_object_get_id (to));

				xmlFree (target);
				return FALSE;
			}
		}
		else
		{
			property = cpg_property_new ((gchar const *)target,
			                             "",
			                             FALSE,
			                             NULL);
		}

		xmlChar const *expression = (xmlChar *)"";

		if (node->children && node->children->type == XML_TEXT_NODE)
		{
			expression = node->children->content;
		}

		cpg_link_add_action (link, property, (gchar const *)expression);

		if (!to)
		{
			g_object_unref (property);
		}

		xmlFree (target);
	}

	return TRUE;
}

static gboolean
parse_link (xmlDocPtr   doc,
            xmlNodePtr  node,
            ParseInfo  *info)
{
	CpgObject *object;
	gboolean new_object;
	gboolean atroot = !info->template && CPG_IS_NETWORK (info->parents->data);

	object = parse_object (CPG_TYPE_LINK, node, info, &new_object);

	if (!object)
	{
		return FALSE;
	}

	/* Fill in from and to */
	xmlChar *from = xmlGetProp (node, (xmlChar *)"from");

	if (!from && atroot && new_object)
	{
		parser_failed (info,
		               node,
		               CPG_NETWORK_LOAD_ERROR_LINK,
		               "Link node %s is missing required `from' attribute",
		               cpg_object_get_id (object));

		g_object_unref (object);
		return FALSE;
	}

	if (from)
	{
		CpgObject *fromobj = cpg_group_get_child (CPG_GROUP (info->parents->data),
		                                          (gchar const *)from);
		gboolean ret = TRUE;

		if (!fromobj)
		{
			parser_failed (info,
			               node,
			               CPG_NETWORK_LOAD_ERROR_LINK,
			               "The `from' object `%s' could not be found for link `%s'",
			               from,
			               cpg_object_get_id (object));
			ret = FALSE;
		}
		else if (CPG_IS_LINK (fromobj))
		{
			cpg_debug_error ("The `from` object can not be a link (%s)",
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

	if (!to && atroot && new_object)
	{
		parser_failed (info,
		               node,
		               CPG_NETWORK_LOAD_ERROR_LINK,
		               "Link node %s is missing required `to' attribute",
		               cpg_object_get_id (object));

		g_object_unref (object);
		return FALSE;
	}

	if (to)
	{
		CpgObject *toobj = cpg_group_get_child (CPG_GROUP (info->parents->data),
		                                       (gchar const *)to);
		gboolean ret = TRUE;

		if (!toobj)
		{
			parser_failed (info,
			               node,
			               CPG_NETWORK_LOAD_ERROR_LINK,
			               "The `to' object `%s' could not be found for link `%s'",
			               to,
			               cpg_object_get_id (object));
			ret = FALSE;
		}
		else if (CPG_IS_LINK (toobj))
		{
			cpg_debug_error ("The `to' object can not be a link (%s)",
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

	info->object = object;

	if (!xml_xpath (doc,
	                node,
	                "action",
	                XML_ELEMENT_NODE,
	                (XPathResultFunc)parse_actions,
	                info))
	{
		cpg_debug_error ("Could not parse actions successfully");

		if (new_object)
		{
			g_object_unref (object);
		}

		return FALSE;
	}

	if (new_object)
	{
		if (!info->template)
		{
			cpg_group_add (CPG_GROUP (info->parents->data), object);
		}
		else
		{
			cpg_network_add_template (info->network,
			                          cpg_object_get_id (object),
			                          object);
		}

		g_object_unref (object);
	}

	return TRUE;
}

static gboolean
parse_group (xmlDocPtr   doc,
             xmlNodePtr  node,
             ParseInfo  *info)
{
	CpgObject *object;

	object = new_object (CPG_TYPE_GROUP, node, info);

	if (!object)
	{
		return FALSE;
	}

	/* Recurse into the group members */
	return parse_all (doc, node, object, info);
}

static gboolean
parse_network (xmlDocPtr   doc,
               GList      *nodes,
               ParseInfo  *info)
{
	GList *item;
	gboolean ret = TRUE;

	gboolean atroot = !info->template && CPG_IS_NETWORK (info->parents->data);

	for (item = nodes; item; item = g_list_next (item))
	{
		xmlNodePtr node = (xmlNodePtr)item->data;

		if (g_strcmp0 ((gchar const *)node->name, "state") == 0)
		{
			ret = new_object (CPG_TYPE_STATE, node, info) != NULL;
		}
		else if (g_strcmp0 ((gchar const *)node->name, "relay") == 0)
		{
			ret = new_object (CPG_TYPE_RELAY, node, info) != NULL;
		}
		else if (g_strcmp0 ((gchar const *)node->name, "link") == 0)
		{
			ret = parse_link (doc, node, info);
		}
		else if (g_strcmp0 ((gchar const *)node->name, "group") == 0)
		{
			ret = parse_group (doc, node, info);
		}
		else if (atroot)
		{
			if (g_strcmp0 ((gchar const *)node->name, "globals") == 0)
			{
				ret = parse_globals (doc, node, info);
			}
			else if (g_strcmp0 ((gchar const *)node->name, "functions") == 0)
			{
				ret = parse_all (doc, node, NULL, info);
			}
			else if (g_strcmp0 ((gchar const *)node->name, "function") == 0)
			{
				ret = parse_function (doc, node, info);
			}
			else if (g_strcmp0 ((gchar const *)node->name, "polynomial") == 0)
			{
				ret = parse_polynomial (doc, node, info);
			}
			else if (g_strcmp0 ((gchar const *)node->name, "templates") == 0)
			{
				/* Ignore */
				ret = TRUE;
			}
			else
			{
				cpg_debug_error ("Unknown element: %s", node->name);
			}
		}
		else
		{
			cpg_debug_error ("Unknown element: %s", node->name);
		}

		if (!ret)
		{
			break;
		}
	}

	return ret;
}

static gboolean
parse_all (xmlDocPtr   doc,
           xmlNodePtr  root,
           gpointer    parent,
           ParseInfo  *info)
{
	gboolean ret;
	info->parents = g_slist_prepend (info->parents, parent);

	ret = xml_xpath (doc,
	                 root,
	                 NULL,
	                 XML_ELEMENT_NODE,
	                 (XPathResultFunc)parse_network,
	                 info);

	info->parents = g_slist_remove (info->parents, parent);
	return ret;
}

static gboolean
parse_objects (xmlDocPtr    doc,
               gchar const *root_path,
               CpgNetwork  *network,
               gboolean     templates,
               GError      **error)
{
	ParseInfo info = {network, error, templates, NULL, NULL};
	xmlNodePtr root = xml_xpath_first (doc,
	                                   NULL,
	                                   root_path,
	                                   XML_ELEMENT_NODE);

	if (!root)
	{
		return TRUE;
	}

	return parse_all (doc, root, network, &info);
}

static gboolean
parse_templates (xmlDocPtr    doc,
                 CpgNetwork  *network,
                 GError     **error)
{
	return parse_objects (doc, "/cpg/network/templates", network, TRUE, error);
}

static gboolean
parse_instances (xmlDocPtr    doc,
                 CpgNetwork  *network,
                 GError     **error)
{
	return parse_objects (doc, "/cpg/network", network, FALSE, error);
}

static gboolean
parse_network_config (xmlDocPtr  doc,
                      GList     *nodes,
                      ParseInfo *info)
{
	if (!nodes)
	{
		return TRUE;
	}

	xmlNodePtr net = (xmlNodePtr)nodes->data;
	xmlChar *it = xmlGetProp (net, (xmlChar *)"integrator");

	if (it)
	{
		GType inttype = cpg_integrators_find ((gchar const *)it);

		if (inttype != G_TYPE_INVALID)
		{
			CpgIntegrator *integrator = CPG_INTEGRATOR (g_object_new (inttype, NULL));

			cpg_network_set_integrator (info->network, integrator);
			g_object_unref (integrator);
		}

		xmlFree (it);
	}

	return TRUE;
}

static gboolean
parse_config (xmlDocPtr   doc,
              CpgNetwork  *network,
              GError     **error)
{
	ParseInfo info = {network, error, TRUE};

	return xml_xpath (doc,
	                  NULL,
	                  "/cpg/network",
	                  XML_ELEMENT_NODE,
	                  (XPathResultFunc)parse_network_config,
	                  &info);
}

static gboolean
reader_xml (CpgNetwork  *network,
            xmlDocPtr    doc,
            GError     **error)
{
	if (!doc)
	{
		if (error)
		{
			g_set_error (error,
			             CPG_NETWORK_LOAD_ERROR,
			             CPG_NETWORK_LOAD_ERROR_XML,
			             "Failed parsing xml at %d:%d: %s",
			             xmlLastError.line,
			             xmlLastError.int2,
			             xmlLastError.message);
		}

		cpg_debug_error ("Could not open network file: %s", strerror (errno));
		return FALSE;
	}

	// We don't really care if the include failed
	xmlXIncludeProcess (doc);

	if (!parse_templates (doc, network, error))
	{
		xmlFreeDoc (doc);
		cpg_debug_error ("Failed to parse templates");
		return FALSE;
	}

	if (!parse_instances (doc, network, error))
	{
		xmlFreeDoc (doc);
		return FALSE;
	}

	gboolean ret = parse_config (doc, network, error);
	xmlFreeDoc (doc);

	return ret;
}

gboolean
cpg_network_reader_xml (CpgNetwork   *network,
                        gchar const  *filename,
                        GError      **error)
{
	return reader_xml (network, xmlParseFile (filename), error);
}

gboolean
cpg_network_reader_xml_string (CpgNetwork   *network,
                               gchar const  *xml,
                               GError      **error)
{
	return reader_xml (network, xmlParseDoc ((xmlChar *)xml), error);
}

