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
} ParseInfo;

typedef gboolean (*XPathResultFunc)(xmlDocPtr doc, GList *nodes, gpointer data);

static void
set_error (ParseInfo *info, int code, gchar const *format, ...)
{
	gchar *message;
	va_list ap;

	va_start (ap, format);
	message = g_strdup_vprintf (format, ap);
	va_end (ap);

	if (info->error && !*info->error)
	{
		g_set_error_literal (info->error,
		                     CPG_NETWORK_LOAD_ERROR,
		                     code,
		                     message);
	}

	cpg_debug_error (message);
	g_free (message);
}

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
	GList *set = NULL;

	for (i = 0; i < obj->nodesetval->nodeNr; ++i)
	{
		if (type == 0 || obj->nodesetval->nodeTab[i]->type == type)
		{
			set = g_list_prepend (set, obj->nodesetval->nodeTab[i]);
		}
	}

	set = g_list_reverse (set);
	gboolean ret = func (doc, set, data);
	g_list_free (set);

	/* Free up */
	xmlXPathFreeObject (obj);
	xmlXPathFreeContext (ctx);

	return ret;
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

			set_error (info,
			           CPG_NETWORK_LOAD_ERROR_PROPERTY,
			           "Property on %s has no name",
			           cpg_object_get_id (info->object));

			return FALSE;
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
				cpg_property_add_hint (property, CPG_PROPERTY_HINT_IN);
			}

			if (attribute_true (node, "out"))
			{
				cpg_property_add_hint (property, CPG_PROPERTY_HINT_OUT);
			}

			if (attribute_true (node, "once"))
			{
				cpg_property_add_hint (property, CPG_PROPERTY_HINT_ONCE);
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

	if (!xml_xpath (node->doc, node, "property", XML_ELEMENT_NODE, (XPathResultFunc)parse_properties, info))
	{
		set_error (info,
		           CPG_NETWORK_LOAD_ERROR_OBJECT,
		           "Could not parse object properties for: %s", cpg_object_get_id (object));

		return FALSE;
	}

	return TRUE;
}

static CpgObject *
parse_object (GType       gtype,
              xmlNodePtr  node,
              ParseInfo  *info)
{
	xmlChar *id = xmlGetProp (node, (xmlChar *)"id");

	if (!id)
	{
		set_error (info,
		           CPG_NETWORK_LOAD_ERROR_OBJECT,
		           "One of the objects does not have an id");

		return NULL;
	}

	xmlChar *ref = xmlGetProp (node, (xmlChar *)"ref");
	CpgObject *obj;

	if (ref)
	{
		CpgObject *template = cpg_network_get_template (info->network,
		                                                (gchar const *)ref);

		if (!template)
		{
			set_error (info,
			           CPG_NETWORK_LOAD_ERROR_OBJECT,
			           "Could not find template %s for object %s",
			           ref,
			           id);

			xmlFree (id);
			xmlFree (ref);

			return NULL;
		}

		obj = _cpg_object_copy (template);
		cpg_object_set_id (obj, (gchar const *)id);
	}
	else
	{
		obj = g_object_new (gtype, "id", (gchar const *)id, NULL);
	}

	xmlFree (id);
	xmlFree (ref);

	// Parse properties
	if (!parse_object_properties (obj, node, info))
	{
		g_object_unref (obj);
		obj = NULL;
	}

	return obj;
}

static gboolean
new_object (GType       gtype,
            xmlNodePtr  node,
            ParseInfo  *info)
{
	CpgObject *object;

	object = parse_object (gtype, node, info);

	if (object)
	{
		if (info->template)
		{
			cpg_network_add_template (info->network,
			                          cpg_object_get_id (object),
			                          object);
		}
		else
		{
			cpg_network_add_object (info->network, object);
		}

		g_object_unref (object);
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

static gboolean
parse_globals (xmlDocPtr   doc,
               xmlNodePtr  node,
               ParseInfo  *info)
{
	return parse_object_properties (cpg_network_get_globals (info->network),
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
		set_error (info,
		           CPG_NETWORK_LOAD_ERROR_FUNCTION,
		           "One of the functions does not have a name");

		return FALSE;
	}

	if (cpg_network_get_function (info->network, (gchar const *)name))
	{
		cpg_debug_error ("Function is already defined");
		xmlFree (name);

		set_error (info,
		           CPG_NETWORK_LOAD_ERROR_FUNCTION,
		           "One of the functions is already defined");

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
		cpg_debug_error ("No expression defined for function: %s", name);
		xmlFree (name);

		set_error (info,
		           CPG_NETWORK_LOAD_ERROR_FUNCTION,
		           "Expression not set for function");

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
		cpg_debug_error ("Failed to parse function arguments: %s",
		                 cpg_object_get_id (CPG_OBJECT (function)));
		g_object_unref (function);

		set_error (info,
		           CPG_NETWORK_LOAD_ERROR_FUNCTION,
		           "Failed to parse function arguments");

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
		cpg_debug_error ("Function does not have a name");

		set_error (info,
		           CPG_NETWORK_LOAD_ERROR_FUNCTION,
		           "One of the functions does not have a name");

		return FALSE;
	}

	if (cpg_network_get_function (info->network, (gchar const *)name))
	{
		cpg_debug_error ("Function is already defined");
		xmlFree (name);

		set_error (info,
		           CPG_NETWORK_LOAD_ERROR_FUNCTION,
		           "One of the functions is already defined");

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
		set_error (info,
		           CPG_NETWORK_LOAD_ERROR_FUNCTION,
		           "Failed to parse polynomial pieces: %s",
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
			set_error (info,
			           CPG_NETWORK_LOAD_ERROR_LINK,
			           "Missing target for action on %s",
			           cpg_object_get_id (CPG_OBJECT (link)));

			return FALSE;
		}

		/* Find target property in link.to */
		CpgProperty *property;

		if (to)
		{
			property = cpg_object_get_property (to, (gchar const *)target);

			if (!property)
			{
				set_error (info,
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
			cpg_ref_counted_unref (property);
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

	object = parse_object (CPG_TYPE_LINK, node, info);

	if (!object)
	{
		return FALSE;
	}

	if (!info->template)
	{
		/* Fill in from and to */
		xmlChar *from = xmlGetProp (node, (xmlChar *)"from");

		if (!from)
		{
			set_error (info,
			           CPG_NETWORK_LOAD_ERROR_LINK,
			           "Link `%s' is missing a `from' object",
			           cpg_object_get_id (object));

			g_object_unref (object);

			return FALSE;
		}

		xmlChar *to = xmlGetProp (node, (xmlChar *)"to");

		if (!to)
		{
			xmlFree (from);

			set_error (info,
			           CPG_NETWORK_LOAD_ERROR_LINK,
			           "Link `%s' is missing a `to' object",
			           cpg_object_get_id (object));

			g_object_unref (object);
			return FALSE;
		}

		CpgObject *fromobj = cpg_network_get_object (info->network, (gchar const *)from);
		CpgObject *toobj = cpg_network_get_object (info->network, (gchar const *)to);
		gboolean ret = TRUE;

		if (!fromobj)
		{
			set_error (info,
			           CPG_NETWORK_LOAD_ERROR_LINK,
			           "From object %s not found for link %s",
			           from,
			           cpg_object_get_id (object));

			ret = FALSE;
		}
		else if (CPG_IS_LINK (fromobj))
		{
			set_error (info,
			           CPG_NETWORK_LOAD_ERROR_LINK,
			           "From object %s cannot be a link (%s)",
			           from,
			           cpg_object_get_id (object));

			ret = FALSE;
		}
		else if (!toobj)
		{
			set_error (info,
			           CPG_NETWORK_LOAD_ERROR_LINK,
			           "To object %s not found for link %s",
			           to,
			           cpg_object_get_id (object));

			ret = FALSE;
		}
		else if (CPG_IS_LINK (toobj))
		{
			set_error (info,
			           CPG_NETWORK_LOAD_ERROR_LINK,
			           "To object %s cannot be a link (%s)",
			           to,
			           cpg_object_get_id (object));

			ret = FALSE;
		}

		xmlFree (from);
		xmlFree (to);

		if (!ret)
		{
			g_object_unref (object);
			return FALSE;
		}

		g_object_set (G_OBJECT (object), "from", fromobj, "to", toobj, NULL);
	}

	info->object = object;
	if (!xml_xpath (doc, node, "action", XML_ELEMENT_NODE, (XPathResultFunc)parse_actions, info))
	{
		cpg_debug_error ("Could not parse actions successfully");
		g_object_unref (object);
		return FALSE;
	}

	if (!info->template)
	{
		cpg_network_add_object (info->network, object);
	}
	else
	{
		cpg_network_add_template (info->network,
		                          cpg_object_get_id (object),
		                          object);
	}

	g_object_unref (object);

	return TRUE;
}

static gboolean
parse_network (xmlDocPtr   doc,
               GList      *nodes,
               ParseInfo  *info)
{
	GList *item;
	gboolean ret = TRUE;

	for (item = nodes; item; item = g_list_next (item))
	{
		xmlNodePtr node = (xmlNodePtr)item->data;

		if (g_strcmp0 ((gchar const *)node->name, "state") == 0)
		{
			ret = new_object (CPG_TYPE_STATE, node, info);
		}
		else if (g_strcmp0 ((gchar const *)node->name, "relay") == 0)
		{
			ret = new_object (CPG_TYPE_RELAY, node, info);
		}
		else if (g_strcmp0 ((gchar const *)node->name, "link") == 0)
		{
			ret = parse_link (doc, node, info);
		}
		else if (g_strcmp0 ((gchar const *)node->name, "globals") == 0)
		{
			ret = parse_globals (doc, node, info);
		}
		else if (g_strcmp0 ((gchar const *)node->name, "function") == 0)
		{
			ret = parse_function (doc, node, info);
		}
		else if (g_strcmp0 ((gchar const *)node->name, "polynomial") == 0)
		{
			ret = parse_polynomial (doc, node, info);
		}
		else
		{
			cpg_debug_error ("Unknown element: %s", node->name);

			set_error (info,
			           CPG_NETWORK_LOAD_ERROR_OBJECT,
			           "Unknown element in object definitions: %s",
			           node->name);

			ret = FALSE;
		}

		if (!ret)
		{
			if (info->error && !*info->error)
			{
				set_error (info,
				           CPG_NETWORK_LOAD_ERROR_OBJECT,
				           "Unknown error occurred...");
			}

			break;
		}
	}

	return ret;
}

static gboolean
parse_objects (xmlDocPtr    doc,
               const gchar *path,
               ParseInfo   *info)
{
	return xml_xpath (doc,
	                  NULL,
	                  path,
	                  XML_ELEMENT_NODE,
	                  (XPathResultFunc)parse_network,
	                  info);
}

static gboolean
parse_templates (xmlDocPtr    doc,
                 CpgNetwork  *network,
                 GError     **error)
{
	ParseInfo info = {network, error, TRUE};

	gboolean ret = parse_objects (doc, "/cpg/network/templates/state", &info) &&
	               parse_objects (doc, "/cpg/network/templates/relay", &info) &&
	               parse_objects (doc, "/cpg/network/templates/link", &info);

	return ret;
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
parser_fail (xmlDocPtr   doc,
             GError    **error)
{
	if (doc)
	{
		return TRUE;
	}

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

static gboolean
reader_xml (CpgNetwork  *network,
            xmlDocPtr    doc,
            GError     **error)
{
	if (!parser_fail (doc, error))
	{
		return FALSE;
	}

	// We don't really care if the include failed
	xmlXIncludeProcess (doc);

	if (!parse_templates (doc, network, error))
	{
		cpg_debug_error ("Failed to parse templates");
		return FALSE;
	}

	ParseInfo info = {network, error, FALSE};

	gboolean ret = parse_objects (doc, "/cpg/network/state", &info) &&
	               parse_objects (doc, "/cpg/network/relay", &info) &&
	               parse_objects (doc, "/cpg/network/link", &info) &&
	               parse_objects (doc, "/cpg/network/globals", &info) &&
	               parse_objects (doc, "/cpg/network/functions/function | /cpg/network/functions/polynomial", &info);

	if (ret)
	{
		ret = parse_config (doc, network, error);
	}
	else if (error && !*error)
	{
		set_error (&info,
		           CPG_NETWORK_LOAD_ERROR_XML,
		           "Unknown error occurred...");
	}

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

gboolean
cpg_network_reader_parse_partial (CpgNetwork   *network,
                                  gchar const  *xml,
                                  GError      **error)
{
	xmlDocPtr doc = xmlParseDoc ((xmlChar *)xml);

	if (!parser_fail (doc, error))
	{
		return FALSE;
	}

	ParseInfo info = {network, error, FALSE};

	gboolean ret = parse_objects (doc, "//state", &info) &&
	               parse_objects (doc, "//relay", &info) &&
	               parse_objects (doc, "//link", &info) &&
	               parse_objects (doc, "//globals", &info) &&
	               parse_objects (doc, "//function | //polynomial", &info);

	xmlFreeDoc(doc);

	if (!ret && error && !*error)
	{
		set_error (&info,
		           CPG_NETWORK_LOAD_ERROR_XML,
		           "Unknown error occurred...");
	}

	return ret;
}
