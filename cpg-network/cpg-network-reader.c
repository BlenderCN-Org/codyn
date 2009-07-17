#include "cpg-network-reader.h"

#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include <libxml/xinclude.h>

#include <errno.h>
#include <string.h>

#include "cpg-network.h"
#include "cpg-debug.h"

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
parse_properties (xmlDocPtr  doc,
                  GList     *nodes,
                  CpgObject *object)
{
	GList *item;
	
	for (item = nodes; item; item = g_list_next (item))
	{
		xmlNodePtr node = (xmlNodePtr)item->data;
		xmlChar *name = xmlGetProp (node, (xmlChar *)"name");
			
		if (!name)
		{
			cpg_debug_error ("Missing 'name' for property");
			continue;
		}
		
		xmlChar const *expression = (xmlChar *)"";
		
		if (node->children && node->children->type == XML_TEXT_NODE)
		{
			expression = node->children->content;
		}
		
		xmlChar *integrated = xmlGetProp (node, (xmlChar *)"integrated");
		gboolean isint = (integrated && (g_ascii_strcasecmp ((const gchar *)integrated, "true") == 0 ||
		                                 g_ascii_strcasecmp ((const gchar *)integrated, "yes") == 0 ||
		                                 g_ascii_strcasecmp ((const gchar *)integrated, "1") == 0));
		xmlFree (integrated);
		
		CpgProperty *property;
		
		property = cpg_object_add_property (object, 
		                                    (const gchar *)name, 
		                                    (const gchar *)expression, 
		                                    isint);

		if (property)
		{
			xmlChar *variant = xmlGetProp (node, (xmlChar *)"variant");
			
			gboolean isvariant = (variant && (g_ascii_strcasecmp ((const gchar *)variant, "true") == 0 ||
		                                 g_ascii_strcasecmp ((const gchar *)variant, "yes") == 0 ||
		                                 g_ascii_strcasecmp ((const gchar *)variant, "1") == 0));

			cpg_property_set_variant (property, isvariant);
			
			xmlFree (variant);
		}
		
		xmlFree (name);
	}
	
	return TRUE;
}

static gboolean
parse_object_properties (CpgObject  *object,
                         xmlNodePtr  node)
{
	if (!xml_xpath (node->doc, node, "property", XML_ELEMENT_NODE, (XPathResultFunc)parse_properties, object))
	{
		cpg_debug_error ("Could not parse object properties for: %s", cpg_object_get_id (object));
		return FALSE;
	}
	
	return TRUE;
}

typedef struct
{
	CpgNetwork *network;
	gboolean template;
} ParseInfo;

static CpgObject *
parse_object (GType       gtype,
              xmlNodePtr  node,
              ParseInfo  *info)
{
	xmlChar *id = xmlGetProp (node, (xmlChar *)"id");
	
	if (!id)
	{
		cpg_debug_error ("Object does not have an id");
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
			cpg_debug_error ("Referenced template not found");

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
	if (!parse_object_properties (obj, node))
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
	                                node);
}

static gboolean
parse_actions (xmlDocPtr doc,
               GList     *nodes,
               CpgLink   *link)
{
	GList *item;
	CpgObject *to = cpg_link_get_to (link);
	
	for (item = nodes; item; item = g_list_next (item))
	{
		xmlNodePtr node = (xmlNodePtr)item->data;
		
		xmlChar *target = xmlGetProp (node, (xmlChar *)"target");
		
		if (!target)
		{
			cpg_debug_error ("Missing target for action of %s", cpg_object_get_id (CPG_OBJECT (link)));
			return FALSE;
		}
		
		/* Find target property in link.to */
		CpgProperty *property;
		
		if (to)
		{
			property = cpg_object_get_property (to, (gchar const *)target);
		
			if (!property)
			{
				cpg_debug_error ("Property %s not found on %s", target, cpg_object_get_id (to));
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
			g_object_unref (object);
			return FALSE;
		}
	
		xmlChar *to = xmlGetProp (node, (xmlChar *)"to");
	
		if (!to)
		{
			xmlFree (from);
			g_object_unref (object);
			return FALSE;
		}
	
		CpgObject *fromobj = cpg_network_get_object (info->network, (gchar const *)from);
		CpgObject *toobj = cpg_network_get_object (info->network, (gchar const *)to);
		gboolean ret = TRUE;
	
		if (!fromobj)
		{
			cpg_debug_error ("From object %s not found link %s", from, cpg_object_get_id (object));
			ret = FALSE;
		}
		else if (CPG_IS_LINK (fromobj))
		{
			cpg_debug_error ("From object cannot be a link (%s)", cpg_object_get_id (object));
			ret = FALSE;
		}
		else if (!toobj)
		{
			cpg_debug_error ("Target object %s not found for link %s", to, cpg_object_get_id (object));
			ret = FALSE;
		}
		else if (CPG_IS_LINK (toobj))
		{
			cpg_debug_error ("Target object cannot be a link (%s)", cpg_object_get_id (object));
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
	
	if (!xml_xpath (doc, node, "action", XML_ELEMENT_NODE, (XPathResultFunc)parse_actions, object))
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
		else
		{
			cpg_debug_error ("Unknown element: %s", node->name);
			ret = FALSE;
		}
		
		if (!ret)
		{
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
parse_templates (xmlDocPtr   doc,
                 CpgNetwork *network)
{
	ParseInfo info = {network, TRUE};
	
	gboolean ret = parse_objects (doc, "/cpg/network/templates/state", &info) &&
	               parse_objects (doc, "/cpg/network/templates/relay", &info) &&
	               parse_objects (doc, "/cpg/network/templates/link", &info);

	return ret;
}

static gboolean
reader_xml (CpgNetwork *network,
            xmlDocPtr   doc)
{
	if (!doc)
	{
		cpg_debug_error ("Could not open network file: %s", strerror (errno));
		return FALSE;
	}
	
	xmlXIncludeProcess (doc);
	
	if (!parse_templates (doc, network))
	{
		cpg_debug_error ("Failed to parse templates");
		return FALSE;
	}
	
	ParseInfo info = {network, FALSE};
	
	gboolean ret = parse_objects (doc, "/cpg/network/state", &info) &&
	               parse_objects (doc, "/cpg/network/relay", &info) &&
	               parse_objects (doc, "/cpg/network/link", &info) &&
	               parse_objects (doc, "/cpg/network/globals", &info);

	xmlFreeDoc (doc);
	return ret;
}

gboolean
cpg_network_reader_xml (CpgNetwork  *network,
                        gchar const *filename)
{
	return reader_xml (network, xmlParseFile (filename));
}

gboolean
cpg_network_reader_xml_string (CpgNetwork  *network,
                               gchar const *xml)
{
	return reader_xml (network, xmlParseDoc ((xmlChar *)xml));
}

