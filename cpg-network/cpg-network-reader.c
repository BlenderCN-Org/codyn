#include "cpg-network-reader.h"

#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xpath.h>

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
		gboolean isint = (integrated && g_ascii_strcasecmp ((const gchar *)integrated, "true") == 0);
		xmlFree (integrated);
		
		cpg_object_add_property (object, 
		                         (const gchar *)name, 
		                         (const gchar *)expression, 
		                         isint);		
		xmlFree (name);
	}
	
	return TRUE;
}

static CpgObject *
parse_object (GType      gtype,
              xmlNodePtr node)
{
	xmlChar *id = xmlGetProp (node, (xmlChar *)"id");
	
	if (!id)
	{
		cpg_debug_error ("Object does not have an id");
		return NULL;
	}
	
	CpgObject *obj = g_object_new (gtype, "id", (gchar const *)id, NULL);
	xmlFree (id);
	
	/* Parse properties */
	if (!xml_xpath (node->doc, node, "property", XML_ELEMENT_NODE, (XPathResultFunc)parse_properties, obj))
	{
		cpg_debug_error ("Could not parse object properties for: %s", cpg_object_get_id (obj));

		g_object_unref (obj);
		obj = NULL;
	}

	return obj;
}

static gboolean
new_object (GType       gtype,
            xmlNodePtr  node,
            CpgNetwork *network)
{
	CpgObject *object;
	
	object = parse_object (gtype, node);
	
	if (object)
	{
		cpg_network_add_object (network, object);
		g_object_unref (object);
		return TRUE;
	}
	else
	{
		return FALSE;
	}
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
		CpgProperty *property = cpg_object_get_property (to, (gchar const *)target);
		
		if (!property)
		{
			cpg_debug_error ("Property %s not found on %s", target, cpg_object_get_id (to));
			xmlFree (target);
			return FALSE;
		}
		
		xmlChar const *expression = (xmlChar *)"";
		
		if (node->children && node->children->type == XML_TEXT_NODE)
		{
			expression = node->children->content;
		}
		
		cpg_link_add_action (link, property, (gchar const *)expression);
		xmlFree (target);
	}
	
	return TRUE;
}

static gboolean
parse_link (xmlDocPtr   doc,
            xmlNodePtr  node,
            CpgNetwork *network)
{
	CpgObject *object;
	
	object = parse_object (CPG_TYPE_LINK, node);
	
	if (!object)
	{
		return FALSE;
	}
	
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
	
	CpgObject *fromobj = cpg_network_get_object (network, (gchar const *)from);
	CpgObject *toobj = cpg_network_get_object (network, (gchar const *)to);
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
	
	if (!xml_xpath (doc, node, "action", XML_ELEMENT_NODE, (XPathResultFunc)parse_actions, object))
	{
		g_object_unref (object);
		return FALSE;
	}
	
	cpg_network_add_object (network, object);
	g_object_unref (object);

	return TRUE;
}

static gboolean
parse_network (xmlDocPtr   doc, 
               GList      *nodes, 
               CpgNetwork *network)
{
	GList *item;
	gboolean ret = TRUE;
	
	for (item = nodes; item; item = g_list_next (item))
	{
		xmlNodePtr node = (xmlNodePtr)item->data;
		
		if (g_strcmp0 ((gchar const *)node->name, "state") == 0)
		{
			ret = new_object (CPG_TYPE_STATE, node, network);
		}
		else if (g_strcmp0 ((gchar const *)node->name, "relay") == 0)
		{
			ret = new_object (CPG_TYPE_RELAY, node, network);
		}
		else if (g_strcmp0 ((gchar const *)node->name, "link") == 0)
		{
			ret = parse_link (doc, node, network);
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

gboolean
parse_objects (xmlDocPtr    doc,
               const gchar *path,
               CpgNetwork  *network)
{
	return xml_xpath (doc, 
	                  NULL,
	                  path, 
	                  XML_ELEMENT_NODE, 
	                  (XPathResultFunc)parse_network, 
	                  network);
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
	
	gboolean ret = parse_objects (doc, "/cpg/network/state", network) &&
	               parse_objects (doc, "/cpg/network/relay", network) &&
	               parse_objects (doc, "/cpg/network/link", network);

	xmlFreeDoc (doc);
	return ret;
}
