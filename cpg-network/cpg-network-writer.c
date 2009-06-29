#include "cpg-network-writer.h"
#include <libxml/tree.h>
#include <gio/gio.h>

extern int xmlIndentTreeOutput;

static void
properties_to_xml (xmlDocPtr   doc,
                   xmlNodePtr  parent,
                   CpgObject  *object)
{
	GSList *item;
	
	for (item = cpg_object_get_properties (object); item; item = g_slist_next (item))
	{
		CpgProperty *property = (CpgProperty *)item->data;
		CpgExpression *expression = cpg_property_get_value_expression (property);

		xmlNodePtr node = xmlNewDocNode (doc, NULL, (xmlChar *)"property", NULL);
		xmlNewProp (node, (xmlChar *)"name", (xmlChar *)cpg_property_get_name (property));
		xmlNodePtr text = xmlNewDocText (doc, (xmlChar *)cpg_expression_get_as_string (expression));
		
		xmlAddChild (node, text);
		xmlAddChild (parent, node);
	}
}

static xmlNodePtr
object_to_xml (xmlDocPtr     doc,
               xmlNodePtr    parent,
               CpgObject    *object,
               gchar const  *name)
{
	xmlNodePtr ptr = xmlNewDocNode (doc, NULL, (xmlChar *)name, NULL);
	
	xmlNewProp (ptr, (xmlChar *)"id", (xmlChar *)cpg_object_get_id (object));
	xmlAddChild (parent, ptr);
	
	properties_to_xml (doc, ptr, object);
	return ptr;
}

static xmlNodePtr
relay_to_xml (xmlDocPtr   doc,
              xmlNodePtr  parent,
              CpgRelay   *relay)
{
	return object_to_xml (doc, parent, CPG_OBJECT (relay), "relay");
}

static xmlNodePtr
state_to_xml (xmlDocPtr   doc,
              xmlNodePtr  parent,
              CpgState   *state)
{
	return object_to_xml (doc, parent, CPG_OBJECT (state), "state");
}

static xmlNodePtr
link_to_xml (xmlDocPtr   doc,
             xmlNodePtr  parent,
             CpgLink    *link)
{
	CpgObject *from = cpg_link_get_from (link);
	CpgObject *to = cpg_link_get_to (link);
	
	xmlNodePtr node = object_to_xml (doc, parent, CPG_OBJECT (link), "link");
	
	if (from != NULL)
	{
		xmlNewProp (node, (xmlChar *)"from", (xmlChar *)cpg_object_get_id (from));
	}
	
	if (to != NULL)
	{
		xmlNewProp (node, (xmlChar *)"to", (xmlChar *)cpg_object_get_id (to));
	}
	
	// Link actions
	GSList *item;
	
	for (item = cpg_link_get_actions (link); item; item = g_slist_next (item))
	{
		CpgLinkAction *action = (CpgLinkAction *)(item->data);
		
		xmlNodePtr ac = xmlNewDocNode (doc, NULL, (xmlChar *)"action", NULL);
		xmlNewProp (ac, (xmlChar *)"target", (xmlChar *)cpg_property_get_name (cpg_link_action_get_target (action)));
		
		xmlNodePtr text = xmlNewDocText (doc, (xmlChar *)cpg_expression_get_as_string (cpg_link_action_get_expression (action)));
		
		xmlAddChild (ac, text);
		xmlAddChild (node, ac);
	}
	
	return node;
}

gchar * 
cpg_network_writer_xml_string (CpgNetwork *network)
{
	xmlDocPtr doc = xmlNewDoc ((xmlChar *)"1.0");
	xmlNodePtr root = xmlNewDocNode (doc, NULL, (xmlChar *)"cpg", NULL);
	
	xmlDocSetRootElement (doc, root);
	
	// Generate templates
	GSList *list = cpg_network_get_templates (network);
	GSList *item;
	xmlNodePtr templates;
	
	if (list)
	{
		templates = xmlNewDocNode (doc, NULL, (xmlChar *)"templates", NULL);
		xmlAddChild (root, templates);
	}
	
	for (item = list; item; item = g_slist_next (item))
	{
		gchar const *name = (gchar const *)item->data;
		CpgObject *template = cpg_network_get_template (network, name);
		
		if (CPG_IS_RELAY (template))
		{
			relay_to_xml (doc, templates, CPG_RELAY (template));
		}
		else if (CPG_IS_STATE (template))
		{
			state_to_xml (doc, templates, CPG_STATE (template));
		}
		else if (CPG_IS_LINK (template))
		{
			link_to_xml (doc, templates, CPG_LINK (template));
		}
	}
	
	g_slist_foreach (list, (GFunc)g_free, NULL);
	g_slist_free (list);

	// Generate state, relay and link nodes
	for (item = cpg_network_get_states (network); item; item = g_slist_next (item))
	{
		if (CPG_IS_RELAY (item->data))
		{
			relay_to_xml (doc, root, CPG_RELAY (item->data));
		}
		else if (CPG_IS_STATE (item->data))
		{
			state_to_xml (doc, root, CPG_STATE (item->data));
		}
	}
	
	for (item = cpg_network_get_links (network); item; item = g_slist_next (item))
	{
		link_to_xml (doc, root, CPG_LINK (item->data));
	}
	
	object_to_xml (doc, root, cpg_network_get_globals (network), "globals");
	
	xmlIndentTreeOutput = 1;
	
	xmlChar *mem;
	int size;
	
	xmlDocDumpFormatMemory (doc, &mem, &size, 1);
	
	gchar *ret = g_strndup ((gchar const *)mem, size);
	xmlFree (mem);
	
	return ret;
}

gboolean 
cpg_network_writer_xml (CpgNetwork  *network,
                        gchar const *filename)
{
	gchar *contents = cpg_network_writer_xml_string (network);

	if (contents == NULL)
	{
		return FALSE;
	}

	gboolean ret = g_file_set_contents (filename, contents, -1, NULL);
	g_free (contents);
	
	return ret;
}

