#include "cpg-network-writer.h"

#include <libxml/tree.h>

#include "cpg-function-polynomial.h"

extern int xmlIndentTreeOutput;

static void group_to_xml (xmlDocPtr doc, xmlNodePtr root, CpgGroup *group);

static gboolean
property_matches_template (CpgProperty  *property,
                           GSList const *templates)
{
	while (templates)
	{
		CpgProperty *tprop;

		tprop = cpg_object_get_property (templates->data,
		                                 cpg_property_get_name (property));

		if (tprop)
		{
			return cpg_property_equal (property, tprop);
		}

		templates = g_slist_next (templates);
	}

	return FALSE;
}

static CpgObject *
template_selector (CpgObject *parent,
                   GSList    *selectors,
                   CpgObject *child)
{
	while (selectors)
	{
		CpgObject *selector = selectors->data;

		parent = cpg_group_get_child (CPG_GROUP (parent),
		                              cpg_object_get_id (selector));

		if (!parent)
		{
			return NULL;
		}

		selectors = g_slist_next (selectors);
	}

	return cpg_group_get_child (CPG_GROUP (parent),
	                            cpg_object_get_id (child));
}

static GSList *
templates_for_object (CpgObject *object,
                      gboolean   include_own)
{
	/* These include the 'direct' templates on object, but also the
	   indirect ones that were applied by a template group */
	GSList *ret = NULL;

	if (include_own)
	{
		ret = g_slist_copy ((GSList *)cpg_object_get_templates (object));
	}

	CpgObject *parent = cpg_object_get_parent (object);
	GSList *selectors = NULL;

	while (parent)
	{
		GSList const *templates = cpg_object_get_templates (parent);

		while (templates)
		{
			CpgObject *template;

			template = template_selector (templates->data,
			                              selectors,
			                              object);

			if (template)
			{
				ret = g_slist_prepend (ret, template);
			}

			templates = g_slist_next (templates);
		}

		selectors = g_slist_prepend (selectors, parent);
		parent = cpg_object_get_parent (parent);
	}

	return g_slist_reverse (ret);
}

static void
properties_to_xml (xmlDocPtr     doc,
                   xmlNodePtr    parent,
                   CpgObject    *object,
                   GSList const *templates)
{
	GSList *item;
	GSList *properties;

	properties = cpg_object_get_properties (object);

	for (item = properties; item; item = g_slist_next (item))
	{
		CpgProperty *property = item->data;

		if (cpg_property_get_object (property) != object)
		{
			continue;
		}

		CpgExpression *expression = cpg_property_get_expression (property);

		// Check if property is different from template
		if (property_matches_template (property, templates))
		{
			continue;
		}

		xmlNodePtr node = xmlNewDocNode (doc, NULL, (xmlChar *)"property", NULL);
		xmlNewProp (node, (xmlChar *)"name", (xmlChar *)cpg_property_get_name (property));

		if (cpg_property_get_integrated (property))
		{
			xmlNewProp (node, (xmlChar *)"integrated", (xmlChar *)"yes");
		}

		if (cpg_property_get_hint (property) & CPG_PROPERTY_HINT_OUT)
		{
			xmlNewProp (node, (xmlChar *)"out", (xmlChar *)"yes");
		}

		if (cpg_property_get_hint (property) & CPG_PROPERTY_HINT_IN)
		{
			xmlNewProp (node, (xmlChar *)"in", (xmlChar *)"yes");
		}

		if (cpg_property_get_hint (property) & CPG_PROPERTY_HINT_ONCE)
		{
			xmlNewProp (node, (xmlChar *)"once", (xmlChar *)"yes");
		}

		xmlNodePtr text = xmlNewDocText (doc, (xmlChar *)cpg_expression_get_as_string (expression));

		xmlAddChild (node, text);
		xmlAddChild (parent, node);
	}

	g_slist_free (properties);
}

static gboolean
check_inherited (CpgObject *object,
                 CpgObject *template)
{
	GSList *fakes = templates_for_object (object, FALSE);
	GSList *item;

	for (item = fakes; item; item = g_slist_next (item))
	{
		GSList const *templates = cpg_object_get_templates (item->data);

		while (templates)
		{
			if (g_strcmp0 (cpg_object_get_id (template),
			               cpg_object_get_id (templates->data)) == 0)
			{
				return TRUE;
			}

			templates = g_slist_next (templates);
		}
	}

	g_slist_free (fakes);
	return FALSE;
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

	GSList const *templates = cpg_object_get_templates (object);

	GPtrArray *refs = g_ptr_array_new ();

	while (templates)
	{
		/* only apply templates that are not inherited */
		if (!check_inherited (object, templates->data))
		{
			g_ptr_array_add (refs, (gpointer)cpg_object_get_id (templates->data));
		}

		templates = g_slist_next (templates);
	}

	g_ptr_array_add (refs, NULL);

	gchar **refs_ptr = (gchar **)g_ptr_array_free (refs, FALSE);

	if (refs_ptr && *refs_ptr)
	{
		gchar *joined = g_strjoinv (", ", refs_ptr);
		xmlNewProp (ptr, (xmlChar *)"ref", (xmlChar *)joined);
		g_free (joined);
	}

	g_free (refs_ptr);

	GSList *all_templates = templates_for_object (object, TRUE);

	properties_to_xml (doc, ptr, object, all_templates);

	g_slist_free (all_templates);

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

static gboolean
action_matches_template (CpgLinkAction *action,
                         GSList const  *templates)
{
	CpgProperty *p1 = cpg_link_action_get_target (action);
	CpgExpression *e1 = cpg_link_action_get_expression (action);

	while (templates)
	{
		CpgObject *template = templates->data;
		templates = g_slist_next (templates);

		if (!CPG_IS_LINK (template))
		{
			continue;
		}

		GSList const *actions = cpg_link_get_actions (CPG_LINK (template));

		while (actions)
		{
			CpgLinkAction *other = actions->data;

			CpgProperty *p2 = cpg_link_action_get_target (other);
			CpgExpression *e2 = cpg_link_action_get_expression (other);

			if (g_strcmp0 (cpg_property_get_name (p1),
			               cpg_property_get_name (p2)) == 0)
			{
				return cpg_expression_equal (e1, e2);
			}

			actions = g_slist_next (actions);
		}
	}

	return FALSE;
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
	GSList const *item;
	GSList *templates = templates_for_object (CPG_OBJECT (link), TRUE);

	for (item = cpg_link_get_actions (link); item; item = g_slist_next (item))
	{
		CpgLinkAction *action = item->data;

		if (action_matches_template (action, templates))
		{
			continue;
		}

		xmlNodePtr ac = xmlNewDocNode (doc, NULL, (xmlChar *)"action", NULL);
		xmlNewProp (ac, (xmlChar *)"target", (xmlChar *)cpg_property_get_name (cpg_link_action_get_target (action)));

		xmlNodePtr text = xmlNewDocText (doc, (xmlChar *)cpg_expression_get_as_string (cpg_link_action_get_expression (action)));

		xmlAddChild (ac, text);
		xmlAddChild (node, ac);
	}

	g_slist_free (templates);

	return node;
}

static void
write_function (CpgNetwork  *network,
                xmlDocPtr    doc,
                CpgFunction *func,
                xmlNodePtr   funcs)
{
	xmlNodePtr funcn = xmlNewDocNode (doc, NULL, (xmlChar *)"function", NULL);
	xmlNewProp (funcn,
	            (xmlChar *)"name",
	            (xmlChar *)cpg_object_get_id (CPG_OBJECT (func)));

	xmlAddChild (funcs, funcn);

	/* Create expression element */
	CpgExpression *expression = cpg_function_get_expression (func);

	if (expression)
	{
		xmlNodePtr exprn = xmlNewDocNode (doc, NULL, (xmlChar *)"expression", NULL);
		xmlNodePtr text = xmlNewDocText (doc,
		                                 (xmlChar *)cpg_expression_get_as_string (expression));

		xmlAddChild (exprn, text);
		xmlAddChild (funcn, exprn);
	}

	/* Create argument elements */
	GList *args = cpg_function_get_arguments (func);
	GList *argitem;

	for (argitem = args; argitem; argitem = g_list_next (argitem))
	{
		CpgFunctionArgument *argument = (CpgFunctionArgument *)argitem->data;

		xmlNodePtr argn = xmlNewDocNode (doc, NULL, (xmlChar *)"argument", NULL);
		xmlNodePtr text = xmlNewDocText (doc,
		                                 (xmlChar *)cpg_function_argument_get_name (argument));

		xmlAddChild (argn, text);

		if (cpg_function_argument_get_optional (argument))
		{
			gchar defPtr[G_ASCII_DTOSTR_BUF_SIZE];

			xmlNewProp (argn, (xmlChar *)"optional", (xmlChar *)"yes");

			g_ascii_dtostr (defPtr,
			                G_ASCII_DTOSTR_BUF_SIZE,
			                cpg_function_argument_get_default_value (argument));
			xmlNewProp (argn, (xmlChar *)"default", (xmlChar *)defPtr);
		}

		xmlAddChild (funcn, argn);
	}
}

static void
write_function_polynomial (CpgNetwork            *network,
                           xmlDocPtr              doc,
                           CpgFunctionPolynomial *func,
                           xmlNodePtr             funcs)
{
	xmlNodePtr funcn = xmlNewDocNode (doc, NULL, (xmlChar *)"polynomial", NULL);
	xmlNewProp (funcn,
	            (xmlChar *)"name",
	            (xmlChar *)cpg_object_get_id (CPG_OBJECT (func)));

	xmlAddChild (funcs, funcn);

	/* Create pieces */
	GSList *pieces = cpg_function_polynomial_get_pieces (func);

	while (pieces)
	{
		CpgFunctionPolynomialPiece *piece = (CpgFunctionPolynomialPiece *)pieces->data;

		gchar beginPtr[G_ASCII_DTOSTR_BUF_SIZE];
		gchar endPtr[G_ASCII_DTOSTR_BUF_SIZE];

		g_ascii_dtostr (beginPtr,
		                G_ASCII_DTOSTR_BUF_SIZE,
		                cpg_function_polynomial_piece_get_begin (piece));

		g_ascii_dtostr (endPtr,
		                G_ASCII_DTOSTR_BUF_SIZE,
		                cpg_function_polynomial_piece_get_end (piece));

		xmlNodePtr piecen = xmlNewDocNode (doc, NULL, (xmlChar *)"piece", NULL);
		xmlNewProp (piecen, (xmlChar *)"begin", (xmlChar *)beginPtr);
		xmlNewProp (piecen, (xmlChar *)"end", (xmlChar *)endPtr);

		GString *str = g_string_new ("");
		guint num;
		guint i;
		gdouble *coefficients = cpg_function_polynomial_piece_get_coefficients (piece,
		                                                                        &num);

		for (i = 0; i < num; ++i)
		{
			gchar coefPtr[G_ASCII_DTOSTR_BUF_SIZE];

			if (i != 0)
			{
				g_string_append (str, ", ");
			}

			g_ascii_dtostr (coefPtr, G_ASCII_DTOSTR_BUF_SIZE, coefficients[i]);
			g_string_append (str, coefPtr);
		}

		xmlNodePtr text = xmlNewDocText (doc,
		                                 (xmlChar *)str->str);

		g_string_free (str, TRUE);

		xmlAddChild (piecen, text);
		xmlAddChild (funcn, piecen);

		pieces = g_slist_next (pieces);
	}
}

static void
write_functions (CpgNetwork *network,
                 xmlDocPtr   doc,
                 xmlNodePtr  nnetwork)
{
	GSList *functions = cpg_network_get_functions (network);
	GSList *item;

	if (functions == NULL)
	{
		return;
	}

	xmlNodePtr funcs = xmlNewDocNode (doc, NULL, (xmlChar *)"functions", NULL);
	xmlAddChild (nnetwork, funcs);

	for (item = functions; item; item = g_slist_next (item))
	{
		CpgFunction *func = CPG_FUNCTION (item->data);

		if (CPG_IS_FUNCTION_POLYNOMIAL (func))
		{
			write_function_polynomial (network, doc, CPG_FUNCTION_POLYNOMIAL (func), funcs);
		}
		else
		{
			write_function (network, doc, func, funcs);
		}
	}
}

static gboolean
skip_object (CpgObject *object)
{
	if (!cpg_object_get_parent (object))
	{
		return FALSE;
	}

	GSList *templates = g_slist_reverse (templates_for_object (object,
	                                                           TRUE));

	if (g_slist_length (templates) ==
	    g_slist_length ((GSList *)cpg_object_get_templates (object)))
	{
		return FALSE;
	}

	CpgObject *dummy = g_object_new (G_TYPE_FROM_INSTANCE (object),
	                                 "id", cpg_object_get_id (object),
	                                 NULL);

	GSList *item;

	for (item = templates; item; item = g_slist_next (item))
	{
		_cpg_object_apply_template (dummy, item->data);
	}

	g_slist_free (templates);

	if (cpg_object_equal (object, dummy))
	{
		g_object_unref (dummy);
		return TRUE;
	}

	g_object_unref (dummy);
	return FALSE;
}

static void
cpg_object_to_xml (xmlDocPtr   doc,
                   xmlNodePtr  root,
                   CpgObject  *object)
{
	/* Check if this object can be omitted because it's covered fully
	   by its fake template */
	if (skip_object (object))
	{
		return;
	}

	if (CPG_IS_GROUP (object))
	{
		group_to_xml (doc, root, CPG_GROUP (object));
	}
	else if (CPG_IS_RELAY (object))
	{
		relay_to_xml (doc, root, CPG_RELAY (object));
	}
	else if (CPG_IS_STATE (object))
	{
		state_to_xml (doc, root, CPG_STATE (object));
	}
	else if (CPG_IS_LINK (object))
	{
		link_to_xml (doc, root, CPG_LINK (object));
	}
}

static gboolean
check_proxy_template (CpgObject *object,
                      CpgObject *proxy)
{
	GSList *templates = templates_for_object (object, TRUE);
	GSList *item;

	for (item = templates; item; item = g_slist_next (item))
	{
		if (CPG_IS_GROUP (item->data))
		{
			CpgObject *other_proxy = cpg_group_get_proxy (item->data);

			if (other_proxy != NULL &&
			    g_strcmp0 (cpg_object_get_id (proxy),
			               cpg_object_get_id (other_proxy)) == 0)
			{
				return TRUE;
			}
		}

		item = g_slist_next (item);
	}

	g_slist_free (templates);
	return FALSE;
}

static void
group_to_xml (xmlDocPtr   doc,
              xmlNodePtr  root,
              CpgGroup   *group)
{
	xmlNodePtr group_node;

	if (!CPG_IS_NETWORK (group))
	{
		group_node = object_to_xml (doc, root, CPG_OBJECT (group), "group");

		CpgObject *proxy = cpg_group_get_proxy (group);

		if (proxy != NULL &&
		    !check_proxy_template (CPG_OBJECT (group), proxy))
		{
			xmlNewProp (group_node,
			            (xmlChar *)"proxy",
			            (xmlChar *)cpg_object_get_id (proxy));
		}
	}
	else
	{
		group_node = root;
	}

	GSList const *children = cpg_group_get_children (group);

	while (children)
	{
		cpg_object_to_xml (doc, group_node, children->data);
		children = g_slist_next (children);
	}

}

static void
write_config (xmlDocPtr   doc,
              CpgNetwork *network,
              xmlNodePtr  nnetwork)
{
	CpgIntegrator *integrator = cpg_network_get_integrator (network);

	if (integrator != NULL)
	{
		xmlNewProp (nnetwork,
		            (xmlChar *)"integrator",
		            (xmlChar *)cpg_object_get_id (CPG_OBJECT (integrator)));
	}
}

gchar *
cpg_network_writer_xml_string (CpgNetwork *network)
{
	xmlDocPtr doc = xmlNewDoc ((xmlChar *)"1.0");
	xmlNodePtr root = xmlNewDocNode (doc, NULL, (xmlChar *)"cpg", NULL);

	xmlDocSetRootElement (doc, root);

	xmlNodePtr nnetwork = xmlNewDocNode (doc, NULL, (xmlChar *)"network", NULL);
	xmlAddChild (root, nnetwork);

	write_config (doc, network, nnetwork);

	// Globals
	GSList *properties = cpg_object_get_properties (CPG_OBJECT (network));

	if (properties)
	{
		xmlNodePtr gbl = xmlNewDocNode (doc, NULL, (xmlChar *)"globals", NULL);
		xmlAddChild (nnetwork, gbl);

		properties_to_xml (doc, gbl, CPG_OBJECT (network), NULL);
	}

	g_slist_free (properties);

	// Generate templates
	GSList *list = cpg_network_get_templates (network);
	xmlNodePtr templates;

	if (list)
	{
		templates = xmlNewDocNode (doc, NULL, (xmlChar *)"templates", NULL);
		xmlAddChild (nnetwork, templates);
	}

	GSList *item;

	for (item = list; item; item = g_slist_next (item))
	{
		gchar const *name = (gchar const *)item->data;
		CpgObject *template = cpg_network_get_template (network, name);

		cpg_object_to_xml (doc, templates, template);
	}

	g_slist_foreach (list, (GFunc)g_free, NULL);
	g_slist_free (list);

	// Generate state, relay and link nodes
	group_to_xml (doc, nnetwork, CPG_GROUP (network));

	write_functions (network, doc, nnetwork);

	xmlIndentTreeOutput = 1;

	xmlChar *mem;
	int size;

	xmlDocDumpFormatMemoryEnc (doc,
	                           &mem,
	                           &size,
	                           xmlGetCharEncodingName (XML_CHAR_ENCODING_UTF8),
	                           1);

	gchar *ret = g_strndup ((gchar const *)mem, size);
	xmlFree (mem);
	xmlFreeDoc (doc);

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

