#include "cpg-network-serializer.h"
#include "cpg-enum-types.h"
#include "cpg-function-polynomial.h"
#include "cpg-network-xml.h"
#include "cpg-import.h"
#include "cpg-input-file.h"
#include "cpg-annotatable.h"

#include <libxml/tree.h>

/**
 * SECTION:cpg-network-serializer
 * @short_description: Network to XML serializer
 *
 * This can be used to serialize a #CpgNetwork to XML.
 *
 */
#define CPG_NETWORK_SERIALIZER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_NETWORK_SERIALIZER, CpgNetworkSerializerPrivate))

struct _CpgNetworkSerializerPrivate
{
	CpgNetwork *network;
	CpgGroup *root;
	xmlDocPtr doc;
};

G_DEFINE_TYPE (CpgNetworkSerializer, cpg_network_serializer, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NETWORK,
	PROP_ROOT
};

static void
cpg_network_serializer_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_network_serializer_parent_class)->finalize (object);
}

static void
cpg_network_serializer_dispose (GObject *object)
{
	CpgNetworkSerializer *serializer = CPG_NETWORK_SERIALIZER (object);

	if (serializer->priv->network)
	{
		g_object_unref (serializer->priv->network);
		serializer->priv->network = NULL;
	}

	if (serializer->priv->root)
	{
		g_object_unref (serializer->priv->root);
		serializer->priv->root = NULL;
	}

	G_OBJECT_CLASS (cpg_network_serializer_parent_class)->dispose (object);
}

static void
cpg_network_serializer_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
	CpgNetworkSerializer *self = CPG_NETWORK_SERIALIZER (object);

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
cpg_network_serializer_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
	CpgNetworkSerializer *self = CPG_NETWORK_SERIALIZER (object);

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
cpg_network_serializer_class_init (CpgNetworkSerializerClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_network_serializer_finalize;
	object_class->dispose = cpg_network_serializer_dispose;

	object_class->get_property = cpg_network_serializer_get_property;
	object_class->set_property = cpg_network_serializer_set_property;

	g_type_class_add_private (object_class, sizeof(CpgNetworkSerializerPrivate));

	/**
	 * CpgSerializer:network:
	 *
	 * The #CpgNetwork to serialize.
	 */
	g_object_class_install_property (object_class,
	                                 PROP_NETWORK,
	                                 g_param_spec_object ("network",
	                                                      "Network",
	                                                      "Network",
	                                                      CPG_TYPE_NETWORK,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT_ONLY));

	/**
	 * CpgSerializer:root:
	 *
	 * The #CpgGroup to serialize
	 *
	 */
	g_object_class_install_property (object_class,
	                                 PROP_ROOT,
	                                 g_param_spec_object ("root",
	                                                      "Root",
	                                                      "Root",
	                                                      CPG_TYPE_GROUP,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT_ONLY));
}

static void
cpg_network_serializer_init (CpgNetworkSerializer *self)
{
	self->priv = CPG_NETWORK_SERIALIZER_GET_PRIVATE (self);
}

/**
 * cpg_network_serializer_new:
 * @network: A #CpgNetwork
 * @root: A #CpgGroup
 *
 * Create a new serializer for @network. When serialized, only objects
 * contained in @root will be serialized. This can be useful to serialize
 * only parts of the network. If @root is %NULL, all objects will be
 * serialized.
 *
 * Returns: A #CpgNetworkSerializer
 *
 **/
CpgNetworkSerializer *
cpg_network_serializer_new (CpgNetwork *network,
                            CpgGroup   *root)
{
	return g_object_new (CPG_TYPE_NETWORK_SERIALIZER,
	                     "network", network,
	                     "root", root,
	                     NULL);
}

extern int xmlIndentTreeOutput;

static void group_to_xml (CpgNetworkSerializer *serializer, xmlNodePtr root, CpgGroup *group);

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
		ret = g_slist_copy ((GSList *)cpg_object_get_applied_templates (object));
	}

	CpgObject *parent = cpg_object_get_parent (object);
	GSList *selectors = NULL;

	while (parent)
	{
		GSList const *templates = cpg_object_get_applied_templates (parent);

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
export_flags (xmlNodePtr   node,
              CpgProperty *property)
{
	CpgPropertyFlags flags = cpg_property_get_flags (property);
	GFlagsClass *klass;
	guint i;

	gboolean flags_attr;

	flags_attr = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (property),
	                                                 CPG_NETWORK_XML_PROPERTY_FLAGS_ATTRIBUTE));

	if (flags_attr)
	{
		gchar *s = cpg_property_flags_to_string (flags);
		xmlNewProp (node, (xmlChar *)"flags", (xmlChar *)s);
		g_free (s);

		return;
	}

	klass = g_type_class_ref (CPG_TYPE_PROPERTY_FLAGS);

	for (i = 0; i < klass->n_values; ++i)
	{
		GFlagsValue *value = &(klass->values[i]);

		if (flags & value->value)
		{
			xmlNewProp (node,
			            (xmlChar *)value->value_nick,
			            (xmlChar *)"yes");
		}
	}
}

static void
restore_comment (CpgNetworkSerializer *serializer,
                 xmlNodePtr            parent,
                 GObject              *object)
{
	gchar *comment;
	gchar *with_spaces;

	if (CPG_IS_ANNOTATABLE (object))
	{
		comment = cpg_annotatable_get_annotation (CPG_ANNOTATABLE (object));
	}
	else
	{
		comment = g_strdup (g_object_get_data (object,
		                                       CPG_NETWORK_XML_COMMENT_DATA_KEY));
	}

	if (comment == NULL)
	{
		return;
	}

	with_spaces = g_strconcat (" ", comment, " ", NULL);

	xmlNodePtr node = xmlNewDocComment (serializer->priv->doc,
	                                    (xmlChar *)with_spaces);

	g_free (with_spaces);

	xmlAddChild (parent, node);
}

static void
properties_to_xml (CpgNetworkSerializer *serializer,
                   xmlNodePtr            parent,
                   CpgObject            *object,
                   GSList const         *templates,
                   GSList const         *props)
{
	GSList *item;
	GSList *properties;

	if (props)
	{
		properties = g_slist_copy ((GSList *)props);
	}
	else
	{
		properties = cpg_object_get_properties (object);
	}

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

		restore_comment (serializer, parent, G_OBJECT (property));

		xmlNodePtr node = xmlNewDocNode (serializer->priv->doc,
		                                 NULL,
		                                 (xmlChar *)"property",
		                                 NULL);
		xmlNewProp (node,
		            (xmlChar *)"name",
		            (xmlChar *)cpg_property_get_name (property));

		export_flags (node, property);

		gchar const *expr = cpg_expression_get_as_string (expression);

		if (expr && *expr)
		{
			xmlNodePtr text = xmlNewDocText (serializer->priv->doc,
			                                 (xmlChar *)expr);

			xmlAddChild (node, text);
		}

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
		GSList const *templates = cpg_object_get_applied_templates (item->data);

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

static gchar *
template_path (CpgObject *orig,
               CpgObject *template)
{
	CpgObject *parent = cpg_object_get_parent (orig);
	CpgObject *shared_root = template;

	GString *ret = g_string_new ("");
	gboolean first = TRUE;

	do
	{
		CpgObject *shared_parent = cpg_object_get_parent (shared_root);

		if (shared_parent != NULL)
		{
			if (!first)
			{
				g_string_prepend (ret, ".");
			}

			g_string_prepend (ret, cpg_object_get_id (shared_root));

			first = FALSE;
		}

		shared_root = shared_parent;
	} while (shared_root && parent != shared_root);

	return g_string_free (ret, FALSE);
}

static xmlNodePtr
object_to_xml (CpgNetworkSerializer *serializer,
               xmlNodePtr            parent,
               CpgObject            *object,
               gchar const          *name,
               GSList const         *properties)
{
	restore_comment (serializer, parent, G_OBJECT (object));

	xmlNodePtr ptr = xmlNewDocNode (serializer->priv->doc,
	                                NULL,
	                                (xmlChar *)name,
	                                NULL);

	xmlNewProp (ptr, (xmlChar *)"id", (xmlChar *)cpg_object_get_id (object));
	xmlAddChild (parent, ptr);

	GSList const *templates = cpg_object_get_applied_templates (object);

	GPtrArray *refs = g_ptr_array_new ();

	while (templates)
	{
		/* only apply templates that are not inherited */
		if (!check_inherited (object, templates->data))
		{
			gchar *path = template_path (object, templates->data);

			g_ptr_array_add (refs, path);
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

	g_strfreev (refs_ptr);

	GSList *all_templates = templates_for_object (object, TRUE);

	properties_to_xml (serializer, ptr, object, all_templates, properties);

	g_slist_free (all_templates);

	return ptr;
}

static xmlNodePtr
state_to_xml (CpgNetworkSerializer *serializer,
              xmlNodePtr            parent,
              CpgObject            *state)
{
	return object_to_xml (serializer, parent, state, "state", NULL);
}

static gboolean
action_matches_template (CpgLinkAction *action,
                         GSList const  *templates)
{
	gchar const *p1 = cpg_link_action_get_target (action);
	CpgExpression *e1 = cpg_link_action_get_equation (action);

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

			gchar const *p2 = cpg_link_action_get_target (other);
			CpgExpression *e2 = cpg_link_action_get_equation (other);

			if (g_strcmp0 (p1, p2) == 0)
			{
				return cpg_expression_equal (e1, e2);
			}

			actions = g_slist_next (actions);
		}
	}

	return FALSE;
}

static xmlNodePtr
link_to_xml (CpgNetworkSerializer *serializer,
             xmlNodePtr            parent,
             CpgLink              *link)
{
	CpgObject *from = cpg_link_get_from (link);
	CpgObject *to = cpg_link_get_to (link);

	xmlNodePtr node = object_to_xml (serializer,
	                                 parent,
	                                 CPG_OBJECT (link),
	                                 "link",
	                                 NULL);

	if (from != NULL)
	{
		xmlNewProp (node,
		            (xmlChar *)"from",
		            (xmlChar *)cpg_object_get_id (from));
	}

	if (to != NULL)
	{
		xmlNewProp (node,
		            (xmlChar *)"to",
		            (xmlChar *)cpg_object_get_id (to));
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

		restore_comment (serializer, node, G_OBJECT (action));

		xmlNodePtr ac = xmlNewDocNode (serializer->priv->doc,
		                               NULL,
		                               (xmlChar *)"action",
		                               NULL);
		xmlNewProp (ac,
		            (xmlChar *)"target",
		            (xmlChar *)cpg_link_action_get_target (action));

		gchar const *expr = cpg_expression_get_as_string (cpg_link_action_get_equation (action));

		if (expr && *expr)
		{
			xmlNodePtr text = xmlNewDocText (serializer->priv->doc,
		                                 (xmlChar *)expr);

			xmlAddChild (ac, text);
		}

		xmlAddChild (node, ac);
	}

	g_slist_free (templates);

	return node;
}

static void
write_function (CpgNetworkSerializer *serializer,
                CpgFunction          *func,
                xmlNodePtr            funcs)
{
	restore_comment (serializer, funcs, G_OBJECT (func));

	xmlNodePtr funcn = xmlNewDocNode (serializer->priv->doc, NULL, (xmlChar *)"function", NULL);
	xmlNewProp (funcn,
	            (xmlChar *)"name",
	            (xmlChar *)cpg_object_get_id (CPG_OBJECT (func)));

	xmlAddChild (funcs, funcn);

	/* Create expression element */
	CpgExpression *expression = cpg_function_get_expression (func);

	if (expression)
	{
		restore_comment (serializer, funcn, G_OBJECT (expression));

		xmlNodePtr exprn = xmlNewDocNode (serializer->priv->doc,
		                                  NULL,
		                                  (xmlChar *)"expression",
		                                  NULL);

		gchar const *expr = cpg_expression_get_as_string (expression);

		if (expr && *expr)
		{
			xmlNodePtr text = xmlNewDocText (serializer->priv->doc,
			                                 (xmlChar *)expr);

			xmlAddChild (exprn, text);
		}

		xmlAddChild (funcn, exprn);
	}

	/* Create argument elements */
	GList const *args = cpg_function_get_arguments (func);
	GList const *argitem;

	for (argitem = args; argitem; argitem = g_list_next (argitem))
	{
		CpgFunctionArgument *argument = argitem->data;

		restore_comment (serializer, funcn, G_OBJECT (argument));

		xmlNodePtr argn = xmlNewDocNode (serializer->priv->doc,
		                                 NULL,
		                                 (xmlChar *)"argument",
		                                 NULL);
		xmlNodePtr text = xmlNewDocText (serializer->priv->doc,
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
write_function_polynomial (CpgNetworkSerializer  *serializer,
                           CpgFunctionPolynomial *func,
                           xmlNodePtr             funcs)
{
	restore_comment (serializer, funcs, G_OBJECT (func));

	xmlNodePtr funcn = xmlNewDocNode (serializer->priv->doc, NULL, (xmlChar *)"polynomial", NULL);
	xmlNewProp (funcn,
	            (xmlChar *)"name",
	            (xmlChar *)cpg_object_get_id (CPG_OBJECT (func)));

	xmlAddChild (funcs, funcn);

	/* Create pieces */
	GSList const *pieces = cpg_function_polynomial_get_pieces (func);

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

		xmlNodePtr piecen = xmlNewDocNode (serializer->priv->doc, NULL, (xmlChar *)"piece", NULL);
		xmlNewProp (piecen, (xmlChar *)"begin", (xmlChar *)beginPtr);
		xmlNewProp (piecen, (xmlChar *)"end", (xmlChar *)endPtr);

		GString *str = g_string_new ("");
		guint num;
		guint i;
		gdouble const *coefficients;

		coefficients = cpg_function_polynomial_piece_get_coefficients (piece,
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

		xmlNodePtr text = xmlNewDocText (serializer->priv->doc,
		                                 (xmlChar *)str->str);

		g_string_free (str, TRUE);

		xmlAddChild (piecen, text);
		xmlAddChild (funcn, piecen);

		pieces = g_slist_next (pieces);
	}
}

static GSList *
filter_functions (GSList const *functions)
{
	GSList *ret = NULL;

	while (functions)
	{
		if (!cpg_object_get_auto_imported (functions->data))
		{
			ret = g_slist_prepend (ret, functions->data);
		}

		functions = g_slist_next (functions);
	}

	return g_slist_reverse (ret);
}

static void
write_functions (CpgNetworkSerializer *serializer,
                 xmlNodePtr            nnetwork)
{
	CpgGroup *function_group = cpg_network_get_function_group (serializer->priv->network);
	GSList *functions = filter_functions (cpg_group_get_children (function_group));
	GSList *item;

	if (functions == NULL)
	{
		return;
	}

	xmlNodePtr funcs = xmlNewDocNode (serializer->priv->doc, NULL, (xmlChar *)"functions", NULL);
	xmlAddChild (nnetwork, funcs);

	for (item = functions; item; item = g_slist_next (item))
	{
		CpgFunction *func = CPG_FUNCTION (item->data);

		if (CPG_IS_FUNCTION_POLYNOMIAL (func))
		{
			write_function_polynomial (serializer, CPG_FUNCTION_POLYNOMIAL (func), funcs);
		}
		else
		{
			write_function (serializer, func, funcs);
		}
	}

	g_slist_free (functions);
}

static gboolean
skip_object (CpgObject *object)
{
	if (!cpg_object_get_parent (object))
	{
		return FALSE;
	}

	if (cpg_object_get_auto_imported (object))
	{
		return TRUE;
	}

	GSList *templates = g_slist_reverse (templates_for_object (object,
	                                                           TRUE));

	if (g_slist_length (templates) ==
	    g_slist_length ((GSList *)cpg_object_get_applied_templates (object)))
	{
		return FALSE;
	}

	CpgObject *dummy = g_object_new (G_TYPE_FROM_INSTANCE (object),
	                                 "id", cpg_object_get_id (object),
	                                 NULL);

	_cpg_object_set_parent (dummy, cpg_object_get_parent (object));

	GSList *item;

	for (item = templates; item; item = g_slist_next (item))
	{
		cpg_object_apply_template (dummy, item->data);
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
import_to_xml (CpgNetworkSerializer *serializer,
               xmlNodePtr            root,
               CpgImport            *import)
{
	if (cpg_modifiable_get_modified (CPG_MODIFIABLE (import)))
	{
		group_to_xml (serializer, root, CPG_GROUP (import));
		return;
	}

	restore_comment (serializer, root, G_OBJECT (import));

	xmlNodePtr node = xmlNewDocNode (serializer->priv->doc, NULL, (xmlChar *)"import", NULL);

	xmlNewProp (node, (xmlChar *)"id", (xmlChar *)cpg_object_get_id (CPG_OBJECT (import)));

	GFile *network_file = cpg_network_get_file (serializer->priv->network);
	GFile *import_file = cpg_import_get_file (import);

	gchar *path = NULL;

	if (network_file)
	{
		GFile *parent;

		parent = g_file_get_parent (network_file);

		path = g_file_get_relative_path (parent, import_file);

		g_object_unref (network_file);
		g_object_unref (parent);
	}

	if (!path)
	{
		gchar *cwd = g_get_current_dir ();
		GFile *cwd_file = g_file_new_for_path (cwd);
		g_free (cwd);

		path = g_file_get_relative_path (cwd_file, import_file);
		g_object_unref (cwd_file);
	}

	if (!path)
	{
		path = g_file_get_path (import_file);
	}

	g_object_unref (import_file);

	xmlNodePtr text = xmlNewDocText (serializer->priv->doc,
	                                 (xmlChar *)path);

	xmlAddChild (node, text);
	g_free (path);

	xmlAddChild (root, node);
}

static void
input_file_to_xml (CpgNetworkSerializer *serializer,
                   xmlNodePtr            parent,
                   CpgInputFile         *input)
{
	xmlNodePtr node;
	GSList *properties;
	gchar **columns;
	gchar **item;

	properties = cpg_object_get_properties (CPG_OBJECT (input));

	columns = cpg_input_file_get_columns (input);

	for (item = columns; *item; ++item)
	{
		properties = g_slist_remove (properties,
		                             cpg_object_get_property (CPG_OBJECT (input), *item));
	}

	g_strfreev (columns);

	node = object_to_xml (serializer,
	                      parent,
	                      CPG_OBJECT (input),
	                      "input-file",
	                      properties);

	g_slist_free (properties);

	/* File */
	GFile *file;
	file = cpg_input_file_get_file (input);

	if (file)
	{
		GFile *network_file;
		GFile *parent;

		network_file = cpg_network_get_file (serializer->priv->network);
		gchar *path = NULL;

		if (network_file)
		{
			parent = g_file_get_parent (network_file);

			path = g_file_get_relative_path (parent, file);

			g_object_unref (parent);
			g_object_unref (network_file);
		}

		if (!path)
		{
			path = g_file_get_path (file);
		}

		if (node->children)
		{
			xmlNewProp (node,
			            (xmlChar *)"filename",
			            (xmlChar *)path);
		}
		else
		{
			xmlNodePtr text;

			text = xmlNewDocText (serializer->priv->doc,
			                      (xmlChar *)path);

			xmlAddChild (node, text);
		}

		g_free (path);
		g_object_unref (file);
	}

	/* Repeat */
	if (cpg_input_file_get_repeat (input))
	{
		xmlNewProp (node, (xmlChar *)"repeat", (xmlChar *)"yes");
	}

	/* Time column */
	gint time_column;
	gboolean isset;

	time_column = cpg_input_file_get_time_column (input, &isset);

	if (isset)
	{
		gchar *ptr = g_strdup_printf ("%d", time_column);
		xmlNewProp (node, (xmlChar *)"time-column", (xmlChar *)ptr);
		g_free (ptr);
	}
}

static void
cpg_object_to_xml (CpgNetworkSerializer *serializer,
                   xmlNodePtr            root,
                   CpgObject            *object)
{
	/* Check if this object can be omitted because it's covered fully
	   by its fake template */
	if (skip_object (object))
	{
		return;
	}

	if (CPG_IS_IMPORT (object))
	{
		import_to_xml (serializer, root, CPG_IMPORT (object));
	}
	else if (CPG_IS_GROUP (object))
	{
		group_to_xml (serializer, root, CPG_GROUP (object));
	}
	else if (CPG_IS_LINK (object))
	{
		link_to_xml (serializer, root, CPG_LINK (object));
	}
	else if (CPG_IS_INPUT_FILE (object))
	{
		input_file_to_xml (serializer, root, CPG_INPUT_FILE (object));
	}
	else
	{
		state_to_xml (serializer, root, object);
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
group_children_to_xml (CpgNetworkSerializer *serializer,
                       xmlNodePtr            group_node,
                       CpgGroup             *group)
{
	GSList const *children = cpg_group_get_children (group);
	GSList *links = NULL;

	while (children)
	{
		if (CPG_IS_LINK (children->data))
		{
			links = g_slist_prepend (links, children->data);
		}
		else
		{
			cpg_object_to_xml (serializer, group_node, children->data);
		}

		children = g_slist_next (children);
	}

	GSList *item;

	for (item = links; item; item = g_slist_next (item))
	{
		cpg_object_to_xml (serializer, group_node, item->data);
	}

	g_slist_free (links);
}

static gboolean
group_interface_is_template (CpgGroup    *group,
                             gchar const *name)
{
	CpgPropertyInterface *iface;
	CpgProperty *prop;
	GSList const *templates;
	gboolean ret = FALSE;
	gchar *path;

	iface = cpg_group_get_property_interface (group);
	prop = cpg_property_interface_lookup (iface, name);

	templates = cpg_object_get_applied_templates (CPG_OBJECT (group));

	path = cpg_object_get_relative_id (cpg_property_get_object (prop),
	                                   CPG_OBJECT (group));

	while (templates)
	{
		CpgGroup *template;
		CpgPropertyInterface *template_iface;
		CpgProperty *tempprop;

		template = templates->data;

		template_iface = cpg_group_get_property_interface (template);
		tempprop = cpg_property_interface_lookup (template_iface, name);

		if (tempprop && g_strcmp0 (cpg_property_get_name (prop),
		                           cpg_property_get_name (tempprop)) == 0)
		{
			gchar *origpath;

			origpath = cpg_object_get_relative_id (cpg_property_get_object (tempprop),
			                                       CPG_OBJECT (template));

			ret = g_strcmp0 (origpath, path) == 0;
			g_free (origpath);

			if (ret)
			{
				break;
			}
		}

		templates = g_slist_next (templates);
	}

	g_free (path);

	return ret;
}

static gboolean
group_interface_is_proxy (CpgGroup    *group,
                          gchar const *name)
{
	CpgObject *proxy;
	CpgProperty *property;
	CpgPropertyInterface *iface;

	/* Check if the interface is an automatically generated interface from
	   a property on the proxy object */

	proxy = cpg_group_get_proxy (group);

	if (!proxy)
	{
		return FALSE;
	}

	property = cpg_object_get_property (proxy, name);

	if (!property)
	{
		return FALSE;
	}

	iface = cpg_group_get_property_interface (group);

	return cpg_property_interface_lookup (iface, name) == property;
}

static gchar **
find_non_template_interfaces (CpgGroup *group)
{
	GPtrArray *ret;
	gchar **names;
	gchar **ptr;
	CpgPropertyInterface *iface;

	ret = g_ptr_array_new ();

	iface = cpg_group_get_property_interface (group);
	names = cpg_property_interface_get_names (iface);

	for (ptr = names; ptr && *ptr; ++ptr)
	{
		if (group_interface_is_template (group, *ptr))
		{
			continue;
		}

		if (group_interface_is_proxy (group, *ptr))
		{
			continue;
		}

		g_ptr_array_add (ret, g_strdup (*ptr));
	}

	g_ptr_array_add (ret, NULL);
	g_strfreev (names);

	return (gchar **)g_ptr_array_free (ret, FALSE);
}

static void
group_interface_to_xml (CpgNetworkSerializer *serializer,
                        xmlNodePtr            group_node,
                        CpgGroup             *group)
{
	CpgPropertyInterface *iface;
	gchar **names;
	gchar **ptr;
	xmlNodePtr parent;

	iface = cpg_group_get_property_interface (group);

	names = find_non_template_interfaces (group);

	if (names && *names)
	{
		parent = xmlNewDocNode (serializer->priv->doc, NULL, (xmlChar *)"interface", NULL);
		xmlAddChild (group_node, parent);
	}

	for (ptr = names; ptr && *ptr; ++ptr)
	{
		CpgProperty *property;
		gchar *path;
		gchar *proppath;
		xmlNodePtr node;
		xmlNodePtr text;

		property = cpg_property_interface_lookup (iface, *ptr);

		path = cpg_object_get_relative_id (cpg_property_get_object (property),
		                                   CPG_OBJECT (group));

		if (path && *path)
		{
			proppath = g_strconcat (path, ".", cpg_property_get_name (property), NULL);
		}
		else
		{
			proppath = g_strdup (cpg_property_get_name (property));
		}

		node = xmlNewDocNode (serializer->priv->doc, NULL, (xmlChar *)"property", NULL);
		xmlNewProp (node, (xmlChar *)"name", (xmlChar *)*ptr);

		text = xmlNewDocText (serializer->priv->doc,
		                      (xmlChar *)proppath);

		xmlAddChild (node, text);
		xmlAddChild (parent, node);

		g_free (path);
		g_free (proppath);
	}

	g_strfreev (names);
}

static void
group_to_xml (CpgNetworkSerializer *serializer,
              xmlNodePtr            root,
              CpgGroup             *group)
{
	xmlNodePtr group_node;

	if (!CPG_IS_NETWORK (group))
	{
		group_node = object_to_xml (serializer,
		                            root,
		                            CPG_OBJECT (group),
		                            "state",
		                            NULL);

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

	group_interface_to_xml (serializer, group_node, group);

	group_children_to_xml (serializer, group_node, group);
}

static void
write_config (CpgNetworkSerializer *serializer,
              xmlNodePtr            nnetwork)
{
	CpgIntegrator *integrator = cpg_network_get_integrator (serializer->priv->network);

	if (integrator != NULL)
	{
		xmlNewProp (nnetwork,
		            (xmlChar *)"integrator",
		            (xmlChar *)cpg_object_get_id (CPG_OBJECT (integrator)));
	}
}

static void
write_extra_nodes (CpgNetworkSerializer *serializer)
{
	/* Restore additional xml nodes from network... */
	xmlDocPtr doc;
	xmlNodePtr root;
	xmlNodePtr write_root;

	doc = g_object_get_data (G_OBJECT (serializer->priv->network),
	                         CPG_NETWORK_XML_EXTRA_DATA_KEY);

	if (!doc)
	{
		return;
	}

	root = xmlDocGetRootElement (doc);
	xmlNodePtr child = root->children;

	write_root = xmlDocGetRootElement (serializer->priv->doc);

	while (child)
	{
		xmlNodePtr cp;

		cp = xmlDocCopyNode (child, serializer->priv->doc, 1);

		if (cp)
		{
			xmlAddChild (write_root, cp);
		}

		child = child->next;
	}
}

/**
 * cpg_network_serializer_serialize:
 * @serializer: A #CpgNetworkSerializer
 * @stream: A #GOutputStream
 * @error: A #GError
 *
 * Serialize a network to an output stream.
 *
 * Returns: %TRUE if the serialization was successful, %FALSE otherwise
 *
 **/
gboolean
cpg_network_serializer_serialize (CpgNetworkSerializer  *serializer,
                                  GOutputStream         *stream,
                                  GError               **error)
{
	g_return_val_if_fail (CPG_IS_NETWORK_SERIALIZER (serializer), FALSE);
	g_return_val_if_fail (G_IS_OUTPUT_STREAM (stream), FALSE);

	xmlDocPtr doc = xmlNewDoc ((xmlChar *)"1.0");
	xmlNodePtr root = xmlNewDocNode (doc, NULL, (xmlChar *)"cpg", NULL);

	xmlDocSetRootElement (doc, root);

	xmlNodePtr nnetwork = xmlNewDocNode (doc, NULL, (xmlChar *)"network", NULL);

	restore_comment (serializer, root, G_OBJECT (serializer->priv->network));

	xmlAddChild (root, nnetwork);


	serializer->priv->doc = doc;

	write_config (serializer, nnetwork);

	// Globals
	GSList *properties = cpg_object_get_properties (CPG_OBJECT (serializer->priv->network));

	if (properties)
	{
		xmlNodePtr gbl = xmlNewDocNode (doc, NULL, (xmlChar *)"globals", NULL);
		xmlAddChild (nnetwork, gbl);

		properties_to_xml (serializer, gbl, CPG_OBJECT (serializer->priv->network), NULL, NULL);
	}

	g_slist_free (properties);

	// Generate templates
	CpgGroup *template_group = cpg_network_get_template_group (serializer->priv->network);
	GSList const *list = cpg_group_get_children (template_group);
	xmlNodePtr templates;

	if (list)
	{
		templates = xmlNewDocNode (doc, NULL, (xmlChar *)"templates", NULL);
		xmlAddChild (nnetwork, templates);

		group_children_to_xml (serializer, templates, template_group);
	}

	// Generate state and link nodes
	if (serializer->priv->root)
	{
		group_to_xml (serializer, nnetwork, CPG_GROUP (serializer->priv->root));
	}
	else
	{
		group_to_xml (serializer, nnetwork, CPG_GROUP (serializer->priv->network));
	}

	write_functions (serializer, nnetwork);
	write_extra_nodes (serializer);

	xmlIndentTreeOutput = 1;

	xmlChar *mem;
	int size;

	xmlDocDumpFormatMemoryEnc (doc,
	                           &mem,
	                           &size,
	                           xmlGetCharEncodingName (XML_CHAR_ENCODING_UTF8),
	                           1);

	gboolean ret;

	ret = g_output_stream_write_all (stream,
	                                 mem,
	                                 size,
	                                 NULL,
	                                 NULL,
	                                 error);

	xmlFree (mem);
	xmlFreeDoc (doc);

	return ret;
}

/**
 * cpg_network_serializer_serialize_file:
 * @serializer: A #CpgNetworkSerializer
 * @file: A #GFile
 * @error: A #GError
 * 
 * Convenience function to serialize to a file.
 *
 * Returns: %TRUE if the serialization was successful, %FALSE otherwise
 *
 **/
gboolean
cpg_network_serializer_serialize_file (CpgNetworkSerializer  *serializer,
                                       GFile                 *file,
                                       GError               **error)
{
	g_return_val_if_fail (CPG_IS_NETWORK_SERIALIZER (serializer), FALSE);
	g_return_val_if_fail (G_IS_FILE (file), FALSE);

	GFileOutputStream *stream = g_file_create (file,
	                                           G_FILE_CREATE_NONE,
	                                           NULL,
	                                           error);

	if (!stream)
	{
		return FALSE;
	}

	gboolean ret;

	ret = cpg_network_serializer_serialize (serializer,
	                                        G_OUTPUT_STREAM (stream),
	                                        error);

	g_object_unref (stream);

	return ret;
}

/**
 * cpg_network_serializer_serialize_path:
 * @serializer: A #CpgNetworkSerializer
 * @path: The file path
 * @error: A #GError
 * 
 * Convenience function to serialize to a file path.
 *
 * Returns: %TRUE if the serialization was successful, %FALSE otherwise
 *
 **/
gboolean
cpg_network_serializer_serialize_path (CpgNetworkSerializer  *serializer,
                                       gchar const           *path,
                                       GError               **error)
{
	g_return_val_if_fail (CPG_IS_NETWORK_SERIALIZER (serializer), FALSE);
	g_return_val_if_fail (path != NULL, FALSE);

	GFile *file = g_file_new_for_path (path);

	gboolean ret;

	ret = cpg_network_serializer_serialize_file (serializer,
	                                             file,
	                                             error);

	g_object_unref (file);

	return ret;
}

/**
 * cpg_network_serializer_serialize_memory:
 * @serializer: A #CpgNetworkSerializer
 * @error: A #GError
 *
 * Convenience function to serialize a network to a string.
 *
 * Returns: The serialized network or %NULL if an error occurred.
 *
 **/
gchar *
cpg_network_serializer_serialize_memory (CpgNetworkSerializer  *serializer,
                                         GError               **error)
{
	g_return_val_if_fail (CPG_IS_NETWORK_SERIALIZER (serializer), NULL);

	GOutputStream *stream = g_memory_output_stream_new (NULL,
	                                                    0,
	                                                    g_realloc,
	                                                    NULL);

	gboolean ret;

	ret = cpg_network_serializer_serialize (serializer,
	                                        stream,
	                                        error);

	gpointer data = g_memory_output_stream_get_data (G_MEMORY_OUTPUT_STREAM (stream));
	g_object_unref (stream);

	if (!ret)
	{
		g_free (data);
		data = NULL;
	}

	return data;
}
