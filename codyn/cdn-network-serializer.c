/*
 * codyn-serializer.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cdn-network-serializer.h"
#include "cdn-enum-types.h"
#include "cdn-function-polynomial.h"
#include "cdn-network-xml.h"
#include "cdn-import.h"
#include "cdn-input-file.h"
#include "cdn-annotatable.h"
#include "cdn-layoutable.h"

#include <libxml/tree.h>

/**
 * SECTION:cdn-network-serializer
 * @short_description: Network to XML serializer
 *
 * This can be used to serialize a #CdnNetwork to XML.
 *
 */
#define CDN_NETWORK_SERIALIZER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_NETWORK_SERIALIZER, CdnNetworkSerializerPrivate))

struct _CdnNetworkSerializerPrivate
{
	CdnNetwork *network;
	CdnNode *root;
	xmlDocPtr doc;
};

G_DEFINE_TYPE (CdnNetworkSerializer, cdn_network_serializer, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NETWORK,
	PROP_ROOT
};

static void
cdn_network_serializer_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_network_serializer_parent_class)->finalize (object);
}

static void
cdn_network_serializer_dispose (GObject *object)
{
	CdnNetworkSerializer *serializer = CDN_NETWORK_SERIALIZER (object);

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

	G_OBJECT_CLASS (cdn_network_serializer_parent_class)->dispose (object);
}

static void
cdn_network_serializer_set_property (GObject      *object,
                                     guint         prop_id,
                                     const GValue *value,
                                     GParamSpec   *pspec)
{
	CdnNetworkSerializer *self = CDN_NETWORK_SERIALIZER (object);

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
cdn_network_serializer_get_property (GObject    *object,
                                     guint       prop_id,
                                     GValue     *value,
                                     GParamSpec *pspec)
{
	CdnNetworkSerializer *self = CDN_NETWORK_SERIALIZER (object);

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
cdn_network_serializer_class_init (CdnNetworkSerializerClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_network_serializer_finalize;
	object_class->dispose = cdn_network_serializer_dispose;

	object_class->get_property = cdn_network_serializer_get_property;
	object_class->set_property = cdn_network_serializer_set_property;

	g_type_class_add_private (object_class, sizeof(CdnNetworkSerializerPrivate));

	/**
	 * CdnSerializer:network:
	 *
	 * The #CdnNetwork to serialize.
	 */
	g_object_class_install_property (object_class,
	                                 PROP_NETWORK,
	                                 g_param_spec_object ("network",
	                                                      "Network",
	                                                      "Network",
	                                                      CDN_TYPE_NETWORK,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT_ONLY));

	/**
	 * CdnSerializer:root:
	 *
	 * The #CdnNode to serialize
	 *
	 */
	g_object_class_install_property (object_class,
	                                 PROP_ROOT,
	                                 g_param_spec_object ("root",
	                                                      "Root",
	                                                      "Root",
	                                                      CDN_TYPE_NODE,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT_ONLY));
}

static void
cdn_network_serializer_init (CdnNetworkSerializer *self)
{
	self->priv = CDN_NETWORK_SERIALIZER_GET_PRIVATE (self);
}

/**
 * cdn_network_serializer_new:
 * @network: A #CdnNetwork
 * @root: A #CdnNode
 *
 * Create a new serializer for @network. When serialized, only objects
 * contained in @root will be serialized. This can be useful to serialize
 * only parts of the network. If @root is %NULL, all objects will be
 * serialized.
 *
 * Returns: A #CdnNetworkSerializer
 *
 **/
CdnNetworkSerializer *
cdn_network_serializer_new (CdnNetwork *network,
                            CdnNode   *root)
{
	return g_object_new (CDN_TYPE_NETWORK_SERIALIZER,
	                     "network", network,
	                     "root", root,
	                     NULL);
}

extern int xmlIndentTreeOutput;

static void group_to_xml (CdnNetworkSerializer *serializer, xmlNodePtr root, CdnNode *group);

static gboolean
property_matches_template (CdnVariable  *property,
                           GSList const *templates)
{
	while (templates)
	{
		CdnVariable *tprop;

		tprop = cdn_object_get_variable (templates->data,
		                                 cdn_variable_get_name (property));

		if (tprop)
		{
			return cdn_variable_equal (property, tprop);
		}

		templates = g_slist_next (templates);
	}

	return FALSE;
}

static void
export_flags (xmlNodePtr   node,
              CdnVariable *property)
{
	CdnVariableFlags add_flags;
	CdnVariableFlags remove_flags;
	GFlagsClass *klass;
	guint i;
	CdnObject *templ;

	gboolean flags_attr;

	flags_attr = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (property),
	                                                 CDN_NETWORK_XML_VARIABLE_FLAGS_ATTRIBUTE));

	add_flags = cdn_variable_get_flags (property);
	remove_flags = CDN_VARIABLE_FLAG_NONE;

	templ = cdn_object_get_variable_template (cdn_variable_get_object (property),
	                                          property,
	                                          FALSE);

	if (templ)
	{
		/* See how to change */
		CdnVariable *orig;

		orig = cdn_object_get_variable (templ, cdn_variable_get_name (property));

		remove_flags = ~add_flags & cdn_variable_get_flags (orig);
		add_flags = add_flags & ~cdn_variable_get_flags (orig);
	}

	if (flags_attr)
	{
		gchar *s = cdn_variable_flags_to_string (add_flags, remove_flags);
		xmlNewProp (node, (xmlChar *)"flags", (xmlChar *)s);
		g_free (s);

		return;
	}

	klass = g_type_class_ref (CDN_TYPE_VARIABLE_FLAGS);

	for (i = 0; i < klass->n_values; ++i)
	{
		GFlagsValue *value = &(klass->values[i]);

		if (value->value == 0)
		{
			continue;
		}

		if ((add_flags & value->value) == value->value)
		{
			xmlNewProp (node,
			            (xmlChar *)value->value_nick,
			            (xmlChar *)"yes");
		}
		else if ((remove_flags & value->value) == value->value)
		{
			xmlNewProp (node,
			            (xmlChar *)value->value_nick,
			            (xmlChar *)"no");
		}
	}
}

static void
restore_comment (CdnNetworkSerializer *serializer,
                 xmlNodePtr            parent,
                 GObject              *object)
{
	gchar *comment;
	gchar *with_spaces;

	if (CDN_IS_ANNOTATABLE (object))
	{
		comment = cdn_annotatable_get_annotation (CDN_ANNOTATABLE (object));
	}
	else
	{
		comment = g_strdup (g_object_get_data (object,
		                                       CDN_NETWORK_XML_COMMENT_DATA_KEY));
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
variables_to_xml (CdnNetworkSerializer *serializer,
                   xmlNodePtr            parent,
                   CdnObject            *object,
                   GSList const         *props)
{
	GSList *item;
	GSList *variables;
	GSList const *templates;

	if (props)
	{
		variables = g_slist_copy ((GSList *)props);
	}
	else
	{
		variables = cdn_object_get_variables (object);
	}

	templates = cdn_object_get_applied_templates (object);

	for (item = variables; item; item = g_slist_next (item))
	{
		CdnVariable *property = item->data;
		CdnExpression *cons;

		if (cdn_variable_get_object (property) != object)
		{
			continue;
		}

		CdnExpression *expression = cdn_variable_get_expression (property);

		// Check if property is different from template
		if (property_matches_template (property, templates))
		{
			continue;
		}

		restore_comment (serializer, parent, G_OBJECT (property));

		xmlNodePtr node = xmlNewDocNode (serializer->priv->doc,
		                                 NULL,
		                                 (xmlChar *)"variable",
		                                 NULL);
		xmlNewProp (node,
		            (xmlChar *)"name",
		            (xmlChar *)cdn_variable_get_name (property));

		export_flags (node, property);

		gchar const *expr = cdn_expression_get_as_string (expression);

		if (expr && *expr)
		{
			xmlNodePtr text = xmlNewDocText (serializer->priv->doc,
			                                 (xmlChar *)expr);

			xmlAddChild (node, text);
		}

		cons = cdn_variable_get_constraint (property);

		if (cons)
		{
			xmlNewProp (node,
			            (xmlChar *)"constraint",
			            (xmlChar *)cdn_expression_get_as_string (cons));
		}

		xmlAddChild (parent, node);
	}

	g_slist_free (variables);
}

static gchar *
template_path (CdnNetwork *network,
               CdnObject *orig,
               CdnObject *template)
{
	CdnNode *parent = cdn_object_get_parent (orig);
	CdnObject *shared_root = template;

	GString *ret = g_string_new ("");
	gboolean first = TRUE;

	CdnObject *tg;

	tg = CDN_OBJECT (cdn_network_get_template_node (network));

	do
	{
		CdnNode *shared_parent = cdn_object_get_parent (shared_root);

		if (shared_root == tg)
		{
			break;
		}

		if (shared_parent != NULL)
		{
			if (!first)
			{
				g_string_prepend (ret, ".");
			}

			g_string_prepend (ret, cdn_object_get_id (shared_root));

			first = FALSE;
		}

		shared_root = CDN_OBJECT (shared_parent);
	} while (shared_root && CDN_OBJECT (parent) != shared_root);

	return g_string_free (ret, FALSE);
}

static gboolean
should_write_layout (CdnObject *object)
{
	CdnLayoutable *layoutable;
	CdnLayoutable *last;
	gint x;
	gint y;
	gint tx;
	gint ty;
	GSList *templates;
	GSList *item;

	if (!CDN_IS_LAYOUTABLE (object))
	{
		return FALSE;
	}

	layoutable = CDN_LAYOUTABLE (object);

	if (!cdn_layoutable_supports_location (layoutable) ||
	    !cdn_layoutable_get_has_location (layoutable))
	{
		return FALSE;
	}

	/* Check if the layout was not applied by some template */
	templates = cdn_node_get_auto_templates_for_child (CDN_NODE (cdn_object_get_parent (object)),
	                                                    object);

	last = NULL;

	for (item = templates; item; item = g_slist_next (item))
	{
		if (CDN_IS_LAYOUTABLE (item->data) &&
		    cdn_layoutable_supports_location (item->data) &&
		    cdn_layoutable_get_has_location (item->data))
		{
			last = item->data;
		}
	}

	g_slist_free (templates);

	if (!last)
	{
		return TRUE;
	}

	cdn_layoutable_get_location (layoutable, &x, &y);
	cdn_layoutable_get_location (last, &tx, &ty);

	return x != tx || y != ty;
}

static void
add_layout (CdnObject  *object,
            xmlNodePtr  ptr)
{
	if (should_write_layout (object))
	{
		gchar *pos;
		gint x;
		gint y;

		cdn_layoutable_get_location (CDN_LAYOUTABLE (object), &x, &y);

		pos = g_strdup_printf ("%d", x);
		xmlNewProp (ptr, (xmlChar *)"x", (xmlChar *)pos);
		g_free (pos);

		pos = g_strdup_printf ("%d", y);
		xmlNewProp (ptr, (xmlChar *)"y", (xmlChar *)pos);
		g_free (pos);
	}
}

static xmlNodePtr
object_to_xml (CdnNetworkSerializer *serializer,
               xmlNodePtr            parent,
               CdnObject            *object,
               gchar const          *name,
               GSList const         *variables)
{
	restore_comment (serializer, parent, G_OBJECT (object));

	xmlNodePtr ptr = xmlNewDocNode (serializer->priv->doc,
	                                NULL,
	                                (xmlChar *)name,
	                                NULL);

	xmlNewProp (ptr, (xmlChar *)"id", (xmlChar *)cdn_object_get_id (object));
	xmlAddChild (parent, ptr);

	add_layout (object, ptr);

	GSList const *templates = cdn_object_get_applied_templates (object);
	GSList *inherited = cdn_node_get_auto_templates_for_child (CDN_NODE (cdn_object_get_parent (object)),
	                                                            object);
	GPtrArray *refs = g_ptr_array_new ();

	while (templates)
	{
		/* only apply templates that are not inherited */
		if (!g_slist_find (inherited, templates->data))
		{
			gchar *path = template_path (serializer->priv->network,
			                             object,
			                             templates->data);

			g_ptr_array_add (refs, path);
		}

		templates = g_slist_next (templates);
	}

	g_slist_free (inherited);

	g_ptr_array_add (refs, NULL);

	gchar **refs_ptr = (gchar **)g_ptr_array_free (refs, FALSE);

	if (refs_ptr && *refs_ptr)
	{
		gchar *joined = g_strjoinv (", ", refs_ptr);
		xmlNewProp (ptr, (xmlChar *)"ref", (xmlChar *)joined);
		g_free (joined);
	}

	g_strfreev (refs_ptr);

	variables_to_xml (serializer,
	                   ptr,
	                   object,
	                   variables);

	return ptr;
}

static xmlNodePtr
state_to_xml (CdnNetworkSerializer *serializer,
              xmlNodePtr            parent,
              CdnObject            *state)
{
	return object_to_xml (serializer, parent, state, "state", NULL);
}

static gboolean
action_matches_template (CdnEdgeAction *action,
                         GSList const  *templates)
{
	gchar const *p1 = cdn_edge_action_get_target (action);
	CdnExpression *e1 = cdn_edge_action_get_equation (action);

	while (templates)
	{
		CdnObject *template = templates->data;
		templates = g_slist_next (templates);

		if (!CDN_IS_EDGE (template))
		{
			continue;
		}

		GSList const *actions = cdn_edge_get_actions (CDN_EDGE (template));

		while (actions)
		{
			CdnEdgeAction *other = actions->data;

			gchar const *p2 = cdn_edge_action_get_target (other);
			CdnExpression *e2 = cdn_edge_action_get_equation (other);

			if (g_strcmp0 (p1, p2) == 0)
			{
				return cdn_expression_equal (e1, e2);
			}

			actions = g_slist_next (actions);
		}
	}

	return FALSE;
}

static xmlNodePtr
link_to_xml (CdnNetworkSerializer *serializer,
             xmlNodePtr            parent,
             CdnEdge              *link)
{
	CdnNode *from = cdn_edge_get_from (link);
	CdnNode *to = cdn_edge_get_to (link);

	xmlNodePtr node = object_to_xml (serializer,
	                                 parent,
	                                 CDN_OBJECT (link),
	                                 "edge",
	                                 NULL);

	if (from != NULL)
	{
		xmlNewProp (node,
		            (xmlChar *)"from",
		            (xmlChar *)cdn_object_get_id (CDN_OBJECT (from)));
	}

	if (to != NULL)
	{
		xmlNewProp (node,
		            (xmlChar *)"to",
		            (xmlChar *)cdn_object_get_id (CDN_OBJECT (to)));
	}

	// Link actions
	GSList const *item;
	GSList const *templates = cdn_object_get_applied_templates (CDN_OBJECT (link));

	for (item = cdn_edge_get_actions (link); item; item = g_slist_next (item))
	{
		CdnEdgeAction *action = item->data;

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
		            (xmlChar *)cdn_edge_action_get_target (action));

		gchar const *expr = cdn_expression_get_as_string (cdn_edge_action_get_equation (action));

		if (expr && *expr)
		{
			xmlNodePtr text = xmlNewDocText (serializer->priv->doc,
		                                 (xmlChar *)expr);

			xmlAddChild (ac, text);
		}

		xmlAddChild (node, ac);
	}

	return node;
}

static void
function_to_xml (CdnNetworkSerializer *serializer,
                 xmlNodePtr            parent,
                 CdnFunction          *func)
{
	xmlNodePtr funcn;

	funcn = xmlNewDocNode (serializer->priv->doc, NULL, (xmlChar *)"function", NULL);

	xmlNewProp (funcn,
	            (xmlChar *)"name",
	            (xmlChar *)cdn_object_get_id (CDN_OBJECT (func)));

	xmlAddChild (parent, funcn);

	/* Create expression element */
	CdnExpression *expression = cdn_function_get_expression (func);

	if (expression)
	{
		restore_comment (serializer, funcn, G_OBJECT (expression));

		xmlNodePtr exprn = xmlNewDocNode (serializer->priv->doc,
		                                  NULL,
		                                  (xmlChar *)"expression",
		                                  NULL);

		gchar const *expr = cdn_expression_get_as_string (expression);

		if (expr && *expr)
		{
			xmlNodePtr text = xmlNewDocText (serializer->priv->doc,
			                                 (xmlChar *)expr);

			xmlAddChild (exprn, text);
		}

		xmlAddChild (funcn, exprn);
	}

	/* Create argument elements */
	GList const *args = cdn_function_get_arguments (func);
	GList const *argitem;

	for (argitem = args; argitem; argitem = g_list_next (argitem))
	{
		CdnFunctionArgument *argument = argitem->data;

		restore_comment (serializer, funcn, G_OBJECT (argument));

		xmlNodePtr argn = xmlNewDocNode (serializer->priv->doc,
		                                 NULL,
		                                 (xmlChar *)"argument",
		                                 NULL);
		xmlNodePtr text = xmlNewDocText (serializer->priv->doc,
		                                 (xmlChar *)cdn_function_argument_get_name (argument));

		xmlAddChild (argn, text);

		if (cdn_function_argument_get_optional (argument))
		{
			CdnExpression *expr;

			expr = cdn_function_argument_get_default_value (argument);
			xmlNewProp (argn, (xmlChar *)"default", (xmlChar *)cdn_expression_get_as_string (expr));
		}

		if (!cdn_function_argument_get_explicit (argument))
		{
			xmlNewProp (argn, (xmlChar *)"implicit", (xmlChar *)"yes");
		}

		xmlAddChild (funcn, argn);
	}

	add_layout (CDN_OBJECT (func), funcn);
}

static void
function_polynomial_to_xml (CdnNetworkSerializer  *serializer,
                            xmlNodePtr             parent,
                            CdnFunctionPolynomial *func)
{
	xmlNodePtr funcn;

	funcn = xmlNewDocNode (serializer->priv->doc, NULL, (xmlChar *)"polynomial", NULL);

	xmlNewProp (funcn,
	            (xmlChar *)"name",
	            (xmlChar *)cdn_object_get_id (CDN_OBJECT (func)));

	xmlAddChild (parent, funcn);

	/* Create pieces */
	GSList const *pieces = cdn_function_polynomial_get_pieces (func);

	while (pieces)
	{
		CdnFunctionPolynomialPiece *piece = (CdnFunctionPolynomialPiece *)pieces->data;

		gchar beginPtr[G_ASCII_DTOSTR_BUF_SIZE];
		gchar endPtr[G_ASCII_DTOSTR_BUF_SIZE];

		g_ascii_dtostr (beginPtr,
		                G_ASCII_DTOSTR_BUF_SIZE,
		                cdn_function_polynomial_piece_get_begin (piece));

		g_ascii_dtostr (endPtr,
		                G_ASCII_DTOSTR_BUF_SIZE,
		                cdn_function_polynomial_piece_get_end (piece));

		xmlNodePtr piecen = xmlNewDocNode (serializer->priv->doc, NULL, (xmlChar *)"piece", NULL);
		xmlNewProp (piecen, (xmlChar *)"begin", (xmlChar *)beginPtr);
		xmlNewProp (piecen, (xmlChar *)"end", (xmlChar *)endPtr);

		GString *str = g_string_new ("");
		guint num;
		guint i;
		gdouble const *coefficients;

		coefficients = cdn_function_polynomial_piece_get_coefficients (piece,
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

	add_layout (CDN_OBJECT (func), funcn);
}

static gboolean
skip_object (CdnObject *object)
{
	GSList *templates;

	if (!cdn_object_get_parent (object))
	{
		return FALSE;
	}

	if (cdn_object_get_auto_imported (object))
	{
		return TRUE;
	}

	if (should_write_layout (object))
	{
		return FALSE;
	}

	/* Check if it was not solely instantiated from templates */
	templates = cdn_node_get_auto_templates_for_child (CDN_NODE (cdn_object_get_parent (object)),
	                                                    object);

	if (!templates)
	{
		return FALSE;
	}

	g_slist_free (templates);

	CdnObject *dummy = g_object_new (G_TYPE_FROM_INSTANCE (object),
	                                 "id", cdn_object_get_id (object),
	                                 NULL);

	_cdn_object_set_parent (dummy, cdn_object_get_parent (object));

	GSList const *item;

	for (item = cdn_object_get_applied_templates (object); item; item = g_slist_next (item))
	{
		cdn_object_apply_template (dummy, item->data, NULL);
	}

	if (cdn_object_equal (object, dummy))
	{
		return TRUE;
	}

	g_object_unref (dummy);
	return FALSE;
}

static void
import_to_xml (CdnNetworkSerializer *serializer,
               xmlNodePtr            root,
               CdnImport            *import)
{
	if (cdn_modifiable_get_modified (CDN_MODIFIABLE (import)))
	{
		group_to_xml (serializer, root, CDN_NODE (import));
		return;
	}

	restore_comment (serializer, root, G_OBJECT (import));

	xmlNodePtr node = xmlNewDocNode (serializer->priv->doc, NULL, (xmlChar *)"import", NULL);

	xmlNewProp (node, (xmlChar *)"id", (xmlChar *)cdn_object_get_id (CDN_OBJECT (import)));

	add_layout (CDN_OBJECT (import), node);

	GFile *network_file = cdn_network_get_file (serializer->priv->network);
	GFile *import_file = cdn_import_get_file (import);

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
input_file_to_xml (CdnNetworkSerializer *serializer,
                   xmlNodePtr            parent,
                   CdnInputFile         *input)
{
	xmlNodePtr node;
	GSList *variables;
	gchar **columns;
	gchar **item;

	variables = cdn_object_get_variables (CDN_OBJECT (input));

	columns = cdn_input_file_get_columns (input);

	for (item = columns; *item; ++item)
	{
		variables = g_slist_remove (variables,
		                             cdn_object_get_variable (CDN_OBJECT (input), *item));
	}

	g_strfreev (columns);

	node = object_to_xml (serializer,
	                      parent,
	                      CDN_OBJECT (input),
	                      "input-file",
	                      variables);

	g_slist_free (variables);

	/* File */
	GFile *file;
	file = cdn_input_file_get_file (input);

	if (file)
	{
		GFile *network_file;
		GFile *parent;

		network_file = cdn_network_get_file (serializer->priv->network);
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
	if (cdn_input_file_get_repeat (input))
	{
		xmlNewProp (node, (xmlChar *)"repeat", (xmlChar *)"yes");
	}

	/* Time column */
	gint time_column;
	gboolean isset;

	time_column = cdn_input_file_get_time_column (input, &isset);

	if (isset)
	{
		gchar *ptr = g_strdup_printf ("%d", time_column);
		xmlNewProp (node, (xmlChar *)"time-column", (xmlChar *)ptr);
		g_free (ptr);
	}
}

static void
any_object_to_xml (CdnNetworkSerializer *serializer,
                   xmlNodePtr            root,
                   CdnObject            *object)
{
	/* Check if this object can be omitted because it's covered fully
	   by its fake template */
	if (skip_object (object))
	{
		return;
	}

	if (CDN_IS_IMPORT (object))
	{
		import_to_xml (serializer, root, CDN_IMPORT (object));
	}
	else if (CDN_IS_NODE (object))
	{
		group_to_xml (serializer, root, CDN_NODE (object));
	}
	else if (CDN_IS_EDGE (object))
	{
		link_to_xml (serializer, root, CDN_EDGE (object));
	}
	else if (CDN_IS_INPUT_FILE (object))
	{
		input_file_to_xml (serializer, root, CDN_INPUT_FILE (object));
	}
	else if (CDN_IS_FUNCTION_POLYNOMIAL (object))
	{
		function_polynomial_to_xml (serializer, root, CDN_FUNCTION_POLYNOMIAL (object));
	}
	else if (CDN_IS_FUNCTION (object))
	{
		function_to_xml (serializer, root, CDN_FUNCTION (object));
	}
	else
	{
		state_to_xml (serializer, root, object);
	}
}

static gboolean
check_proxy_template (CdnObject *object,
                      CdnObject *proxy)
{
	GSList const *templates = cdn_object_get_applied_templates (object);

	while (templates)
	{
		if (CDN_IS_NODE (templates->data))
		{
			CdnObject *other_proxy = cdn_node_get_proxy (templates->data);

			if (other_proxy != NULL &&
			    g_strcmp0 (cdn_object_get_id (proxy),
			               cdn_object_get_id (other_proxy)) == 0)
			{
				return TRUE;
			}
		}

		templates = g_slist_next (templates);
	}

	return FALSE;
}

static void
group_children_to_xml (CdnNetworkSerializer *serializer,
                       xmlNodePtr            group_node,
                       CdnNode             *group)
{
	GSList const *children = cdn_node_get_children (group);
	GSList *links = NULL;

	while (children)
	{
		if (CDN_IS_EDGE (children->data))
		{
			links = g_slist_prepend (links, children->data);
		}
		else
		{
			any_object_to_xml (serializer, group_node, children->data);
		}

		children = g_slist_next (children);
	}

	GSList *item;
	links = g_slist_reverse (links);

	for (item = links; item; item = g_slist_next (item))
	{
		any_object_to_xml (serializer, group_node, item->data);
	}

	g_slist_free (links);
}

static gboolean
group_interface_is_template (CdnNode    *group,
                             gchar const *name)
{
	CdnVariableInterface *iface;
	GSList const *templates;
	gboolean ret = FALSE;
	gchar const *my_child_name;
	gchar const *my_property_name;

	iface = cdn_node_get_variable_interface (group);

	my_child_name = cdn_variable_interface_lookup_child_name (iface, name);
	my_property_name = cdn_variable_interface_lookup_variable_name (iface, name);

	templates = cdn_object_get_applied_templates (CDN_OBJECT (group));

	while (templates)
	{
		CdnNode *template;
		CdnVariableInterface *template_iface;
		gchar const *child_name;
		gchar const *property_name;

		template = templates->data;

		template_iface = cdn_node_get_variable_interface (template);

		child_name = cdn_variable_interface_lookup_child_name (template_iface, name);
		property_name = cdn_variable_interface_lookup_variable_name (template_iface, name);

		if (g_strcmp0 (my_child_name, child_name) == 0 &&
		    g_strcmp0 (my_property_name, property_name) == 0)
		{
			ret = TRUE;
			break;
		}

		templates = g_slist_next (templates);
	}

	return ret;
}

static gboolean
group_interface_is_proxy (CdnNode    *group,
                          gchar const *name)
{
	CdnObject *proxy;
	gchar const *child_name;
	CdnVariableInterface *iface;

	/* Check if the interface is an automatically generated interface from
	   a property on the proxy object */
	proxy = cdn_node_get_proxy (group);

	if (!proxy)
	{
		return FALSE;
	}

	iface = cdn_node_get_variable_interface (group);
	child_name = cdn_variable_interface_lookup_child_name (iface, name);

	return g_strcmp0 (cdn_object_get_id (proxy), child_name) == 0;
}

static gchar **
find_non_template_interfaces (CdnNode *group)
{
	GPtrArray *ret;
	gchar **names;
	gchar **ptr;
	CdnVariableInterface *iface;

	ret = g_ptr_array_new ();

	iface = cdn_node_get_variable_interface (group);
	names = cdn_variable_interface_get_names (iface);

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
group_interface_to_xml (CdnNetworkSerializer *serializer,
                        xmlNodePtr            group_node,
                        CdnNode             *group)
{
	CdnVariableInterface *iface;
	gchar **names;
	gchar **ptr;
	xmlNodePtr parent;

	iface = cdn_node_get_variable_interface (group);

	names = find_non_template_interfaces (group);

	if (names && *names)
	{
		parent = xmlNewDocNode (serializer->priv->doc, NULL, (xmlChar *)"interface", NULL);
		xmlAddChild (group_node, parent);
	}

	for (ptr = names; ptr && *ptr; ++ptr)
	{
		gchar const *child_name;
		gchar const *property_name;
		xmlNodePtr node;
		xmlNodePtr text;

		child_name = cdn_variable_interface_lookup_child_name (iface, *ptr);
		property_name = cdn_variable_interface_lookup_variable_name (iface, *ptr);

		node = xmlNewDocNode (serializer->priv->doc, NULL, (xmlChar *)"variable", NULL);
		xmlNewProp (node, (xmlChar *)"name", (xmlChar *)*ptr);
		xmlNewProp (node, (xmlChar *)"child", (xmlChar *)child_name);

		text = xmlNewDocText (serializer->priv->doc,
		                      (xmlChar *)property_name);

		xmlAddChild (node, text);
		xmlAddChild (parent, node);
	}

	g_strfreev (names);
}

static void
group_to_xml (CdnNetworkSerializer *serializer,
              xmlNodePtr            root,
              CdnNode             *group)
{
	xmlNodePtr group_node;

	if (!CDN_IS_NETWORK (group))
	{
		group_node = object_to_xml (serializer,
		                            root,
		                            CDN_OBJECT (group),
		                            "node",
		                            NULL);

		CdnObject *proxy = cdn_node_get_proxy (group);

		if (proxy != NULL &&
		    !check_proxy_template (CDN_OBJECT (group), proxy))
		{
			xmlNewProp (group_node,
			            (xmlChar *)"proxy",
			            (xmlChar *)cdn_object_get_id (proxy));
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
write_config (CdnNetworkSerializer *serializer,
              xmlNodePtr            nnetwork)
{
	CdnIntegrator *integrator = cdn_network_get_integrator (serializer->priv->network);

	if (integrator != NULL)
	{
		xmlNewProp (nnetwork,
		            (xmlChar *)"integrator",
		            (xmlChar *)cdn_object_get_id (CDN_OBJECT (integrator)));
	}
}

static void
write_extra_nodes (CdnNetworkSerializer *serializer)
{
	/* Restore additional xml nodes from network... */
	xmlDocPtr doc;
	xmlNodePtr root;
	xmlNodePtr write_root;

	doc = g_object_get_data (G_OBJECT (serializer->priv->network),
	                         CDN_NETWORK_XML_EXTRA_DATA_KEY);

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
 * cdn_network_serializer_serialize:
 * @serializer: A #CdnNetworkSerializer
 * @stream: A #GOutputStream
 * @error: A #GError
 *
 * Serialize a network to an output stream.
 *
 * Returns: %TRUE if the serialization was successful, %FALSE otherwise
 *
 **/
gboolean
cdn_network_serializer_serialize (CdnNetworkSerializer  *serializer,
                                  GOutputStream         *stream,
                                  GError               **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK_SERIALIZER (serializer), FALSE);
	g_return_val_if_fail (G_IS_OUTPUT_STREAM (stream), FALSE);

	xmlDocPtr doc = xmlNewDoc ((xmlChar *)"1.0");
	xmlNodePtr root = xmlNewDocNode (doc, NULL, (xmlChar *)"cdn", NULL);

	xmlDocSetRootElement (doc, root);

	xmlNodePtr nnetwork = xmlNewDocNode (doc, NULL, (xmlChar *)"network", NULL);

	restore_comment (serializer, root, G_OBJECT (serializer->priv->network));

	xmlAddChild (root, nnetwork);


	serializer->priv->doc = doc;

	write_config (serializer, nnetwork);

	// Globals
	GSList *variables = cdn_object_get_variables (CDN_OBJECT (serializer->priv->network));

	if (variables)
	{
		xmlNodePtr gbl = xmlNewDocNode (doc, NULL, (xmlChar *)"globals", NULL);
		xmlAddChild (nnetwork, gbl);

		variables_to_xml (serializer, gbl, CDN_OBJECT (serializer->priv->network), NULL);
	}

	g_slist_free (variables);

	// Generate templates
	CdnNode *template_group = cdn_network_get_template_node (serializer->priv->network);
	GSList const *list = cdn_node_get_children (template_group);
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
		group_to_xml (serializer, nnetwork, CDN_NODE (serializer->priv->root));
	}
	else
	{
		group_to_xml (serializer, nnetwork, CDN_NODE (serializer->priv->network));
	}

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
 * cdn_network_serializer_serialize_file:
 * @serializer: A #CdnNetworkSerializer
 * @file: A #GFile
 * @error: A #GError
 * 
 * Convenience function to serialize to a file.
 *
 * Returns: %TRUE if the serialization was successful, %FALSE otherwise
 *
 **/
gboolean
cdn_network_serializer_serialize_file (CdnNetworkSerializer  *serializer,
                                       GFile                 *file,
                                       GError               **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK_SERIALIZER (serializer), FALSE);
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

	ret = cdn_network_serializer_serialize (serializer,
	                                        G_OUTPUT_STREAM (stream),
	                                        error);

	g_object_unref (stream);

	return ret;
}

/**
 * cdn_network_serializer_serialize_path:
 * @serializer: A #CdnNetworkSerializer
 * @path: The file path
 * @error: A #GError
 * 
 * Convenience function to serialize to a file path.
 *
 * Returns: %TRUE if the serialization was successful, %FALSE otherwise
 *
 **/
gboolean
cdn_network_serializer_serialize_path (CdnNetworkSerializer  *serializer,
                                       gchar const           *path,
                                       GError               **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK_SERIALIZER (serializer), FALSE);
	g_return_val_if_fail (path != NULL, FALSE);

	GFile *file = g_file_new_for_path (path);

	gboolean ret;

	ret = cdn_network_serializer_serialize_file (serializer,
	                                             file,
	                                             error);

	g_object_unref (file);

	return ret;
}

/**
 * cdn_network_serializer_serialize_memory:
 * @serializer: A #CdnNetworkSerializer
 * @error: A #GError
 *
 * Convenience function to serialize a network to a string.
 *
 * Returns: The serialized network or %NULL if an error occurred.
 *
 **/
gchar *
cdn_network_serializer_serialize_memory (CdnNetworkSerializer  *serializer,
                                         GError               **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK_SERIALIZER (serializer), NULL);

	GOutputStream *stream = g_memory_output_stream_new (NULL,
	                                                    0,
	                                                    g_realloc,
	                                                    NULL);

	gboolean ret;

	ret = cdn_network_serializer_serialize (serializer,
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
