/*
 * cpg-embedded-string.c
 * This file is part of cpg-network
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

#include "cpg-embedded-string.h"
#include <string.h>
#include <stdlib.h>
#include "cpg-statement.h"

#define CPG_EMBEDDED_STRING_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_EMBEDDED_STRING, CpgEmbeddedStringPrivate))

typedef struct
{
	CpgEmbeddedStringNodeType type;

	gchar *text;
	GSList *nodes;

	gint depth;
	gint position;
} Node;

struct _CpgEmbeddedStringPrivate
{
	GSList *stack;
	gchar *cached;

	GSList *filters;

	CpgEmbeddedContext *cached_context;
	gulong cached_marker;
	GRegex *indirection_regex;

	gint braces;

	gint line_start;
	gint line_end;
	gint column_start;
	gint column_end;
};

static void cpg_statement_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CpgEmbeddedString,
                         cpg_embedded_string,
                         G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (CPG_TYPE_STATEMENT, cpg_statement_iface_init))

enum
{
	PROP_0,
	PROP_LINE_START,
	PROP_LINE_END,
	PROP_COLUMN_START,
	PROP_COLUMN_END
};

static void
cpg_statement_iface_init (gpointer iface)
{
}

static Node *
node_new (CpgEmbeddedStringNodeType  type,
          gchar const               *text,
          gint                       depth)
{
	Node *ret;

	ret = g_slice_new0 (Node);

	ret->position = -1;
	ret->type = type;
	ret->text = g_strdup (text ? text : "");
	ret->depth = depth;

	return ret;
}

static Node *
node_copy (Node *other)
{
	Node *ret;
	GSList *child;

	ret = node_new (other->type, other->text, other->depth);

	ret->position = other->position;

	for (child = other->nodes; child; child = g_slist_next (child))
	{
		ret->nodes = g_slist_prepend (ret->nodes,
		                              node_copy (child->data));
	}

	ret->nodes = g_slist_reverse (ret->nodes);

	return ret;
}
static void
node_free (Node *node)
{
	g_slist_foreach (node->nodes, (GFunc)node_free, NULL);
	g_slist_free (node->nodes);
	g_free (node->text);

	g_slice_free (Node, node);
}

static void
cpg_embedded_string_finalize (GObject *object)
{
	CpgEmbeddedString *s;

	s = CPG_EMBEDDED_STRING (object);

	while (s->priv->stack)
	{
		node_free (s->priv->stack->data);
		s->priv->stack = g_slist_delete_link (s->priv->stack,
		                                      s->priv->stack);
	}

	if (s->priv->indirection_regex)
	{
		g_regex_unref (s->priv->indirection_regex);
	}

	cpg_embedded_string_clear_cache (s);

	G_OBJECT_CLASS (cpg_embedded_string_parent_class)->finalize (object);
}

static void
cpg_embedded_string_set_property (GObject      *object,
                                  guint         prop_id,
                                  const GValue *value,
                                  GParamSpec   *pspec)
{
	CpgEmbeddedString *self = CPG_EMBEDDED_STRING (object);

	switch (prop_id)
	{
		case PROP_LINE_START:
			self->priv->line_start = g_value_get_int (value);
			break;
		case PROP_LINE_END:
			self->priv->line_end = g_value_get_int (value);
			break;
		case PROP_COLUMN_START:
			self->priv->column_start = g_value_get_int (value);
			break;
		case PROP_COLUMN_END:
			self->priv->column_end = g_value_get_int (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_embedded_string_get_property (GObject    *object,
                                  guint       prop_id,
                                  GValue     *value,
                                  GParamSpec *pspec)
{
	CpgEmbeddedString *self = CPG_EMBEDDED_STRING (object);

	switch (prop_id)
	{
		case PROP_LINE_START:
			g_value_set_int (value, self->priv->line_start);
			break;
		case PROP_LINE_END:
			g_value_set_int (value, self->priv->line_end);
			break;
		case PROP_COLUMN_START:
			g_value_set_int (value, self->priv->column_start);
			break;
		case PROP_COLUMN_END:
			g_value_set_int (value, self->priv->column_end);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_embedded_string_class_init (CpgEmbeddedStringClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_embedded_string_finalize;

	object_class->get_property = cpg_embedded_string_get_property;
	object_class->set_property = cpg_embedded_string_set_property;

	g_type_class_add_private (object_class, sizeof (CpgEmbeddedStringPrivate));

	g_object_class_override_property (object_class,
	                                  PROP_LINE_START,
	                                  "line-start");

	g_object_class_override_property (object_class,
	                                  PROP_LINE_END,
	                                  "line-end");

	g_object_class_override_property (object_class,
	                                  PROP_COLUMN_START,
	                                  "column-start");

	g_object_class_override_property (object_class,
	                                  PROP_COLUMN_END,
	                                  "column-end");
}

static void
cpg_embedded_string_init (CpgEmbeddedString *self)
{
	self->priv = CPG_EMBEDDED_STRING_GET_PRIVATE (self);

	cpg_embedded_string_push (self, CPG_EMBEDDED_STRING_NODE_TEXT, 0);
}

CpgEmbeddedString *
cpg_embedded_string_new ()
{
	return g_object_new (CPG_TYPE_EMBEDDED_STRING, NULL);
}

CpgEmbeddedString *
cpg_embedded_string_new_from_string (gchar const *s)
{
	CpgEmbeddedString *ret;

	ret = g_object_new (CPG_TYPE_EMBEDDED_STRING, NULL);
	cpg_embedded_string_add_text (ret, s);

	return ret;
}

CpgEmbeddedString *
cpg_embedded_string_new_from_double (gdouble s)
{
	gchar buffer[G_ASCII_DTOSTR_BUF_SIZE];
	g_ascii_dtostr (buffer, G_ASCII_DTOSTR_BUF_SIZE, s);

	return cpg_embedded_string_new_from_string (buffer);
}

CpgEmbeddedString *
cpg_embedded_string_new_from_integer (gint s)
{
	gchar *ss;
	CpgEmbeddedString *ret;

	ss = g_strdup_printf ("%d", s);

	ret = cpg_embedded_string_new_from_string (ss);

	g_free (ss);
	return ret;
}

/**
 * cpg_embedded_string_add_text:
 * @s: A #CpgEmbeddedString
 * @text: The text to add
 *
 * Adds a text node to the embedded string.
 *
 * Returns: (transfer none): A #CpgEmbeddedString
 *
 **/
CpgEmbeddedString *
cpg_embedded_string_add_text (CpgEmbeddedString *s,
                              gchar const       *text)
{
	Node *node;
	Node *par;

	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (s), s);

	if (text == NULL || !*text)
	{
		return s;
	}

	node = node_new (CPG_EMBEDDED_STRING_NODE_TEXT, text, 0);

	if (s->priv->stack)
	{
		par = s->priv->stack->data;
		par->nodes = g_slist_prepend (par->nodes, node);
	}
	else
	{
		s->priv->stack = g_slist_prepend (NULL, node);
	}

	cpg_embedded_string_clear_cache (s);
	return s;
}

/**
 * cpg_embedded_string_prepend_text:
 * @s: A #CpgEmbeddedString
 * @text: The text to add
 *
 * Prepends a text node to the embedded string.
 *
 * Returns: (transfer none): A #CpgEmbeddedString
 *
 **/
CpgEmbeddedString *
cpg_embedded_string_prepend_text (CpgEmbeddedString *s,
                                  gchar const       *text)
{
	Node *node;
	Node *par;

	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (s), s);

	if (text == NULL || !*text)
	{
		return s;
	}

	node = node_new (CPG_EMBEDDED_STRING_NODE_TEXT, text, 0);

	if (s->priv->stack)
	{
		par = s->priv->stack->data;
		par->nodes = g_slist_append (par->nodes, node);
	}
	else
	{
		s->priv->stack = g_slist_prepend (NULL, node);
	}

	cpg_embedded_string_clear_cache (s);
	return s;
}

/**
 * cpg_embedded_string_push:
 * @s: A #CpgEmbeddedString
 * @type: A #CpgEmbeddedStringNodeType
 * @depth: The depth of the embedded string
 *
 * Push a context type into the embedded string.
 *
 * Returns: (transfer none): The #CpgEmbeddedString (@s)
 *
 **/
CpgEmbeddedString *
cpg_embedded_string_push (CpgEmbeddedString         *s,
                          CpgEmbeddedStringNodeType  type,
                          gint                       depth)
{
	Node *node;

	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (s), NULL);

	node = node_new (type, NULL, depth);

	s->priv->stack = g_slist_prepend (s->priv->stack,
	                                  node);

	cpg_embedded_string_clear_cache (s);

	return s;
}

/**
 * cpg_embedded_string_pop:
 * @s: A #CpgEmbeddedString
 *
 * Pop an embedded context.
 *
 * Returns: (transfer none): The #CpgEmbeddedString (@s)
 *
 **/
CpgEmbeddedString *
cpg_embedded_string_pop (CpgEmbeddedString *s)
{
	Node *node;
	Node *parent;

	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (s), NULL);

	if (!s->priv->stack || !s->priv->stack->next)
	{
		return s;
	}

	node = s->priv->stack->data;
	s->priv->stack = g_slist_delete_link (s->priv->stack,
	                                      s->priv->stack);

	parent = s->priv->stack->data;

	parent->nodes = g_slist_prepend (parent->nodes,
	                                 node);

	cpg_embedded_string_clear_cache (s);

	return s;
}

typedef enum
{
	INDIRECTION_NONE = 0,
	INDIRECTION_EXISTS,
	INDIRECTION_COUNT,
	INDIRECTION_INDEX,
	INDIRECTION_INCREMENT,
	INDIRECTION_DECREMENT,
	INDIRECTION_UNEXPAND
} IndirectionFlags;

static IndirectionFlags
get_indirection_flags (gchar const *s)
{
	if (!s || !*s)
	{
		return INDIRECTION_NONE;
	}

	switch (*s)
	{
		case '?':
			return INDIRECTION_EXISTS;
		case '!':
			return INDIRECTION_INDEX;
		case '~':
			return INDIRECTION_COUNT;
		case '+':
			return INDIRECTION_INCREMENT;
		case '-':
			return INDIRECTION_DECREMENT;
		case ',':
			return INDIRECTION_UNEXPAND;
		default:
			return INDIRECTION_NONE;
	}
}

static gchar *
unexpanded_expansion (CpgExpansion *ex)
{
	gint i;
	GString *ret = g_string_new ("");

	for (i = 1; i < cpg_expansion_num (ex); ++i)
	{
		gchar *escaped;

		if (i != 1)
		{
			g_string_append_c (ret, ',');
		}

		escaped = cpg_embedded_string_escape (cpg_expansion_get (ex, i));
		g_string_append (ret, escaped);
		g_free (escaped);
	}

	return g_string_free (ret, FALSE);
}

static gchar *
resolve_indirection (CpgEmbeddedString  *em,
                     CpgEmbeddedContext *context,
                     Node               *node,
                     gchar const        *s)
{
	IndirectionFlags flags;
	CpgExpansion *ex;
	GMatchInfo *info;
	gint exidx;
	gint inc = 0;
	gint dec = 0;
	gchar *name = NULL;
	gchar *ret;
	gchar *num;

	if (em->priv->indirection_regex == NULL)
	{
		em->priv->indirection_regex = g_regex_new ("^([0-9]+)([?!~])?|(.*?)([?!~,]|[+]+|[-]+)?$",
		                                           0, 0, NULL);
	}

	if (!g_regex_match (em->priv->indirection_regex, s, 0, &info))
	{
		return g_strdup("");
	}

	num = g_match_info_fetch (info, 1);

	if (num && *num)
	{
		// This is a numbered expansion
		gchar *flag;

		num = g_match_info_fetch (info, 1);
		flag = g_match_info_fetch (info, 2);

		flags = get_indirection_flags (flag);

		ex = cpg_embedded_context_get_expansion (context,
		                                         node->depth);

		exidx = (gint)g_ascii_strtoll (num, NULL, 10);

		g_free (flag);
	}
	else
	{
		// This is a define
		gchar *flag;

		name = g_match_info_fetch (info, 3);
		flag = g_match_info_fetch (info, 4);

		flags = get_indirection_flags (flag);

		if (flags == INDIRECTION_INCREMENT)
		{
			inc = g_ascii_strtoll (flag, NULL, 10);
		}
		else if (flags == INDIRECTION_DECREMENT)
		{
			dec = g_ascii_strtoll (flag, NULL, 10);
		}

		ex = cpg_embedded_context_get_define (context,
		                                      name);

		exidx = 0;

		g_free (flag);
	}

	switch (flags)
	{
		case INDIRECTION_EXISTS:
			if (ex && *(cpg_expansion_get (ex, exidx > 0 ? exidx : 0)))
			{
				ret = g_strdup ("1");
			}
			else
			{
				ret = g_strdup ("0");
			}
		break;
		case INDIRECTION_COUNT:
		{
			gint idx = ex ? cpg_expansion_num (ex) : 0;
			ret = g_strdup_printf ("%d", name ? (idx - 1) : idx);
		}
		break;
		case INDIRECTION_INDEX:
		{
			gint idx = ex ? cpg_expansion_get_index (ex, exidx) : 0;
			ret = g_strdup_printf ("%d", idx);
		}
		break;
		case INDIRECTION_INCREMENT:
		case INDIRECTION_DECREMENT: // fallthrough
		{
			gint idx = cpg_embedded_context_increment_define (context,
			                                                  name,
			                                                  -dec + inc);

			ret = g_strdup_printf ("%d", idx);
		}
		break;
		case INDIRECTION_UNEXPAND:
			ret = unexpanded_expansion (ex);
		break;
		default:
			ret = g_strdup (ex ? cpg_expansion_get (ex, exidx) : "");
		break;
	}

	g_free (name);
	g_free (num);

	return ret;
}

static void
increase_filters (CpgEmbeddedString *em,
                  gint               size)
{
	GSList *item;

	for (item = em->priv->filters; item; item = g_slist_next (item))
	{
		Node *node;

		node = item->data;

		node->position += size;
	}
}

typedef enum
{
	EVALUATE_SELF = 1 << 0,
	UPDATE_FILTERS = 1 << 1
} EvaluateFlags;

static gchar *
evaluate_node (CpgEmbeddedString   *em,
               Node                *node,
               CpgEmbeddedContext  *context,
               EvaluateFlags        flags,
               GError             **error)
{
	GString *ret;
	GSList *item;
	gchar *r = NULL;
	GSList *children = NULL;

	ret = g_string_new ("");

	if (!(flags & EVALUATE_SELF) ||
	    (node->type != CPG_EMBEDDED_STRING_NODE_REDUCE &&
	     node->type != CPG_EMBEDDED_STRING_NODE_MAP))
	{

		for (item = node->nodes; item; item = g_slist_next (item))
		{
			gchar *s;

			s = evaluate_node (em,
			                   item->data,
			                   context,
			                   flags | EVALUATE_SELF,
			                   error);

			if (!s)
			{
				g_string_free (ret, TRUE);
				return NULL;
			}

			if (node->type == CPG_EMBEDDED_STRING_NODE_EQUATION && !*s)
			{
				g_free (s);
				s = g_strdup ("0");
			}

			g_string_prepend (ret, s);

			if (flags & EVALUATE_SELF)
			{
				children = g_slist_prepend (children, s);
			}
			else
			{
				g_free (s);
			}
		}
	}

	if (!(flags & EVALUATE_SELF))
	{
		return g_string_free (ret, FALSE);
	}

	switch (node->type)
	{
		case CPG_EMBEDDED_STRING_NODE_TEXT:
			r = g_strconcat (node->text, ret->str, NULL);
		break;
		case CPG_EMBEDDED_STRING_NODE_CONDITION:
			if (context)
			{
				gboolean istrue;

				if (children)
				{
					r = cpg_embedded_context_calculate (context, children->data, error);
				}

				istrue = r && (gint)g_ascii_strtoll (r, NULL, 10) != 0;
				g_free (r);

				if (istrue && children->next)
				{
					r = g_strdup (children->next->data);
				}
				else if (children->next->next)
				{
					r = g_strdup (children->next->next->data);
				}
				else
				{
					r = g_strdup ("");
				}
			}
			else
			{
				r = g_strdup_printf ("$$(%s)",
				                     ret->str);
			}
		break;
		case CPG_EMBEDDED_STRING_NODE_EQUATION:
			if (context)
			{
				r = cpg_embedded_context_calculate (context, ret->str, error);
			}
			else
			{
				r = g_strdup_printf ("$(%s)",
				                     ret->str);
			}
		break;
		case CPG_EMBEDDED_STRING_NODE_INDIRECTION:
			if (context)
			{
				r = resolve_indirection (em, context, node, ret->str);
			}
			else
			{
				gchar *s;

				s = g_strnfill (node->depth + 1, '@');
				r = g_strdup_printf ("%s[%s]", s, ret->str);
				g_free (s);
			}
		break;
		case CPG_EMBEDDED_STRING_NODE_REDUCE:
		case CPG_EMBEDDED_STRING_NODE_MAP:
			/* Filters do not appear in the output... */
			r = g_strdup ("");
		break;
	}

	if ((flags & UPDATE_FILTERS) && em->priv->filters)
	{
		increase_filters (em, -ret->len);
		increase_filters (em, strlen (r));
	}

	if (node->type == CPG_EMBEDDED_STRING_NODE_REDUCE ||
	    node->type == CPG_EMBEDDED_STRING_NODE_MAP)
	{
		node->position = 0;

		em->priv->filters = g_slist_prepend (em->priv->filters,
		                                     node);
	}

	g_slist_foreach (children, (GFunc)g_free, NULL);
	g_slist_free (children);

	g_string_free (ret, TRUE);

	return r;
}

static gchar const *
embedded_string_expand (CpgEmbeddedString   *s,
                        CpgEmbeddedContext  *ctx,
                        GError             **error)
{
	if (s->priv->cached &&
	    ctx == s->priv->cached_context &&
	    (ctx == NULL ||
	     cpg_embedded_context_get_marker (ctx) == s->priv->cached_marker))
	{
		return s->priv->cached;
	}

	cpg_embedded_string_clear_cache (s);

	if (!s->priv->stack)
	{
		s->priv->cached = g_strdup ("");
	}
	else
	{
		s->priv->cached = evaluate_node (s,
		                                 s->priv->stack->data,
		                                 ctx,
		                                 EVALUATE_SELF | UPDATE_FILTERS,
		                                 error);
	}

	if (!s->priv->cached && !error)
	{
		s->priv->cached = g_strdup ("");
	}

	s->priv->cached_context = ctx;

	if (ctx)
	{
		s->priv->cached_marker = cpg_embedded_context_get_marker (ctx);
	}

	return s->priv->cached;
}

gchar const *
cpg_embedded_string_expand (CpgEmbeddedString   *s,
                            CpgEmbeddedContext  *ctx,
                            GError             **error)
{
	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (s), NULL);
	g_return_val_if_fail (ctx == NULL || CPG_IS_EMBEDDED_CONTEXT (ctx), NULL);

	return embedded_string_expand (s, ctx, error);
}

static GSList *
parse_expansion_range (gchar const *s)
{
	static GRegex *rangereg = NULL;
	static GRegex *timesreg = NULL;

	GMatchInfo *info;
	GSList *ret = NULL;

	if (rangereg == NULL)
	{
		rangereg = g_regex_new ("\\s*([0-9]+):([0-9]+)(:([0-9]+))?\\s*$",
		                        G_REGEX_ANCHORED,
		                        G_REGEX_MATCH_ANCHORED,
		                        NULL);
	}

	if (timesreg == NULL)
	{
		timesreg = g_regex_new ("\\s*([0-9]+)\\s*[*](.*)",
		                        G_REGEX_ANCHORED,
		                        G_REGEX_MATCH_ANCHORED,
		                        NULL);
	}

	if (g_regex_match (rangereg, s, 0, &info))
	{
		gchar *start = g_match_info_fetch (info, 1);
		gchar *step = g_match_info_fetch (info, 2);
		gchar *end = g_match_info_fetch (info, 4);

		if (!end || !*end)
		{
			g_free (end);

			end = step;
			step = NULL;
		}

		gint cstart = (gint)g_ascii_strtoll (start, NULL, 10);
		gint cend = (gint)g_ascii_strtoll (end, NULL, 10);
		gint cstep = 1;

		if (step)
		{
			cstep = (gint)g_ascii_strtoll (step, NULL, 10);
		}

		if (cend - (cstart + cstep) < cend - cstart)
		{
			while (cstart <= cend)
			{
				gchar *it;

				it = g_strdup_printf ("%d", cstart);

				ret = g_slist_prepend (ret,
				                       cpg_expansion_new_one (it));

				g_free (it);

				cstart += cstep;
			}
		}

		g_free (start);
		g_free (end);
		g_free (step);

		g_match_info_free (info);
	}
	else if (g_regex_match (timesreg, s, 0, &info))
	{
		gchar *start = g_match_info_fetch (info, 1);
		gchar *rest = g_match_info_fetch (info, 2);

		gint cstart = (gint)g_ascii_strtoll (start, NULL, 10);

		while (cstart > 0)
		{
			ret = g_slist_prepend (ret,
			                       cpg_expansion_new_one (rest));
			--cstart;
		}

		g_free (start);
		g_free (rest);
	}
	else
	{
		ret = g_slist_prepend (ret,
		                       cpg_expansion_new_one (s));
	}

	return g_slist_reverse (ret);
}

static GSList *
find_filter (CpgEmbeddedString *s,
             gint position)
{
	GSList *item;
	GSList *ret = NULL;

	for (item = s->priv->filters; item; item = g_slist_next (item))
	{
		Node *node = item->data;

		if (node->position == position)
		{
			ret = g_slist_prepend (ret, node);
		}
	}

	return g_slist_reverse (ret);
}

static gchar *
apply_reduce (CpgEmbeddedString *s,
              CpgEmbeddedContext *ctx,
              GSList *parts,
              Node   *filter,
              GError  **error)
{
	gchar *ret;

	if (!parts)
	{
		return g_strdup ("");
	}

	ret = g_strdup (cpg_expansion_get (parts->data, 0));
	parts = g_slist_next (parts);

	while (ret && parts)
	{
		CpgExpansion *ex;
		gchar *item;

		gchar const *cc[] = {
			ret,
			cpg_expansion_get (parts->data, 0),
			NULL
		};

		// Make a context
		cpg_embedded_context_save (ctx);

		ex = cpg_expansion_new ((gchar const * const *)cc);

		cpg_embedded_context_add_expansion (ctx, ex);
		g_object_unref (ex);

		item = evaluate_node (s, filter, ctx, 0, error);
		cpg_embedded_context_restore (ctx);

		g_free (ret);
		ret = item;

		parts = g_slist_next (parts);
	}

	return ret;
}

static GSList *
apply_map (CpgEmbeddedString *s,
           CpgEmbeddedContext *ctx,
           GSList *parts,
           Node   *filter,
           GError  **error)
{
	GSList *ret = NULL;
	GSList *part = parts;

	while (part)
	{
		gchar *item;

		// Make a context
		cpg_embedded_context_save (ctx);
		cpg_embedded_context_add_expansion (ctx, part->data);

		item = evaluate_node (s, filter, ctx, 0, error);

		cpg_embedded_context_restore (ctx);

		ret = g_slist_prepend (ret, cpg_expansion_new_one (item));
		g_free (item);

		g_object_unref (part->data);

		part = g_slist_next (part);
	}

	g_slist_free (parts);

	return g_slist_reverse (ret);
}

static GSList *
apply_filters_rev (CpgEmbeddedString *s,
                   CpgEmbeddedContext *ctx,
                   GSList *items,
                   gint position,
                   GError **error)
{
	GSList *filt;
	gchar *val;
	CpgExpansion *all;
	GPtrArray *ptr;
	gchar **ptrs;
	GSList *part;
	GSList *fitem;

	filt = find_filter (s, position);

	if (!filt)
	{
		return items;
	}

	items = g_slist_reverse (items);

	// Add one expansion of all the parts
	all = cpg_expansion_new (NULL);
	ptr = g_ptr_array_new ();

	for (part = items; part; part = g_slist_next (part))
	{
		g_ptr_array_add (ptr, g_strdup (cpg_expansion_get (part->data, 0)));
	}

	g_ptr_array_add (ptr, NULL);
	ptrs = (gchar **)g_ptr_array_free (ptr, FALSE);

	all = cpg_expansion_new ((gchar const * const *)ptrs);
	g_strfreev (ptrs);

	cpg_embedded_context_save (ctx);
	cpg_embedded_context_add_expansion (ctx, all);

	g_object_unref (all);

	for (fitem = filt; fitem; fitem = g_slist_next (fitem))
	{
		Node *n = fitem->data;

		/* First filter here what we have until now */
		if (n->type == CPG_EMBEDDED_STRING_NODE_REDUCE)
		{
			val = apply_reduce (s, ctx, items, n, error);

			g_slist_foreach (items, (GFunc)g_object_unref, NULL);
			g_slist_free (items);

			items = g_slist_prepend (NULL,
			                         cpg_expansion_new_one (val));

			g_free (val);
		}
		else
		{
			items = apply_map (s, ctx, items, n, error);
		}
	}

	cpg_embedded_context_restore (ctx);

	return g_slist_reverse (items);
}

typedef enum
{
	EX_NODE_TYPE_CONCAT,
	EX_NODE_TYPE_ELEMENTS,
	EX_NODE_TYPE_TEXT
} ExNodeType;

typedef struct _ExNode ExNode;

struct _ExNode
{
	ExNodeType type;

	ExNode *parent;
	ExNode *child;
	ExNode *last_child;

	ExNode *prev;
	ExNode *next;

	gchar *text;

	gint begin;
	gint end;
};

static ExNode *
ex_node_new (ExNode      *parent,
             ExNodeType   type,
             gchar const *text,
             gint         begin,
             gint         end)
{
	ExNode *ret;

	ret = g_slice_new0 (ExNode);

	ret->parent = parent;
	ret->type = type;

	if (parent)
	{
		if (parent->last_child)
		{
			parent->last_child->next = ret;
			ret->prev = parent->last_child;
		}
		else
		{
			parent->child = ret;
		}

		parent->last_child = ret;
	}

	ret->begin = begin;
	ret->end = end;
	ret->text = g_strdup (text);

	return ret;
}

static ExNode *
ex_node_new_root ()
{
	return ex_node_new (NULL, EX_NODE_TYPE_CONCAT, NULL, -1, -1);
}

static void
ex_node_remove (ExNode *self,
                ExNode *child)
{
	ExNode *ch;

	if (!self || !child)
	{
		return;
	}

	ch = self->child;

	while (ch)
	{
		if (ch == child)
		{
			ExNode *next = ch->next;

			if (self->child == ch)
			{
				self->child = ch->next;
			}

			if (self->last_child == ch)
			{
				self->last_child = ch->prev;
			}

			if (ch->next)
			{
				ch->next->prev = ch->prev;
				ch->next = NULL;
			}

			if (ch->prev)
			{
				ch->prev->next = next;
				ch->prev = NULL;
			}

			ch->parent = NULL;
			break;
		}

		ch = ch->next;
	}
}

static void
ex_node_free (ExNode *self)
{
	ExNode *child;

	g_free (self->text);
	self->text = NULL;

	child = self->child;

	while (child)
	{
		ExNode *next = child->next;
		ex_node_free (child);

		child = next;
	}

	ex_node_remove (self->parent, self);
	g_slice_free (ExNode, self);
}

static ExNode *
ex_node_append_text (ExNode *parent,
                     GString *buf,
                     gchar const *text,
                     gchar const *ptr,
                     gint        *last)
{
	// Store text until here as a text node
	gint end = ptr - text;
	ExNode *n;

	n = ex_node_new (parent, EX_NODE_TYPE_TEXT, buf->str, *last, end);

	*last = end + 1;

	g_string_erase (buf, 0, -1);
	return n;
}

static ExNode *
ex_node_expand (gchar const *text)
{
	ExNode *root = ex_node_new_root ();
	ExNode *current = root;
	gchar const *ptr = text;
	GString *buf;
	gint last = 0;

	buf = g_string_sized_new (strlen (text));

	while (*ptr)
	{
		switch (*ptr)
		{
			case '\\':
				// Skip escaped
				++ptr;

				if (*ptr)
				{
					g_string_append_c (buf, *ptr);
				}
				else
				{
					g_string_append_c (buf, '\\');
				}
			break;
			case '{':
			{
				ExNode *elems;

				// Append the text to the current concat node
				ex_node_append_text (current, buf, text, ptr, &last);

				// Create a new elements node
				elems = ex_node_new (current,
				                     EX_NODE_TYPE_ELEMENTS,
				                     NULL,
				                     ptr - text,
				                     0);

				// Create also the first concat node
				current = ex_node_new (elems,
				                       EX_NODE_TYPE_CONCAT,
				                       NULL,
				                       ptr - text,
				                       0);
			}
			break;
			case '}':
				// We check here the parent because we are
				// always in a CONCAT node, but we need to
				// close here the current ELEMENTS node which
				// is the parent of the CONCAT node
				if (current->parent)
				{
					ex_node_append_text (current, buf, text, ptr, &last);
					current->end = ptr - text;

					last = current->end + 1;

					current->parent->end = current->end;

					// We go two levels because the first
					// parent is the elements node
					current = current->parent->parent;
				}
				else
				{
					g_string_append_c (buf, *ptr);
				}
			break;
			case ',':
				if (current->parent)
				{
					// Append the remainder of the text to the
					// current CONCAT node
					ex_node_append_text (current, buf, text, ptr, &last);

					// Close the CONCAT node
					current->end = ptr - text;

					// Create a new CONCAT node in the containing
					// parent
					current = ex_node_new (current->parent,
					                       EX_NODE_TYPE_CONCAT,
					                       NULL,
					                       ptr - text,
					                       0);
				}
				else
				{
					g_string_append_c (buf, *ptr);
				}
			break;
			default:
				g_string_append_c (buf, *ptr);
			break;
		}

		if (*ptr)
		{
			++ptr;
		}
	}

	if (root != current)
	{
		ex_node_free (root);
		return NULL;
	}

	ex_node_append_text (current, buf, text, ptr, &last);
	return root;
}

static GSList *expand_node (CpgEmbeddedString   *s,
                            CpgEmbeddedContext  *ctx,
                            ExNode              *node,
                            GError             **error);

static gchar *
expand_node_escape (CpgEmbeddedString   *s,
                    CpgEmbeddedContext  *ctx,
                    ExNode              *node,
                    GError             **error);

static GSList *
annotate_first (GSList *items)
{
	GSList *item;
	gint i = 0;

	items = g_slist_reverse (items);

	for (item = items; item; item = g_slist_next (item))
	{
		cpg_expansion_insert (item->data,
		                      1,
		                      cpg_expansion_get (item->data, 0));

		cpg_expansion_set_index (item->data, 1, i);
		++i;
	}

	return g_slist_reverse (items);
}

static GSList *
expand_elements (CpgEmbeddedString   *s,
                 CpgEmbeddedContext  *ctx,
                 ExNode              *elements,
                 GError             **error)
{
	ExNode *child;
	GSList *ret = NULL;
	gint idx = 0;
	gint pos;

	child = elements->child;
	pos = elements->begin;

	while (child)
	{
		GSList *items;
		GSList *item;

		ret = apply_filters_rev (s, ctx, ret, pos, error);

		items = expand_node (s, ctx, child, error);

		// Append the items to the result
		for (item = items; item; item = g_slist_next (item))
		{
			if (cpg_expansion_num (item->data) == 1)
			{
				// This means no additional expansions, just
				// text. We are going to expand ranges on this
				GSList *more = parse_expansion_range (cpg_expansion_get (item->data, 0));

				ret = g_slist_concat (g_slist_reverse (more), ret);
				g_object_unref (item->data);
			}
			else
			{
				ret = g_slist_prepend (ret, item->data);
			}
		}

		pos = child->end;

		child = child->next;
		++idx;
	}

	// All items that are the result of a list, will have in their
	// expansions an additional @1 which refers to that element
	ret = apply_filters_rev (s, ctx, ret, pos, error);
	ret = annotate_first (ret);

	return g_slist_reverse (ret);
}

static CpgExpansion *
expansion_append (CpgExpansion *a,
                  CpgExpansion *b)
{
	// Merge @0 of a and b and append @1.. of b to a
	CpgExpansion *ret;
	gchar *cc;

	cc = g_strconcat (cpg_expansion_get (a, 0), cpg_expansion_get (b, 0), NULL);

	ret = cpg_expansion_new_one (cc);

	cpg_expansion_append (ret, a, 1);
	cpg_expansion_append (ret, b, 1);

	return ret;
}

static GSList *
expand_concat (CpgEmbeddedString   *s,
               CpgEmbeddedContext  *ctx,
               ExNode              *node,
               GError             **error)
{
	ExNode *child;
	GSList *ret = NULL;

	child = node->child;

	while (child)
	{
		GSList *ex;
		GSList *item;
		GSList *ritm;
		GSList *newret = NULL;

		ex = expand_node (s, ctx, child, error);
		gint i = 0;

		for (ritm = ret; ritm || !ret; ritm = g_slist_next (ritm))
		{
			for (item = ex; item; item = g_slist_next (item))
			{
				CpgExpansion *et;

				// Append the item onto the ritm
				if (ritm)
				{
					et = expansion_append (ritm->data, item->data);
				}
				else
				{
					et = cpg_expansion_copy (item->data);
				}

				cpg_expansion_set_index (et, 0, i++);

				newret = g_slist_prepend (newret,
				                          et);
			}

			if (ritm)
			{
				g_object_unref (ritm->data);
			}
			else
			{
				break;
			}
		}

		g_slist_foreach (ex, (GFunc)g_object_unref, NULL);
		g_slist_free (ex);

		g_slist_free (ret);
		ret = g_slist_reverse (newret);

		child = child->next;
	}

	return ret;
}

static GSList *
expand_node (CpgEmbeddedString   *s,
             CpgEmbeddedContext  *ctx,
             ExNode              *node,
             GError             **error)
{
	switch (node->type)
	{
		case EX_NODE_TYPE_TEXT:
			return g_slist_prepend (NULL,
			                        cpg_expansion_new_one (node->text));
		break;
		case EX_NODE_TYPE_CONCAT:
			return expand_concat (s, ctx, node, error);
		break;
		case EX_NODE_TYPE_ELEMENTS:
			return expand_elements (s, ctx, node, error);
		break;
	}

	return NULL;
}

static GSList *
expand_multiple (CpgEmbeddedString   *s,
                 CpgEmbeddedContext  *ctx,
                 gchar const         *t,
                 GError             **error)
{
	ExNode *root;
	GSList *ret;

	root = ex_node_expand (t);

	if (!root)
	{
		return NULL;
	}

	ret = expand_node (s, ctx, root, error);
	ex_node_free (root);

	return ret;
}

/**
 * cpg_embedded_string_expand_multiple:
 * @s: A #CpgEmbeddedString
 * @ctx: A #CpgEmbeddedContext
 *
 * Expand string with braces syntax.
 *
 * Returns: (element-type CpgExpansion) (transfer full): A #GSList of #CpgExpansion
 *
 **/
GSList *
cpg_embedded_string_expand_multiple (CpgEmbeddedString   *s,
                                     CpgEmbeddedContext  *ctx,
                                     GError             **error)
{
	gchar const *id;
	GSList *ret;

	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (s), NULL);
	g_return_val_if_fail (ctx == NULL || CPG_IS_EMBEDDED_CONTEXT (ctx), NULL);

	id = embedded_string_expand (s, ctx, error);

	if (!id)
	{
		return NULL;
	}

	if (!*id)
	{
		CpgExpansion *ex = cpg_expansion_new_one ("");
		ret = g_slist_prepend (NULL, ex);
	}
	else
	{
		ret = expand_multiple (s, ctx, id, error);
	}

	return ret;
}

void
cpg_embedded_string_clear_cache (CpgEmbeddedString *s)
{
	g_return_if_fail (CPG_IS_EMBEDDED_STRING (s));

	g_free (s->priv->cached);

	s->priv->cached = NULL;
	s->priv->cached_context = NULL;

	g_slist_free (s->priv->filters);
	s->priv->filters = NULL;
}

gchar *
cpg_embedded_string_escape (gchar const *s)
{
	GString *ret;

	ret = g_string_new ("");

	while (*s)
	{
		if (*s == '{' || *s == '\\' || *s == ',' || *s == '}')
		{
			g_string_append_c (ret, '\\');
		}

		g_string_append_c (ret, *s);
		++s;
	}

	return g_string_free (ret, FALSE);
}

static gchar *
expand_concat_escape (CpgEmbeddedString   *s,
                      CpgEmbeddedContext  *ctx,
                      ExNode              *node,
                      GError             **error)
{
	ExNode *child;
	GString *ret;

	ret = g_string_new ("");
	child = node->child;

	while (child)
	{
		gchar *ex;

		ex = expand_node_escape (s, ctx, child, error);
		g_string_append (ret, ex);
		g_free (ex);

		child = child->next;
	}

	return g_string_free (ret, FALSE);
}

static GSList *
apply_filters_rev_escape (CpgEmbeddedString   *s,
                          CpgEmbeddedContext  *ctx,
                          GSList              *elements,
                          gint                 pos,
                          GSList             **text,
                          GError             **error)
{
	GSList *ptr;

	ptr = apply_filters_rev (s, ctx, elements, pos, error);

	if (ptr != elements)
	{
		GSList *item;

		// Remove all previous items
		g_slist_foreach (*text, (GFunc)g_free, NULL);
		g_slist_free (*text);
		*text = NULL;

		for (item = ptr; item; item = g_slist_next (item))
		{
			*text = g_slist_prepend (*text,
			                          g_strdup (cpg_expansion_get (item->data, 0)));
		}

		*text = g_slist_reverse (*text);
	}

	return ptr;
}

static gchar *
expand_elements_escape (CpgEmbeddedString   *s,
                        CpgEmbeddedContext  *ctx,
                        ExNode              *elements,
                        GError             **error)
{
	ExNode *child;
	GSList *ret = NULL;
	gint idx = 0;
	gint pos;
	GSList *text = NULL;
	GString *sret;
	gboolean first;

	child = elements->child;
	pos = elements->begin;

	while (child)
	{
		GSList *items;
		GSList *item;

		ret = apply_filters_rev_escape (s, ctx, ret, pos, &text, error);

		items = expand_node (s, ctx, child, error);

		text = g_slist_prepend (text,
		                        expand_node_escape (s,
		                                            ctx,
		                                            child,
		                                            error));

		// Append the items to the result
		for (item = items; item; item = g_slist_next (item))
		{
			if (cpg_expansion_num (item->data) == 1)
			{
				// This means no additional expansions, just
				// text. We are going to expand ranges on this
				GSList *more = parse_expansion_range (cpg_expansion_get (item->data, 0));

				ret = g_slist_concat (g_slist_reverse (more), ret);
				g_object_unref (item->data);
			}
			else
			{
				ret = g_slist_prepend (ret, item->data);
			}
		}

		pos = child->end;
		child = child->next;

		++idx;
	}

	// All items that are the result of a list, will have in their
	// expansions an additional @1 which refers to that element
	ret = apply_filters_rev_escape (s, ctx, ret, pos, &text, error);

	g_slist_foreach (ret, (GFunc)g_object_unref, NULL);
	g_slist_free (ret);

	text = g_slist_reverse (text);

	sret = g_string_new ("{");
	first = TRUE;

	while (text)
	{
		if (!first)
		{
			g_string_append_c (sret, ',');
		}
		else
		{
			first = FALSE;
		}

		g_string_append (sret, text->data);
		g_free (text->data);

		text = g_slist_delete_link (text, text);
	}

	g_string_append_c (sret, '}');

	return g_string_free (sret, FALSE);
}

static gchar *
expand_node_escape (CpgEmbeddedString   *s,
                    CpgEmbeddedContext  *ctx,
                    ExNode              *node,
                    GError             **error)
{
	switch (node->type)
	{
		case EX_NODE_TYPE_TEXT:
			return cpg_embedded_string_escape (node->text);
		break;
		case EX_NODE_TYPE_CONCAT:
			return expand_concat_escape (s, ctx, node, error);
		break;
		case EX_NODE_TYPE_ELEMENTS:
			return expand_elements_escape (s, ctx, node, error);
		break;
	}

	return NULL;
}

static gchar *
expand_multiple_escape (CpgEmbeddedString   *s,
                        CpgEmbeddedContext  *ctx,
                        gchar const         *t,
                        GError             **error)
{
	ExNode *root;
	gchar *ret;

	root = ex_node_expand (t);

	if (!root)
	{
		return NULL;
	}

	ret = expand_node_escape (s, ctx, root, error);
	ex_node_free (root);

	return ret;
}

gchar *
cpg_embedded_string_expand_escape (CpgEmbeddedString   *s,
                                   CpgEmbeddedContext  *ctx,
                                   GError             **error)
{
	gchar const *id;
	gchar *ret;

	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (s), NULL);
	g_return_val_if_fail (ctx == NULL || CPG_IS_EMBEDDED_CONTEXT (ctx), NULL);

	id = embedded_string_expand (s, ctx, error);

	if (!id)
	{
		return NULL;
	}

	if (!*id)
	{
		return g_strdup ("");
	}
	else
	{
		ret = expand_multiple_escape (s, ctx, id, error);
	}

	return ret;
}

CpgEmbeddedString *
cpg_embedded_string_push_brace (CpgEmbeddedString *s)
{
	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (s), NULL);

	cpg_embedded_string_add_text (s, "{");
	++s->priv->braces;

	return s;
}

CpgEmbeddedString *
cpg_embedded_string_pop_brace (CpgEmbeddedString *s)
{
	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (s), NULL);

	cpg_embedded_string_add_text (s, "}");
	--s->priv->braces;

	return s;
}

gint
cpg_embedded_string_brace_level (CpgEmbeddedString *s)
{
	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (s), 0);

	return s->priv->braces;
}

CpgEmbeddedString *
cpg_embedded_string_add_string (CpgEmbeddedString *s,
                                CpgEmbeddedString *other)
{
	GSList *item;
	GSList *copied = NULL;
	Node *parent;

	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (s), NULL);
	g_return_val_if_fail (CPG_IS_EMBEDDED_STRING (other), NULL);

	for (item = other->priv->stack; item; item = g_slist_next (item))
	{
		copied = g_slist_prepend (copied, node_copy (item->data));
	}

	parent = s->priv->stack->data;

	parent->nodes = g_slist_concat (g_slist_reverse (copied),
	                                parent->nodes);

	cpg_embedded_string_clear_cache (s);
	return s;
}

