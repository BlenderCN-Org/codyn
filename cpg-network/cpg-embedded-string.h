/*
 * cpg-embedded-string.h
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

#ifndef __CPG_EMBEDDED_STRING_H__
#define __CPG_EMBEDDED_STRING_H__

#include <glib-object.h>
#include <cpg-network/cpg-embedded-context.h>
#include <cpg-network/cpg-expansion.h>

G_BEGIN_DECLS

#define CPG_TYPE_EMBEDDED_STRING		(cpg_embedded_string_get_type ())
#define CPG_EMBEDDED_STRING(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_EMBEDDED_STRING, CpgEmbeddedString))
#define CPG_EMBEDDED_STRING_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_EMBEDDED_STRING, CpgEmbeddedString const))
#define CPG_EMBEDDED_STRING_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_EMBEDDED_STRING, CpgEmbeddedStringClass))
#define CPG_IS_EMBEDDED_STRING(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_EMBEDDED_STRING))
#define CPG_IS_EMBEDDED_STRING_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_EMBEDDED_STRING))
#define CPG_EMBEDDED_STRING_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_EMBEDDED_STRING, CpgEmbeddedStringClass))

typedef struct _CpgEmbeddedString		CpgEmbeddedString;
typedef struct _CpgEmbeddedStringClass		CpgEmbeddedStringClass;
typedef struct _CpgEmbeddedStringPrivate	CpgEmbeddedStringPrivate;

typedef enum
{
	CPG_EMBEDDED_STRING_NODE_TEXT,
	CPG_EMBEDDED_STRING_NODE_EQUATION,
	CPG_EMBEDDED_STRING_NODE_INDIRECTION,
	CPG_EMBEDDED_STRING_NODE_REDUCE,
	CPG_EMBEDDED_STRING_NODE_MAP,
	CPG_EMBEDDED_STRING_NODE_CONDITION
} CpgEmbeddedStringNodeType;

struct _CpgEmbeddedString
{
	GObject parent;

	CpgEmbeddedStringPrivate *priv;
};

struct _CpgEmbeddedStringClass
{
	GObjectClass parent_class;
};

GType              cpg_embedded_string_get_type         (void) G_GNUC_CONST;

CpgEmbeddedString *cpg_embedded_string_new              (void);

CpgEmbeddedString *cpg_embedded_string_new_from_string  (gchar const               *s);
CpgEmbeddedString *cpg_embedded_string_new_from_double  (gdouble                    s);
CpgEmbeddedString *cpg_embedded_string_new_from_integer (gint                       s);

CpgEmbeddedString *cpg_embedded_string_push             (CpgEmbeddedString         *s,
                                                         CpgEmbeddedStringNodeType  type,
                                                         gint                       depth);

CpgEmbeddedString *cpg_embedded_string_pop              (CpgEmbeddedString         *s);

CpgEmbeddedString *cpg_embedded_string_push_brace       (CpgEmbeddedString         *s);
CpgEmbeddedString *cpg_embedded_string_pop_brace        (CpgEmbeddedString         *s);

gint               cpg_embedded_string_brace_level      (CpgEmbeddedString         *s);

CpgEmbeddedString *cpg_embedded_string_add_string       (CpgEmbeddedString         *s,
                                                         CpgEmbeddedString         *other);

CpgEmbeddedString *cpg_embedded_string_add_text         (CpgEmbeddedString         *s,
                                                         gchar const               *text);

CpgEmbeddedString *cpg_embedded_string_prepend_text     (CpgEmbeddedString         *s,
                                                         gchar const               *text);


gchar const       *cpg_embedded_string_expand           (CpgEmbeddedString         *s,
                                                         CpgEmbeddedContext        *ctx,
                                                         GError                   **error);

GSList            *cpg_embedded_string_expand_multiple  (CpgEmbeddedString         *s,
                                                         CpgEmbeddedContext        *ctx,
                                                         GError                   **error);

gchar             *cpg_embedded_string_escape           (gchar const               *item);

gchar             *cpg_embedded_string_expand_escape    (CpgEmbeddedString         *s,
                                                         CpgEmbeddedContext        *ctx,
                                                         GError                   **error);

void               cpg_embedded_string_clear_cache      (CpgEmbeddedString         *s);

gchar             *cpg_embedded_string_collapse         (gchar const * const       *s);

G_END_DECLS

#endif /* __CPG_EMBEDDED_STRING_H__ */
