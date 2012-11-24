/*
 * cdn-embedded-string.h
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

#ifndef __CDN_EMBEDDED_STRING_H__
#define __CDN_EMBEDDED_STRING_H__

#include <glib-object.h>
#include <codyn/cdn-expansion-context.h>
#include <codyn/cdn-expansion.h>

G_BEGIN_DECLS

#define CDN_TYPE_EMBEDDED_STRING		(cdn_embedded_string_get_type ())
#define CDN_EMBEDDED_STRING(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EMBEDDED_STRING, CdnEmbeddedString))
#define CDN_EMBEDDED_STRING_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EMBEDDED_STRING, CdnEmbeddedString const))
#define CDN_EMBEDDED_STRING_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_EMBEDDED_STRING, CdnEmbeddedStringClass))
#define CDN_IS_EMBEDDED_STRING(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_EMBEDDED_STRING))
#define CDN_IS_EMBEDDED_STRING_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_EMBEDDED_STRING))
#define CDN_EMBEDDED_STRING_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_EMBEDDED_STRING, CdnEmbeddedStringClass))

typedef struct _CdnEmbeddedString		CdnEmbeddedString;
typedef struct _CdnEmbeddedStringClass		CdnEmbeddedStringClass;
typedef struct _CdnEmbeddedStringPrivate	CdnEmbeddedStringPrivate;

typedef enum
{
	CDN_EMBEDDED_STRING_NODE_TEXT,
	CDN_EMBEDDED_STRING_NODE_EQUATION,
	CDN_EMBEDDED_STRING_NODE_INDIRECTION,
	CDN_EMBEDDED_STRING_NODE_REDUCE,
	CDN_EMBEDDED_STRING_NODE_MAP,
	CDN_EMBEDDED_STRING_NODE_CONDITION
} CdnEmbeddedStringNodeType;

struct _CdnEmbeddedString
{
	GObject parent;

	CdnEmbeddedStringPrivate *priv;
};

struct _CdnEmbeddedStringClass
{
	GObjectClass parent_class;
};

typedef enum
{
	CDN_EMBEDDED_STRING_ERROR_BRACES,
	CDN_EMBEDDED_STRING_ERROR_INVALID_EXPANSION
} CdnEmbeddedStringError;

#define CDN_EMBEDDED_STRING_ERROR (cdn_embedded_string_error_quark ())

GQuark             cdn_embedded_string_error_quark      ();

GType              cdn_embedded_string_get_type         (void) G_GNUC_CONST;

CdnEmbeddedString *cdn_embedded_string_new              (void);

CdnEmbeddedString *cdn_embedded_string_new_from_string  (gchar const               *s);
CdnEmbeddedString *cdn_embedded_string_new_from_double  (gdouble                    s);
CdnEmbeddedString *cdn_embedded_string_new_from_integer (gint                       s);

CdnEmbeddedString *cdn_embedded_string_push             (CdnEmbeddedString         *s,
                                                         CdnEmbeddedStringNodeType  type,
                                                         gint                       depth);

CdnEmbeddedString *cdn_embedded_string_pop              (CdnEmbeddedString         *s);

CdnEmbeddedString *cdn_embedded_string_push_brace       (CdnEmbeddedString         *s);
CdnEmbeddedString *cdn_embedded_string_pop_brace        (CdnEmbeddedString         *s);

gint               cdn_embedded_string_brace_level      (CdnEmbeddedString         *s);

CdnEmbeddedString *cdn_embedded_string_add_string       (CdnEmbeddedString         *s,
                                                         CdnEmbeddedString         *other);

CdnEmbeddedString *cdn_embedded_string_add_text         (CdnEmbeddedString         *s,
                                                         gchar const               *text);

CdnEmbeddedString *cdn_embedded_string_prepend_text     (CdnEmbeddedString         *s,
                                                         gchar const               *text);


gchar const       *cdn_embedded_string_expand           (CdnEmbeddedString         *s,
                                                         CdnExpansionContext       *ctx,
                                                         GError                   **error);

GSList            *cdn_embedded_string_expand_multiple  (CdnEmbeddedString         *s,
                                                         CdnExpansionContext       *ctx,
                                                         GError                   **error);

gchar             *cdn_embedded_string_escape           (gchar const               *item);

gchar             *cdn_embedded_string_expand_escape    (CdnEmbeddedString         *s,
                                                         CdnExpansionContext       *ctx,
                                                         GError                   **error);

void               cdn_embedded_string_clear_cache      (CdnEmbeddedString         *s);

gchar             *cdn_embedded_string_collapse         (gchar const * const       *s);

CdnExpansion      *cdn_embedded_string_as_expansion     (CdnEmbeddedString         *s,
                                                         CdnExpansionContext       *context,
                                                         GError                   **error);

G_END_DECLS

#endif /* __CDN_EMBEDDED_STRING_H__ */
