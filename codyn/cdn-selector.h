/*
 * cdn-selector.h
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

#ifndef __CDN_SELECTOR_H__
#define __CDN_SELECTOR_H__

#include <codyn/cdn-network.h>
#include <codyn/cdn-edge.h>
#include <codyn/cdn-embedded-string.h>
#include <codyn/cdn-selection.h>

G_BEGIN_DECLS

#define CDN_TYPE_SELECTOR		(cdn_selector_get_type ())
#define CDN_SELECTOR(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_SELECTOR, CdnSelector))
#define CDN_SELECTOR_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_SELECTOR, CdnSelector const))
#define CDN_SELECTOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_SELECTOR, CdnSelectorClass))
#define CDN_IS_SELECTOR(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_SELECTOR))
#define CDN_IS_SELECTOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_SELECTOR))
#define CDN_SELECTOR_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_SELECTOR, CdnSelectorClass))

typedef struct _CdnSelector		CdnSelector;
typedef struct _CdnSelectorClass	CdnSelectorClass;
typedef struct _CdnSelectorPrivate	CdnSelectorPrivate;

typedef enum
{
	CDN_SELECTOR_TYPE_NONE = 0,
	CDN_SELECTOR_TYPE_ANY = 1 << 0,
	CDN_SELECTOR_TYPE_STATE = 1 << 1,
	CDN_SELECTOR_TYPE_EDGE = 1 << 2,
	CDN_SELECTOR_TYPE_NODE = 1 << 3,
	CDN_SELECTOR_TYPE_VARIABLE = 1 << 4,
	CDN_SELECTOR_TYPE_ACTION = 1 << 5,
	CDN_SELECTOR_TYPE_FUNCTION = 1 << 6,
	CDN_SELECTOR_TYPE_TEMPLATE = 1 << 7,
	CDN_SELECTOR_TYPE_OBJECT = CDN_SELECTOR_TYPE_STATE |
	                           CDN_SELECTOR_TYPE_EDGE |
	                           CDN_SELECTOR_TYPE_NODE |
	                           CDN_SELECTOR_TYPE_FUNCTION |
	                           CDN_SELECTOR_TYPE_TEMPLATE
} CdnSelectorType;

typedef enum
{
	CDN_SELECTOR_PSEUDO_TYPE_ROOT,
	CDN_SELECTOR_PSEUDO_TYPE_TEMPLATES_ROOT,
	CDN_SELECTOR_PSEUDO_TYPE_CHILDREN,
	CDN_SELECTOR_PSEUDO_TYPE_PARENT,
	CDN_SELECTOR_PSEUDO_TYPE_FIRST,
	CDN_SELECTOR_PSEUDO_TYPE_LAST,
	CDN_SELECTOR_PSEUDO_TYPE_SUBSET,
	CDN_SELECTOR_PSEUDO_TYPE_STATES,
	CDN_SELECTOR_PSEUDO_TYPE_EDGES,
	CDN_SELECTOR_PSEUDO_TYPE_NODES,
	CDN_SELECTOR_PSEUDO_TYPE_IMPORTS,
	CDN_SELECTOR_PSEUDO_TYPE_VARIABLES,
	CDN_SELECTOR_PSEUDO_TYPE_ACTIONS,
	CDN_SELECTOR_PSEUDO_TYPE_FUNCTIONS,
	CDN_SELECTOR_PSEUDO_TYPE_OBJECTS,
	CDN_SELECTOR_PSEUDO_TYPE_SIBLINGS,
	CDN_SELECTOR_PSEUDO_TYPE_TEMPLATES,
	CDN_SELECTOR_PSEUDO_TYPE_COUNT,
	CDN_SELECTOR_PSEUDO_TYPE_FROM,
	CDN_SELECTOR_PSEUDO_TYPE_TO,
	CDN_SELECTOR_PSEUDO_TYPE_SELF,
	CDN_SELECTOR_PSEUDO_TYPE_DEBUG,
	CDN_SELECTOR_PSEUDO_TYPE_NAME,
	CDN_SELECTOR_PSEUDO_TYPE_DESCENDANTS,
	CDN_SELECTOR_PSEUDO_TYPE_ANCESTORS,
	CDN_SELECTOR_PSEUDO_TYPE_UNIQUE,
	CDN_SELECTOR_PSEUDO_TYPE_IF,
	CDN_SELECTOR_PSEUDO_TYPE_IS_EMPTY,
	CDN_SELECTOR_PSEUDO_TYPE_REMOVE,
	CDN_SELECTOR_PSEUDO_TYPE_FROM_SET,
	CDN_SELECTOR_PSEUDO_TYPE_TYPE,
	CDN_SELECTOR_PSEUDO_TYPE_HAS_FLAG,
	CDN_SELECTOR_PSEUDO_TYPE_HAS_TEMPLATE,
	CDN_SELECTOR_PSEUDO_TYPE_HAS_TAG,
	CDN_SELECTOR_PSEUDO_TYPE_REVERSE,
	CDN_SELECTOR_PSEUDO_TYPE_SOURCE_NAME,
	CDN_SELECTOR_PSEUDO_TYPE_SINK_NAME,
	CDN_SELECTOR_PSEUDO_NUM
} CdnSelectorPseudoType;

struct _CdnSelector
{
	GObject parent;

	CdnSelectorPrivate *priv;
};

struct _CdnSelectorClass
{
	GObjectClass parent_class;
};

GType         cdn_selector_get_type          (void) G_GNUC_CONST;

CdnSelector  *cdn_selector_new               (CdnObject              *root);
CdnSelector  *cdn_selector_parse             (CdnObject              *root,
                                              gchar const            *s,
                                              GError                **error);

CdnSelector  *cdn_selector_copy_with         (CdnSelector            *selector);

gchar        *cdn_selector_as_string         (CdnSelector            *selector);

guint         cdn_selector_append            (CdnSelector            *selector,
                                              CdnEmbeddedString      *identifier);

guint         cdn_selector_append_partial    (CdnSelector            *selector,
                                              CdnEmbeddedString      *identifier);

guint         cdn_selector_append_pseudo     (CdnSelector            *selector,
                                              CdnSelectorPseudoType  type,
                                              GSList                 *arguments);

guint         cdn_selector_append_regex      (CdnSelector            *selector,
                                              CdnEmbeddedString      *regex);

guint         cdn_selector_append_regex_partial (CdnSelector            *selector,
                                                 CdnEmbeddedString      *regex);

GSList       *cdn_selector_select            (CdnSelector            *selector,
                                              GObject                *parent,
                                              CdnSelectorType         type,
                                              CdnEmbeddedContext     *context);

void          cdn_selector_set_partial       (CdnSelector          *selector,
                                              gboolean              partial);

void          cdn_selector_set_self          (CdnSelector          *selector,
                                              CdnSelection         *selection);

guint         cdn_selector_get_last_id       (CdnSelector          *selector);

GSList const *cdn_selector_get_in_context    (CdnSelector          *selector,
                                              guint                 id);

GSList const *cdn_selector_get_out_context    (CdnSelector          *selector,
                                               guint                 id);

gboolean      cdn_selector_is_pseudo_name     (gchar const          *name);
gchar        *cdn_selector_escape_identifier  (gchar const          *name);

void          cdn_selector_set_from_set      (CdnSelector            *selector,
                                              GSList                 *selections);

void          cdn_selector_set_implicit_children (CdnSelector *selector,
                                                  gboolean     isimplicit);

G_END_DECLS

#endif /* __CDN_SELECTOR_H__ */
