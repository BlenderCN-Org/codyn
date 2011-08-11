/*
 * cpg-selector.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_SELECTOR_H__
#define __CPG_SELECTOR_H__

#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-link.h>
#include <cpg-network/cpg-embedded-string.h>
#include <cpg-network/cpg-selection.h>

G_BEGIN_DECLS

#define CPG_TYPE_SELECTOR		(cpg_selector_get_type ())
#define CPG_SELECTOR(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_SELECTOR, CpgSelector))
#define CPG_SELECTOR_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_SELECTOR, CpgSelector const))
#define CPG_SELECTOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_SELECTOR, CpgSelectorClass))
#define CPG_IS_SELECTOR(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_SELECTOR))
#define CPG_IS_SELECTOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_SELECTOR))
#define CPG_SELECTOR_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_SELECTOR, CpgSelectorClass))

typedef struct _CpgSelector		CpgSelector;
typedef struct _CpgSelectorClass	CpgSelectorClass;
typedef struct _CpgSelectorPrivate	CpgSelectorPrivate;

typedef enum
{
	CPG_SELECTOR_TYPE_NONE = 0,
	CPG_SELECTOR_TYPE_ANY = 1 << 0,
	CPG_SELECTOR_TYPE_STATE = 1 << 1,
	CPG_SELECTOR_TYPE_LINK = 1 << 2,
	CPG_SELECTOR_TYPE_GROUP = 1 << 3,
	CPG_SELECTOR_TYPE_PROPERTY = 1 << 4,
	CPG_SELECTOR_TYPE_ACTION = 1 << 5,
	CPG_SELECTOR_TYPE_FUNCTION = 1 << 6,
	CPG_SELECTOR_TYPE_OBJECT = CPG_SELECTOR_TYPE_STATE |
	                           CPG_SELECTOR_TYPE_LINK |
	                           CPG_SELECTOR_TYPE_GROUP |
	                           CPG_SELECTOR_TYPE_FUNCTION
} CpgSelectorType;

typedef enum
{
	CPG_SELECTOR_PSEUDO_TYPE_ROOT,
	CPG_SELECTOR_PSEUDO_TYPE_CHILDREN,
	CPG_SELECTOR_PSEUDO_TYPE_PARENT,
	CPG_SELECTOR_PSEUDO_TYPE_FIRST,
	CPG_SELECTOR_PSEUDO_TYPE_LAST,
	CPG_SELECTOR_PSEUDO_TYPE_SUBSET,
	CPG_SELECTOR_PSEUDO_TYPE_STATES,
	CPG_SELECTOR_PSEUDO_TYPE_LINKS,
	CPG_SELECTOR_PSEUDO_TYPE_GROUPS,
	CPG_SELECTOR_PSEUDO_TYPE_PROPERTIES,
	CPG_SELECTOR_PSEUDO_TYPE_ACTIONS,
	CPG_SELECTOR_PSEUDO_TYPE_FUNCTIONS,
	CPG_SELECTOR_PSEUDO_TYPE_OBJECTS,
	CPG_SELECTOR_PSEUDO_TYPE_SIBLINGS,
	CPG_SELECTOR_PSEUDO_TYPE_TEMPLATES,
	CPG_SELECTOR_PSEUDO_TYPE_COUNT,
	CPG_SELECTOR_PSEUDO_TYPE_FROM,
	CPG_SELECTOR_PSEUDO_TYPE_TO,
	CPG_SELECTOR_PSEUDO_TYPE_SELF,
	CPG_SELECTOR_PSEUDO_TYPE_DEBUG,
	CPG_SELECTOR_PSEUDO_TYPE_NAME,
	CPG_SELECTOR_PSEUDO_TYPE_DESCENDANTS,
	CPG_SELECTOR_PSEUDO_TYPE_ANCESTORS,
	CPG_SELECTOR_PSEUDO_TYPE_UNIQUE,
	CPG_SELECTOR_PSEUDO_TYPE_IF,
	CPG_SELECTOR_PSEUDO_TYPE_ISEMPTY,
	CPG_SELECTOR_PSEUDO_TYPE_REMOVE,
	CPG_SELECTOR_PSEUDO_TYPE_FROM_SET,
	CPG_SELECTOR_PSEUDO_TYPE_TYPE,
	CPG_SELECTOR_PSEUDO_NUM
} CpgSelectorPseudoType;

struct _CpgSelector
{
	GObject parent;

	CpgSelectorPrivate *priv;
};

struct _CpgSelectorClass
{
	GObjectClass parent_class;
};

GType         cpg_selector_get_type          (void) G_GNUC_CONST;

CpgSelector  *cpg_selector_new               (void);
CpgSelector  *cpg_selector_parse             (gchar const            *ptr,
                                              GError                **error);

gchar const  *cpg_selector_as_string         (CpgSelector            *selector);

guint         cpg_selector_append            (CpgSelector            *selector,
                                              CpgEmbeddedString      *identifier);

guint         cpg_selector_append_partial    (CpgSelector            *selector,
                                              CpgEmbeddedString      *identifier);

guint         cpg_selector_prepend            (CpgSelector            *selector,
                                               CpgEmbeddedString      *identifier);

guint         cpg_selector_prepend_partial    (CpgSelector            *selector,
                                               CpgEmbeddedString      *identifier);

guint         cpg_selector_append_pseudo     (CpgSelector            *selector,
                                              CpgSelectorPseudoType  type,
                                              GSList                 *arguments);

guint         cpg_selector_prepend_pseudo    (CpgSelector            *selector,
                                              CpgSelectorPseudoType  type,
                                              GSList                 *arguments);

guint         cpg_selector_append_regex      (CpgSelector            *selector,
                                              CpgEmbeddedString      *regex);

guint         cpg_selector_append_regex_partial (CpgSelector            *selector,
                                                 CpgEmbeddedString      *regex);

guint         cpg_selector_prepend_regex      (CpgSelector            *selector,
                                               CpgEmbeddedString      *regex);

guint         cpg_selector_prepend_regex_partial (CpgSelector            *selector,
                                                  CpgEmbeddedString      *regex);

GSList       *cpg_selector_select            (CpgSelector            *selector,
                                              GObject                *parent,
                                              CpgSelectorType         type,
                                              CpgEmbeddedContext     *context);

void          cpg_selector_set_partial       (CpgSelector          *selector,
                                              gboolean              partial);

guint         cpg_selector_get_last_id       (CpgSelector          *selector);

GSList const *cpg_selector_get_in_context    (CpgSelector          *selector,
                                              guint                 id);

GSList const *cpg_selector_get_out_context    (CpgSelector          *selector,
                                               guint                 id);

gboolean      cpg_selector_is_pseudo_name     (gchar const          *name);
gchar        *cpg_selector_escape_identifier  (gchar const          *name);

void          cpg_selector_set_from_set      (CpgSelector            *selector,
                                              GSList                 *selections);

G_END_DECLS

#endif /* __CPG_SELECTOR_H__ */
