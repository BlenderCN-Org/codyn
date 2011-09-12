/*
 * cpg-link.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_LINK_H__
#define __CPG_LINK_H__

#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-link-action.h>

G_BEGIN_DECLS

#define CPG_TYPE_LINK            (cpg_link_get_type ())
#define CPG_LINK(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_LINK, CpgLink))
#define CPG_LINK_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_LINK, CpgLink const))
#define CPG_LINK_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_LINK, CpgLinkClass))
#define CPG_IS_LINK(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_LINK))
#define CPG_IS_LINK_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_LINK))
#define CPG_LINK_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_LINK, CpgLinkClass))

typedef struct _CpgLink        CpgLink;
typedef struct _CpgLinkClass   CpgLinkClass;
typedef struct _CpgLinkPrivate CpgLinkPrivate;

struct _CpgLink
{
	/*< private >*/
	CpgObject parent;

	CpgLinkPrivate *priv;
};

struct _CpgLinkClass
{
	/*< private >*/
	CpgObjectClass parent_class;

	void (*action_added)   (CpgLink *link, CpgLinkAction *action);
	void (*action_removed) (CpgLink *link, CpgLinkAction *action);
};

GType          cpg_link_get_type              (void) G_GNUC_CONST;

CpgLink       *cpg_link_new                   (const gchar   *id,
                                               CpgObject     *from,
                                               CpgObject     *to);

CpgObject     *cpg_link_get_from              (CpgLink       *link);
CpgObject     *cpg_link_get_to                (CpgLink       *link);

gboolean       cpg_link_add_action            (CpgLink       *link,
                                               CpgLinkAction *action);

gboolean       cpg_link_remove_action         (CpgLink       *link,
                                               CpgLinkAction *action);

const GSList  *cpg_link_get_actions           (CpgLink       *link);
CpgLinkAction *cpg_link_get_action            (CpgLink       *link,
                                               const gchar   *target);
void           cpg_link_attach                (CpgLink       *link,
                                               CpgObject     *from,
                                               CpgObject     *to);

CpgLink       *cpg_link_get_action_template   (CpgLink       *link,
                                               CpgLinkAction *action,
                                               gboolean       match_full);

G_END_DECLS

#endif /* __CPG_LINK_H__ */
