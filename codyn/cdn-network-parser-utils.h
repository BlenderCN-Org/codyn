/*
 * cdn-network-parser-utils.h
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

#ifndef __CDN_NETWORK_PARSER_UTILS_H__
#define __CDN_NETWORK_PARSER_UTILS_H__

#include <codyn/cdn-network.h>
#include <codyn/cdn-import.h>
#include <codyn/cdn-selector.h>

gboolean     cdn_network_parser_utils_get_templates        (CdnNetwork           *network,
                                                            CdnNode              *parent,
                                                            gboolean              for_template,
                                                            GSList               *selectors,
                                                            CdnExpansionContext  *context,
                                                            gchar               **missing,
                                                            GSList              **templates);

GType        cdn_network_parser_utils_type_from_templates  (GType                 orig,
                                                            GSList               *templates);

GFile       *cdn_network_parser_utils_resolve_import       (GFile                *root,
                                                            gchar const          *filename);

CdnImport   *cdn_network_parser_utils_find_template_import (CdnObject            *child,
                                                            GFile                *file);

#endif /* __CDN_NETWORK_PARSER_UTILS_H__ */

