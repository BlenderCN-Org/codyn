/*
 * cpg-network-parser-utils.h
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

#ifndef __CPG_NETWORK_PARSER_UTILS_H__
#define __CPG_NETWORK_PARSER_UTILS_H__

#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-import.h>
#include <cpg-network/cpg-selector.h>

gboolean     cpg_network_parser_utils_get_templates        (CpgNetwork           *network,
                                                            CpgGroup             *parent,
                                                            gboolean              for_template,
                                                            GSList               *selectors,
                                                            CpgEmbeddedContext   *context,
                                                            gchar               **missing,
                                                            GSList              **templates);

GType        cpg_network_parser_utils_type_from_templates  (GType                 orig,
                                                            GSList               *templates);

GFile       *cpg_network_parser_utils_resolve_import       (GFile                *root,
                                                            gchar const          *filename);

CpgImport   *cpg_network_parser_utils_find_template_import (CpgObject            *child,
                                                            GFile                *file);

#endif /* __CPG_NETWORK_PARSER_UTILS_H__ */

