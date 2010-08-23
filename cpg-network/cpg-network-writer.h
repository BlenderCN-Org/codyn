/*
 * cpg-network-writer.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_NETWORK_WRITER_H__
#define __CPG_NETWORK_WRITER_H__

#include <glib.h>
#include "cpg-network.h"

G_BEGIN_DECLS

gboolean cpg_network_writer_xml (CpgNetwork  *network,
                                 gchar const *filename);

gchar *cpg_network_writer_xml_string (CpgNetwork *network);

G_END_DECLS

#endif /* __CPG_NETWORK_WRITER_H__ */

