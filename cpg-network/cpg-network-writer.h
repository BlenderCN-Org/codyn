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

