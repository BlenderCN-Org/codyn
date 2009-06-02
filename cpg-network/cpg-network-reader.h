#ifndef __CPG_NETWORK_READER_H__
#define __CPG_NETWORK_READER_H__

#include <glib.h>
#include "cpg-network.h"

gboolean cpg_network_reader_xml (CpgNetwork  *network,
                                 gchar const *filename);

gboolean cpg_network_reader_xml_string (CpgNetwork  *network,
                                        gchar const *filename);


#endif /* __CPG_NETWORK_READER_H__ */

