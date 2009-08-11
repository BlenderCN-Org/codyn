#ifndef __CPG_NETWORK_READER_H__
#define __CPG_NETWORK_READER_H__

#include <glib.h>
#include "cpg-network.h"

G_BEGIN_DECLS

gboolean cpg_network_reader_xml (CpgNetwork   *network,
                                 gchar const  *filename,
                                 GError      **error);

gboolean cpg_network_reader_xml_string (CpgNetwork   *network,
                                        gchar const  *xml,
                                        GError      **error);

G_END_DECLS

#endif /* __CPG_NETWORK_READER_H__ */

