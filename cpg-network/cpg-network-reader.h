#ifndef __CPG_NETWORK_READER_H__
#define __CPG_NETWORK_READER_H__

#include <glib.h>
#include "cpg-network.h"

G_BEGIN_DECLS

gboolean cpg_network_reader_merge_from_file (CpgNetwork   *network,
                                             CpgGroup     *root,
                                             gchar const  *filename,
                                             GError      **error);

gboolean cpg_network_reader_merge_from_xml (CpgNetwork   *network,
                                            CpgGroup     *root,
                                            gchar const  *xml,
                                            GError      **error);

G_END_DECLS

#endif /* __CPG_NETWORK_READER_H__ */

