#ifndef __CPG_NETWORK_PARSER_UTILS_H__
#define __CPG_NETWORK_PARSER_UTILS_H__

#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-import.h>

typedef struct _CpgExpandedId CpgExpandedId;

gboolean cpg_network_parser_utils_get_templates (CpgNetwork           *network,
                                                 CpgGroup             *parent,
                                                 gboolean              for_template,
                                                 gchar const * const  *names,
                                                 gchar               **missing,
                                                 GSList              **templates);

GType cpg_network_parser_utils_type_from_templates (GType   orig,
                                                    GSList *templates);

GFile *cpg_network_parser_utils_resolve_import (GFile       *root,
                                                gchar const *filename);

CpgImport *cpg_network_parser_utils_find_template_import (CpgObject *child,
                                                          GFile     *file);

GSList *cpg_network_parser_utils_expand_id (gchar const *id);

gchar const *cpg_expanded_id_get_id (CpgExpandedId *id);

gint cpg_expanded_id_get_num_expanded (CpgExpandedId *id);

gchar const *cpg_expanded_id_get_expanded (CpgExpandedId *id,
                                           gint           idx);

void cpg_expanded_id_free (CpgExpandedId *id);

#endif /* __CPG_NETWORK_PARSER_UTILS_H__ */

