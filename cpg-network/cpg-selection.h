#ifndef __CPG_SELECTION_H__
#define __CPG_SELECTION_H__

#include <cpg-network/cpg-object.h>

G_BEGIN_DECLS

typedef struct _CpgSelection CpgSelection;

CpgSelection *cpg_selection_new              (gpointer                object,
                                              GSList                 *expansions);

void          cpg_selection_free             (CpgSelection           *selection);
CpgSelection *cpg_selection_copy             (CpgSelection           *selection);

CpgObject    *cpg_selection_get_object       (CpgSelection           *selection);
CpgProperty  *cpg_selection_get_property     (CpgSelection           *selection);
GSList       *cpg_selection_get_expansions   (CpgSelection           *selection);

G_END_DECLS

#endif /* __CPG_SELECTION_H__ */

