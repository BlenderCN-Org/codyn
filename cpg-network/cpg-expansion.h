#ifndef __CPG_EXPANSION_H__
#define __CPG_EXPANSION_H__

#include <glib-object.h>

G_BEGIN_DECLS

typedef struct _CpgExpansion CpgExpansion;

CpgExpansion *cpg_expansion_new              (gchar const * const    *items);
CpgExpansion *cpg_expansion_new_one          (gchar const            *item);

CpgExpansion *cpg_expansion_copy             (CpgExpansion           *id);

gint          cpg_expansion_num              (CpgExpansion           *id);

gchar const  *cpg_expansion_get              (CpgExpansion           *id,
                                              gint                    idx);

void          cpg_expansion_add              (CpgExpansion           *id,
                                              gchar const            *item);

void          cpg_expansion_set              (CpgExpansion           *id,
                                              gint                    idx,
                                              gchar const            *val);

void          cpg_expansion_free             (CpgExpansion           *id);

gchar        *cpg_expansions_expand          (GSList                 *expansions,
                                              gchar const            *s,
                                              GRegex                 *regex);

G_END_DECLS

#endif /* __CPG_EXPANSION_H__ */

