#ifndef __CPG_OPERATORS_H__
#define __CPG_OPERATORS_H__

#include <cpg-network/cpg-operator.h>

G_BEGIN_DECLS

void                 cpg_operators_register                   (GType            gtype);
void                 cpg_operators_unregister                 (GType            gtype);
GType                cpg_operators_find                       (gchar const     *name);
CpgOperatorClass    *cpg_operators_find_class                 (gchar const     *name);
CpgOperator         *cpg_operators_instantiate                (gchar const     *name,
                                                               GSList const    *expressions);
GSList const        *cpg_operators_list                       ();

G_END_DECLS

#endif /* __CPG_OPERATORS_H__ */

