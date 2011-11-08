#ifndef __CPG_SYMBOLIC_H__
#define __CPG_SYMBOLIC_H__

#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-property.h>

CpgExpression *cpg_symbolic_derive (CpgExpression *expression,
                                    CpgProperty   *property,
                                    gint           order);

#endif /* __CPG_SYMBOLIC_H__ */

