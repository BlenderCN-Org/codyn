#ifndef __CPG_SYMBOLIC_H__
#define __CPG_SYMBOLIC_H__

#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-property.h>

typedef enum
{
	CPG_SYMBOLIC_DERIVE_NONE = 0,
	CPG_SYMBOLIC_DERIVE_PARTIAL = 1 << 0
} CpgSymbolicDeriveFlags;

#define CPG_SYMBOLIC_ERROR (cpg_symbolic_error_quark ())

typedef enum
{
	CPG_SYMBOLIC_ERROR_UNSUPPORTED,
	CPG_SYMBOLIC_ERROR_INVALID
} CpgSymbolicError;

GQuark            cpg_symbolic_error_quark      (void);

CpgExpression *cpg_symbolic_derive (CpgExpression           *expression,
                                    GSList                  *symbols,
                                    GHashTable              *property_map,
                                    GHashTable              *diff_map,
                                    gint                     order,
                                    CpgSymbolicDeriveFlags   flags,
                                    GError                 **error);

CpgExpression *cpg_symbolic_simplify (CpgExpression *expression);

#endif /* __CPG_SYMBOLIC_H__ */

