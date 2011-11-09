#ifndef __CPG_SYMBOLIC_H__
#define __CPG_SYMBOLIC_H__

#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-property.h>

typedef enum
{
	CPG_SYMBOLIC_DERIVE_NONE = 0,
	CPG_SYMBOLIC_DERIVE_PARTIAL = 1 << 0
} CpgSymbolicDeriveFlags;

#define CPG_SYMBOLIC_DERIVE_ERROR (cpg_symbolic_derive_error_quark ())

typedef enum
{
	CPG_SYMBOLIC_DERIVE_ERROR_UNSUPPORTED,
	CPG_SYMBOLIC_DERIVE_ERROR_INVALID
} CpgSymbolicDeriveError;

GQuark            cpg_symbolic_derive_error_quark      (void);

CpgExpression *cpg_symbolic_derive (CpgExpression           *expression,
                                    GSList                  *symbols,
                                    GHashTable              *property_map,
                                    CpgProperty             *property,
                                    gint                     order,
                                    CpgSymbolicDeriveFlags   flags,
                                    GError                 **error);

#endif /* __CPG_SYMBOLIC_H__ */

