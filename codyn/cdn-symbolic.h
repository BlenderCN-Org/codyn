#ifndef __CDN_SYMBOLIC_H__
#define __CDN_SYMBOLIC_H__

#include <codyn/cdn-expression.h>
#include <codyn/cdn-variable.h>

typedef enum
{
	CDN_SYMBOLIC_DERIVE_NONE = 0,
	CDN_SYMBOLIC_DERIVE_PARTIAL = 1 << 0,
	CDN_SYMBOLIC_DERIVE_SIMPLIFY = 1 << 1
} CdnSymbolicDeriveFlags;

#define CDN_SYMBOLIC_ERROR (cdn_symbolic_error_quark ())

typedef enum
{
	CDN_SYMBOLIC_ERROR_UNSUPPORTED,
	CDN_SYMBOLIC_ERROR_INVALID
} CdnSymbolicError;

GQuark            cdn_symbolic_error_quark      (void);

CdnExpression *cdn_symbolic_derive (CdnExpression           *expression,
                                    GSList                  *symbols,
                                    GHashTable              *property_map,
                                    GHashTable              *diff_map,
                                    gint                     order,
                                    CdnSymbolicDeriveFlags   flags,
                                    GError                 **error);

CdnExpression *cdn_symbolic_simplify (CdnExpression *expression);

#endif /* __CDN_SYMBOLIC_H__ */

