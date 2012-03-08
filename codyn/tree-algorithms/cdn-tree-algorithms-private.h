#ifndef __CDN_TREE_ALGORITHMS_PRIVATE_H__
#define __CDN_TREE_ALGORITHMS_PRIVATE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#include <codyn/cdn-expression-tree-iter.h>
#include <codyn/cdn-math.h>
#include <math.h>
#include "codyn/instructions/cdn-instructions.h"

struct _CdnExpressionTreeIter
{
	CdnExpressionTreeIter *parent;
	CdnInstruction *instruction;

	CdnExpressionTreeIter **children;
	gint num_children;

	gchar *cached_to_string;
	guint cache_is_dbg : 1;
};

CdnExpressionTreeIter * iter_copy                  (CdnExpressionTreeIter       *iter);
CdnExpressionTreeIter * iter_new                   (CdnInstruction              *instruction);
CdnExpressionTreeIter * iter_new_take              (CdnInstruction              *instruction);
CdnExpressionTreeIter * iter_new_numstr            (gchar const                 *num);
CdnExpressionTreeIter * iter_new_num               (gdouble                      value);

CdnExpressionTreeIter * iter_new_sized             (CdnInstruction              *instruction,
                                                    gint                         num);

CdnExpressionTreeIter * iter_new_sized_take        (CdnInstruction              *instruction,
                                                    gint                         num);

GSList *                iter_remove_variables      (CdnExpressionTreeIter       *iter,
                                                    CdnVariable                 *prop,
                                                    GSList                      *ret);

gint                    iter_index_of              (CdnExpressionTreeIter       *parent,
                                                    CdnExpressionTreeIter       *iter);

void                    iter_replace               (CdnExpressionTreeIter       *iter,
                                                    CdnExpressionTreeIter       *other);

void                    iter_set_child             (CdnExpressionTreeIter       *parent,
                                                    CdnExpressionTreeIter       *child,
                                                    gint                         idx);

void                    iter_replace_or_copy_into  (CdnExpressionTreeIter       *cp,
                                                    CdnExpressionTreeIter       *dest,
                                                    gboolean                     make_copy);

void                    iter_replace_into          (CdnExpressionTreeIter       *cp,
                                                    CdnExpressionTreeIter       *dest);

void                    iter_copy_into             (CdnExpressionTreeIter       *cp,
                                                    CdnExpressionTreeIter       *dest);

CdnExpressionTreeIter * iter_brother               (CdnExpressionTreeIter       *iter);

gboolean                iter_is_multiply           (CdnExpressionTreeIter const *iter);

gboolean                iter_is_power              (CdnExpressionTreeIter const *iter);

gboolean                iter_is_plus               (CdnExpressionTreeIter const *iter);

gboolean                iter_is_minus              (CdnExpressionTreeIter const *iter);

gboolean                iter_is_divide             (CdnExpressionTreeIter const *iter);

gboolean                iter_is_unary_minus        (CdnExpressionTreeIter const *iter);

gboolean                iter_is_number             (CdnExpressionTreeIter const *iter,
                                                    gdouble                     *num);

gboolean                iter_is_natural_number     (CdnExpressionTreeIter const *iter,
                                                    gint                        *num);

gboolean                iter_is_function           (CdnExpressionTreeIter const *iter,
                                                    CdnMathFunctionType         *type);

gboolean                iter_is_matrix             (CdnExpressionTreeIter const *iter);

void                    iter_invalidate_cache_up   (CdnExpressionTreeIter       *iter);

void                    iter_invalidate_cache_down (CdnExpressionTreeIter       *iter);

void                    iter_canonical_resort      (CdnExpressionTreeIter       *iter);

gboolean                iter_canonicalize          (CdnExpressionTreeIter       *iter,
                                                    gboolean                     canonicalize_children,
                                                    gboolean                     dodefactor);

CdnExpressionTreeIter   *iter_new_bfunc            (CdnMathFunctionType          type,
                                                    CdnExpressionTreeIter       *a,
                                                    CdnExpressionTreeIter       *b,
                                                    gboolean                     take_a,
                                                    gboolean                     take_b);

CdnExpressionTreeIter   *iter_new_ufunc            (CdnMathFunctionType          type,
                                                    CdnExpressionTreeIter       *a,
                                                    gboolean                     take_a);

G_END_DECLS

#endif /* __CDN_TREE_ALGORITHMS_PRIVATE_H__ */

