#ifndef __CPG_TREE_ALGORITHMS_PRIVATE_H__
#define __CPG_TREE_ALGORITHMS_PRIVATE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#include <cpg-network/cpg-expression-tree-iter.h>
#include <cpg-network/cpg-math.h>
#include <math.h>
#include "cpg-network/instructions/cpg-instructions.h"

struct _CpgExpressionTreeIter
{
	CpgExpressionTreeIter *parent;
	CpgInstruction *instruction;

	CpgExpressionTreeIter **children;
	gint num_children;

	gchar *cached_to_string;
	guint cache_is_dbg : 1;
};

CpgExpressionTreeIter * iter_copy                 (CpgExpressionTreeIter *iter);
CpgExpressionTreeIter * iter_new                  (CpgInstruction        *instruction);
CpgExpressionTreeIter * iter_new_numstr           (gchar const           *num);

CpgExpressionTreeIter * iter_new_sized            (CpgInstruction        *instruction,
                                                   gint                   num);

GSList *                iter_find_properties      (CpgExpressionTreeIter *iter,
                                                   CpgProperty           *prop,
                                                   GSList                *ret);

gint                    iter_index_of             (CpgExpressionTreeIter *parent,
                                                   CpgExpressionTreeIter *iter);

void                    iter_replace              (CpgExpressionTreeIter *iter,
                                                   CpgExpressionTreeIter *other);

void                    iter_set_child            (CpgExpressionTreeIter *parent,
                                                   CpgExpressionTreeIter *child,
                                                   gint                   idx);

void                    iter_replace_or_copy_into (CpgExpressionTreeIter *cp,
                                                   CpgExpressionTreeIter *dest,
                                                   gboolean               make_copy);

void                    iter_replace_into         (CpgExpressionTreeIter *cp,
                                                   CpgExpressionTreeIter *dest);

void                    iter_copy_into            (CpgExpressionTreeIter *cp,
                                                   CpgExpressionTreeIter *dest);

CpgExpressionTreeIter * iter_brother              (CpgExpressionTreeIter *iter);

gboolean                iter_is_operator          (CpgExpressionTreeIter const *iter,
                                                   CpgMathOperatorType   *type);

gboolean                iter_is_multiply          (CpgExpressionTreeIter const *iter);

gboolean                iter_is_power             (CpgExpressionTreeIter const *iter);

gboolean                iter_is_plus              (CpgExpressionTreeIter const *iter);

gboolean                iter_is_minus             (CpgExpressionTreeIter const *iter);

gboolean                iter_is_divide            (CpgExpressionTreeIter const *iter);

gboolean                iter_is_unary_minus       (CpgExpressionTreeIter const *iter);

gboolean                iter_is_number            (CpgExpressionTreeIter const *iter,
                                                   gdouble               *num);

gboolean                iter_is_function          (CpgExpressionTreeIter const *iter,
                                                   CpgMathFunctionType   *type);

void                    iter_invalidate_cache_up  (CpgExpressionTreeIter *iter);

void                    iter_invalidate_cache_down (CpgExpressionTreeIter *iter);

void                    iter_canonical_resort      (CpgExpressionTreeIter *iter);

gboolean                iter_canonicalize          (CpgExpressionTreeIter *iter,
                                                    gboolean                 canonicalize_children,
                                                    gboolean               dodefactor);

G_END_DECLS

#endif /* __CPG_TREE_ALGORITHMS_PRIVATE_H__ */

