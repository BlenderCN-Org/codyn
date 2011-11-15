#ifndef __CPG_EXPRESSION_TREE_ITER_H__
#define __CPG_EXPRESSION_TREE_ITER_H__

#include <glib.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/instructions/cpg-instruction.h>
#include <cpg-network/cpg-property.h>

G_BEGIN_DECLS

typedef struct _CpgExpressionTreeIter CpgExpressionTreeIter;

CpgExpressionTreeIter *cpg_expression_tree_iter_new             (CpgExpression         *expression);
CpgExpressionTreeIter *cpg_expression_tree_iter_new_from_instructions (GSList const *instructions);

CpgExpressionTreeIter *cpg_expression_tree_iter_copy            (CpgExpressionTreeIter *iter);
void                   cpg_expression_tree_iter_free            (CpgExpressionTreeIter *iter);

CpgInstruction        *cpg_expression_tree_iter_get_instruction (CpgExpressionTreeIter *iter);
void                   cpg_expression_tree_iter_set_instruction (CpgExpressionTreeIter *iter,
                                                                 CpgInstruction        *instr);

CpgExpressionTreeIter *cpg_expression_tree_iter_get_child       (CpgExpressionTreeIter *iter,
                                                                 gint                   nth);

void                   cpg_expression_tree_iter_take_child      (CpgExpressionTreeIter *iter,
                                                                 gint                   nth,
                                                                 CpgExpressionTreeIter *child);

void                   cpg_expression_tree_iter_set_child       (CpgExpressionTreeIter *iter,
                                                                 gint                   nth,
                                                                 CpgExpressionTreeIter *child);

gint                   cpg_expression_tree_iter_num_children    (CpgExpressionTreeIter *iter);

gchar                 *cpg_expression_tree_iter_to_string       (CpgExpressionTreeIter *iter);
gchar                 *cpg_expression_tree_iter_to_string_dbg   (CpgExpressionTreeIter *iter);

GSList                *cpg_expression_tree_iter_to_instructions (CpgExpressionTreeIter *iter);

void                   cpg_expression_tree_iter_canonicalize    (CpgExpressionTreeIter *iter);

CpgExpressionTreeIter *cpg_expression_tree_iter_simplify        (CpgExpressionTreeIter *iter) G_GNUC_WARN_UNUSED_RESULT;

gboolean               cpg_expression_tree_iter_equal           (CpgExpressionTreeIter *iter,
                                                                 CpgExpressionTreeIter *other);

CpgExpressionTreeIter *cpg_expression_tree_iter_solve_for       (CpgExpressionTreeIter *iter,
                                                                 CpgProperty           *prop);

G_END_DECLS

#endif /* __CPG_EXPRESSION_TREE_ITER_H__ */
