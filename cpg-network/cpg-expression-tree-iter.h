#ifndef __CPG_EXPRESSION_TREE_ITER_H__
#define __CPG_EXPRESSION_TREE_ITER_H__

#include <glib.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/instructions/cpg-instruction.h>

G_BEGIN_DECLS

typedef struct _CpgExpressionTreeIter CpgExpressionTreeIter;

CpgExpressionTreeIter *cpg_expression_tree_iter_new             (CpgExpression         *expression);
CpgExpressionTreeIter *cpg_expression_tree_iter_new_from_instructions (GSList const *instructions);

CpgExpressionTreeIter *cpg_expression_tree_iter_copy            (CpgExpressionTreeIter *iter);
void                   cpg_expression_tree_iter_free            (CpgExpressionTreeIter *iter);

CpgExpression         *cpg_expression_tree_iter_get_expression  (CpgExpressionTreeIter *iter);
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

GSList                *cpg_expression_tree_iter_to_instructions (CpgExpressionTreeIter *iter);

G_END_DECLS

#endif /* __CPG_EXPRESSION_TREE_ITER_H__ */
