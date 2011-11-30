#ifndef __CDN_EXPRESSION_TREE_ITER_H__
#define __CDN_EXPRESSION_TREE_ITER_H__

#include <glib.h>
#include <codyn/cdn-expression.h>
#include <codyn/instructions/cdn-instruction.h>
#include <codyn/cdn-variable.h>

G_BEGIN_DECLS

typedef struct _CdnExpressionTreeIter CdnExpressionTreeIter;

CdnExpressionTreeIter *cdn_expression_tree_iter_new             (CdnExpression         *expression);
CdnExpressionTreeIter *cdn_expression_tree_iter_new_from_instructions (GSList const *instructions);
CdnExpressionTreeIter *cdn_expression_tree_iter_new_from_instruction (CdnInstruction *instruction);

CdnExpressionTreeIter *cdn_expression_tree_iter_copy            (CdnExpressionTreeIter *iter);
void                   cdn_expression_tree_iter_free            (CdnExpressionTreeIter *iter);

CdnInstruction        *cdn_expression_tree_iter_get_instruction (CdnExpressionTreeIter *iter);
void                   cdn_expression_tree_iter_set_instruction (CdnExpressionTreeIter *iter,
                                                                 CdnInstruction        *instr);

CdnExpressionTreeIter *cdn_expression_tree_iter_get_child       (CdnExpressionTreeIter *iter,
                                                                 gint                   nth);

void                   cdn_expression_tree_iter_take_child      (CdnExpressionTreeIter *iter,
                                                                 gint                   nth,
                                                                 CdnExpressionTreeIter *child);

void                   cdn_expression_tree_iter_set_child       (CdnExpressionTreeIter *iter,
                                                                 gint                   nth,
                                                                 CdnExpressionTreeIter *child);

gint                   cdn_expression_tree_iter_num_children    (CdnExpressionTreeIter *iter);

gchar const           *cdn_expression_tree_iter_to_string       (CdnExpressionTreeIter *iter);
gchar const           *cdn_expression_tree_iter_to_string_dbg   (CdnExpressionTreeIter *iter);

GSList                *cdn_expression_tree_iter_to_instructions (CdnExpressionTreeIter *iter);

CdnExpressionTreeIter *cdn_expression_tree_iter_canonicalize    (CdnExpressionTreeIter *iter);

CdnExpressionTreeIter *cdn_expression_tree_iter_simplify        (CdnExpressionTreeIter *iter);

gboolean               cdn_expression_tree_iter_equal           (CdnExpressionTreeIter *iter,
                                                                 CdnExpressionTreeIter *other);

CdnExpressionTreeIter *cdn_expression_tree_iter_solve_for       (CdnExpressionTreeIter  *iter,
                                                                 CdnVariable            *prop,
                                                                 GError                **error);

CdnExpressionTreeIter *cdn_expression_tree_iter_substitute      (CdnExpressionTreeIter *iter,
                                                                 CdnVariable           *property,
                                                                 CdnExpressionTreeIter *subst);

CdnExpressionTreeIter *cdn_expression_tree_iter_substitute_hash (CdnExpressionTreeIter *iter,
                                                                 GHashTable            *table);

G_END_DECLS

#endif /* __CDN_EXPRESSION_TREE_ITER_H__ */
