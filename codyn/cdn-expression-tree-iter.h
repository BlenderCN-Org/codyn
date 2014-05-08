#ifndef __CDN_EXPRESSION_TREE_ITER_H__
#define __CDN_EXPRESSION_TREE_ITER_H__

#include <glib-object.h>
#include <codyn/cdn-expression.h>
#include <codyn/instructions/cdn-instruction.h>
#include <codyn/cdn-variable.h>
#include <codyn/cdn-stack.h>

G_BEGIN_DECLS

/**
 * CdnExpressionTreeIterDeriveFlags:
 * @CDN_EXPRESSION_TREE_ITER_DERIVE_NONE: none
 * @CDN_EXPRESSION_TREE_ITER_DERIVE_PARTIAL: partial
 * @CDN_EXPRESSION_TREE_ITER_DERIVE_TIME: time
 * @CDN_EXPRESSION_TREE_ITER_DERIVE_SIMPLIFY: simplify
 *
 * Flags for tree iter derivation.
 */
typedef enum
{
	CDN_EXPRESSION_TREE_ITER_DERIVE_NONE = 0,
	CDN_EXPRESSION_TREE_ITER_DERIVE_PARTIAL = 1 << 0,
	CDN_EXPRESSION_TREE_ITER_DERIVE_TIME = 1 << 1,
	CDN_EXPRESSION_TREE_ITER_DERIVE_SIMPLIFY = 1 << 2
} CdnExpressionTreeIterDeriveFlags;

#define CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR (cdn_expression_tree_iter_derive_error_quark ())

/**
 * CdnExpressionTreeIterDeriveError:
 * @CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED: unsupported
 * @CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_INVALID: invalid
 * @CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_FUNCTION: function
 *
 * Tree iter derivation errors.
 */
typedef enum
{
	CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
	CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_INVALID,
	CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_FUNCTION
} CdnExpressionTreeIterDeriveError;


typedef struct _CdnExpressionTreeIter CdnExpressionTreeIter;

GType cdn_expression_tree_iter_get_type ();

CdnExpressionTreeIter *cdn_expression_tree_iter_new             (CdnExpression const *expression);
CdnExpressionTreeIter *cdn_expression_tree_iter_new_from_instructions (GSList const *instructions);
CdnExpressionTreeIter *cdn_expression_tree_iter_new_from_instruction (CdnInstruction *instruction);
CdnExpressionTreeIter *cdn_expression_tree_iter_new_from_instruction_take (CdnInstruction *instruction);

CdnExpressionTreeIter *cdn_expression_tree_iter_copy            (CdnExpressionTreeIter *iter);
void                   cdn_expression_tree_iter_free            (CdnExpressionTreeIter *iter);

void                   cdn_expression_tree_iter_set_num_children (CdnExpressionTreeIter *iter,
                                                                  gint                   num);

void                   cdn_expression_tree_iter_swap_children   (CdnExpressionTreeIter *iter,
                                                                 gint                   first,
                                                                 gint                   second);

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

gint                   cdn_expression_tree_iter_get_num_children    (CdnExpressionTreeIter *iter);

gchar const           *cdn_expression_tree_iter_to_string       (CdnExpressionTreeIter *iter);
gchar const           *cdn_expression_tree_iter_to_string_dbg   (CdnExpressionTreeIter *iter);

GSList                *cdn_expression_tree_iter_to_instructions (CdnExpressionTreeIter *iter);

CdnExpressionTreeIter *cdn_expression_tree_iter_canonicalize    (CdnExpressionTreeIter *iter);

CdnExpressionTreeIter *cdn_expression_tree_iter_simplify        (CdnExpressionTreeIter *iter);

gboolean               cdn_expression_tree_iter_equal           (CdnExpressionTreeIter *iter,
                                                                 CdnExpressionTreeIter *other,
                                                                 gboolean               asstring);

CdnExpressionTreeIter *cdn_expression_tree_iter_solve_for       (CdnExpressionTreeIter  *iter,
                                                                 CdnVariable            *variable,
                                                                 GError                **error);

CdnExpressionTreeIter *cdn_expression_tree_iter_substitute      (CdnExpressionTreeIter *iter,
                                                                 CdnVariable           *variable,
                                                                 CdnExpressionTreeIter *subst);

CdnExpressionTreeIter *cdn_expression_tree_iter_substitute_hash (CdnExpressionTreeIter *iter,
                                                                 GHashTable            *table);

CdnExpression         *cdn_expression_tree_iter_to_expression   (CdnExpressionTreeIter *iter);

GQuark                 cdn_expression_tree_iter_derive_error_quark (void);

CdnExpressionTreeIter *cdn_expression_tree_iter_derive             (CdnExpressionTreeIter             *iter,
                                                                    GSList                            *symbols,
                                                                    GHashTable                        *towards,
                                                                    gint                               order,
                                                                    CdnExpressionTreeIterDeriveFlags   flags,
                                                                    GError                           **error);

void                   cdn_expression_tree_iter_initialize_stack (CdnExpressionTreeIter *iter,
                                                                  CdnStack              *stack);

G_END_DECLS

#endif /* __CDN_EXPRESSION_TREE_ITER_H__ */
