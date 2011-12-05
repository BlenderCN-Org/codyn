#ifndef __CDN_INSTRUCTION_MATRIX_H__
#define __CDN_INSTRUCTION_MATRIX_H__

#include <codyn/instructions/cdn-instruction.h>

G_BEGIN_DECLS

#define CDN_TYPE_INSTRUCTION_MATRIX		(cdn_instruction_matrix_get_type ())
#define CDN_INSTRUCTION_MATRIX(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_MATRIX, CdnInstructionMatrix))
#define CDN_INSTRUCTION_MATRIX_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_MATRIX, CdnInstructionMatrix const))
#define CDN_INSTRUCTION_MATRIX_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INSTRUCTION_MATRIX, CdnInstructionMatrixClass))
#define CDN_IS_INSTRUCTION_MATRIX(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INSTRUCTION_MATRIX))
#define CDN_IS_INSTRUCTION_MATRIX_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INSTRUCTION_MATRIX))
#define CDN_INSTRUCTION_MATRIX_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INSTRUCTION_MATRIX, CdnInstructionMatrixClass))

typedef struct _CdnInstructionMatrix		CdnInstructionMatrix;
typedef struct _CdnInstructionMatrixClass	CdnInstructionMatrixClass;
typedef struct _CdnInstructionMatrixPrivate	CdnInstructionMatrixPrivate;

/**
 * CdnInstructionMatrix:
 * @value: the numeric value
 *
 * The instruction class for %CDN_INSTRUCTION_TYPE_MATRIX
 *
 */
struct _CdnInstructionMatrix
{
	/*< private >*/
	CdnInstruction parent;
	CdnInstructionMatrixPrivate *priv;
};

struct _CdnInstructionMatrixClass
{
	/*< private >*/
	CdnInstructionClass parent_class;
};

GType cdn_instruction_matrix_get_type (void) G_GNUC_CONST;

CdnInstruction *cdn_instruction_matrix_new (gint numr, gint numc);

G_END_DECLS

#endif /* __CDN_INSTRUCTION_MATRIX_H__ */

