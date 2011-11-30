#ifndef __CDN_INSTRUCTION_CUSTOM_OPERATOR_H__
#define __CDN_INSTRUCTION_CUSTOM_OPERATOR_H__

#include <codyn/instructions/cdn-instruction.h>
#include <codyn/operators/cdn-operator.h>

G_BEGIN_DECLS

#define CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR		(cdn_instruction_custom_operator_get_type ())
#define CDN_INSTRUCTION_CUSTOM_OPERATOR(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR, CdnInstructionCustomOperator))
#define CDN_INSTRUCTION_CUSTOM_OPERATOR_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR, CdnInstructionCustomOperator const))
#define CDN_INSTRUCTION_CUSTOM_OPERATOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR, CdnInstructionCustomOperatorClass))
#define CDN_IS_INSTRUCTION_CUSTOM_OPERATOR(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR))
#define CDN_IS_INSTRUCTION_CUSTOM_OPERATOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR))
#define CDN_INSTRUCTION_CUSTOM_OPERATOR_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR, CdnInstructionCustomOperatorClass))

typedef struct _CdnInstructionCustomOperator		CdnInstructionCustomOperator;
typedef struct _CdnInstructionCustomOperatorClass	CdnInstructionCustomOperatorClass;
typedef struct _CdnInstructionCustomOperatorPrivate	CdnInstructionCustomOperatorPrivate;

struct _CdnInstructionCustomOperator
{
	/*< private >*/
	CdnInstruction parent;
	CdnInstructionCustomOperatorPrivate *priv;
};

struct _CdnInstructionCustomOperatorClass
{
	/*< private >*/
	CdnInstructionClass parent_class;
};

GType cdn_instruction_custom_operator_get_type (void) G_GNUC_CONST;

CdnInstruction *cdn_instruction_custom_operator_new (CdnOperator *op);

CdnOperator     *cdn_instruction_custom_operator_get_operator (CdnInstructionCustomOperator *op);

G_END_DECLS

#endif /* __CDN_INSTRUCTION_CUSTOM_OPERATOR_H__ */
