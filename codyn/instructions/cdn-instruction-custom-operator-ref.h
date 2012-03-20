#ifndef __CDN_INSTRUCTION_CUSTOM_OPERATOR_REF_H__
#define __CDN_INSTRUCTION_CUSTOM_OPERATOR_REF_H__

#include <codyn/instructions/cdn-instruction.h>
#include <codyn/operators/cdn-operator.h>

G_BEGIN_DECLS

#define CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF		(cdn_instruction_custom_operator_ref_get_type ())
#define CDN_INSTRUCTION_CUSTOM_OPERATOR_REF(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF, CdnInstructionCustomOperatorRef))
#define CDN_INSTRUCTION_CUSTOM_OPERATOR_REF_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF, CdnInstructionCustomOperatorRef const))
#define CDN_INSTRUCTION_CUSTOM_OPERATOR_REF_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF, CdnInstructionCustomOperatorRefClass))
#define CDN_IS_INSTRUCTION_CUSTOM_OPERATOR_REF(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF))
#define CDN_IS_INSTRUCTION_CUSTOM_OPERATOR_REF_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF))
#define CDN_INSTRUCTION_CUSTOM_OPERATOR_REF_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF, CdnInstructionCustomOperatorRefClass))

typedef struct _CdnInstructionCustomOperatorRef		CdnInstructionCustomOperatorRef;
typedef struct _CdnInstructionCustomOperatorRefClass	CdnInstructionCustomOperatorRefClass;
typedef struct _CdnInstructionCustomOperatorRefPrivate	CdnInstructionCustomOperatorRefPrivate;

struct _CdnInstructionCustomOperatorRef
{
	/*< private >*/
	CdnInstruction parent;
	CdnInstructionCustomOperatorRefPrivate *priv;
};

struct _CdnInstructionCustomOperatorRefClass
{
	/*< private >*/
	CdnInstructionClass parent_class;
};

GType cdn_instruction_custom_operator_ref_get_type (void) G_GNUC_CONST;

CdnInstruction *cdn_instruction_custom_operator_ref_new (CdnOperator *op);

CdnOperator     *cdn_instruction_custom_operator_ref_get_operator (CdnInstructionCustomOperatorRef *op);

G_END_DECLS

#endif /* __CDN_INSTRUCTION_CUSTOM_OPERATOR_REF_H__ */
