#ifndef __CPG_INSTRUCTION_CUSTOM_OPERATOR_REF_H__
#define __CPG_INSTRUCTION_CUSTOM_OPERATOR_REF_H__

#include <cpg-network/instructions/cpg-instruction.h>
#include <cpg-network/cpg-operator.h>

G_BEGIN_DECLS

#define CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF		(cpg_instruction_custom_operator_ref_get_type ())
#define CPG_INSTRUCTION_CUSTOM_OPERATOR_REF(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF, CpgInstructionCustomOperatorRef))
#define CPG_INSTRUCTION_CUSTOM_OPERATOR_REF_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF, CpgInstructionCustomOperatorRef const))
#define CPG_INSTRUCTION_CUSTOM_OPERATOR_REF_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF, CpgInstructionCustomOperatorRefClass))
#define CPG_IS_INSTRUCTION_CUSTOM_OPERATOR_REF(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF))
#define CPG_IS_INSTRUCTION_CUSTOM_OPERATOR_REF_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF))
#define CPG_INSTRUCTION_CUSTOM_OPERATOR_REF_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF, CpgInstructionCustomOperatorRefClass))

typedef struct _CpgInstructionCustomOperatorRef		CpgInstructionCustomOperatorRef;
typedef struct _CpgInstructionCustomOperatorRefClass	CpgInstructionCustomOperatorRefClass;
typedef struct _CpgInstructionCustomOperatorRefPrivate	CpgInstructionCustomOperatorRefPrivate;

struct _CpgInstructionCustomOperatorRef
{
	/*< private >*/
	CpgInstruction parent;
	CpgInstructionCustomOperatorRefPrivate *priv;
};

struct _CpgInstructionCustomOperatorRefClass
{
	/*< private >*/
	CpgInstructionClass parent_class;
};

GType cpg_instruction_custom_operator_ref_get_type (void) G_GNUC_CONST;

CpgInstruction *cpg_instruction_custom_operator_ref_new (CpgOperator *op);

CpgOperator     *cpg_instruction_custom_operator_ref_get_operator (CpgInstructionCustomOperatorRef *op);

G_END_DECLS

#endif /* __CPG_INSTRUCTION_CUSTOM_OPERATOR_REF_H__ */
