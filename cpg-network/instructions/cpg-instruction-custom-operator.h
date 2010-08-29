#ifndef __CPG_INSTRUCTION_CUSTOM_OPERATOR_H__
#define __CPG_INSTRUCTION_CUSTOM_OPERATOR_H__

#include <cpg-network/instructions/cpg-instruction.h>
#include <cpg-network/cpg-operator.h>

G_BEGIN_DECLS

#define CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR		(cpg_instruction_custom_operator_get_type ())
#define CPG_INSTRUCTION_CUSTOM_OPERATOR(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR, CpgInstructionCustomOperator))
#define CPG_INSTRUCTION_CUSTOM_OPERATOR_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR, CpgInstructionCustomOperator const))
#define CPG_INSTRUCTION_CUSTOM_OPERATOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR, CpgInstructionCustomOperatorClass))
#define CPG_IS_INSTRUCTION_CUSTOM_OPERATOR(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR))
#define CPG_IS_INSTRUCTION_CUSTOM_OPERATOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR))
#define CPG_INSTRUCTION_CUSTOM_OPERATOR_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR, CpgInstructionCustomOperatorClass))

typedef struct _CpgInstructionCustomOperator		CpgInstructionCustomOperator;
typedef struct _CpgInstructionCustomOperatorClass	CpgInstructionCustomOperatorClass;
typedef struct _CpgInstructionCustomOperatorPrivate	CpgInstructionCustomOperatorPrivate;

struct _CpgInstructionCustomOperator
{
	/*< private >*/
	CpgInstruction parent;
	CpgInstructionCustomOperatorPrivate *priv;
};

struct _CpgInstructionCustomOperatorClass
{
	/*< private >*/
	CpgInstructionClass parent_class;
};

GType cpg_instruction_custom_operator_get_type (void) G_GNUC_CONST;

CpgInstruction *cpg_instruction_custom_operator_new (CpgOperator  *op,
                                                     GSList const *expressions);

CpgOperator     *cpg_instruction_custom_operator_get_operator (CpgInstructionCustomOperator *op);
CpgOperatorData *cpg_instruction_custom_operator_get_data (CpgInstructionCustomOperator *op);

G_END_DECLS

#endif /* __CPG_INSTRUCTION_CUSTOM_OPERATOR_H__ */
