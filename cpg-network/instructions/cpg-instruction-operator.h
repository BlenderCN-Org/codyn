#ifndef __CPG_INSTRUCTION_OPERATOR_H__
#define __CPG_INSTRUCTION_OPERATOR_H__

#include <cpg-network/instructions/cpg-instruction-function.h>

G_BEGIN_DECLS

#define CPG_TYPE_INSTRUCTION_OPERATOR			(cpg_instruction_operator_get_type ())
#define CPG_INSTRUCTION_OPERATOR(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_OPERATOR, CpgInstructionOperator))
#define CPG_INSTRUCTION_OPERATOR_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_OPERATOR, CpgInstructionOperator const))
#define CPG_INSTRUCTION_OPERATOR_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INSTRUCTION_OPERATOR, CpgInstructionOperatorClass))
#define CPG_IS_INSTRUCTION_OPERATOR(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INSTRUCTION_OPERATOR))
#define CPG_IS_INSTRUCTION_OPERATOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INSTRUCTION_OPERATOR))
#define CPG_INSTRUCTION_OPERATOR_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INSTRUCTION_OPERATOR, CpgInstructionOperatorClass))

typedef struct _CpgInstructionOperator		CpgInstructionOperator;
typedef struct _CpgInstructionOperatorClass	CpgInstructionOperatorClass;

struct _CpgInstructionOperator
{
	/*< private >*/
	CpgInstructionFunction parent;

	gpointer stub;
};

struct _CpgInstructionOperatorClass
{
	/*< private >*/
	CpgInstructionFunctionClass parent_class;

	/*< public >*/
};

GType cpg_instruction_operator_get_type (void) G_GNUC_CONST;

CpgInstruction *cpg_instruction_operator_new (guint        id,
                                              const gchar *name,
                                              gint         arguments);

G_END_DECLS

#endif /* __CPG_INSTRUCTION_OPERATOR_H__ */
