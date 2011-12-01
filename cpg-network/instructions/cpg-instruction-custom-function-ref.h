#ifndef __CPG_INSTRUCTION_CUSTOM_FUNCTION_REF_H__
#define __CPG_INSTRUCTION_CUSTOM_FUNCTION_REF_H__

#include <cpg-network/instructions/cpg-instruction.h>
#include <cpg-network/cpg-function.h>

G_BEGIN_DECLS

#define CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF		(cpg_instruction_custom_function_ref_get_type ())
#define CPG_INSTRUCTION_CUSTOM_FUNCTION_REF(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF, CpgInstructionCustomFunctionRef))
#define CPG_INSTRUCTION_CUSTOM_FUNCTION_REF_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF, CpgInstructionCustomFunctionRef const))
#define CPG_INSTRUCTION_CUSTOM_FUNCTION_REF_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF, CpgInstructionCustomFunctionRefClass))
#define CPG_IS_INSTRUCTION_CUSTOM_FUNCTION_REF(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF))
#define CPG_IS_INSTRUCTION_CUSTOM_FUNCTION_REF_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF))
#define CPG_INSTRUCTION_CUSTOM_FUNCTION_REF_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF, CpgInstructionCustomFunctionRefClass))

typedef struct _CpgInstructionCustomFunctionRef		CpgInstructionCustomFunctionRef;
typedef struct _CpgInstructionCustomFunctionRefClass	CpgInstructionCustomFunctionRefClass;
typedef struct _CpgInstructionCustomFunctionRefPrivate	CpgInstructionCustomFunctionRefPrivate;

/**
 * CpgInstructionCustomFunctionRef:
 * @function: the custom function
 * @arguments: the number of arguments the function receives
 *
 * The instruction class for %CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION_REF
 *
 */
struct _CpgInstructionCustomFunctionRef
{
	/*< private >*/
	CpgInstruction parent;
	CpgInstructionCustomFunctionRefPrivate *priv;

};

struct _CpgInstructionCustomFunctionRefClass
{
	/*< private >*/
	CpgInstructionClass parent_class;
};

GType           cpg_instruction_custom_function_ref_get_type     (void) G_GNUC_CONST;

CpgInstruction *cpg_instruction_custom_function_ref_new          (CpgFunction                     *function);

void            cpg_instruction_custom_function_ref_set_function (CpgInstructionCustomFunctionRef *function,
                                                                  CpgFunction                     *func);
CpgFunction    *cpg_instruction_custom_function_ref_get_function (CpgInstructionCustomFunctionRef *function);

G_END_DECLS

#endif /* __CPG_INSTRUCTION_CUSTOM_FUNCTION_REF_H__ */
