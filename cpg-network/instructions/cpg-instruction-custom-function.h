#ifndef __CPG_INSTRUCTION_CUSTOM_FUNCTION_H__
#define __CPG_INSTRUCTION_CUSTOM_FUNCTION_H__

#include <cpg-network/instructions/cpg-instruction.h>
#include <cpg-network/cpg-function.h>

G_BEGIN_DECLS

#define CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION		(cpg_instruction_custom_function_get_type ())
#define CPG_INSTRUCTION_CUSTOM_FUNCTION(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION, CpgInstructionCustomFunction))
#define CPG_INSTRUCTION_CUSTOM_FUNCTION_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION, CpgInstructionCustomFunction const))
#define CPG_INSTRUCTION_CUSTOM_FUNCTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION, CpgInstructionCustomFunctionClass))
#define CPG_IS_INSTRUCTION_CUSTOM_FUNCTION(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION))
#define CPG_IS_INSTRUCTION_CUSTOM_FUNCTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION))
#define CPG_INSTRUCTION_CUSTOM_FUNCTION_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION, CpgInstructionCustomFunctionClass))

typedef struct _CpgInstructionCustomFunction		CpgInstructionCustomFunction;
typedef struct _CpgInstructionCustomFunctionClass	CpgInstructionCustomFunctionClass;
typedef struct _CpgInstructionCustomFunctionPrivate	CpgInstructionCustomFunctionPrivate;

/**
 * CpgInstructionCustomFunction:
 * @function: the custom function
 * @arguments: the number of arguments the function receives
 *
 * The instruction class for %CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION
 *
 */
struct _CpgInstructionCustomFunction
{
	/*< private >*/
	CpgInstruction parent;
	CpgInstructionCustomFunctionPrivate *priv;

};

struct _CpgInstructionCustomFunctionClass
{
	/*< private >*/
	CpgInstructionClass parent_class;
};

GType cpg_instruction_custom_function_get_type (void) G_GNUC_CONST;

CpgInstruction *cpg_instruction_custom_function_new (CpgFunction *function,
                                                     gint         arguments);

void         cpg_instruction_custom_function_set_function (CpgInstructionCustomFunction *function,
                                                           CpgFunction                  *func);
CpgFunction *cpg_instruction_custom_function_get_function (CpgInstructionCustomFunction *function);

void         cpg_instruction_custom_function_set_arguments (CpgInstructionCustomFunction *function,
                                                            gint                          arguments);
gint         cpg_instruction_custom_function_get_arguments (CpgInstructionCustomFunction *function);

G_END_DECLS

#endif /* __CPG_INSTRUCTION_CUSTOM_FUNCTION_H__ */
