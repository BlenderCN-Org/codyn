#ifndef __CPG_INSTRUCTION_FUNCTION_H__
#define __CPG_INSTRUCTION_FUNCTION_H__

#include <cpg-network/instructions/cpg-instruction.h>

G_BEGIN_DECLS

#define CPG_TYPE_INSTRUCTION_FUNCTION			(cpg_instruction_function_get_type ())
#define CPG_INSTRUCTION_FUNCTION(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_FUNCTION, CpgInstructionFunction))
#define CPG_INSTRUCTION_FUNCTION_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_FUNCTION, CpgInstructionFunction const))
#define CPG_INSTRUCTION_FUNCTION_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INSTRUCTION_FUNCTION, CpgInstructionFunctionClass))
#define CPG_IS_INSTRUCTION_FUNCTION(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INSTRUCTION_FUNCTION))
#define CPG_IS_INSTRUCTION_FUNCTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INSTRUCTION_FUNCTION))
#define CPG_INSTRUCTION_FUNCTION_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INSTRUCTION_FUNCTION, CpgInstructionFunctionClass))

typedef struct _CpgInstructionFunction		CpgInstructionFunction;
typedef struct _CpgInstructionFunctionClass	CpgInstructionFunctionClass;
typedef struct _CpgInstructionFunctionPrivate	CpgInstructionFunctionPrivate;

/**
 * CpgInstructionFunction:
 * @id: The function/operator id (see #cpg_math_function_lookup)
 * @name: The function/operator name
 * @arguments: The number of arguments the function receives
 * @variable: Whether the function is called with a variable number of
 *            arguments
 *
 * The instruction class for %CPG_INSTRUCTION_TYPE_FUNCTION. Note: this
 * instruction is used both for functions and for operators!
 *
 */
struct _CpgInstructionFunction
{
	/*< private >*/
	CpgInstruction parent;
	CpgInstructionFunctionPrivate *priv;
};

struct _CpgInstructionFunctionClass
{
	/*< private >*/
	CpgInstructionClass parent_class;
};

GType cpg_instruction_function_get_type (void) G_GNUC_CONST;

CpgInstruction *cpg_instruction_function_new (guint        id,
                                              const gchar *name,
                                              gint         arguments,
                                              gboolean     variable);

void         cpg_instruction_function_set_id          (CpgInstructionFunction *func,
                                                       guint                   id);
guint        cpg_instruction_function_get_id          (CpgInstructionFunction *func);

void         cpg_instruction_function_set_name        (CpgInstructionFunction *func,
                                                       gchar const            *name);
gchar const *cpg_instruction_function_get_name        (CpgInstructionFunction *func);

void         cpg_instruction_function_set_arguments   (CpgInstructionFunction *func,
                                                       gint                    arguments);
gint         cpg_instruction_function_get_arguments   (CpgInstructionFunction *func);

void         cpg_instruction_function_set_variable    (CpgInstructionFunction *func,
                                                       gboolean                variable);
gboolean     cpg_instruction_function_get_variable    (CpgInstructionFunction *func);

G_END_DECLS

#endif /* __CPG_INSTRUCTION_FUNCTION_H__ */
