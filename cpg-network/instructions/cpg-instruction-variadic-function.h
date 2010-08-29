#ifndef __CPG_INSTRUCTION_VARIADIC_FUNCTION_H__
#define __CPG_INSTRUCTION_VARIADIC_FUNCTION_H__

#include <cpg-network/instructions/cpg-instruction-function.h>

G_BEGIN_DECLS

#define CPG_TYPE_INSTRUCTION_VARIADIC_FUNCTION			(cpg_instruction_variadic_function_get_type ())
#define CPG_INSTRUCTION_VARIADIC_FUNCTION(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_VARIADIC_FUNCTION, CpgInstructionVariadicFunction))
#define CPG_INSTRUCTION_VARIADIC_FUNCTION_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_VARIADIC_FUNCTION, CpgInstructionVariadicFunction const))
#define CPG_INSTRUCTION_VARIADIC_FUNCTION_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INSTRUCTION_VARIADIC_FUNCTION, CpgInstructionVariadicFunctionClass))
#define CPG_IS_INSTRUCTION_VARIADIC_FUNCTION(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INSTRUCTION_VARIADIC_FUNCTION))
#define CPG_IS_INSTRUCTION_VARIADIC_FUNCTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INSTRUCTION_VARIADIC_FUNCTION))
#define CPG_INSTRUCTION_VARIADIC_FUNCTION_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INSTRUCTION_VARIADIC_FUNCTION, CpgInstructionVariadicFunctionClass))

typedef struct _CpgInstructionVariadicFunction		CpgInstructionVariadicFunction;
typedef struct _CpgInstructionVariadicFunctionClass	CpgInstructionVariadicFunctionClass;
typedef struct _CpgInstructionVariadicFunctionPrivate	CpgInstructionVariadicFunctionPrivate;

struct _CpgInstructionVariadicFunction
{
	/*< private >*/
	CpgInstructionFunction parent;
	CpgInstructionVariadicFunctionPrivate *priv;
};

struct _CpgInstructionVariadicFunctionClass
{
	/*< private >*/
	CpgInstructionFunctionClass parent_class;
};

GType cpg_instruction_variadic_function_get_type (void) G_GNUC_CONST;

CpgInstruction *cpg_instruction_variadic_function_new (guint        id,
                                                       gchar const *name,
                                                       gint         arguments,
                                                       gboolean     variable);

void cpg_instruction_variadic_function_reset_cache (CpgInstructionVariadicFunction *self);

G_END_DECLS

#endif /* __CPG_INSTRUCTION_VARIADIC_FUNCTION_H__ */
