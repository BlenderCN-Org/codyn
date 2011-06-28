#ifndef __CPG_INSTRUCTION_CONSTANT_H__
#define __CPG_INSTRUCTION_CONSTANT_H__

#include <cpg-network/instructions/cpg-instruction-number.h>

G_BEGIN_DECLS

#define CPG_TYPE_INSTRUCTION_CONSTANT		(cpg_instruction_constant_get_type ())
#define CPG_INSTRUCTION_CONSTANT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_CONSTANT, CpgInstructionConstant))
#define CPG_INSTRUCTION_CONSTANT_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_CONSTANT, CpgInstructionConstant const))
#define CPG_INSTRUCTION_CONSTANT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INSTRUCTION_CONSTANT, CpgInstructionConstantClass))
#define CPG_IS_INSTRUCTION_CONSTANT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INSTRUCTION_CONSTANT))
#define CPG_IS_INSTRUCTION_CONSTANT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INSTRUCTION_CONSTANT))
#define CPG_INSTRUCTION_CONSTANT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INSTRUCTION_CONSTANT, CpgInstructionConstantClass))

typedef struct _CpgInstructionConstant		CpgInstructionConstant;
typedef struct _CpgInstructionConstantClass	CpgInstructionConstantClass;
typedef struct _CpgInstructionConstantPrivate	CpgInstructionConstantPrivate;

/**
 * CpgInstructionConstant:
 * @value: the numeric value
 *
 * The instruction class for %CPG_INSTRUCTION_TYPE_CONSTANT
 *
 */
struct _CpgInstructionConstant
{
	/*< private >*/
	CpgInstructionNumber parent;
	CpgInstructionConstantPrivate *priv;
};

struct _CpgInstructionConstantClass
{
	/*< private >*/
	CpgInstructionNumberClass parent_class;
};

GType cpg_instruction_constant_get_type (void) G_GNUC_CONST;

CpgInstruction *cpg_instruction_constant_new (gchar const *symbol);

gchar const *cpg_instruction_constant_get_symbol (CpgInstructionConstant *instruction);
void cpg_instruction_constant_set_symbol (CpgInstructionConstant *instruction,
                                          gchar const            *symbol);

G_END_DECLS

#endif /* __CPG_INSTRUCTION_CONSTANT_H__ */
