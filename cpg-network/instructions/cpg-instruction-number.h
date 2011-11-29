#ifndef __CPG_INSTRUCTION_NUMBER_H__
#define __CPG_INSTRUCTION_NUMBER_H__

#include <cpg-network/instructions/cpg-instruction.h>

G_BEGIN_DECLS

#define CPG_TYPE_INSTRUCTION_NUMBER		(cpg_instruction_number_get_type ())
#define CPG_INSTRUCTION_NUMBER(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_NUMBER, CpgInstructionNumber))
#define CPG_INSTRUCTION_NUMBER_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_NUMBER, CpgInstructionNumber const))
#define CPG_INSTRUCTION_NUMBER_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INSTRUCTION_NUMBER, CpgInstructionNumberClass))
#define CPG_IS_INSTRUCTION_NUMBER(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INSTRUCTION_NUMBER))
#define CPG_IS_INSTRUCTION_NUMBER_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INSTRUCTION_NUMBER))
#define CPG_INSTRUCTION_NUMBER_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INSTRUCTION_NUMBER, CpgInstructionNumberClass))

typedef struct _CpgInstructionNumber		CpgInstructionNumber;
typedef struct _CpgInstructionNumberClass	CpgInstructionNumberClass;
typedef struct _CpgInstructionNumberPrivate	CpgInstructionNumberPrivate;

/**
 * CpgInstructionNumber:
 * @value: the numeric value
 *
 * The instruction class for %CPG_INSTRUCTION_TYPE_NUMBER
 *
 */
struct _CpgInstructionNumber
{
	/*< private >*/
	CpgInstruction parent;
	CpgInstructionNumberPrivate *priv;
};

struct _CpgInstructionNumberClass
{
	/*< private >*/
	CpgInstructionClass parent_class;
};

GType cpg_instruction_number_get_type (void) G_GNUC_CONST;

CpgInstruction *cpg_instruction_number_new (gdouble value);
CpgInstruction *cpg_instruction_number_new_from_string (gchar const *repr);

gchar *cpg_instruction_number_get_representation (CpgInstructionNumber *number);

gdouble cpg_instruction_number_get_value (CpgInstructionNumber *number);
void    cpg_instruction_number_set_value (CpgInstructionNumber *number,
                                          gdouble               value);

G_END_DECLS

#endif /* __CPG_INSTRUCTION_NUMBER_H__ */

