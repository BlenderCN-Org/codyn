#ifndef __CPG_INSTRUCTION_RAND_H__
#define __CPG_INSTRUCTION_RAND_H__

#include <cpg-network/instructions/cpg-instruction-function.h>

G_BEGIN_DECLS

#define CPG_TYPE_INSTRUCTION_RAND			(cpg_instruction_rand_get_type ())
#define CPG_INSTRUCTION_RAND(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_RAND, CpgInstructionRand))
#define CPG_INSTRUCTION_RAND_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_RAND, CpgInstructionRand const))
#define CPG_INSTRUCTION_RAND_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INSTRUCTION_RAND, CpgInstructionRandClass))
#define CPG_IS_INSTRUCTION_RAND(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INSTRUCTION_RAND))
#define CPG_IS_INSTRUCTION_RAND_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INSTRUCTION_RAND))
#define CPG_INSTRUCTION_RAND_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INSTRUCTION_RAND, CpgInstructionRandClass))

typedef struct _CpgInstructionRand		CpgInstructionRand;
typedef struct _CpgInstructionRandClass	CpgInstructionRandClass;
typedef struct _CpgInstructionRandPrivate	CpgInstructionRandPrivate;

struct _CpgInstructionRand
{
	/*< private >*/
	CpgInstruction parent;
	CpgInstructionRandPrivate *priv;
};

struct _CpgInstructionRandClass
{
	/*< private >*/
	CpgInstructionClass parent_class;
};

GType cpg_instruction_rand_get_type (void) G_GNUC_CONST;

CpgInstruction *cpg_instruction_rand_new (gint arguments);

void cpg_instruction_rand_next (CpgInstructionRand *self);

G_END_DECLS

#endif /* __CPG_INSTRUCTION_RAND_H__ */
