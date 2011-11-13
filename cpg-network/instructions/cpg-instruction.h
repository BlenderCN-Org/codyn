#ifndef __CPG_INSTRUCTION_H__
#define __CPG_INSTRUCTION_H__

#include <cpg-network/cpg-mini-object.h>
#include <cpg-network/cpg-stack.h>

G_BEGIN_DECLS

#define CPG_TYPE_INSTRUCTION		(cpg_instruction_get_type ())
#define CPG_INSTRUCTION(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION, CpgInstruction))
#define CPG_INSTRUCTION_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION, CpgInstruction const))
#define CPG_INSTRUCTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INSTRUCTION, CpgInstructionClass))
#define CPG_IS_INSTRUCTION(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INSTRUCTION))
#define CPG_IS_INSTRUCTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INSTRUCTION))
#define CPG_INSTRUCTION_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INSTRUCTION, CpgInstructionClass))

typedef struct _CpgInstruction		CpgInstruction;
typedef struct _CpgInstructionClass	CpgInstructionClass;
typedef struct _CpgInstructionPrivate	CpgInstructionPrivate;

/**
 * CpgInstruction:
 * @type: the instruction type
 *
 * The base instruction. All other instructions are derived from this.
 *
 */
struct _CpgInstruction
{
	/*< private >*/
	CpgMiniObject parent;

	CpgInstructionPrivate *priv;
};

struct _CpgInstructionClass
{
	CpgMiniObjectClass parent_class;

	gchar  *(*to_string)	    (CpgInstruction *instruction);
	void    (*execute)          (CpgInstruction *instruction,
	                             CpgStack       *stack);
	gint    (*get_stack_count)  (CpgInstruction *instruction);
	GSList *(*get_dependencies) (CpgInstruction *instruction);

	gboolean (*get_is_commutative) (CpgInstruction *instruction);
	gboolean (*equal)           (CpgInstruction *i1,
	                             CpgInstruction *i2);
};

GType cpg_instruction_get_type (void) G_GNUC_CONST;

gchar  *cpg_instruction_to_string        (CpgInstruction *instruction);

void    cpg_instruction_execute          (CpgInstruction *instruction,
                                          CpgStack       *stack);

gint    cpg_instruction_get_stack_count  (CpgInstruction *instruction);
GSList *cpg_instruction_get_dependencies (CpgInstruction *instruction);

gboolean cpg_instruction_equal           (CpgInstruction *i1,
                                          CpgInstruction *i2);

gboolean cpg_instruction_get_is_commutative (CpgInstruction *instruction);

G_END_DECLS

#endif /* __CPG_INSTRUCTION_H__ */
