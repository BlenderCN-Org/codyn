#ifndef __CPG_INSTRUCTION_PROPERTY_H__
#define __CPG_INSTRUCTION_PROPERTY_H__

#include <cpg-network/instructions/cpg-instruction.h>
#include <cpg-network/cpg-property.h>

G_BEGIN_DECLS

#define CPG_TYPE_INSTRUCTION_PROPERTY			(cpg_instruction_property_get_type ())
#define CPG_INSTRUCTION_PROPERTY(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_PROPERTY, CpgInstructionProperty))
#define CPG_INSTRUCTION_PROPERTY_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INSTRUCTION_PROPERTY, CpgInstructionProperty const))
#define CPG_INSTRUCTION_PROPERTY_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INSTRUCTION_PROPERTY, CpgInstructionPropertyClass))
#define CPG_IS_INSTRUCTION_PROPERTY(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INSTRUCTION_PROPERTY))
#define CPG_IS_INSTRUCTION_PROPERTY_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INSTRUCTION_PROPERTY))
#define CPG_INSTRUCTION_PROPERTY_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INSTRUCTION_PROPERTY, CpgInstructionPropertyClass))

/**
 * CpgInstructionPropertyBinding:
 * @CPG_INSTRUCTION_PROPERTY_BINDING_NONE: none
 * @CPG_INSTRUCTION_PROPERTY_BINDING_FROM: from
 * @CPG_INSTRUCTION_PROPERTY_BINDING_TO: to
 *
 * Enum used to indicate how the property in a #CpgInstructionProperty
 * was bound when the instruction was compiled.
 *
 **/
typedef enum
{
	CPG_INSTRUCTION_PROPERTY_BINDING_NONE = 0,
	CPG_INSTRUCTION_PROPERTY_BINDING_FROM = 1 << 0,
	CPG_INSTRUCTION_PROPERTY_BINDING_TO = 1 << 1,
	CPG_INSTRUCTION_PROPERTY_BINDING_DIFF = 1 << 2,
} CpgInstructionPropertyBinding;

typedef struct _CpgInstructionProperty		CpgInstructionProperty;
typedef struct _CpgInstructionPropertyClass	CpgInstructionPropertyClass;
typedef struct _CpgInstructionPropertyPrivate	CpgInstructionPropertyPrivate;

struct _CpgInstructionProperty
{
	/*< private >*/
	CpgInstruction parent;
	CpgInstructionPropertyPrivate *priv;
};

struct _CpgInstructionPropertyClass
{
	/*< private >*/
	CpgInstructionClass parent_class;

	/*< public >*/
};

typedef CPG_FORWARD_DECL(CpgExpression) CpgExpressionForward;

GType cpg_instruction_property_get_type (void) G_GNUC_CONST;

CpgInstruction *cpg_instruction_property_new (CpgProperty                   *property,
                                              CpgInstructionPropertyBinding  binding);

void cpg_instruction_property_set_property (CpgInstructionProperty *instruction,
                                            CpgProperty            *property);
CpgProperty *cpg_instruction_property_get_property (CpgInstructionProperty *instruction);

void cpg_instruction_property_set_binding (CpgInstructionProperty *instruction,
                                           CpgInstructionPropertyBinding binding);
CpgInstructionPropertyBinding
	cpg_instruction_property_get_binding (CpgInstructionProperty *instruction);

CpgExpressionForward *cpg_instruction_property_get_diff (CpgInstructionProperty *instruction);

void cpg_instruction_property_set_diff_for (CpgInstructionProperty *instruction,
                                            CpgProperty            *property);

CpgProperty *cpg_instruction_property_get_diff_for (CpgInstructionProperty *instruction);

G_END_DECLS

#endif /* __CPG_INSTRUCTION_PROPERTY_H__ */
