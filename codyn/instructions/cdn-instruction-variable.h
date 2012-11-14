#ifndef __CDN_INSTRUCTION_VARIABLE_H__
#define __CDN_INSTRUCTION_VARIABLE_H__

#include <codyn/instructions/cdn-instruction.h>
#include <codyn/cdn-variable.h>

G_BEGIN_DECLS

#define CDN_TYPE_INSTRUCTION_VARIABLE			(cdn_instruction_variable_get_type ())
#define CDN_INSTRUCTION_VARIABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_VARIABLE, CdnInstructionVariable))
#define CDN_INSTRUCTION_VARIABLE_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_VARIABLE, CdnInstructionVariable const))
#define CDN_INSTRUCTION_VARIABLE_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INSTRUCTION_VARIABLE, CdnInstructionVariableClass))
#define CDN_IS_INSTRUCTION_VARIABLE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INSTRUCTION_VARIABLE))
#define CDN_IS_INSTRUCTION_VARIABLE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INSTRUCTION_VARIABLE))
#define CDN_INSTRUCTION_VARIABLE_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INSTRUCTION_VARIABLE, CdnInstructionVariableClass))

/**
 * CdnInstructionVariableBinding:
 * @CDN_INSTRUCTION_VARIABLE_BINDING_NONE: none
 * @CDN_INSTRUCTION_VARIABLE_BINDING_INPUT: input
 * @CDN_INSTRUCTION_VARIABLE_BINDING_OUTPUT: output
 *
 * Enum used to indicate how the property in a #CdnInstructionVariable
 * was bound when the instruction was compiled.
 *
 **/
typedef enum
{
	CDN_INSTRUCTION_VARIABLE_BINDING_NONE = 0,
	CDN_INSTRUCTION_VARIABLE_BINDING_INPUT = 1 << 0,
	CDN_INSTRUCTION_VARIABLE_BINDING_OUTPUT = 1 << 1
} CdnInstructionVariableBinding;

typedef struct _CdnInstructionVariable		CdnInstructionVariable;
typedef struct _CdnInstructionVariableClass	CdnInstructionVariableClass;
typedef struct _CdnInstructionVariablePrivate	CdnInstructionVariablePrivate;

struct _CdnInstructionVariable
{
	/*< private >*/
	CdnInstruction parent;
	CdnInstructionVariablePrivate *priv;
};

struct _CdnInstructionVariableClass
{
	/*< private >*/
	CdnInstructionClass parent_class;

	/*< public >*/
};

typedef CDN_FORWARD_DECL(CdnExpression) CdnExpressionForward;

GType           cdn_instruction_variable_get_type         (void) G_GNUC_CONST;

CdnInstruction *cdn_instruction_variable_new              (CdnVariable                   *property);
CdnInstruction *cdn_instruction_variable_new_with_binding (CdnVariable                   *property,
                                                           CdnInstructionVariableBinding  binding);

void            cdn_instruction_variable_set_variable     (CdnInstructionVariable        *instruction,
                                                           CdnVariable                   *property);
CdnVariable    *cdn_instruction_variable_get_variable     (CdnInstructionVariable        *instruction);

void            cdn_instruction_variable_apply_slice      (CdnInstructionVariable        *instruction,
                                                           guint const                   *slice,
                                                           guint                          length,
                                                           CdnDimension const            *dim);

void            cdn_instruction_variable_set_slice        (CdnInstructionVariable        *instruction,
                                                           guint const                   *slice,
                                                           guint                          length,
                                                           CdnDimension const            *dim);

guint const    *cdn_instruction_variable_get_slice        (CdnInstructionVariable        *instruction,
                                                           guint                         *length,
                                                           CdnDimension                  *dim);

gboolean        cdn_instruction_variable_has_slice        (CdnInstructionVariable        *instruction);

void            cdn_instruction_variable_set_binding      (CdnInstructionVariable        *instruction,
                                                           CdnInstructionVariableBinding  binding);

CdnInstructionVariableBinding
                cdn_instruction_variable_get_binding      (CdnInstructionVariable        *instruction);

G_END_DECLS

#endif /* __CDN_INSTRUCTION_VARIABLE_H__ */
