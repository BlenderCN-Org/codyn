#ifndef __CDN_INSTRUCTION_CONSTANT_H__
#define __CDN_INSTRUCTION_CONSTANT_H__

#include <codyn/instructions/cdn-instruction-number.h>

G_BEGIN_DECLS

#define CDN_TYPE_INSTRUCTION_CONSTANT		(cdn_instruction_constant_get_type ())
#define CDN_INSTRUCTION_CONSTANT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_CONSTANT, CdnInstructionConstant))
#define CDN_INSTRUCTION_CONSTANT_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_CONSTANT, CdnInstructionConstant const))
#define CDN_INSTRUCTION_CONSTANT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INSTRUCTION_CONSTANT, CdnInstructionConstantClass))
#define CDN_IS_INSTRUCTION_CONSTANT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INSTRUCTION_CONSTANT))
#define CDN_IS_INSTRUCTION_CONSTANT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INSTRUCTION_CONSTANT))
#define CDN_INSTRUCTION_CONSTANT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INSTRUCTION_CONSTANT, CdnInstructionConstantClass))

typedef struct _CdnInstructionConstant		CdnInstructionConstant;
typedef struct _CdnInstructionConstantClass	CdnInstructionConstantClass;
typedef struct _CdnInstructionConstantPrivate	CdnInstructionConstantPrivate;

/**
 * CdnInstructionConstant:
 * @value: the numeric value
 *
 * The instruction class for %CDN_INSTRUCTION_TYPE_CONSTANT
 *
 */
struct _CdnInstructionConstant
{
	/*< private >*/
	CdnInstructionNumber parent;
	CdnInstructionConstantPrivate *priv;
};

struct _CdnInstructionConstantClass
{
	/*< private >*/
	CdnInstructionNumberClass parent_class;
};

GType cdn_instruction_constant_get_type (void) G_GNUC_CONST;

CdnInstruction *cdn_instruction_constant_new (gchar const *symbol);

gchar const *cdn_instruction_constant_get_symbol (CdnInstructionConstant *instruction);
void cdn_instruction_constant_set_symbol (CdnInstructionConstant *instruction,
                                          gchar const            *symbol);

G_END_DECLS

#endif /* __CDN_INSTRUCTION_CONSTANT_H__ */
