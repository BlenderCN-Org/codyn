#ifndef __CDN_INSTRUCTION_NUMBER_H__
#define __CDN_INSTRUCTION_NUMBER_H__

#include <codyn/instructions/cdn-instruction.h>

G_BEGIN_DECLS

#define CDN_TYPE_INSTRUCTION_NUMBER		(cdn_instruction_number_get_type ())
#define CDN_INSTRUCTION_NUMBER(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_NUMBER, CdnInstructionNumber))
#define CDN_INSTRUCTION_NUMBER_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_NUMBER, CdnInstructionNumber const))
#define CDN_INSTRUCTION_NUMBER_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INSTRUCTION_NUMBER, CdnInstructionNumberClass))
#define CDN_IS_INSTRUCTION_NUMBER(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INSTRUCTION_NUMBER))
#define CDN_IS_INSTRUCTION_NUMBER_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INSTRUCTION_NUMBER))
#define CDN_INSTRUCTION_NUMBER_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INSTRUCTION_NUMBER, CdnInstructionNumberClass))

typedef struct _CdnInstructionNumber		CdnInstructionNumber;
typedef struct _CdnInstructionNumberClass	CdnInstructionNumberClass;
typedef struct _CdnInstructionNumberPrivate	CdnInstructionNumberPrivate;

struct _CdnInstructionNumber
{
	/*< private >*/
	CdnInstruction parent;
	CdnInstructionNumberPrivate *priv;
};

struct _CdnInstructionNumberClass
{
	/*< private >*/
	CdnInstructionClass parent_class;
};

GType cdn_instruction_number_get_type (void) G_GNUC_CONST;

CdnInstruction *cdn_instruction_number_new (gdouble value);
CdnInstruction *cdn_instruction_number_new_from_string (gchar const *repr);

gchar *cdn_instruction_number_get_representation (CdnInstructionNumber *number);
void cdn_instruction_number_set_representation (CdnInstructionNumber *number,
                                                gchar const          *repr);

gdouble cdn_instruction_number_get_value (CdnInstructionNumber *number);
void    cdn_instruction_number_set_value (CdnInstructionNumber *number,
                                          gdouble               value);

G_END_DECLS

#endif /* __CDN_INSTRUCTION_NUMBER_H__ */

