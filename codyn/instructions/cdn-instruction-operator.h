#ifndef __CDN_INSTRUCTION_OPERATOR_H__
#define __CDN_INSTRUCTION_OPERATOR_H__

#include <codyn/instructions/cdn-instruction-function.h>

G_BEGIN_DECLS

#define CDN_TYPE_INSTRUCTION_OPERATOR			(cdn_instruction_operator_get_type ())
#define CDN_INSTRUCTION_OPERATOR(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_OPERATOR, CdnInstructionOperator))
#define CDN_INSTRUCTION_OPERATOR_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_OPERATOR, CdnInstructionOperator const))
#define CDN_INSTRUCTION_OPERATOR_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INSTRUCTION_OPERATOR, CdnInstructionOperatorClass))
#define CDN_IS_INSTRUCTION_OPERATOR(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INSTRUCTION_OPERATOR))
#define CDN_IS_INSTRUCTION_OPERATOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INSTRUCTION_OPERATOR))
#define CDN_INSTRUCTION_OPERATOR_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INSTRUCTION_OPERATOR, CdnInstructionOperatorClass))

typedef struct _CdnInstructionOperator		CdnInstructionOperator;
typedef struct _CdnInstructionOperatorClass	CdnInstructionOperatorClass;

struct _CdnInstructionOperator
{
	/*< private >*/
	CdnInstructionFunction parent;

	gpointer stub;
};

struct _CdnInstructionOperatorClass
{
	/*< private >*/
	CdnInstructionFunctionClass parent_class;

	/*< public >*/
};

GType cdn_instruction_operator_get_type (void) G_GNUC_CONST;

CdnInstruction *cdn_instruction_operator_new (guint        id,
                                              const gchar *name,
                                              gint         arguments);

G_END_DECLS

#endif /* __CDN_INSTRUCTION_OPERATOR_H__ */
