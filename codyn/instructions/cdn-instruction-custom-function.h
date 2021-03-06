#ifndef __CDN_INSTRUCTION_CUSTOM_FUNCTION_H__
#define __CDN_INSTRUCTION_CUSTOM_FUNCTION_H__

#include <codyn/instructions/cdn-instruction.h>
#include <codyn/cdn-function.h>

G_BEGIN_DECLS

#define CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION		(cdn_instruction_custom_function_get_type ())
#define CDN_INSTRUCTION_CUSTOM_FUNCTION(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION, CdnInstructionCustomFunction))
#define CDN_INSTRUCTION_CUSTOM_FUNCTION_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION, CdnInstructionCustomFunction const))
#define CDN_INSTRUCTION_CUSTOM_FUNCTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION, CdnInstructionCustomFunctionClass))
#define CDN_IS_INSTRUCTION_CUSTOM_FUNCTION(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION))
#define CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION))
#define CDN_INSTRUCTION_CUSTOM_FUNCTION_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION, CdnInstructionCustomFunctionClass))

typedef struct _CdnInstructionCustomFunction		CdnInstructionCustomFunction;
typedef struct _CdnInstructionCustomFunctionClass	CdnInstructionCustomFunctionClass;
typedef struct _CdnInstructionCustomFunctionPrivate	CdnInstructionCustomFunctionPrivate;

struct _CdnInstructionCustomFunction
{
	/*< private >*/
	CdnInstruction parent;
	CdnInstructionCustomFunctionPrivate *priv;

};

struct _CdnInstructionCustomFunctionClass
{
	/*< private >*/
	CdnInstructionClass parent_class;
};

GType           cdn_instruction_custom_function_get_type     (void) G_GNUC_CONST;

CdnInstruction *cdn_instruction_custom_function_new          (CdnFunction                  *function,
                                                              CdnStackArgs const           *argdim);

CdnFunction    *cdn_instruction_custom_function_get_function (CdnInstructionCustomFunction *function);
void            cdn_instruction_custom_function_set_function (CdnInstructionCustomFunction *function,
                                                              CdnFunction                  *f);

G_END_DECLS

#endif /* __CDN_INSTRUCTION_CUSTOM_FUNCTION_H__ */
