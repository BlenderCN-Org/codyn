#ifndef __CDN_INSTRUCTION_CUSTOM_FUNCTION_REF_H__
#define __CDN_INSTRUCTION_CUSTOM_FUNCTION_REF_H__

#include <codyn/instructions/cdn-instruction.h>
#include <codyn/cdn-function.h>

G_BEGIN_DECLS

#define CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF		(cdn_instruction_custom_function_ref_get_type ())
#define CDN_INSTRUCTION_CUSTOM_FUNCTION_REF(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF, CdnInstructionCustomFunctionRef))
#define CDN_INSTRUCTION_CUSTOM_FUNCTION_REF_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF, CdnInstructionCustomFunctionRef const))
#define CDN_INSTRUCTION_CUSTOM_FUNCTION_REF_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF, CdnInstructionCustomFunctionRefClass))
#define CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_REF(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF))
#define CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_REF_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF))
#define CDN_INSTRUCTION_CUSTOM_FUNCTION_REF_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF, CdnInstructionCustomFunctionRefClass))

typedef struct _CdnInstructionCustomFunctionRef		CdnInstructionCustomFunctionRef;
typedef struct _CdnInstructionCustomFunctionRefClass	CdnInstructionCustomFunctionRefClass;
typedef struct _CdnInstructionCustomFunctionRefPrivate	CdnInstructionCustomFunctionRefPrivate;

/**
 * CdnInstructionCustomFunctionRef:
 * @function: the custom function
 * @arguments: the number of arguments the function receives
 *
 * The instruction class for %CDN_INSTRUCTION_TYPE_CUSTOM_FUNCTION_REF
 *
 */
struct _CdnInstructionCustomFunctionRef
{
	/*< private >*/
	CdnInstruction parent;
	CdnInstructionCustomFunctionRefPrivate *priv;

};

struct _CdnInstructionCustomFunctionRefClass
{
	/*< private >*/
	CdnInstructionClass parent_class;
};

GType           cdn_instruction_custom_function_ref_get_type     (void) G_GNUC_CONST;

CdnInstruction *cdn_instruction_custom_function_ref_new          (CdnFunction                     *function);

void            cdn_instruction_custom_function_ref_set_function (CdnInstructionCustomFunctionRef *function,
                                                                  CdnFunction                     *func);
CdnFunction    *cdn_instruction_custom_function_ref_get_function (CdnInstructionCustomFunctionRef *function);

G_END_DECLS

#endif /* __CDN_INSTRUCTION_CUSTOM_FUNCTION_REF_H__ */
