#ifndef __CDN_INSTRUCTION_FUNCTION_H__
#define __CDN_INSTRUCTION_FUNCTION_H__

#include <codyn/instructions/cdn-instruction.h>

G_BEGIN_DECLS

#define CDN_TYPE_INSTRUCTION_FUNCTION			(cdn_instruction_function_get_type ())
#define CDN_INSTRUCTION_FUNCTION(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_FUNCTION, CdnInstructionFunction))
#define CDN_INSTRUCTION_FUNCTION_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_FUNCTION, CdnInstructionFunction const))
#define CDN_INSTRUCTION_FUNCTION_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INSTRUCTION_FUNCTION, CdnInstructionFunctionClass))
#define CDN_IS_INSTRUCTION_FUNCTION(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INSTRUCTION_FUNCTION))
#define CDN_IS_INSTRUCTION_FUNCTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INSTRUCTION_FUNCTION))
#define CDN_INSTRUCTION_FUNCTION_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INSTRUCTION_FUNCTION, CdnInstructionFunctionClass))

typedef struct _CdnInstructionFunction		CdnInstructionFunction;
typedef struct _CdnInstructionFunctionClass	CdnInstructionFunctionClass;
typedef struct _CdnInstructionFunctionPrivate	CdnInstructionFunctionPrivate;

struct _CdnInstructionFunction
{
	/*< private >*/
	CdnInstruction parent;
	CdnInstructionFunctionPrivate *priv;
};

struct _CdnInstructionFunctionClass
{
	/*< private >*/
	CdnInstructionClass parent_class;
};

GType cdn_instruction_function_get_type (void) G_GNUC_CONST;

CdnInstruction *cdn_instruction_function_new (guint               id,
                                              const gchar        *name,
                                              CdnStackArgs const *args);

guint        cdn_instruction_function_get_id          (CdnInstructionFunction *func);

void         cdn_instruction_function_set_name        (CdnInstructionFunction *func,
                                                       gchar const            *name);

gchar const *cdn_instruction_function_get_name        (CdnInstructionFunction *func);

G_END_DECLS

#endif /* __CDN_INSTRUCTION_FUNCTION_H__ */
