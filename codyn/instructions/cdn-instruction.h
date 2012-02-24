#ifndef __CDN_INSTRUCTION_H__
#define __CDN_INSTRUCTION_H__

#include <codyn/cdn-mini-object.h>
#include <codyn/cdn-stack.h>

G_BEGIN_DECLS

#define CDN_TYPE_INSTRUCTION		(cdn_instruction_get_type ())
#define CDN_INSTRUCTION(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION, CdnInstruction))
#define CDN_INSTRUCTION_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION, CdnInstruction const))
#define CDN_INSTRUCTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INSTRUCTION, CdnInstructionClass))
#define CDN_IS_INSTRUCTION(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INSTRUCTION))
#define CDN_IS_INSTRUCTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INSTRUCTION))
#define CDN_INSTRUCTION_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INSTRUCTION, CdnInstructionClass))

typedef struct _CdnInstruction		CdnInstruction;
typedef struct _CdnInstructionClass	CdnInstructionClass;
typedef struct _CdnInstructionPrivate	CdnInstructionPrivate;

/**
 * CdnInstruction:
 * @type: the instruction type
 *
 * The base instruction. All other instructions are derived from this.
 *
 */
struct _CdnInstruction
{
	/*< private >*/
	CdnMiniObject parent;

	CdnInstructionPrivate *priv;
};

struct _CdnInstructionClass
{
	CdnMiniObjectClass parent_class;

	gchar  *(*to_string)	    (CdnInstruction *instruction);
	void    (*execute)          (CdnInstruction *instruction,
	                             CdnStack       *stack);
	CdnStackManipulation const *(*get_stack_manipulation) (CdnInstruction *instruction, GError **error);
	GSList *(*get_dependencies) (CdnInstruction *instruction);

	gboolean (*get_is_commutative) (CdnInstruction *instruction);
	gboolean (*equal)           (CdnInstruction *i1,
	                             CdnInstruction *i2,
	                             gboolean        asstring);
};

GType cdn_instruction_get_type (void) G_GNUC_CONST;

gchar  *cdn_instruction_to_string        (CdnInstruction *instruction);

void    cdn_instruction_execute          (CdnInstruction *instruction,
                                          CdnStack       *stack);

CdnStackManipulation const *cdn_instruction_get_stack_manipulation  (CdnInstruction *instruction, GError **error);
GSList *cdn_instruction_get_dependencies (CdnInstruction *instruction);

gboolean cdn_instruction_equal           (CdnInstruction *i1,
                                          CdnInstruction *i2,
                                          gboolean        asstring);

gboolean cdn_instruction_get_is_commutative (CdnInstruction *instruction);

G_END_DECLS

#endif /* __CDN_INSTRUCTION_H__ */
