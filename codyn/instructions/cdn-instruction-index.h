#ifndef __CDN_INSTRUCTION_INDEX_H__
#define __CDN_INSTRUCTION_INDEX_H__

#include <codyn/instructions/cdn-instruction.h>

G_BEGIN_DECLS

#define CDN_TYPE_INSTRUCTION_INDEX		(cdn_instruction_index_get_type ())
#define CDN_INSTRUCTION_INDEX(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_INDEX, CdnInstructionIndex))
#define CDN_INSTRUCTION_INDEX_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_INDEX, CdnInstructionIndex const))
#define CDN_INSTRUCTION_INDEX_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INSTRUCTION_INDEX, CdnInstructionIndexClass))
#define CDN_IS_INSTRUCTION_INDEX(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INSTRUCTION_INDEX))
#define CDN_IS_INSTRUCTION_INDEX_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INSTRUCTION_INDEX))
#define CDN_INSTRUCTION_INDEX_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INSTRUCTION_INDEX, CdnInstructionIndexClass))

typedef struct _CdnInstructionIndex		CdnInstructionIndex;
typedef struct _CdnInstructionIndexClass	CdnInstructionIndexClass;
typedef struct _CdnInstructionIndexPrivate	CdnInstructionIndexPrivate;

/**
 * CdnInstructionIndex:
 * @value: the numeric value
 *
 * The instruction class for %CDN_INSTRUCTION_TYPE_INDEX
 *
 */
struct _CdnInstructionIndex
{
	/*< private >*/
	CdnInstruction parent;
	CdnInstructionIndexPrivate *priv;
};

struct _CdnInstructionIndexClass
{
	/*< private >*/
	CdnInstructionClass parent_class;
};

GType           cdn_instruction_index_get_type   (void) G_GNUC_CONST;

CdnInstruction *cdn_instruction_index_new        (gint               *indices,
                                                  CdnDimension const *retdim,
                                                  CdnStackArg const  *arg);

CdnInstruction *cdn_instruction_index_new_offset (gint                start,
                                                  CdnDimension const *retdim,
                                                  CdnStackArg const  *arg);

gboolean        cdn_instruction_index_is_offset   (CdnInstructionIndex *instr);
gint            cdn_instruction_index_get_offset  (CdnInstructionIndex *instr);
gint const     *cdn_instruction_index_get_indices (CdnInstructionIndex *instr,
                                                   gint                *length);

G_END_DECLS

#endif /* __CDN_INSTRUCTION_INDEX_H__ */

