#ifndef __CDN_INSTRUCTION_RAND_H__
#define __CDN_INSTRUCTION_RAND_H__

#include <codyn/instructions/cdn-instruction-function.h>

G_BEGIN_DECLS

#define CDN_TYPE_INSTRUCTION_RAND			(cdn_instruction_rand_get_type ())
#define CDN_INSTRUCTION_RAND(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_RAND, CdnInstructionRand))
#define CDN_INSTRUCTION_RAND_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INSTRUCTION_RAND, CdnInstructionRand const))
#define CDN_INSTRUCTION_RAND_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INSTRUCTION_RAND, CdnInstructionRandClass))
#define CDN_IS_INSTRUCTION_RAND(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INSTRUCTION_RAND))
#define CDN_IS_INSTRUCTION_RAND_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INSTRUCTION_RAND))
#define CDN_INSTRUCTION_RAND_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INSTRUCTION_RAND, CdnInstructionRandClass))

typedef struct _CdnInstructionRand		CdnInstructionRand;
typedef struct _CdnInstructionRandClass	CdnInstructionRandClass;
typedef struct _CdnInstructionRandPrivate	CdnInstructionRandPrivate;

struct _CdnInstructionRand
{
	/*< private >*/
	CdnInstruction parent;
	CdnInstructionRandPrivate *priv;
};

struct _CdnInstructionRandClass
{
	/*< private >*/
	CdnInstructionClass parent_class;
};

GType           cdn_instruction_rand_get_type (void) G_GNUC_CONST;

CdnInstruction *cdn_instruction_rand_new      (CdnStackArgs const *argdim);

void            cdn_instruction_rand_next     (CdnInstructionRand *self);

#ifndef MINGW
void            cdn_instruction_rand_set_use_streams (gboolean use);

void            cdn_instruction_rand_set_seed (CdnInstructionRand *self,
                                               guint               seed);

guint           cdn_instruction_rand_get_seed (CdnInstructionRand *self);
#endif

G_END_DECLS

#endif /* __CDN_INSTRUCTION_RAND_H__ */
