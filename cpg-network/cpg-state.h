#ifndef __CPG_STATE_H__
#define __CPG_STATE_H__

#include "cpg-object.h"

G_BEGIN_DECLS

#define CPG_TYPE_STATE				(cpg_state_get_type ())
#define CPG_STATE(obj)				(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_STATE, CpgState))
#define CPG_STATE_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_STATE, CpgState const))
#define CPG_STATE_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_STATE, CpgStateClass))
#define CPG_IS_STATE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_STATE))
#define CPG_IS_STATE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_STATE))
#define CPG_STATE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_STATE, CpgStateClass))

typedef struct _CpgState		CpgState;
typedef struct _CpgStateClass	CpgStateClass;
typedef struct _CpgStatePrivate	CpgStatePrivate;

struct _CpgState {
	/*< private >*/
	CpgObject parent;
	
	CpgStatePrivate *priv;
};

struct _CpgStateClass {
	/*< private >*/
	CpgObjectClass parent_class;
};

GType cpg_state_get_type(void) G_GNUC_CONST;
CpgState *cpg_state_new(gchar const *id);

G_END_DECLS

#endif /* __CPG_STATE_H__ */
