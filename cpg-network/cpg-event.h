#ifndef __CPG_EVENT_H__
#define __CPG_EVENT_H__

#include <glib-object.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-property.h>
#include <cpg-network/cpg-link-action.h>
#include <cpg-network/cpg-compile-context.h>

G_BEGIN_DECLS

#define CPG_TYPE_EVENT			(cpg_event_get_type ())
#define CPG_EVENT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_EVENT, CpgEvent))
#define CPG_EVENT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_EVENT, CpgEvent const))
#define CPG_EVENT_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_EVENT, CpgEventClass))
#define CPG_IS_EVENT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_EVENT))
#define CPG_IS_EVENT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_EVENT))
#define CPG_EVENT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_EVENT, CpgEventClass))

typedef struct _CpgEvent	CpgEvent;
typedef struct _CpgEventClass	CpgEventClass;
typedef struct _CpgEventPrivate	CpgEventPrivate;

typedef enum
{
	CPG_EVENT_DIRECTION_POSITIVE = 1 << 0,
	CPG_EVENT_DIRECTION_NEGATIVE = 2 << 0,
} CpgEventDirection;

struct _CpgEvent
{
	/*< private >*/
	GInitiallyUnowned parent;

	CpgEventPrivate *priv;
};

struct _CpgEventClass
{
	/*< private >*/
	GInitiallyUnownedClass parent_class;
};

GType              cpg_event_get_type                  (void) G_GNUC_CONST;

CpgEvent          *cpg_event_new                       (CpgExpression      *condition,
                                                        CpgEventDirection   direction);

gboolean           cpg_event_happened                  (CpgEvent           *event,
                                                        gdouble            *dist);

void               cpg_event_update                    (CpgEvent           *event);
CpgExpression *    cpg_event_get_condition             (CpgEvent           *event);
CpgExpression *    cpg_event_get_condition_dfdt        (CpgEvent           *event);
CpgEventDirection  cpg_event_get_direction             (CpgEvent           *event);

void               cpg_event_add_set_property          (CpgEvent           *event,
                                                        CpgProperty        *property,
                                                        CpgExpression      *value);

void               cpg_event_add_set_link_action_flags (CpgEvent           *event,
                                                        CpgLinkAction      *action,
                                                        CpgLinkActionFlags  add_flags,
                                                        CpgLinkActionFlags  remove_flags);

void               cpg_event_execute                   (CpgEvent           *event);

gboolean           cpg_event_compile                   (CpgEvent               *event,
                                                        CpgCompileContext      *context,
                                                        CpgCompileErrorForward *error);

G_END_DECLS

#endif /* __CPG_EVENT_H__ */
