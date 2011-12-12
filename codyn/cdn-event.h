#ifndef __CDN_EVENT_H__
#define __CDN_EVENT_H__

#include <codyn/cdn-node.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-variable.h>
#include <codyn/cdn-edge-action.h>
#include <codyn/cdn-compile-context.h>

G_BEGIN_DECLS

#define CDN_TYPE_EVENT			(cdn_event_get_type ())
#define CDN_EVENT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EVENT, CdnEvent))
#define CDN_EVENT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EVENT, CdnEvent const))
#define CDN_EVENT_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_EVENT, CdnEventClass))
#define CDN_IS_EVENT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_EVENT))
#define CDN_IS_EVENT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_EVENT))
#define CDN_EVENT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_EVENT, CdnEventClass))

typedef struct _CdnEvent	CdnEvent;
typedef struct _CdnEventClass	CdnEventClass;
typedef struct _CdnEventPrivate	CdnEventPrivate;

typedef enum
{
	CDN_EVENT_DIRECTION_POSITIVE = 1 << 0,
	CDN_EVENT_DIRECTION_NEGATIVE = 1 << 1,
	CDN_EVENT_DIRECTION_ZERO = 1 << 2,
} CdnEventDirection;

struct _CdnEvent
{
	/*< private >*/
	CdnNode parent;

	CdnEventPrivate *priv;
};

struct _CdnEventClass
{
	/*< private >*/
	CdnNodeClass parent_class;
};

GType              cdn_event_get_type                  (void) G_GNUC_CONST;

CdnEvent          *cdn_event_new                       (gchar const        *id,
                                                        CdnExpression      *condition,
                                                        CdnEventDirection   direction);

gboolean           cdn_event_happened                  (CdnEvent           *event,
                                                        gdouble            *dist);

void               cdn_event_update                    (CdnEvent           *event);
CdnExpression *    cdn_event_get_condition             (CdnEvent           *event);
CdnEventDirection  cdn_event_get_direction             (CdnEvent           *event);

void               cdn_event_add_set_variable          (CdnEvent           *event,
                                                        CdnVariable        *property,
                                                        CdnExpression      *value);

void               cdn_event_set_goto_phase             (CdnEvent           *event,
                                                         gchar const        *phase);

gchar const       *cdn_event_get_goto_phase             (CdnEvent           *event);

void               cdn_event_execute                   (CdnEvent           *event);

G_END_DECLS

#endif /* __CDN_EVENT_H__ */
