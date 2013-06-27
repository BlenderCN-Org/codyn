#ifndef __CDN_EVENT_H__
#define __CDN_EVENT_H__

#include <codyn/cdn-node.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-variable.h>
#include <codyn/cdn-edge-action.h>
#include <codyn/cdn-compile-context.h>
#include <codyn/cdn-math.h>

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

typedef struct _CdnEventLogicalNode CdnEventLogicalNode;
typedef struct _CdnEventSetVariable CdnEventSetVariable;

GType            cdn_event_get_type          (void) G_GNUC_CONST;

CdnEvent        *cdn_event_new               (gchar const   *id,
                                              CdnExpression *condition,
                                              gdouble        approximation);

gboolean         cdn_event_happened          (CdnEvent      *event,
                                              gdouble       *dist);

gdouble          cdn_event_last_distance     (CdnEvent      *event);

void             cdn_event_update            (CdnEvent      *event);
CdnExpression *  cdn_event_get_condition     (CdnEvent      *event);
gdouble          cdn_event_get_approximation (CdnEvent      *event);

void             cdn_event_set_condition     (CdnEvent      *event,
                                              CdnExpression *condition);

void             cdn_event_set_approximation (CdnEvent      *event,
                                              gdouble        approximation);

void             cdn_event_add_set_variable  (CdnEvent      *event,
                                              CdnVariable   *variable,
                                              CdnExpression *value);

void             cdn_event_set_goto_state    (CdnEvent      *event,
                                              gchar const   *state);

gchar const     *cdn_event_get_goto_state    (CdnEvent      *event);

void             cdn_event_set_terminal      (CdnEvent      *event,
                                              gboolean       terminal);

gboolean         cdn_event_get_terminal      (CdnEvent      *event);

void             cdn_event_execute           (CdnEvent      *event);

GType            cdn_event_logical_node_get_type (void);

CdnEventLogicalNode const *
                 cdn_event_get_logical_tree  (CdnEvent      *event);

CdnEventLogicalNode const *
                 cdn_event_logical_node_get_left (CdnEventLogicalNode const *node);

CdnEventLogicalNode const *
                 cdn_event_logical_node_get_right (CdnEventLogicalNode const *node);

CdnMathFunctionType
                 cdn_event_logical_node_get_compare_type (CdnEventLogicalNode const *node);

CdnExpression const *
                 cdn_event_logical_node_get_expression (CdnEventLogicalNode const *node);

GSList const    *cdn_event_get_set_variables         (CdnEvent            *event);

GType            cdn_event_set_variable_get_type     (void);
CdnExpression   *cdn_event_set_variable_get_value    (CdnEventSetVariable *variable);
CdnVariable     *cdn_event_set_variable_get_variable (CdnEventSetVariable *variable);

G_END_DECLS

#endif /* __CDN_EVENT_H__ */
