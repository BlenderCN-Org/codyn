#include <string.h>

#include "cpg-object.h"
#include "cpg-debug.h"
#include "cpg-relay.h"
#include "cpg-link.h"

/**
 * SECTION:relay
 * @short_description: An immediate transfer object
 *
 * When using a #CpgState and a #CpgLink to transfer information, each transfer
 * will take exactly one time step. This can be inconvenient when data should
 * be transfered immediately within one timestep, over more than one state.
 * To address this problem, a #CpgRelay can be used instead. All links 
 * connected to a #CpgRelay will transfer information immediately in a separate
 * phase, before the standard simulation update phase, at each timestep.
 *
 */
 
#define CPG_RELAY_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CPG_TYPE_RELAY, CpgRelayPrivate))

struct _CpgRelayPrivate
{
	gboolean done;
};

G_DEFINE_TYPE (CpgRelay, cpg_relay, CPG_TYPE_OBJECT)

static void
cpg_relay_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_relay_parent_class)->finalize (object);
}

static void
ensure_dependencies (CpgRelay       *relay,
                     CpgExpression  *expression)
{
	GSList *dependencies = cpg_expression_get_dependencies (expression);
	
	while (dependencies)
	{
		CpgObject *obj = cpg_property_get_object ((CpgProperty *)dependencies->data);
		
		if (CPG_IS_RELAY (obj))
		{
			cpg_object_evaluate (obj);
		}
		
		dependencies = g_slist_next (dependencies);
	}
}

static void
cpg_relay_reset_cache_impl (CpgObject *object)
{
	CpgRelay *relay = CPG_RELAY (object);

	if (CPG_OBJECT_CLASS (cpg_relay_parent_class)->reset_cache)
	{
		CPG_OBJECT_CLASS (cpg_relay_parent_class)->reset_cache (object);
	}

	relay->priv->done = FALSE;
}

static void
cpg_relay_evaluate_impl (CpgObject *object)
{
	CpgRelay *relay = CPG_RELAY (object);

	if (relay->priv->done)
		return;
	
	// Set this first to avoid cyclic loops
	relay->priv->done = TRUE;
	
	GSList *actors = cpg_object_get_actors (object);
	GSList *actor;

	// Prepare update values (ready for accumulation)
	for (actor = actors; actor; actor = g_slist_next (actor))
		_cpg_property_set_update ((CpgProperty *)actor->data, 0.0);
	
	GSList *links = _cpg_object_get_links (object);
	
	while (links)
	{
		CpgLink *link = CPG_LINK (links->data);		
		GSList *actions = cpg_link_get_actions (link);
		
		// Iterate over all the expressions in the link
		while (actions)
		{
			CpgLinkAction *action = (CpgLinkAction *)actions->data;
			CpgExpression *expression = cpg_link_action_get_expression (action);
			
			// Ensure relay dependencies
			ensure_dependencies (relay, expression);
			
			// Evaluate expression and add value to the update
			gdouble val = cpg_expression_evaluate (expression);
			CpgProperty *target = cpg_link_action_get_target (action);

			_cpg_property_set_update (target, _cpg_property_get_update (target) + val);
			actions = g_slist_next (actions);
		}
		
		links = g_slist_next (links);
	}
	
	// instantly set values, that's what the relay does
	for (actor = actors; actor; actor = g_slist_next (actor))
	{
		cpg_property_set_value ((CpgProperty *)actor->data, _cpg_property_get_update ((CpgProperty *)actor->data));
	}
}

static void
cpg_relay_class_init (CpgRelayClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgObjectClass *cpg_class = CPG_OBJECT_CLASS (klass);
	
	object_class->finalize = cpg_relay_finalize;
	cpg_class->evaluate = cpg_relay_evaluate_impl;
	cpg_class->reset_cache = cpg_relay_reset_cache_impl;

	g_type_class_add_private (object_class, sizeof (CpgRelayPrivate));
}

static void
cpg_relay_init (CpgRelay *self)
{
	self->priv = CPG_RELAY_GET_PRIVATE (self);
}

/**
 * cpg_relay_new:
 * @id: the relay id
 *
 * Creates a new #CpgRelay object
 *
 * Return value: a new #CpgRelay object
 *
 **/
CpgRelay*
cpg_relay_new (gchar const *id)
{
	return g_object_new (CPG_TYPE_RELAY, "id", id, NULL);
}
