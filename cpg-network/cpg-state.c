#include "cpg-state.h"
#include "cpg-expression.h"
#include "cpg-link.h"
#include "cpg-debug.h"

#define CPG_STATE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CPG_TYPE_STATE, CpgStatePrivate))

/*struct _CpgStatePrivate
{
};*/

G_DEFINE_TYPE (CpgState, cpg_state, CPG_TYPE_OBJECT)

static void
cpg_state_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_state_parent_class)->finalize (object);
}

static void
cpg_state_update_impl (CpgObject  *object,
                       gdouble     timestep)
{
	GSList *item;
	
	for (item = _cpg_object_get_actors (object); item; item = g_slist_next (item))
	{
		CpgProperty *property = (CpgProperty *)item->data;
		gdouble value;
			
		if (cpg_property_get_integrated (property))
			value = cpg_property_get_value (property) + _cpg_property_get_update (property) * timestep;
		else
			value = _cpg_property_get_update (property);

		cpg_debug_evaluate ("Updating target %s.%s to %f",
		                    cpg_object_get_id (object),
		                    cpg_property_get_name (property),
		                    value);

		cpg_property_set_value (property, value);
	}
}

static void
cpg_state_evaluate_impl (CpgObject  *object,
                         gdouble     timestep)
{
	GSList *item;
	
	// Prepare update values (ready for accumulation)
	for (item = _cpg_object_get_actors (object); item; item = g_slist_next (item))
	{
		CpgProperty *property = (CpgProperty *)item->data;

		cpg_debug_evaluate ("Setting update for actor %s.%s to 0", 
		                     cpg_object_get_id (object),
		                     cpg_property_get_name (property));

		_cpg_property_set_update (property, 0.0);
	}

	// Iterate over all the links
	for (item = _cpg_object_get_links (object); item; item = g_slist_next (item))
	{
		CpgLink *link = CPG_LINK (item->data);
		GSList *ac;
		
		cpg_debug_evaluate ("Evaluating link on %s", cpg_object_get_id (object));

		// Iterate over all the expressions in the link
		for (ac = cpg_link_get_actions (link); ac; ac = g_slist_next (ac))
		{
			CpgLinkAction *action = (CpgLinkAction *)ac->data;
			CpgExpression *expression = cpg_link_action_get_expression (action);

			cpg_debug_evaluate ("Evaluating link action on %s.%s", 
			                    cpg_object_get_id (object),
			                    cpg_property_get_name (cpg_link_action_get_target (action)));
			
			// Evaluate expression and add value to the update
			gdouble val = cpg_expression_evaluate (expression);
			CpgProperty *target = cpg_link_action_get_target (action);

			_cpg_property_set_update (target, _cpg_property_get_update (target) + val);
		}
	}
}


static void
cpg_state_class_init (CpgStateClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgObjectClass *cpg_class = CPG_OBJECT_CLASS (klass);
	
	object_class->finalize = cpg_state_finalize;
	cpg_class->update = cpg_state_update_impl;
	cpg_class->evaluate = cpg_state_evaluate_impl;

	//g_type_class_add_private (object_class, sizeof (CpgStatePrivate));
}

static void
cpg_state_init (CpgState *self)
{
	//self->priv = CPG_STATE_GET_PRIVATE (self);
}

/** 
 * cpg_state_new:
 * @id: the state id
 *
 * Creates a new #CpgState object
 *
 * Returns: the new #CpgState object
 **/
CpgState *
cpg_state_new (gchar const *id)
{
	return g_object_new (CPG_TYPE_STATE, "id", id, NULL);
}
