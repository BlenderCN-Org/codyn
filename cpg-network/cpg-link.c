#include "cpg-link.h"
#include "cpg-ref-counted-private.h"
#include <string.h>

#define CPG_LINK_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CPG_TYPE_LINK, CpgLinkPrivate))

struct _CpgLinkAction
{
	CpgRefCounted parent;
	CpgExpression *expression;
	CpgProperty *target;
};

struct _CpgLinkPrivate
{
	// from and to objects
	CpgObject *from;
	CpgObject *to;
	
	// list of expressions to evaluate
	GSList *actions;
};

/* Properties */
enum
{
	PROP_0,
	PROP_TO,
	PROP_FROM
};

G_DEFINE_TYPE (CpgLink, cpg_link, CPG_TYPE_OBJECT)

static void
cpg_link_action_free (CpgLinkAction *action)
{
	cpg_ref_counted_unref (action->expression);
	
	if (action->target)
	{
		_cpg_property_unuse (action->target);
	}
	
	// do not free target, borrowed reference
	g_slice_free (CpgLinkAction, action);
}

static void
cpg_link_finalize (GObject *object)
{
	CpgLink *link = CPG_LINK (object);
	
	g_slist_foreach (link->priv->actions, (GFunc)cpg_ref_counted_unref, NULL);
	g_slist_free (link->priv->actions);
	
	G_OBJECT_CLASS (cpg_link_parent_class)->finalize (object);
}

static void
cpg_link_get_property (GObject     *object,
                       guint        prop_id,
                       GValue      *value,
                       GParamSpec  *pspec)
{
	CpgLink *link = CPG_LINK (object);

	switch (prop_id)
	{
		case PROP_TO:
			g_value_set_object (value, link->priv->to);
		break;
		case PROP_FROM:
			g_value_set_object (value, link->priv->from);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_link_set_property (GObject       *object,
                       guint          prop_id,
                       GValue const  *value,
                       GParamSpec    *pspec)
{
	CpgLink *link = CPG_LINK (object);

	switch (prop_id)
	{
		case PROP_TO:
		{
			if (link->priv->to)
			{
				g_object_unref (link->priv->to);
			}
			
			link->priv->to = g_value_dup_object (value);
			
			if (link->priv->to)
			{
				_cpg_object_link (link->priv->to, link);
			}
			
			cpg_object_taint (CPG_OBJECT (link));
		}
		break;
		case PROP_FROM:
		{
			if (link->priv->from)
			{
				g_object_unref (link->priv->from);
			}
			
			link->priv->from = g_value_dup_object (value);
			cpg_object_taint (CPG_OBJECT (link));
		}
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_link_dispose (GObject *object)
{
	CpgLink *link = CPG_LINK (object);
	
	if (link->priv->to)
		g_object_unref (link->priv->to);
	
	if (link->priv->from)
		g_object_unref (link->priv->from);
		
	link->priv->to = NULL;
	link->priv->from = NULL;
}	

static void
action_reset_cache (CpgLinkAction *action)
{
	cpg_expression_reset_cache (action->expression);
}

static void
cpg_link_reset_cache_impl (CpgObject *object)
{
	/* Chain up */
	if (CPG_OBJECT_CLASS (cpg_link_parent_class)->reset_cache != NULL)
	{
		CPG_OBJECT_CLASS (cpg_link_parent_class)->reset_cache (object);
	}
	
	/* Reset action expressions */
	g_slist_foreach (CPG_LINK (object)->priv->actions, (GFunc)action_reset_cache, NULL);
}

static void
cpg_link_class_init (CpgLinkClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgObjectClass *cpgobject_class = CPG_OBJECT_CLASS (klass);
	
	object_class->finalize = cpg_link_finalize;
	object_class->dispose = cpg_link_dispose;
	
	object_class->get_property = cpg_link_get_property;
	object_class->set_property = cpg_link_set_property;
	
	cpgobject_class->reset_cache = cpg_link_reset_cache_impl;

	g_object_class_install_property (object_class, PROP_FROM,
				 g_param_spec_object ("from",
							  "FROM",
							  "The link from object",
							  CPG_TYPE_OBJECT,
							  G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class, PROP_TO,
				 g_param_spec_object ("to",
							  "TO",
							  "The link to object",
							  CPG_TYPE_OBJECT,
							  G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_type_class_add_private (object_class, sizeof (CpgLinkPrivate));
}

static void
cpg_link_init (CpgLink *self)
{
	self->priv = CPG_LINK_GET_PRIVATE (self);
}

CpgLink *
cpg_link_new (gchar const  *id,
              CpgObject    *from,
              CpgObject    *to)
{
	return g_object_new (CPG_TYPE_LINK, "id", id, "from", from, "to", to, NULL);
}

/**
 * cpg_link_add_action:
 * @link: the #CpgLink
 * @target: the target #CpgProperty
 * @expression: the expression to evaluate and push to @target
 *
 * Add a new action to be performed when the link is evaluated during
 * simulation. An action consists of a target property and an expression
 * who's result will be pushed in the target at every simulation step
 *
 * Returns: the new #CpgLinkAction
 **/
CpgLinkAction *
cpg_link_add_action (CpgLink      *link,
                     CpgProperty  *target,
                     gchar const  *expression)
{
	g_return_val_if_fail (CPG_IS_LINK (link), NULL);
	g_return_val_if_fail (target != NULL, NULL);
	g_return_val_if_fail (expression != NULL, NULL);

	CpgLinkAction *action = g_slice_new (CpgLinkAction);
	cpg_ref_counted_init (action, (GDestroyNotify)cpg_link_action_free);
	
	action->expression = cpg_expression_new (expression);
	
	// target is a borrowed reference
	action->target = target;
	_cpg_property_use (target);
	
	link->priv->actions = g_slist_append (link->priv->actions, action);
	
	cpg_object_taint (CPG_OBJECT (link));
	return action;
}

/**
 * cpg_link_remove_action:
 * @link: the #CpgLink
 * @action: the #CpgLinkAction
 *
 * Removes an action from the link.
 *
 * Returns: %TRUE if the action was successfully removed
 *
 **/
gboolean
cpg_link_remove_action (CpgLink       *link,
                        CpgLinkAction *action)
{
	g_return_val_if_fail (CPG_IS_LINK (link), FALSE);
	g_return_val_if_fail (action != NULL, FALSE);

	GSList *item = g_slist_find (link->priv->actions, action);
	
	if (item != NULL)
	{
		cpg_ref_counted_unref (action);
		link->priv->actions = g_slist_remove_link (link->priv->actions, item);
		
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

/**
 * cpg_link_get_from:
 * @link: the #CpgLink
 *
 * Returns the from #CpgObject of the link
 *
 * Return value: the from #CpgObject
 *
 **/
CpgObject *
cpg_link_get_from (CpgLink *link)
{
	g_return_val_if_fail (CPG_IS_LINK (link), NULL);
	
	return link->priv->from;
}

/**
 * cpg_link_get_to:
 * @link: the #CpgLink
 *
 * Returns the to #CpgObject of the link
 *
 * Return value: the to #CpgObject
 *
 **/
CpgObject *
cpg_link_get_to (CpgLink *link)
{
	g_return_val_if_fail (CPG_IS_LINK (link), NULL);

	return link->priv->to;
}

/**
 * cpg_link_get_actions:
 * @link: the #CpgLink
 *
 * Get link actions
 *
 * Return value: list of #CpgLinkAction. The list is owned by the link and
 *               should not be freed
 *
 **/
GSList *
cpg_link_get_actions (CpgLink *link)
{
	g_return_val_if_fail (CPG_IS_LINK (link), NULL);
	
	return link->priv->actions;
}

/**
 * cpg_link_action_expression:
 * @action: the #CpgLinkAction
 *
 * Returns the expression associated with the action
 *
 * Return value: the #CpgExpression associated with the action. It is owned
 * by the action object and should not be freed.
 *
 **/
CpgExpression *
cpg_link_action_get_expression (CpgLinkAction *action)
{
	return action->expression;
}

/**
 * cpg_link_action_target:
 * @action: the #CpgLinkAction
 *
 * Returns the target #CpgProperty associated with the action
 *
 * Return value: the target #CpgProperty. It is owned
 * by the action object and should not be freed.
 *
 **/
CpgProperty	*
cpg_link_action_get_target (CpgLinkAction *action)
{
	return action->target;
}

GType
cpg_link_action_get_type ()
{
	static GType type_id = 0;
	
	if (G_UNLIKELY (type_id == 0))
		type_id = g_boxed_type_register_static ("CpgLinkAction", cpg_ref_counted_ref, cpg_ref_counted_unref);
	
	return type_id;
}

