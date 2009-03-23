#include "cpg-link.h"
#include "cpg-ref-counted.h"
#include <string.h>

#define CPG_LINK_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_LINK, CpgLinkPrivate))

struct _CpgLinkAction
{
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

G_DEFINE_TYPE(CpgLink, cpg_link, CPG_TYPE_OBJECT)

static void
link_action_free(CpgLinkAction *action)
{
	cpg_ref_counted_unref(action->expression);
	
	// do not free target, borrowed reference
	g_free(action);
}

static void
cpg_link_finalize(GObject *object)
{
	CpgLink *link = CPG_LINK(object);
	
	g_slist_foreach(link->priv->actions, (GFunc)link_action_free, NULL);
	g_slist_free(link->priv->actions);

	G_OBJECT_CLASS(cpg_link_parent_class)->finalize(object);
}

static void
cpg_link_get_property(GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
	CpgLink *link = CPG_LINK(object);

	switch (prop_id)
	{
		case PROP_TO:
			g_value_set_object(value, link->priv->to);
		break;
		case PROP_FROM:
			g_value_set_object(value, link->priv->from);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_link_set_property(GObject *object, guint prop_id, GValue const *value, GParamSpec *pspec)
{
	CpgLink *link = CPG_LINK(object);

	switch (prop_id)
	{
		case PROP_TO:
		{
			if (link->priv->to)
				g_object_unref(link->priv->to);
			
			link->priv->to = g_value_dup_object(value);
			_cpg_object_link(link->priv->to, link);
		}
		break;
		case PROP_FROM:
		{
			if (link->priv->from)
				g_object_unref(link->priv->from);
			
			link->priv->from = g_value_dup_object(value);
		}
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_link_class_init(CpgLinkClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS(klass);
	
	object_class->finalize = cpg_link_finalize;
	object_class->get_property = cpg_link_get_property;
	object_class->set_property = cpg_link_set_property;

	g_object_class_install_property(object_class, PROP_FROM,
				 g_param_spec_object("from",
							  "FROM",
							  "The link from object",
							  CPG_TYPE_OBJECT,
							  G_PARAM_READWRITE));

	g_object_class_install_property(object_class, PROP_TO,
				 g_param_spec_object("to",
							  "TO",
							  "The link to object",
							  CPG_TYPE_OBJECT,
							  G_PARAM_READWRITE));

	g_type_class_add_private(object_class, sizeof(CpgLinkPrivate));
}

static void
cpg_link_init(CpgLink *self)
{
	self->priv = CPG_LINK_GET_PRIVATE(self);
}

CpgLink*
cpg_link_new(gchar const *id, CpgObject *from, CpgObject *to)
{
	return g_object_new(CPG_TYPE_LINK, "id", id, "from", from, "to", to, NULL);
}

/**
 * cpg_link_add_action:
 * @link: the #CpgLink
 * @destination: the destination #CpgProperty
 * @expression: the expression to evaluate and push to @destination
 *
 * Add a new action to be performed when the link is evaluated during
 * simulation. An action consists of a destination property and an expression
 * who's result will be pushed in the destination at every simulation step
 *
 **/
void
cpg_link_add_action(CpgLink     *link, 
					CpgProperty *target, 
					gchar const *expression)
{
	g_return_if_fail(CPG_IS_LINK(link));
	g_return_if_fail(target != NULL);
	g_return_if_fail(expression != NULL);

	CpgLinkAction *action = g_new(CpgLinkAction, 1);
	
	action->expression = cpg_expression_new(expression);
	
	// target is a borrowed reference
	action->target = target;
	
	link->priv->actions = g_slist_append(link->priv->actions, action);

	// let target object know that link has changed
	_cpg_object_update_link(link->priv->to, link);
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
cpg_link_get_from(CpgLink *link)
{
	g_return_val_if_fail(CPG_IS_LINK(link), NULL);
	
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
cpg_link_get_to(CpgLink *link)
{
	g_return_val_if_fail(CPG_IS_LINK(link), NULL);

	return link->priv->to;
}

/**
 * cpg_link_get_actions:
 * @link: the #CpgLink
 * @size: return pointer for the number of actions
 *
 * Returns an array of all actions of the link
 *
 * Return value: array of #CpgLinkAction pointers
 *
 **/
GSList *
cpg_link_get_actions(CpgLink *link)
{
	g_return_val_if_fail(CPG_IS_LINK(link), NULL);
	
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
cpg_link_action_get_expression(CpgLinkAction *action)
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
cpg_link_action_get_target(CpgLinkAction *action)
{
	return action->target;
}

