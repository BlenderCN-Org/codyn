#include "cpg-link.h"
#include "cpg-compile-error.h"
#include "cpg-debug.h"
#include <string.h>
#include "cpg-group.h"

/**
 * SECTION:cpg-link
 * @short_description: Information transfer link
 *
 * A #CpgLink is a connection between two #CpgObject. The link defines actions
 * which consist of a target property in the object to which the link is
 * connected, and an expression by which this target property needs to be
 * updated.
 *
 * <refsect2 id="CpgLink-COPY">
 * <title>CpgLink Copy Semantics</title>
 * When a link is copied with #cpg_object_copy, the link actions are also
 * copied. However, the link #CpgLink:from and #CpgLink:to properties are
 * <emphasis>NOT</emphasis> copied, so that you are free to attach it to
 * two new objects.
 * </refsect2>
 */

#define CPG_LINK_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CPG_TYPE_LINK, CpgLinkPrivate))

enum
{
	EXT_PROPERTY_ADDED,
	EXT_PROPERTY_REMOVED,
	NUM_EXT_SIGNALS
};

struct _CpgLinkPrivate
{
	// from and to objects
	CpgObject *from;
	CpgObject *to;

	// list of expressions to evaluate
	GSList *actions;

	guint ext_signals[NUM_EXT_SIGNALS];
};

/* Properties */
enum
{
	PROP_0,
	PROP_TO,
	PROP_FROM
};

/* Signals */
enum
{
	ACTION_ADDED,
	ACTION_REMOVED,
	NUM_SIGNALS
};

guint signals[NUM_SIGNALS] = {0,};

G_DEFINE_TYPE (CpgLink, cpg_link, CPG_TYPE_OBJECT)

static void
cpg_link_finalize (GObject *object)
{
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
update_action_property (CpgLink       *link,
                        CpgLinkAction *action)
{
	gchar const *target = cpg_link_action_get_target (action);
	CpgProperty *prop = NULL;

	if (link->priv->to)
	{
		prop =  cpg_object_get_property (link->priv->to, target);
	}

	_cpg_link_action_set_target_property (action, prop);
}


static void
resolve_link_actions (CpgLink *link)
{
	GSList *item;
	GSList *copy = g_slist_copy (link->priv->actions);

	for (item = copy; item; item = g_slist_next (item))
	{
		update_action_property (link, item->data);
	}

	g_slist_free (copy);

	cpg_object_taint (CPG_OBJECT (link));
}

static void
on_property_added_removed (CpgLink *link)
{
	resolve_link_actions (link);
}

static void
set_to (CpgLink   *link,
        CpgObject *target)
{
	if (link->priv->to)
	{
		_cpg_object_unlink (link->priv->to, link);

		g_signal_handler_disconnect (link->priv->to,
		                             link->priv->ext_signals[EXT_PROPERTY_ADDED]);

		g_signal_handler_disconnect (link->priv->to,
		                             link->priv->ext_signals[EXT_PROPERTY_REMOVED]);

		g_object_unref (link->priv->to);

		link->priv->to = NULL;
	}

	if (target)
	{
		link->priv->to = g_object_ref (target);
		_cpg_object_link (target, link);

		link->priv->ext_signals[EXT_PROPERTY_ADDED] =
			g_signal_connect_swapped (link->priv->to,
			                          "property-added",
			                          G_CALLBACK (on_property_added_removed),
			                          link);

		link->priv->ext_signals[EXT_PROPERTY_REMOVED] =
			g_signal_connect_swapped (link->priv->to,
			                          "property-removed",
			                          G_CALLBACK (on_property_added_removed),
			                          link);
	}

	resolve_link_actions (link);

	cpg_object_taint (CPG_OBJECT (link));
}

static void
set_from (CpgLink   *link,
          CpgObject *target)
{
	if (link->priv->from)
	{
		g_object_unref (link->priv->from);
		link->priv->from = NULL;
	}

	if (target)
	{
		link->priv->from = g_object_ref (target);
	}

	cpg_object_taint (CPG_OBJECT (link));
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
			set_to (link, g_value_get_object (value));
		}
		break;
		case PROP_FROM:
		{
			set_from (link, g_value_get_object (value));
		}
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
check_modified_for_template (CpgLink       *link,
                             CpgLinkAction *action)
{
	CpgLink *templ;

	templ = cpg_link_get_action_template (link,
	                                      action,
	                                      TRUE);

	if (templ != NULL)
	{
		cpg_modifiable_set_modified (CPG_MODIFIABLE (action), FALSE);
	}
}

static void
on_template_action_equation_changed (CpgLinkAction *action,
                                     GParamSpec    *spec,
                                     CpgLink       *link)
{
	CpgLinkAction *orig = cpg_link_get_action (link,
	                                           cpg_link_action_get_target (action));

	if (!orig)
	{
		return;
	}

	if (cpg_modifiable_get_modified (CPG_MODIFIABLE (orig)))
	{
		return;
	}

	CpgLink *templ = cpg_link_get_action_template (link, orig, FALSE);

	if (templ == NULL)
	{
		return;
	}

	CpgLinkAction *over = cpg_link_get_action (templ,
	                                           cpg_link_action_get_target (action));

	if (over != action)
	{
		return;
	}

	cpg_link_action_set_equation (orig,
	                              cpg_expression_copy (cpg_link_action_get_equation (action)));

	cpg_modifiable_set_modified (CPG_MODIFIABLE (orig), FALSE);
}

static void
on_action_target_changed (CpgLinkAction *action,
                          GParamSpec    *spec,
                          CpgLink       *link)
{
	update_action_property (link, action);
}

static void
on_action_equation_changed (CpgLinkAction *action,
                            GParamSpec    *spec,
                            CpgLink       *link)
{
	check_modified_for_template (link, action);
}

static void
on_action_modified (CpgLink       *link,
                    GParamSpec    *spec,
                    CpgLinkAction *action)
{
	check_modified_for_template (link, action);
}

static void
remove_action (CpgLink       *link,
               CpgLinkAction *action)
{
	_cpg_link_action_set_target_property (action, NULL);

	g_signal_handlers_disconnect_by_func (action,
	                                      on_action_target_changed,
	                                      link);

	g_signal_handlers_disconnect_by_func (action,
	                                      on_action_equation_changed,
	                                      link);

	g_signal_handlers_disconnect_by_func (action,
	                                      on_action_modified,
	                                      link);
}

static void
disconnect_template_action (CpgLink       *link,
                            CpgLink       *templ,
                            CpgLinkAction *action)
{
	g_signal_handlers_disconnect_by_func (action,
	                                      on_template_action_equation_changed,
	                                      link);
}

static void
on_template_action_added (CpgLink       *templ,
                          CpgLinkAction *action,
                          CpgLink       *link)
{
	CpgLinkAction *orig =
		cpg_link_get_action (link,
		                     cpg_link_action_get_target (action));

	if (orig == NULL ||
	    cpg_link_get_action_template (link, orig, TRUE))
	{
		if (cpg_link_add_action (link,
		                         cpg_link_action_copy (action)))
		{
			orig = cpg_link_get_action (link,
			                            cpg_link_action_get_target (action));

			cpg_modifiable_set_modified (CPG_MODIFIABLE (orig), FALSE);
		}
		else
		{
			return;
		}
	}

	g_signal_connect (action,
	                  "notify::equation",
	                  G_CALLBACK (on_template_action_equation_changed),
	                  link);
}

static void
on_template_action_removed (CpgLink       *templ,
                            CpgLinkAction *action,
                            CpgLink       *link)
{
	CpgLinkAction *orig =
		cpg_link_get_action (link,
		                     cpg_link_action_get_target (action));

	if (orig && !cpg_modifiable_get_modified (CPG_MODIFIABLE (orig)) &&
	    cpg_link_get_action_template (link, orig, TRUE) == templ)
	{
		/* Remove the original property as well */
		cpg_link_remove_action (link, orig);
	}

	disconnect_template_action (link, templ, action);
}

static CpgLink *
find_template_for_attachments (CpgLink *link)
{
	GSList const *templates;
	CpgLink *ret = NULL;

	templates = cpg_object_get_applied_templates (CPG_OBJECT (link));

	/* Find the last template that has both to and from set */
	while (templates)
	{
		if (CPG_IS_LINK (templates->data))
		{
			CpgLink *templ = templates->data;

			if (templ->priv->to != NULL && templ->priv->from != NULL)
			{
				ret = templ;
			}
		}

		templates = g_slist_next (templates);
	}

	return ret;
}

static CpgObject *
find_in_parent (CpgLink   *link,
                CpgObject *obj)
{
	if (obj == NULL)
	{
		return NULL;
	}

	CpgGroup *parent;

	parent = CPG_GROUP (cpg_object_get_parent (CPG_OBJECT (link)));

	if (parent)
	{
		return cpg_group_get_child (parent,
		                            cpg_object_get_id (obj));
	}
	else
	{
		return NULL;
	}
}

static void
attach_from_template (CpgLink *link)
{
	CpgLink *ret = find_template_for_attachments (link);

	/* Find the corresponding child in the parent */
	cpg_link_attach (link,
	                 find_in_parent (link, ret ? ret->priv->from : NULL),
	                 find_in_parent (link, ret ? ret->priv->to : NULL));
}

static void
on_template_to_changed (CpgLink    *link,
                        GParamSpec *spec,
                        CpgLink    *templ)
{
	attach_from_template (link);
}

static void
on_template_from_changed (CpgLink    *link,
                          GParamSpec *spec,
                          CpgLink    *templ)
{
	attach_from_template (link);
}

static void
disconnect_template (CpgLink   *link,
                     CpgObject *templ,
                     gboolean   disconnect_actions)
{
	if (disconnect_actions && CPG_IS_LINK (templ))
	{
		CpgLink *templ_link = CPG_LINK (templ);
		GSList *item;

		for (item = templ_link->priv->actions; item; item = g_slist_next (item))
		{
			disconnect_template_action (link, templ_link, item->data);
		}
	}

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_action_added,
	                                      link);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_action_removed,
	                                      link);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_to_changed,
	                                      link);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_from_changed,
	                                      link);
}

static void
cpg_link_dispose (GObject *object)
{
	CpgLink *link = CPG_LINK (object);

	set_to (link, NULL);
	set_from (link, NULL);

	GSList *item;

	for (item = link->priv->actions; item; item = g_slist_next (item))
	{
		remove_action (link, item->data);
		g_object_unref (item->data);
	}

	g_slist_free (link->priv->actions);
	link->priv->actions = NULL;

	GSList const *templates = cpg_object_get_applied_templates (CPG_OBJECT (object));

	while (templates)
	{
		disconnect_template (link, templates->data, TRUE);

		templates = g_slist_next (templates);
	}

	G_OBJECT_CLASS (cpg_link_parent_class)->dispose (object);
}

static void
cpg_link_foreach_expression_impl (CpgObject                *object,
                                  CpgForeachExpressionFunc  func,
                                  gpointer                  userdata)
{
	/* Chain up */
	if (CPG_OBJECT_CLASS (cpg_link_parent_class)->foreach_expression != NULL)
	{
		CPG_OBJECT_CLASS (cpg_link_parent_class)->foreach_expression (object,
		                                                              func,
		                                                              userdata);
	}

	/* Reset action expressions */
	GSList *item;

	for (item = CPG_LINK (object)->priv->actions; item; item = g_slist_next (item))
	{
		func (cpg_link_action_get_equation (item->data), userdata);
	}
}

static void
copy_link_actions (CpgLink *dest,
                   CpgLink *source)
{
	GSList *item;

	for (item = source->priv->actions; item; item = g_slist_next (item))
	{
		cpg_link_add_action (dest,
		                     cpg_link_action_copy (item->data));
	}
}

static void
cpg_link_copy_impl (CpgObject *object,
                    CpgObject *source)
{
	/* Chain up */
	if (CPG_OBJECT_CLASS (cpg_link_parent_class)->copy != NULL)
	{
		CPG_OBJECT_CLASS (cpg_link_parent_class)->copy (object, source);
	}

	/* Copy over link actions */
	copy_link_actions (CPG_LINK (object), CPG_LINK (source));
}

static gboolean
cpg_link_compile_impl (CpgObject         *object,
                       CpgCompileContext *context,
                       CpgCompileError   *error)
{
	CpgLink *link = CPG_LINK (object);

	cpg_compile_context_save (context);
	cpg_compile_context_prepend_object (context, link->priv->from);
	cpg_compile_context_prepend_object (context, object);

	/* Chain up, compile object */
	if (CPG_OBJECT_CLASS (cpg_link_parent_class)->compile)
	{
		if (!CPG_OBJECT_CLASS (cpg_link_parent_class)->compile (object, context, error))
		{
			cpg_compile_context_restore (context);
			return FALSE;
		}
	}

	/* Parse all link expressions */
	GSList const *actions = cpg_link_get_actions (link);
	gboolean ret = TRUE;

	while (actions)
	{
		CpgLinkAction *action = actions->data;
		CpgExpression *expr = cpg_link_action_get_equation (action);
		GError *gerror = NULL;

		if (cpg_link_action_get_target_property (action) == NULL)
		{
			if (error)
			{
				gerror = g_error_new (CPG_COMPILE_ERROR_TYPE,
				                      CPG_COMPILE_ERROR_PROPERTY_NOT_FOUND,
				                      "The property `%s' for a link action of `%s' could not be found",
				                      cpg_link_action_get_target (action),
				                      cpg_object_get_id (object));

				cpg_compile_error_set (error,
				                       gerror,
				                       object,
				                       NULL,
				                       action);

				g_error_free (gerror);
			}
			
			ret = FALSE;
			break;
		}

		if (!cpg_expression_compile (expr, context, &gerror))
		{
			cpg_debug_error ("Error while parsing expression [%s]<%s>: %s",
			                 cpg_object_get_id (object),
			                 cpg_expression_get_as_string (expr),
			                 gerror->message);

			if (error)
			{
				cpg_compile_error_set (error,
				                       gerror,
				                       object,
				                       NULL,
				                       action);
			}

			g_error_free (gerror);

			ret = FALSE;
			break;
		}

		actions = g_slist_next (actions);
	}

	cpg_compile_context_restore (context);

	return ret;
}

static gboolean
cpg_link_equal_impl (CpgObject *first,
                     CpgObject *second)
{
	if (!CPG_OBJECT_CLASS (cpg_link_parent_class)->equal (first, second))
	{
		return FALSE;
	}

	CpgLink *link1 = CPG_LINK (first);
	CpgLink *link2 = CPG_LINK (second);

	if ((link1->priv->from == NULL && link2->priv->from != NULL) ||
	    (link2->priv->from == NULL && link1->priv->from != NULL) ||
	    (link1->priv->to == NULL && link2->priv->to != NULL) ||
	    (link2->priv->to == NULL && link1->priv->to != NULL))
	{
		return FALSE;
	}

	if (link1->priv->from &&
	    g_strcmp0 (cpg_object_get_id (link1->priv->from),
	               cpg_object_get_id (link2->priv->from)) != 0)
	{
		return FALSE;
	}

	if (link1->priv->to &&
	    g_strcmp0 (cpg_object_get_id (link1->priv->to),
	               cpg_object_get_id (link2->priv->to)) != 0)
	{
		return FALSE;
	}

	if (g_slist_length (link1->priv->actions) != g_slist_length (link2->priv->actions))
	{
		return FALSE;
	}

	GSList const *actions1 = cpg_link_get_actions (link1);

	while (actions1)
	{
		CpgLinkAction *ac1 = actions1->data;
		CpgLinkAction *ac2 = cpg_link_get_action (link2,
		                                          cpg_link_action_get_target (ac1));

		if (!ac2 || !cpg_link_action_equal (ac1, ac2))
		{
			return FALSE;
		}

		actions1 = g_slist_next (actions1);
	}

	return TRUE;
}

static void
cpg_link_unapply_template_impl (CpgObject *object,
                                CpgObject *templ)
{
	if (CPG_IS_LINK (templ))
	{
		GSList *item;
		CpgLink *templ_link = CPG_LINK (templ);
		CpgLink *link = CPG_LINK (object);

		for (item = templ_link->priv->actions; item; item = g_slist_next (item))
		{
			on_template_action_removed (templ_link, item->data, link);
		}

		disconnect_template (link, templ, FALSE);

		attach_from_template (link);
	}

	/* Chain up */
	if (CPG_OBJECT_CLASS (cpg_link_parent_class)->unapply_template)
	{
		CPG_OBJECT_CLASS (cpg_link_parent_class)->unapply_template (object, templ);
	}
}

static void
connect_template (CpgLink *link,
                  CpgLink *templ)
{
	g_signal_connect (templ,
	                  "action-added",
	                  G_CALLBACK (on_template_action_added),
	                  link);

	g_signal_connect (templ,
	                  "action-removed",
	                  G_CALLBACK (on_template_action_removed),
	                  link);

	g_signal_connect_swapped (templ,
	                          "notify::to",
	                          G_CALLBACK (on_template_to_changed),
	                          link);

	g_signal_connect_swapped (templ,
	                          "notify::from",
	                          G_CALLBACK (on_template_from_changed),
	                          link);
}

static void
cpg_link_apply_template_impl (CpgObject *object,
                              CpgObject *templ)
{
	/* Chain up first, transfer properties and such */
	if (CPG_OBJECT_CLASS (cpg_link_parent_class)->apply_template)
	{
		CPG_OBJECT_CLASS (cpg_link_parent_class)->apply_template (object, templ);
	}

	if (CPG_IS_LINK (templ))
	{
		CpgLink *templ_link = CPG_LINK (templ);
		GSList *item;

		for (item = templ_link->priv->actions; item; item = g_slist_next (item))
		{
			on_template_action_added (templ_link, item->data, CPG_LINK (object));
		}

		attach_from_template (CPG_LINK (object));

		connect_template (CPG_LINK (object),
		                  templ_link);
	}
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

	cpgobject_class->foreach_expression = cpg_link_foreach_expression_impl;
	cpgobject_class->copy = cpg_link_copy_impl;
	cpgobject_class->compile = cpg_link_compile_impl;
	cpgobject_class->equal = cpg_link_equal_impl;
	cpgobject_class->apply_template = cpg_link_apply_template_impl;
	cpgobject_class->unapply_template = cpg_link_unapply_template_impl;

	/**
	 * CpgLink::action-added:
	 * @object: a #CpgObject
	 * @action: the added #CpgLinkAction
	 *
	 * Emitted when a link action is added to the link
	 *
	 **/
	signals[ACTION_ADDED] =
		g_signal_new ("action-added",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgLinkClass,
		                               action_added),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_LINK_ACTION);

	/**
	 * CpgLink::action-removed:
	 * @object: a #CpgObject
	 * @action: the removed #CpgLinkAction
	 *
	 * Emitted when a link action is removed from the link
	 *
	 **/
	signals[ACTION_REMOVED] =
		g_signal_new ("action-removed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgLinkClass,
		                               action_removed),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_LINK_ACTION);

	/**
	 * CpgLink:from:
	 *
	 * The from #CpgObject
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_FROM,
	                                 g_param_spec_object ("from",
	                                                      "FROM",
	                                                      "The link from object",
	                                                      CPG_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CpgLink:to:
	 *
	 * The to #CpgObject
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_TO,
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

/**
 * cpg_link_new:
 * @id: the object id
 * @from: (allow-none): a #CpgObject
 * @to: (allow-none): a #CpgObject
 *
 * Create a new #CpgLink
 *
 * Returns: a new #CpgLink
 *
 **/
CpgLink *
cpg_link_new (gchar const  *id,
              CpgObject    *from,
              CpgObject    *to)
{
	return g_object_new (CPG_TYPE_LINK,
	                     "id", id,
	                     "from", from,
	                     "to", to, NULL);
}

/**
 * cpg_link_add_action:
 * @link: the #CpgLink
 * @action: the #CpgLinkAction
 *
 * Add a new action to be performed when the link is evaluated during
 * simulation. Note that if an action with the same
 * target already exists, the action information is transfered to the existing
 * action instance. This means that the specified @action might not actually
 * be added to the object. Also, since a #CpgLinkAction is a #GInitiallyUnowned,
 * @action will be destroyed after the call to #cpg_link_add_action in
 * the above described case, unless you explicitly sink the floating reference.
 *
 * In the case that you can not know whether an action is overriding an
 * existing action in @link, never use @action after a call to
 * #cpg_link_add_action. Instead, retrieve the corresponding action
 * using #cpg_link_get_action after the call to #cpg_link_add_action.
 *
 * Returns: %TRUE if @action could be successfully added, %FALSE otherwise
 *
 **/
gboolean
cpg_link_add_action (CpgLink       *link,
                     CpgLinkAction *action)
{
	g_return_val_if_fail (CPG_IS_LINK (link), FALSE);
	g_return_val_if_fail (CPG_IS_LINK_ACTION (action), FALSE);

	gchar const *target = cpg_link_action_get_target (action);
	CpgLinkAction *orig;

	orig = cpg_link_get_action (link, target);

	if (orig != NULL)
	{
		cpg_link_action_set_equation (orig,
		                              cpg_link_action_get_equation (action));

		if (g_object_is_floating (action))
		{
			g_object_unref (action);
		}

		return TRUE;
	}

	link->priv->actions = g_slist_append (link->priv->actions,
	                                      action);

	g_object_ref_sink (action);
	update_action_property (link, action);

	g_signal_connect (action,
	                  "notify::target",
	                  G_CALLBACK (on_action_target_changed),
	                  link);

	g_signal_connect (action,
	                  "notify::equation",
	                  G_CALLBACK (on_action_equation_changed),
	                  link);

	g_signal_connect_swapped (action,
	                          "notify::modified",
	                          G_CALLBACK (on_action_modified),
	                          link);

	cpg_object_taint (CPG_OBJECT (link));

	g_signal_emit (link, signals[ACTION_ADDED], 0, action);

	return TRUE;
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
	g_return_val_if_fail (CPG_IS_LINK_ACTION (action), FALSE);

	GSList *item = g_slist_find (link->priv->actions, action);

	if (item != NULL)
	{
		link->priv->actions = g_slist_delete_link (link->priv->actions, item);

		remove_action (link, action);

		g_signal_emit (link, signals[ACTION_REMOVED], 0, action);
		g_object_unref (action);

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
 * Returns: (element-type CpgLinkAction) (transfer none): list of #CpgLinkAction. The list is
 *          owned by the link and should not be freed
 *
 **/
GSList const *
cpg_link_get_actions (CpgLink *link)
{
	g_return_val_if_fail (CPG_IS_LINK (link), NULL);

	return link->priv->actions;
}

/**
 * cpg_link_get_action:
 * @link: A #CpgLink
 * @target: The target property name
 *
 * Get a #CpgLinkAction targetting the property @target.
 *
 * Returns: A #CpgLinkAction
 *
 **/
CpgLinkAction *
cpg_link_get_action (CpgLink     *link,
                     gchar const *target)
{
	g_return_val_if_fail (CPG_IS_LINK (link), NULL);
	g_return_val_if_fail (target != NULL, NULL);

	GSList *actions = link->priv->actions;

	while (actions)
	{
		CpgLinkAction *action = actions->data;

		if (g_strcmp0 (cpg_link_action_get_target (action), target) == 0)
		{
			return action;
		}

		actions = g_slist_next (actions);
	}

	return NULL;
}

/**
 * cpg_link_attach:
 * @link: (allow-none): A #CpgLink
 * @from: (allow-none): A #CpgObject
 * @to: A #CpgObject
 *
 * Attach @link to the objects @from and @to. This is equivalent to:
 * <informalexample>
 * <programlisting>
 * g_object_set (link, "from", from, "to", to);
 * </programlisting>
 * </informalexample>
 *
 **/
void
cpg_link_attach (CpgLink   *link,
                 CpgObject *from,
                 CpgObject *to)
{
	g_return_if_fail (CPG_IS_LINK (link));
	g_return_if_fail ((from == NULL) == (to == NULL));
	g_return_if_fail (from == NULL || CPG_IS_OBJECT (from));
	g_return_if_fail (to == NULL || CPG_IS_OBJECT (to));

	g_object_set (link, "from", from, "to", to, NULL);
}

/**
 * cpg_link_get_action_template:
 * @link: A #CpgLink
 * @action: A #CpgLinkAction
 * @match_full: How to match the action
 *
 * Get the template on which @action is defined, if any. If @match_full is
 * %TRUE, the template will only be possitively matched if both actions are
 * equal (i.e. if an action originated from a template, but was later modified,
 * this function will not return the original template object).
 *
 * Returns: A #CpgLink or %NULL if the template could not be found
 *
 **/
CpgLink *
cpg_link_get_action_template (CpgLink       *link,
                              CpgLinkAction *action,
                              gboolean       match_full)
{
	g_return_val_if_fail (CPG_IS_LINK (link), NULL);
	g_return_val_if_fail (CPG_IS_LINK_ACTION (action), NULL);

	GSList *templates = g_slist_copy ((GSList *)cpg_object_get_applied_templates (CPG_OBJECT (link)));
	templates = g_slist_reverse (templates);
	GSList *item;

	gchar const *target = cpg_link_action_get_target (action);

	for (item = templates; item; item = g_slist_next (item))
	{
		if (!CPG_IS_LINK (item->data))
		{
			continue;
		}

		CpgLinkAction *taction;
		CpgLink *templ = item->data;

		taction = cpg_link_get_action (templ, target);

		if (taction && (!match_full || cpg_link_action_equal (action, taction)))
		{
			g_slist_free (templates);
			return templ;
		}
	}

	g_slist_free (templates);

	return NULL;
}

