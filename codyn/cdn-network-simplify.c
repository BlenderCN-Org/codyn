#include "cdn-network.h"
#include "cdn-expression-tree-iter.h"
#include "instructions/cdn-instruction-variable.h"
#include "instructions/cdn-instruction-custom-function.h"

static gboolean
update_sparsity_expression (CdnExpression *e,
                            CdnStackArg   *a2)
{
	CdnStackArg *a1;
	gint i1 = 0;
	gint i2 = 0;
	gint wp = 0;

	a1 = cdn_expression_get_stack_arg (e);

	// Remove from a1 the sparse entries which are not in a2
	while (i1 < a1->num_sparse)
	{
		if (i2 >= a2->num_sparse)
		{
			break;
		}

		if (a1->sparsity[i1] < a2->sparsity[i2])
		{
			// Remove it
			++i1;
			continue;
		}

		// write
		if (i1 != wp)
		{
			a1->sparsity[wp] = a1->sparsity[i1];
		}

		if (a1->sparsity[i1] == a2->sparsity[i2])
		{
			// Equal, increment all
			++i1;
			++i2;
			++wp;
		}
		else
		{
			// Increment just i2
			++i2;
		}
	}

	if (wp != a1->num_sparse)
	{
		GSList const *deps;

		a1->num_sparse = wp;

		// Recurse recalculate sparsity of expressions depending on
		// this expression
		deps = cdn_expression_get_depends_on_me (e);

		while (deps)
		{
			cdn_expression_recalculate_sparsity (deps->data);
			deps = g_slist_next (deps);
		}

		return TRUE;
	}

	return FALSE;
}

static gboolean
update_sparsity_edge (CdnEdgeAction *action)
{
	CdnExpression *e;
	CdnVariable *t;
	CdnStackArg *arg;

	e = cdn_edge_action_get_equation (action);
	t = cdn_edge_action_get_target_variable (action);

	// Get the sparsity of 'e' and see if it's less restrictive then
	// the one from 't'
	arg = cdn_expression_get_stack_arg (e);

	return update_sparsity_expression (cdn_variable_get_expression (t),
	                                   arg);
}

static void
mark_pinned (GSList const *l)
{
	while (l)
	{
		CdnExpression *e;

		e = cdn_variable_get_expression (l->data);
		cdn_expression_set_pinned_sparsity (e, TRUE);
		l = g_slist_next (l);
	}
}

static gboolean
update_sparsity_actions (GSList const *ac)
{
	gboolean changed = FALSE;

	while (ac)
	{
		if (update_sparsity_edge (ac->data))
		{
			changed = TRUE;
		}

		ac = g_slist_next (ac);
	}

	return changed;
}

static void
update_sparsity (CdnNetwork *network)
{
	// Need to update the sparsity information for integrated variables
	// and variables with direct edges
	CdnIntegrator *integrator;
	CdnIntegratorState *state;

	integrator = cdn_network_get_integrator (network);
	state = cdn_integrator_get_state (integrator);

	mark_pinned (cdn_integrator_state_integrated_variables (state));
	mark_pinned (cdn_integrator_state_direct_variables (state));

	while (TRUE)
	{
		GSList const *ac;
		GSList const *acd;

		ac = cdn_integrator_state_integrated_edge_actions (state);
		acd = cdn_integrator_state_direct_edge_actions (state);

		if (!update_sparsity_actions (ac) || update_sparsity_actions (acd))
		{
			break;
		}
	}
}

static void
simplify_network (CdnNetwork *network)
{
	CdnIntegrator *integrator;
	CdnIntegratorState *state;
	GQueue exprs;
	GSList const *l;
	GHashTable *processed;
	GHashTable *mappie;

	integrator = cdn_network_get_integrator (network);
	state = cdn_integrator_get_state (integrator);

	// Collect expressions that need simplification
	l = cdn_integrator_state_all_variables (state);

	g_queue_init (&exprs);

	mappie = g_hash_table_new (g_direct_hash, g_direct_equal);
	processed = g_hash_table_new (g_direct_hash, g_direct_equal);

	while (l)
	{
		CdnVariable *v;

		v = l->data;

		if ((cdn_variable_get_flags (v) & (CDN_VARIABLE_FLAG_OUT | CDN_VARIABLE_FLAG_INTEGRATED)) ||
		    cdn_variable_get_actions (v))
		{
			CdnExpression *e;

			e = cdn_variable_get_expression (v);
			g_queue_push_head (&exprs, e);

			g_hash_table_insert (mappie, e, cdn_variable_get_full_name_for_display (v));
		}

		l = g_slist_next (l);
	}

	l = cdn_integrator_state_integrated_edge_actions (state);

	while (l)
	{
		CdnExpression *e;

		e = cdn_edge_action_get_equation (l->data);
		g_queue_push_head (&exprs, e);

		g_hash_table_insert (mappie, e, g_strdup_printf ("%s<%s",
		                                                 cdn_object_get_full_id_for_display (CDN_OBJECT (cdn_edge_action_get_edge (l->data))),
		                                                 cdn_edge_action_get_target (l->data)));

		l = g_slist_next (l);
	}

	l = cdn_integrator_state_direct_edge_actions (state);

	while (l)
	{
		CdnExpression *e;

		e = cdn_edge_action_get_equation (l->data);
		g_queue_push_head (&exprs, e);

		g_hash_table_insert (mappie, e, g_strdup_printf ("%s<%s",
		                                                 cdn_object_get_full_id_for_display (CDN_OBJECT (cdn_edge_action_get_edge (l->data))),
		                                                 cdn_edge_action_get_target (l->data)));

		l = g_slist_next (l);
	}

	while (!g_queue_is_empty (&exprs))
	{
		CdnExpression *e;
		GSList *newinstr;
		CdnExpressionTreeIter *iter;
		GSList *instr;
		gchar const *ss;

		e = g_queue_pop_head (&exprs);

		if (g_hash_table_lookup_extended (processed, e, NULL, NULL))
		{
			continue;
		}

		g_hash_table_insert (processed, e, NULL);

		iter = cdn_expression_tree_iter_new (e);

		if (iter == NULL)
		{
			continue;
		}

		ss = g_hash_table_lookup (mappie, e);

		if (ss)
		{
			g_print ("\nSimplifying globally: %s\n======================\n", ss);
		}
		else
		{
			g_print ("\n");
		}

		cdn_expression_tree_iter_simplify (iter);

		newinstr = cdn_expression_tree_iter_to_instructions (iter);
		cdn_expression_tree_iter_free (iter);

		cdn_expression_set_instructions_take (e, newinstr);

		// Go deep
		for (instr = newinstr; instr; instr = g_slist_next (instr))
		{
			if (CDN_IS_INSTRUCTION_VARIABLE (instr->data))
			{
				CdnVariable *v;
				CdnInstructionVariable *vinstr;

				vinstr = CDN_INSTRUCTION_VARIABLE (instr->data);
				v = cdn_instruction_variable_get_variable (vinstr);

				e = cdn_variable_get_expression (v);

				if (!g_hash_table_lookup_extended (processed, e, NULL, NULL))
				{
					g_queue_push_head (&exprs, e);
				}
			}
		}

		g_slist_foreach (newinstr, (GFunc)cdn_mini_object_unref, NULL);
		g_slist_free (newinstr);
	}

	g_hash_table_destroy (processed);
}

/**
 * cdn_network_simplify:
 * @network a #CdnNetwork.
 *
 * This function is an experimental feature which tries to symbolically
 * simplify each expression in the network. This can potentially improve the
 * simulation time. Note that at this time, this feature has not been tested
 * well, and most likely will cause your network to behave incorrectly.
 */
void
cdn_network_simplify (CdnNetwork *network)
{
	g_return_if_fail (CDN_IS_NETWORK (network));

	if (!cdn_object_compile (CDN_OBJECT (network), NULL, NULL))
	{
		return;
	}

	// Pass 1: update full sparsity taking edges into account
	update_sparsity (network);

	// Pass 2: simplify all expressions
	simplify_network (network);
}
