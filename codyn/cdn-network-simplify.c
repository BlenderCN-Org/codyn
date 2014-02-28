#include "cdn-network.h"
#include "cdn-expression-tree-iter.h"
#include "instructions/cdn-instruction-variable.h"
#include "instructions/cdn-instruction-custom-function.h"

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
		    cdn_variable_has_actions (v))
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

	// Pass 2: simplify all expressions
	simplify_network (network);
}
