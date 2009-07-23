#include <stdlib.h>
#include <math.h>
#include <glib.h>
#include <glib/gprintf.h>

#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-debug.h>

static void
print_object (CpgObject *object)
{
	g_printf ("[\e[1m%s\e[0m] \e[31m%s\e[0m", G_OBJECT_TYPE_NAME (object), cpg_object_get_id (object));
	
	if (CPG_IS_LINK (object))
	{
		CpgLink *link = CPG_LINK (object);
		
		g_printf (": \e[34m%s\e[0m -> \e[35m%s\e[0m", cpg_object_get_id (cpg_link_get_from (link)),
		                        cpg_object_get_id (cpg_link_get_to (link)));
	}
	
	g_printf ("\n");
	
	GSList *properties = cpg_object_get_properties (object);
	
	while (properties)
	{
		CpgProperty *prop = (CpgProperty *)properties->data;
		
		g_printf ("  %s\e[31m%s\e[0m: %s\n", cpg_property_get_integrated (prop) ? "*" : " ",
	                              cpg_property_get_name (prop),                         
		                          cpg_expression_get_as_string (cpg_property_get_value_expression (prop)));
		
		properties = g_slist_next (properties);
	}
}

static void
print_link (CpgLink *link)
{
	print_object (CPG_OBJECT (link));
	
	GSList *actions = cpg_link_get_actions (link);
	
	while (actions)
	{
		CpgLinkAction *a = (CpgLinkAction *)actions->data;
		CpgProperty *prop = cpg_link_action_get_target (a);
		CpgExpression *expr = cpg_link_action_get_expression (a);
		
		g_printf (" >%s\e[35m%s\e[0m: %s\n", cpg_property_get_integrated (prop) ? "*" : " ",
		                          cpg_property_get_name (prop),
		                          cpg_expression_get_as_string (expr));
		
		actions = g_slist_next (actions);
	}
}

int 
main (int argc, char *argv[])
{
	g_type_init ();
	
	if (argc <= 1)
	{
		g_printf ("Please specify a network file to read from\n");
		return 1;
	}
	
	cpg_debug_add (CPG_DEBUG_TYPE_ERROR);
	CpgNetwork *network = cpg_network_new_from_file (argv[1], NULL);
	
	if (!network)
	{
		g_printf ("** Failed to load network\n");
		return 1;
	}
	
	CpgCompileError *error = cpg_compile_error_new ();
	
	if (!cpg_network_compile (network, error))
	{
		GError *gerror = *cpg_compile_error_get_error (error);
		
		g_printf ("** Compile error in network: %s, %s", 
		          cpg_compile_error_string (error),
		          gerror->message);

		cpg_ref_counted_unref (error);
		g_object_unref (network);
		return 1;
	}
	
	cpg_ref_counted_unref (error);
	
	GSList *states = cpg_network_get_states (network);
	GSList *links = cpg_network_get_links (network);
	
	g_printf ("\n***\n");
	g_printf ("*** \e[34;1mLoaded network: %d states, %d links\e[0m\n", g_slist_length (states), g_slist_length (links));
	g_printf ("***\n\n");
	
	GSList *item;
	
	for (item = states; item; item = g_slist_next (item))
	{
		print_object (CPG_OBJECT (item->data));
		g_printf("\n");
	}
	
	for (item = links; item; item = g_slist_next (item))
	{
		print_link (CPG_LINK (item->data));
	}
	
	g_printf ("\n\e[1mRunning network for 0.1s...\e[0m\n");
	
	CpgMonitor *monitor;
	monitor = cpg_monitor_new (network, 
	                           cpg_network_get_object (network, "state"),
	                           "x");

	cpg_network_run (network, 0, 0.01, 0.1);
	
	guint size;
	double const *data = cpg_monitor_get_data (monitor, &size);
	
	g_printf ("Monitor on x(%d): [", size);
	guint i;
	
	for (i = 0; i < size; ++i)
	{
		if (i != 0)
			g_printf (", ");
			
		g_printf ("%.3f", data[i]);
	}
	
	g_printf ("]\n");
	
	cpg_ref_counted_unref (monitor);
	
	g_printf ("\n");
	
	g_object_unref (network);
	
	return 0;
}
