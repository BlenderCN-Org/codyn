#include <stdlib.h>
#include <math.h>

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
		
		g_printf ("\n");
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
	CpgNetwork *network = cpg_network_new_from_file (argv[1]);
	
	if (!network)
	{
		g_printf ("** Failed to load network\n");
		return 1;
	}
	
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
	
	g_object_unref (network);
	
	return 0;
}
