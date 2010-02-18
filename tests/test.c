#include <stdlib.h>
#include <math.h>
#include <glib.h>
#include <glib/gprintf.h>

#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-debug.h>
#include <cpg-network/cpg-integrator-runge-kutta.h>
#include <cpg-network/cpg-integrator-euler.h>

#define ANSI_BOLD "\e[1m"
#define ANSI_OFF "\e[0m"
#define ANSI_RED "\e[31m"
#define ANSI_BLUE "\e[34m"
#define ANSI_PURPLE "\e[35m"

static gchar *integrator_name = NULL;

static GOptionEntry entries[] =
{
	{"integrator", 'i', 0, G_OPTION_ARG_STRING, &integrator_name, "Type of integrator to use", "rk|euler"},
	{NULL}
};

static void
print_object (CpgObject *object)
{
	g_printf ("[" ANSI_BOLD "%s" ANSI_OFF "] " ANSI_BLUE "%s" ANSI_OFF,
	          G_OBJECT_TYPE_NAME (object),
	          cpg_object_get_id (object));
	
	if (CPG_IS_LINK (object))
	{
		CpgLink *link = CPG_LINK (object);
		
		g_printf (": " ANSI_RED "%s" ANSI_OFF " -> " ANSI_PURPLE "%s" ANSI_OFF,
		          cpg_object_get_id (cpg_link_get_from (link)),
		          cpg_object_get_id (cpg_link_get_to (link)));
	}
	
	g_printf ("\n");
	
	GSList *properties = cpg_object_get_properties (object);
	
	while (properties)
	{
		CpgProperty *prop = (CpgProperty *)properties->data;
		
		g_printf ("  %s" ANSI_RED "%s" ANSI_OFF ": %s\n",
		          cpg_property_get_integrated (prop) ? "*" : " ",
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
		
		g_printf (" >%s" ANSI_PURPLE "%s" ANSI_OFF ": %s\n",
		          cpg_property_get_integrated (prop) ? "*" : " ",
		          cpg_property_get_name (prop),
		          cpg_expression_get_as_string (expr));

		actions = g_slist_next (actions);
	}
}

static void
print_function (CpgFunction *function)
{
	print_object (CPG_OBJECT (function));

	g_printf (" > %s\n", cpg_expression_get_as_string (cpg_function_get_expression (function)));
}

static CpgIntegrator *
make_integrator ()
{
	if (g_strcmp0 (integrator_name, "rk") == 0)
	{
		return CPG_INTEGRATOR (cpg_integrator_runge_kutta_new ());
	}
	else
	{
		return CPG_INTEGRATOR (cpg_integrator_euler_new ());
	}
}

int 
main (int argc, char *argv[])
{
	g_type_init ();

	GOptionContext *context = g_option_context_new ("- test cpg network");

	g_option_context_add_main_entries (context, entries, NULL);
	g_option_context_set_help_enabled (context, TRUE);

	if (!g_option_context_parse (context, &argc, &argv, NULL))
	{
		exit (1);
	}

	if (argc <= 1)
	{
		g_printf ("Please specify a network file to read from\n");
		return 1;
	}
	
	cpg_debug_add (CPG_DEBUG_TYPE_ERROR);

	GError *gerror = NULL;
	CpgNetwork *network = cpg_network_new_from_file (argv[1], &gerror);
	
	if (!network)
	{
		g_printf ("** Failed to load network: %s\n", gerror->message);

		g_error_free (gerror);
		exit (1);
	}

	CpgIntegrator *integrator = make_integrator ();
	cpg_network_set_integrator (network, integrator);
	g_object_unref (integrator);

	CpgCompileError *error = cpg_compile_error_new ();

	if (!cpg_network_compile (network, error))
	{
		GError *gerror = cpg_compile_error_get_error (error);
		
		g_printf ("** Compile error in network: %s, %s\n", 
		          cpg_compile_error_string (error),
		          gerror ? gerror->message : "No error set");

		cpg_ref_counted_unref (error);
		g_object_unref (network);
		return 1;
	}
	
	cpg_ref_counted_unref (error);
	
	GSList *states = cpg_network_get_states (network);
	GSList *links = cpg_network_get_links (network);
	GSList *functions = cpg_network_get_functions (network);
	
	g_printf ("\n***\n");
	g_printf ("*** " ANSI_BLUE ANSI_BOLD "Loaded network: %d states, %d links, %d functions" ANSI_OFF "\n", 
	          g_slist_length (states),
	          g_slist_length (links),
	          g_slist_length (functions));

	g_printf ("*** " ANSI_BLUE ANSI_BOLD "Integrator: %s" ANSI_OFF "\n",
	          g_type_name (G_TYPE_FROM_INSTANCE (cpg_network_get_integrator (network))));
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

	for (item = functions; item; item = g_slist_next (item))
	{
		print_function (CPG_FUNCTION (item->data));
	}
	
	g_printf ("\n" ANSI_BOLD "Running network for 0.1s..." ANSI_OFF "\n");
	
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
