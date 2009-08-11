#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-debug.h>

#include <stdlib.h>
#include <glib/gprintf.h>

int 
main (int argc, char *argv[])
{
	g_type_init ();
	
	cpg_debug_add (CPG_DEBUG_TYPE_ERROR);

	CpgNetwork *network = cpg_network_new_from_file ("template.cpg", NULL);
	
	if (!network)
	{
		g_error ("Could not open network");
		exit(1);
	}
	
	gchar *xml = cpg_network_write_to_xml (network);
	
	g_printf ("\n%s\n", xml);
	g_free (xml);
	
	g_object_unref (network);
	return 0;
}
