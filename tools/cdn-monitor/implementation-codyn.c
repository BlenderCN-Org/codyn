#include "implementation.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef ENABLE_GIO_UNIX
#include <gio/gunixinputstream.h>
#endif

static gboolean
monitor_free (CdnMonitorImplementation *implementation)
{
	g_object_unref (implementation->network);
	return FALSE;
}

static gchar *
variable_get_name (CdnMonitorVariable *v)
{
	return cdn_variable_get_full_name (v->variable);
}

static gdouble const *
variable_get_values (CdnMonitorVariable *v)
{
	return cdn_variable_get_values (v->variable, NULL);
}

static CdnMonitorVariable *
create_monitor_variable (CdnVariable *v)
{
	CdnMonitorVariable *ret;

	ret = g_slice_new0 (CdnMonitorVariable);

	ret->variable = v;

	cdn_variable_get_dimension (v, &ret->dimension);

	ret->get_name = variable_get_name;
	ret->get_values = variable_get_values;
	ret->row = -1;
	ret->col = -1;

	return ret;
}

static CdnMonitorVariable *
monitor_get_time (CdnMonitorImplementation *implementation)
{
	CdnIntegrator *integrator;
	CdnVariable *t;

	integrator = cdn_network_get_integrator (implementation->network);
	t = cdn_object_get_variable (CDN_OBJECT (integrator), "t");

	return create_monitor_variable (t);
}

static gdouble
monitor_step (CdnMonitorImplementation *implementation,
              gdouble                   t,
              gdouble                   timestep)
{
	CdnIntegrator *integrator;

	integrator = cdn_network_get_integrator (implementation->network);

	return cdn_integrator_step (integrator, t, timestep);
}

static void
monitor_begin (CdnMonitorImplementation *implementation,
               gdouble                   t,
               gdouble                   timestep)
{
	CdnIntegrator *integrator;

	integrator = cdn_network_get_integrator (implementation->network);

	cdn_integrator_begin (integrator, t, NULL);
}

static gboolean
monitor_terminated (CdnMonitorImplementation *implementation)
{
	CdnIntegrator *integrator;

	integrator = cdn_network_get_integrator (implementation->network);

	return cdn_integrator_get_terminate (integrator);
}

static GSList *
monitor_resolve (CdnMonitorImplementation *implementation,
                 CdnSelector              *selector)
{
	GSList *selection;
	GSList *variables = NULL;

	selection = cdn_selector_select (selector,
	                                 G_OBJECT (implementation->network),
	                                 CDN_SELECTOR_TYPE_VARIABLE,
	                                 NULL);

	while (selection)
	{
		CdnVariable *var;
		CdnMonitorVariable *v;

		var = cdn_selection_get_object (selection->data);

		v = create_monitor_variable (var);

		variables = g_slist_prepend (variables, v);

		cdn_selection_unref (selection->data);
		selection = g_slist_delete_link (selection, selection);
	}

	return variables;
}

CdnMonitorImplementation *
cdn_monitor_implementation_codyn_new (gchar const *filename)
{
	CdnNetwork *network;
	CdnCompileError *err;
	GError *error = NULL;
	CdnMonitorImplementation *ret;

#ifdef ENABLE_GIO_UNIX
	if (g_strcmp0 (filename, "-") == 0)
	{
		GInputStream *stream = g_unix_input_stream_new (STDIN_FILENO, TRUE);
		network = cdn_network_new_from_stream (stream, &error);
		g_object_unref (stream);
	}
	else
#endif
	{
		GFile *file = g_file_new_for_commandline_arg (filename);
		network = cdn_network_new_from_file (file, &error);
		g_object_unref (file);
	}

	if (!network)
	{
		g_printerr ("Failed to load network `%s': %s\n", filename, error->message);
		g_error_free (error);

		return NULL;
	}

	err = cdn_compile_error_new ();

	if (!cdn_object_compile (CDN_OBJECT (network), NULL, err))
	{
		gchar *msg;

		msg = cdn_compile_error_get_formatted_string (err);

		g_printerr ("Failed to compile network `%s'\n\n%s\n",
		            filename,
		            msg);

		g_free (msg);

		g_object_unref (network);
		g_object_unref (err);

		return NULL;
	}

	g_object_unref (err);

	ret = cdn_monitor_implementation_new ();

	ret->free = monitor_free;
	ret->network = network;

	ret->resolve = monitor_resolve;
	ret->begin = monitor_begin;
	ret->step = monitor_step;
	ret->get_time = monitor_get_time;
	ret->terminated = monitor_terminated;

	return ret;
}
