#include "utils.h"
#include <codyn/cdn-annotatable.h>
#include <glib/gprintf.h>

void
cdn_assert_float (gchar const *file,
                  gchar const *func,
                  gint         line,
                  gdouble      real,
                  gdouble      expect)
{
	if (!cdn_cmp_tol (real, expect))
	{
		g_printf ("\nERROR:%s:%d:%s: expected %f but got %f\n",
		          file,
		          line,
		          func,
		          expect,
		          real);

		abort ();
	}
}

void
cdn_assert_nfloat (gchar const *file,
                  gchar const *func,
                  gint         line,
                  gdouble      real,
                  gdouble      expect)
{
	if (cdn_cmp_tol (real, expect))
	{
		g_printf ("\nERROR:%s:%d:%s: expected unequal values, but got %f and %f\n",
		          file,
		          line,
		          func,
		          expect,
		          real);

		abort ();
	}
}

CdnEdgeAction *
find_action (CdnNode    *parent,
             gchar const *path)
{
	gchar *copy = g_strdup (path);
	gchar *ptr = g_utf8_strrchr (copy, -1, '.');

	if (!ptr)
	{
		g_free (copy);
		return NULL;
	}

	*ptr = '\0';
	CdnObject *object;

	object = cdn_node_find_object (CDN_NODE (parent), copy);

	if (!object || !CDN_IS_EDGE (object))
	{
		g_free (copy);
		return NULL;
	}

	CdnEdgeAction *ret = cdn_edge_get_action (CDN_EDGE (object), ptr + 1);
	g_free (copy);

	return ret;
}

#define assert_find(ret, format, ...) {\
	if (!ret)\
	{\
		gchar *_msg = g_strdup_printf (format, __VA_ARGS__);\
		g_assertion_message (G_LOG_DOMAIN,\
		                     __FILE__,\
		                     __LINE__,\
		                     G_STRFUNC,\
		                    _msg);\
		g_free (_msg);\
	}\
	else\
	{\
		retobj = G_OBJECT (ret);\
	}\
}

static void
check_objects (CdnNetwork *network,
               gboolean    get_objects,
               va_list     ap)
{
	CdnPath type;

	while ((type = va_arg (ap, CdnPath)) != CDN_PATH_NONE)
	{
		gchar const *path = va_arg (ap, gchar const *);
		GObject *retobj = NULL;

		switch (type)
		{
			case CDN_PATH_OBJECT:
				assert_find (cdn_node_find_object (CDN_NODE (network),
				                                    path),
				             "Object `%s' cannot be found",
				             path);
			break;
			case CDN_PATH_TEMPLATE_OBJECT:
				assert_find (cdn_node_find_object (cdn_network_get_template_node (network),
				                                    path),
				             "Template object `%s' cannot be found",
				             path);
			break;
			case CDN_PATH_TEMPLATE_PROPERTY:
				assert_find (cdn_node_find_variable (cdn_network_get_template_node (network),
				                                     path),
				             "Template property `%s' cannot be found",
				             path);
			break;
			case CDN_PATH_PROPERTY:
				assert_find (cdn_node_find_variable (CDN_NODE (network),
				                                     path),
				             "Property `%s' cannot be found",
				             path);
			break;
			case CDN_PATH_ACTION:
				assert_find (find_action (CDN_NODE (network),
				                          path),
				             "Action `%s' cannot be found",
				             path);
			break;
			default:
			break;
		}

		if (get_objects)
		{
			GObject **retval = va_arg (ap, GObject **);

			if (retval)
			{
				*retval = retobj;
			}
		}
	}
}

static CdnNetwork *
load_network_from_path (gchar const *path,
                        gboolean     get_objects,
                        va_list      ap)
{
	CdnCompileError *err;

	CdnNetwork *network;
	GError *error = NULL;
	gchar *p;

	if (!g_path_is_absolute (path) && g_getenv ("srcdir"))
	{
		p = g_build_filename (g_getenv ("srcdir"), path, NULL);
	}
	else
	{
		p = g_strdup (path);
	}

	network = cdn_network_new_from_path (p, &error);

	g_free (p);

	g_assert_no_error (error);
	g_assert (network != NULL);

	err = cdn_compile_error_new ();
	cdn_object_compile (CDN_OBJECT (network), NULL, err);

	g_assert_no_error (cdn_compile_error_get_error (err));

	check_objects (network, get_objects, ap);

	return network;
}

CdnNetwork *
test_load_network_from_path (gchar const *path,
                             ...)
{
	va_list ap;
	CdnNetwork *ret;

	va_start (ap, path);

	ret = load_network_from_path (path,
	                              FALSE,
	                              ap);

	va_end (ap);
	return ret;
}

CdnNetwork *
test_load_network_from_path_with_objects (gchar const *path,
                                          ...)
{
	va_list ap;
	CdnNetwork *ret;

	va_start (ap, path);

	ret = load_network_from_path (path,
	                              TRUE,
	                              ap);

	va_end (ap);
	return ret;
}

CdnNetwork *
test_load_network (gchar const *xml,
                   ...)
{
	va_list ap;

	CdnNetwork *network;
	GError *error = NULL;
	CdnCompileError *err;

	network = cdn_network_new_from_string (xml, &error);

	g_assert_no_error (error);
	g_assert (network != NULL);

	err = cdn_compile_error_new ();
	cdn_object_compile (CDN_OBJECT (network), NULL, err);

	g_assert_no_error (cdn_compile_error_get_error (err));

	va_start (ap, xml);
	check_objects (network, FALSE, ap);
	va_end (ap);

	return network;
}

static gdouble *
values_from_annotation (gchar const *a, gint *l)
{
	gchar **parts;
	gdouble *ret;
	gint i;
	gint n;

	parts = g_strsplit (a, " ", -1);
	n = g_strv_length (parts);

	ret = g_new0 (gdouble, n);
	*l = 0;

	for (i = 0; i < n; ++i)
	{
		if (parts[i] && *parts[i])
		{
			ret[(*l)++] = g_ascii_strtod (parts[i], NULL);
		}
	}

	g_strfreev (parts);
	return ret;
}

static void
remove_empty (gchar **parts)
{
	gint wr = 0;
	gint rd = 0;

	while (parts[rd] != NULL)
	{
		if (*parts[rd])
		{
			if (wr != rd)
			{
				g_free (parts[wr]);

				parts[wr] = parts[rd];
				parts[rd] = NULL;
			}

			++wr;
		}
		else
		{
			g_free (parts[rd]);
			parts[rd] = NULL;
		}

		++rd;
	}
}

static GSList *
integrate_annotated (CdnNetwork  *network,
                     gboolean    *monitored,
                     GSList     **nonmon)
{
	gchar *a;
	gchar **parts;
	gdouble from, step, to;
	GSList *ret = NULL;
	GSList *variables;

	a = cdn_annotatable_get_annotation (CDN_ANNOTATABLE (network));
	*monitored = FALSE;

	if (!a || !*a)
	{
		g_free (a);
		return NULL;
	}

	*monitored = TRUE;

	parts = g_strsplit (a, ":", 3);

	from = g_ascii_strtod (parts[0], NULL);
	step = g_ascii_strtod (parts[1], NULL);
	to = g_ascii_strtod (parts[2], NULL);

	g_strfreev (parts);

	variables = cdn_node_find_variables (CDN_NODE (network),
	                                     "recurse(children) | variables");

	while (variables)
	{
		CdnVariable *v = variables->data;
		gchar *va;

		va = cdn_annotatable_get_annotation (CDN_ANNOTATABLE (v));

		if (va && g_utf8_strchr (va, -1, '\n') != NULL)
		{
			ret = g_slist_prepend (ret, cdn_monitor_new (network, v));
		}
		else if (va && *va)
		{
			*nonmon = g_slist_prepend (*nonmon, v);
		}

		g_free (va);
		variables = g_slist_delete_link (variables, variables);
	}

	if (ret)
	{
		ret = g_slist_reverse (ret);
		cdn_network_run (network, from, step, to, NULL);
	}

	*nonmon = g_slist_reverse (*nonmon);

	return ret;
}

static void
test_monitor (CdnMonitor  *m,
              gchar const *file,
              gchar const *func,
              gint         line)
{
	CdnVariable *v;
	gchar *va;
	gchar **rows;
	const gdouble *d;
	guint n;
	CdnDimension dim;
	gint nrows;
	gint i;

	v = cdn_monitor_get_variable (m);
	va = cdn_annotatable_get_annotation (CDN_ANNOTATABLE (v));

	rows = g_strsplit (va, "\n", -1);
	remove_empty (rows);

	d = cdn_monitor_get_data (m, &n);
	cdn_variable_get_dimension (v, &dim);

	g_printf ("   - Testing %s%s ... ",
	          cdn_variable_get_full_name_for_display (v),
	          cdn_variable_get_integrated (v) ? "'" : "");

	nrows = (gint)n / cdn_dimension_size (&dim);

	if (nrows != g_strv_length (rows))
	{
		g_printf ("\nFAILED:%s:%d:%s: expected %d rows of values but got %d\n",
		          file,
		          line,
		          func,
		          g_strv_length (rows),
		          nrows);

		abort ();
	}

	for (i = 0; i < nrows; ++i)
	{
		gint l;
		gint c;
		gdouble *expected_vals;

		expected_vals = values_from_annotation (rows[i], &l);

		if (l != cdn_dimension_size (&dim))
		{
			g_printf ("\nFAILED:%s:%d:%s: expected %d values but got %d\n",
			          file,
			          line,
			          func,
			          l,
			          cdn_dimension_size (&dim));

			abort ();
		}

		for (c = 0; c < l; ++c)
		{
			if (!cdn_cmp_tol (*d, expected_vals[c]))
			{
				g_printf ("\nFAILED:%s:%d:%s: expected %g but got %g at [%d, %d]\n",
				          file,
				          line,
				          func,
				          expected_vals[c],
				          *d,
				          i,
				          c);

				abort ();
			}

			d++;
		}
	}

	g_printf ("OK\n");

	g_strfreev (rows);
	g_free (va);
}

void
cdn_test_variables_with_annotated_output_from_path_impl (gchar const *file,
                                                         gchar const *func,
                                                         gint         line,
                                                         gchar const *path)
{
	CdnNetwork *network;
	GError *error = NULL;
	CdnCompileError *err;
	GSList *variables;
	GSList *monitors;
	gboolean monitored;

	network = cdn_network_new_from_path (path, &error);

	g_assert_no_error (error);
	g_assert (network != NULL);

	err = cdn_compile_error_new ();
	cdn_object_compile (CDN_OBJECT (network), NULL, err);

	g_assert_no_error (cdn_compile_error_get_error (err));

	g_printf("\n\n  Testing network %s:\n", path);

	variables = NULL;
	monitors = integrate_annotated (network, &monitored, &variables);

	while (monitors)
	{
		test_monitor (monitors->data, file, func, line);
		monitors = g_slist_delete_link (monitors, monitors);
	}

	while (variables)
	{
		CdnVariable *v = variables->data;
		gchar *a;
		gdouble *expected_vals;
		gint l;
		CdnMatrix const *vals;
		gint i;

		variables = g_slist_delete_link (variables, variables);

		a = cdn_annotatable_get_annotation (CDN_ANNOTATABLE (v));
		expected_vals = values_from_annotation (a, &l);
		g_free (a);

		vals = cdn_variable_get_values (v);

		g_printf ("   - Testing %s ... ", cdn_variable_get_full_name_for_display (v));

		if (cdn_matrix_size (vals) != l)
		{
			g_printf ("\nFAILED:%s:%d:%s: expected %d values but got %d\n",
			          file,
			          line,
			          func,
			          l,
			          cdn_matrix_size (vals));

			abort ();
		}

		for (i = 0; i < l; ++i)
		{
			if (!cdn_cmp_tol (cdn_matrix_get (vals)[i], expected_vals[i]))
			{
				g_printf ("\nFAILED:%s:%d:%s: expected %g but got %g at [%d, %d]\n",
				          file,
				          line,
				          func,
				          expected_vals[i],
				          cdn_matrix_get (vals)[i],
				          i % vals->dimension.rows,
				          i / vals->dimension.rows);

				abort ();
			}
		}

		g_printf ("OK\n");

		g_free (expected_vals);
	}

	g_printf("\n");
}
