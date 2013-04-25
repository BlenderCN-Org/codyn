#include "utils.h"
#include <codyn/cdn-annotatable.h>
#include <glib/gprintf.h>

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

void
test_variables_with_annotated_output_from_path (gchar const *path)
{
	CdnNetwork *network;
	GError *error = NULL;
	CdnCompileError *err;
	GSList const *variables;

	network = cdn_network_new_from_path (path, &error);

	g_assert_no_error (error);
	g_assert (network != NULL);

	err = cdn_compile_error_new ();
	cdn_object_compile (CDN_OBJECT (network), NULL, err);

	g_assert_no_error (cdn_compile_error_get_error (err));

	variables = cdn_object_get_variables (CDN_OBJECT (network));

	while (variables)
	{
		CdnVariable *v = variables->data;
		gchar *a;
		gdouble *expected_vals;
		gint l;
		CdnMatrix const *vals;
		gint i;

		a = cdn_annotatable_get_annotation (CDN_ANNOTATABLE (v));
		variables = g_slist_next (variables);

		if (!a || !*a)
		{
			g_free (a);
			continue;
		}

		expected_vals = values_from_annotation (a, &l);
		g_free (a);

		vals = cdn_variable_get_values (v);

		g_printf ("Testing %s:%s ... ", path, cdn_variable_get_name (v));

		if (cdn_matrix_size (vals) != l)
		{
			g_printf ("FAILED\n");

			g_error ("Failed running %s:%s, expected %d values but got %d",
			         path,
			         cdn_variable_get_name (v),
			         l,
			         cdn_matrix_size (vals));

			abort ();
		}

		for (i = 0; i < l; ++i)
		{
			if (!cdn_cmp_tol (vals->values[i], expected_vals[i]))
			{
				g_printf ("FAILED\n");

				g_error ("Failed running %s:%s, expected %f but got %f at %d",
				         path,
				         cdn_variable_get_name (v),
				         expected_vals[i],
				         vals->values[i],
				         i + 1);

				abort ();
			}
		}

		g_printf ("OK\n");

		g_free (expected_vals);
	}
}
