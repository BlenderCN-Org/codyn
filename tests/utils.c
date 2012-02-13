#include "utils.h"

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
}

static void
check_objects (CdnNetwork *network,
               va_list     ap)
{
	CdnPath type;

	while ((type = va_arg (ap, CdnPath)) != CDN_PATH_NONE)
	{
		gchar const *path = va_arg (ap, gchar const *);

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
	}
}

CdnNetwork *
test_load_network_from_path (gchar const *path,
                             ...)
{
	va_list ap;
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

	va_start (ap, path);
	check_objects (network, ap);
	va_end (ap);

	return network;
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
	check_objects (network, ap);
	va_end (ap);

	return network;
}
