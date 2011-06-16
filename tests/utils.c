#include "utils.h"

CpgLinkAction *
find_action (CpgGroup    *parent,
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
	CpgObject *object;

	object = cpg_group_find_object (CPG_GROUP (parent), copy);

	if (!object || !CPG_IS_LINK (object))
	{
		g_free (copy);
		return NULL;
	}

	CpgLinkAction *ret = cpg_link_get_action (CPG_LINK (object), ptr + 1);
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
check_objects (CpgNetwork *network,
               va_list     ap)
{
	CpgPath type;

	while ((type = va_arg (ap, CpgPath)) != CPG_PATH_NONE)
	{
		gchar const *path = va_arg (ap, gchar const *);

		switch (type)
		{
			case CPG_PATH_OBJECT:
				assert_find (cpg_group_find_object (CPG_GROUP (network),
				                                    path),
				             "Object `%s' cannot be found",
				             path);
			break;
			case CPG_PATH_TEMPLATE_OBJECT:
				assert_find (cpg_group_find_object (cpg_network_get_template_group (network),
				                                    path),
				             "Template object `%s' cannot be found",
				             path);
			break;
			case CPG_PATH_TEMPLATE_PROPERTY:
				assert_find (cpg_group_find_property (cpg_network_get_template_group (network),
				                                    path),
				             "Template property `%s' cannot be found",
				             path);
			break;
			case CPG_PATH_PROPERTY:
				assert_find (cpg_group_find_property (CPG_GROUP (network),
				                                      path),
				             "Property `%s' cannot be found",
				             path);
			break;
			case CPG_PATH_ACTION:
				assert_find (find_action (CPG_GROUP (network),
				                          path),
				             "Action `%s' cannot be found",
				             path);
			break;
			default:
			break;
		}
	}
}

CpgNetwork *
test_load_network_from_path (gchar const *path,
                             ...)
{
	va_list ap;

	CpgNetwork *network;
	GError *error = NULL;

	network = cpg_network_new_from_path (path, &error);

	g_assert (network != NULL);
	g_assert_no_error (error);

	g_assert (cpg_object_compile (CPG_OBJECT (network), NULL, NULL));

	va_start (ap, path);
	check_objects (network, ap);
	va_end (ap);

	return network;
}

CpgNetwork *
test_load_network (gchar const *xml,
                   ...)
{
	va_list ap;

	CpgNetwork *network;
	GError *error = NULL;

	network = cpg_network_new_from_string (xml, &error);

	g_assert (network != NULL);
	g_assert_no_error (error);

	g_assert (cpg_object_compile (CPG_OBJECT (network), NULL, NULL));

	va_start (ap, xml);
	check_objects (network, ap);
	va_end (ap);

	return network;
}
