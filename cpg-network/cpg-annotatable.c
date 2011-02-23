#include "cpg-annotatable.h"

G_DEFINE_INTERFACE (CpgAnnotatable, cpg_annotatable, G_TYPE_OBJECT);

static void
cpg_annotatable_default_init (CpgAnnotatableInterface *iface)
{
	static gboolean initialized = FALSE;

	if (!initialized)
	{
		g_object_interface_install_property (iface,
		                                     g_param_spec_string ("annotation",
		                                                          "Annotation",
		                                                          "Object annotation",
		                                                          NULL,
		                                                          G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

		initialized = TRUE;
	}
}

void
cpg_annotatable_set_annotation (CpgAnnotatable *annotatable,
                                gchar const    *annotation)
{
	g_object_set (annotatable, "annotation", annotation, NULL);
}

gchar *
cpg_annotatable_get_annotation (CpgAnnotatable *annotatable)
{
	gchar *annotation = NULL;

	g_object_get (annotatable, "annotation", &annotation, NULL);

	return annotation;
}
