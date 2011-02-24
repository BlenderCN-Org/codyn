#include "cpg-annotatable.h"

G_DEFINE_INTERFACE (CpgAnnotatable, cpg_annotatable, G_TYPE_OBJECT);

static gchar *
cpg_annotatable_get_title_default (CpgAnnotatable *annotatable)
{
	return NULL;
}

static gchar *
cpg_annotatable_get_annotation_default (CpgAnnotatable *annotatable)
{
	gchar *annotation;

	g_object_get (annotatable, "annotation", &annotation, NULL);

	return annotation;
}

static void
cpg_annotatable_set_annotation_default (CpgAnnotatable *annotatable,
                                        gchar const    *annotation)
{
	g_object_set (annotatable, "annotation", annotation, NULL);
}

static void
cpg_annotatable_default_init (CpgAnnotatableInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->get_title = cpg_annotatable_get_title_default;
	iface->set_annotation = cpg_annotatable_set_annotation_default;
	iface->get_annotation = cpg_annotatable_get_annotation_default;

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

gchar *
cpg_annotatable_get_title (CpgAnnotatable *annotatable)
{
	g_return_val_if_fail (CPG_IS_ANNOTATABLE (annotatable), NULL);

	return CPG_ANNOTATABLE_GET_INTERFACE (annotatable)->get_title (annotatable);
}

void
cpg_annotatable_set_annotation (CpgAnnotatable *annotatable,
                                gchar const    *annotation)
{
	g_return_if_fail (CPG_IS_ANNOTATABLE (annotatable));

	CPG_ANNOTATABLE_GET_INTERFACE (annotatable)->set_annotation (annotatable,
	                                                             annotation);
}

gchar *
cpg_annotatable_get_annotation (CpgAnnotatable *annotatable)
{
	g_return_val_if_fail (CPG_IS_ANNOTATABLE (annotatable), NULL);

	return CPG_ANNOTATABLE_GET_INTERFACE (annotatable)->get_annotation (annotatable);
}
