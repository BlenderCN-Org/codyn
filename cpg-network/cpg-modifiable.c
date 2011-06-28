#include "cpg-modifiable.h"

G_DEFINE_INTERFACE (CpgModifiable, cpg_modifiable, G_TYPE_OBJECT);

/* Default implementation */
static gboolean
cpg_modifiable_get_modified_default (CpgModifiable *modifiable)
{
	gboolean ret = FALSE;

	g_object_get (modifiable, "modified", &ret, NULL);
	return ret;
}

static void
cpg_modifiable_set_modified_default (CpgModifiable *modifiable,
                                     gboolean       modified)
{
	gboolean orig = cpg_modifiable_get_modified (modifiable);

	if (orig != modified)
	{
		g_object_set (modifiable, "modified", modified, NULL);
	}
}

static void
cpg_modifiable_default_init (CpgModifiableInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->get_modified = cpg_modifiable_get_modified_default;
	iface->set_modified = cpg_modifiable_set_modified_default;

	if (!initialized)
	{
		g_object_interface_install_property (iface,
		                                     g_param_spec_boolean ("modified",
		                                                           "Modified",
		                                                           "Whether the object is modified",
		                                                           FALSE,
		                                                           G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

		initialized = TRUE;
	}
}

gboolean
cpg_modifiable_get_modified (CpgModifiable *modifiable)
{
	g_return_val_if_fail (CPG_MODIFIABLE (modifiable), FALSE);

	return CPG_MODIFIABLE_GET_INTERFACE (modifiable)->get_modified (modifiable);
}

void
cpg_modifiable_set_modified (CpgModifiable *modifiable,
                             gboolean       modified)
{
	g_return_if_fail (CPG_MODIFIABLE (modifiable));

	CPG_MODIFIABLE_GET_INTERFACE (modifiable)->set_modified (modifiable,
	                                                         modified ? TRUE : FALSE);
}
