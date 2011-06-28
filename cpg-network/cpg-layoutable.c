#include "cpg-layoutable.h"
#include <cpg-network/cpg-network.h>

G_DEFINE_INTERFACE (CpgLayoutable, cpg_layoutable, G_TYPE_OBJECT)

/* Default implementation */
static void
cpg_layoutable_get_location_default (CpgLayoutable *layoutable,
                                     gint          *x,
                                     gint          *y)
{
	g_object_get (layoutable, "x", x, "y", y, NULL);
}

static void
cpg_layoutable_set_location_default (CpgLayoutable *layoutable,
                                     gint           x,
                                     gint           y)
{
	g_object_set (layoutable, "x", x, "y", y, NULL);
}

static gboolean
cpg_layoutable_supports_location_default (CpgLayoutable *layoutable)
{
	return TRUE;
}

static void
cpg_layoutable_default_init (CpgLayoutableInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->get_location = cpg_layoutable_get_location_default;
	iface->set_location = cpg_layoutable_set_location_default;
	iface->supports_location = cpg_layoutable_supports_location_default;

	if (!initialized)
	{
		g_object_interface_install_property (iface,
		                                     g_param_spec_int ("x",
		                                                       "X",
		                                                       "X location",
		                                                       G_MININT,
		                                                       G_MAXINT,
		                                                       0,
		                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

		g_object_interface_install_property (iface,
		                                     g_param_spec_int ("y",
		                                                       "Y",
		                                                       "Y location",
		                                                       G_MININT,
		                                                       G_MAXINT,
		                                                       0,
		                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

		initialized = TRUE;
	}
}

void
cpg_layoutable_get_location (CpgLayoutable *layoutable,
                             gint          *x,
                             gint          *y)
{
	g_return_if_fail (CPG_IS_LAYOUTABLE (layoutable));

	if (cpg_layoutable_supports_location (layoutable))
	{
		CPG_LAYOUTABLE_GET_INTERFACE (layoutable)->get_location (layoutable,
		                                                                x,
		                                                                y);
	}
	else
	{
		if (x)
		{
			*x = 0;
		}

		if (y)
		{
			*y = 0;
		}
	}
}

void
cpg_layoutable_set_location (CpgLayoutable *layoutable,
                             gint           x,
                             gint           y)
{
	g_return_if_fail (CPG_IS_LAYOUTABLE (layoutable));

	if (cpg_layoutable_supports_location (layoutable))
	{
		CPG_LAYOUTABLE_GET_INTERFACE (layoutable)->set_location (layoutable,
		                                                         x,
		                                                         y);
	}
}

gboolean
cpg_layoutable_supports_location (CpgLayoutable *layoutable)
{
	g_return_val_if_fail (CPG_IS_LAYOUTABLE (layoutable), FALSE);

	return CPG_LAYOUTABLE_GET_INTERFACE (layoutable)->supports_location (layoutable);
}
