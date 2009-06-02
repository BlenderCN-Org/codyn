#include "cpg-compile-error.h"
#include "cpg-ref-counted-private.h"

struct _CpgCompileError
{
	CpgRefCounted parent;
	
	GError *error;
	
	CpgObject *object;
	
	CpgProperty *property;
	CpgLinkAction *action;
};

GType 
cpg_compile_error_get_type ()
{
	static GType type_id = 0;
	
	if (G_UNLIKELY (type_id == 0))
	{
		type_id = g_boxed_type_register_static ("CpgCompileError", 
		                                        cpg_ref_counted_ref, 
		                                        cpg_ref_counted_unref);
	}

	return type_id;
}

static void
cpg_compile_error_free (CpgCompileError *error)
{
	if (error->error)
	{
		g_error_free (error->error);
	}
	
	if (error->object)
	{
		g_object_unref (error->object);
	}
}

GQuark
cpg_compile_error_type_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
		quark = g_quark_from_static_string ("cpg_compile_error_type");

	return quark;
}

CpgCompileError *
cpg_compile_error_new ()
{
	CpgCompileError *res = g_slice_new0 (CpgCompileError);
	
	cpg_ref_counted_init (res, (GDestroyNotify)cpg_compile_error_free);
	return res;
}

void
_cpg_compile_error_set (CpgCompileError *error,
                        CpgObject       *object,
                        CpgProperty     *property,
                        CpgLinkAction   *action)
{
	if (error->object)
	{
		g_object_unref (error->object);
	}
	
	error->object = g_object_ref (object);
	error->property = property;
	error->action = action;
}

GError **
cpg_compile_error_get_error	(CpgCompileError *error)
{
	return &(error->error);
}

CpgObject *
cpg_compile_error_get_object (CpgCompileError *error)
{
	return error->object;
}

CpgProperty	*
cpg_compile_error_get_property (CpgCompileError *error)
{
	return error->property;
}

CpgLinkAction *
cpg_compile_error_get_link_action (CpgCompileError *error)
{
	return error->action;
}

gchar const *
cpg_compile_error_code_string (gint error)
{
	switch (error)
	{
		case CPG_COMPILE_ERROR_PROPERTY_NOT_FOUND:
			return "Property not found";
		break;
		case CPG_COMPILE_ERROR_FUNCTION_NOT_FOUND:
			return "Function not found";
		break;
		case CPG_COMPILE_ERROR_INVALID_TOKEN:
			return "Invalid token";
		break;
		case CPG_COMPILE_ERROR_MAXARG:
			return "Maximum number of arguments";
		break;
		case CPG_COMPILE_ERROR_INVALID_STACK:
			return "Invalid stack";
		break;
		default:
			return "Unknown";
		break;
	}
}

gchar const *
cpg_compile_error_string (CpgCompileError *error)
{
	return cpg_compile_error_code_string (error->error->code);
}

gint
cpg_compile_error_get_code (CpgCompileError *error)
{
	return error->error->code;
}

gchar const *
cpg_compile_error_get_message (CpgCompileError *error)
{
	return error->error->message;
}
