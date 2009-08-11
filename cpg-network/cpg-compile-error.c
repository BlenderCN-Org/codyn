#include "cpg-compile-error.h"
#include "cpg-ref-counted-private.h"

/**
 * SECTION:cpg-compile-error
 * @short_description: Compile error message container
 *
 * Object used to store information on expression compile errors.
 *
 */
 
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
	
	g_slice_free (CpgCompileError, error);
}

GQuark
cpg_compile_error_type_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
		quark = g_quark_from_static_string ("cpg_compile_error_type");

	return quark;
}

/**
 * cpg_compile_error_new:
 *
 * Create new empty compile error
 * 
 * Returns: a new #CpgCompileError
 *
 **/
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

/**
 * cpg_compile_error_get_error:
 * @error: a #CpgCompileError
 *
 * Get the associated #GError
 *
 * Returns: the associated #GError
 *
 **/
GError **
cpg_compile_error_get_error	(CpgCompileError *error)
{
	return &(error->error);
}

/**
 * cpg_compile_error_get_object:
 * @error: a #CpgCompileError
 *
 * Get the associated #CpgObject
 *
 * Returns: the associated #CpgObject
 *
 **/
CpgObject *
cpg_compile_error_get_object (CpgCompileError *error)
{
	return error->object;
}

/**
 * cpg_compile_error_get_property:
 * @error: a #CpgCompileError
 *
 * Get the associated #CpgProperty
 *
 * Returns: the associated #CpgProperty
 *
 **/
CpgProperty	*
cpg_compile_error_get_property (CpgCompileError *error)
{
	return error->property;
}

/**
 * cpg_compile_error_get_link_action:
 * @error: a #CpgCompileError
 *
 * Get the associated #CpgLinkAction
 *
 * Returns: the associated #CpgLinkAction
 *
 **/
CpgLinkAction *
cpg_compile_error_get_link_action (CpgCompileError *error)
{
	return error->action;
}

/**
 * cpg_compile_error_code_string:
 * @code: the error code
 *
 * Get the string describing an error with error code @error
 *
 * Returns: the error string message
 *
 **/
gchar const *
cpg_compile_error_code_string (gint code)
{
	switch (code)
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

/**
 * cpg_compile_error_string:
 * @error: a #CpgCompileError
 *
 * Get the string describing @error
 *
 * Returns: the error string message
 *
 **/
gchar const *
cpg_compile_error_string (CpgCompileError *error)
{
	return cpg_compile_error_code_string (error->error->code);
}

/**
 * cpg_compile_error_get_code:
 * @error: a #CpgCompileError
 *
 * Get the error code
 *
 * Returns: the error code
 *
 **/
gint
cpg_compile_error_get_code (CpgCompileError *error)
{
	return error->error->code;
}

/**
 * cpg_compile_error_get_message:
 * @error: a #CpgCompileError
 *
 * Get the error message
 *
 * Returns: the error message
 *
 **/
gchar const *
cpg_compile_error_get_message (CpgCompileError *error)
{
	return error->error->message;
}
