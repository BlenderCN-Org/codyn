#ifndef __CPG_COMPILE_ERROR_H__
#define __CPG_COMPILE_ERROR_H__

#include <glib-object.h>
#include <cpg-network/cpg-ref-counted.h>
#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-link.h>

#define CPG_TYPE_COMPILE_ERROR (cpg_compile_error_get_type ())
#define CPG_COMPILE_ERROR_TYPE (cpg_compile_error_type_quark ())

/**
 * CpgCompileErrorType:
 * @CPG_COMPILE_ERROR_PROPERTY_NOT_FOUND: property not found
 * @CPG_COMPILE_ERROR_FUNCTION_NOT_FOUND: function not found
 * @CPG_COMPILE_ERROR_INVALID_TOKEN: invalid token
 * @CPG_COMPILE_ERROR_MAXARG: maximum number of arguments exceeded
 * @CPG_COMPILE_ERROR_INVALID_STACK: invalid stack produced
 * @CPG_COMPILE_ERROR_NUM_ERRORS: num errors
 *
 * Enum used to indicate the type of compile error
 *
 **/
typedef enum {
	CPG_COMPILE_ERROR_PROPERTY_NOT_FOUND,
	CPG_COMPILE_ERROR_FUNCTION_NOT_FOUND,
	CPG_COMPILE_ERROR_INVALID_TOKEN,
	CPG_COMPILE_ERROR_MAXARG,
	CPG_COMPILE_ERROR_INVALID_STACK,
	CPG_COMPILE_ERROR_NUM_ERRORS 
} CpgCompileErrorType;

typedef struct _CpgCompileError CpgCompileError;

GQuark			  cpg_compile_error_type_quark			(void);

GType cpg_compile_error_get_type						(void);

CpgCompileError *cpg_compile_error_new					();
void _cpg_compile_error_set								(CpgCompileError *error,
														 CpgObject       *object,
														 CpgProperty     *property,
														 CpgLinkAction   *action);

GError 			**cpg_compile_error_get_error			(CpgCompileError *error);
CpgObject 		 *cpg_compile_error_get_object			(CpgCompileError *error);
CpgProperty		 *cpg_compile_error_get_property		(CpgCompileError *error);
CpgLinkAction 	 *cpg_compile_error_get_link_action		(CpgCompileError *error);

gchar const 	 *cpg_compile_error_string				(CpgCompileError *error);
gchar const 	 *cpg_compile_error_code_string			(gint             code);
gint			  cpg_compile_error_get_code            (CpgCompileError *error);
gchar const      *cpg_compile_error_get_message         (CpgCompileError *error);

#endif /* __CPG_COMPILE_ERROR_H__ */


