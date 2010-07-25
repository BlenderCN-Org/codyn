#ifndef __CPG_COMPILE_ERROR_H__
#define __CPG_COMPILE_ERROR_H__

#include <glib-object.h>
#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-link.h>

G_BEGIN_DECLS

#define CPG_TYPE_COMPILE_ERROR			(cpg_compile_error_get_type ())
#define CPG_COMPILE_ERROR(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_COMPILE_ERROR, CpgCompileError))
#define CPG_COMPILE_ERROR_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_COMPILE_ERROR, CpgCompileError const))
#define CPG_COMPILE_ERROR_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_COMPILE_ERROR, CpgCompileErrorClass))
#define CPG_IS_COMPILE_ERROR(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_COMPILE_ERROR))
#define CPG_IS_COMPILE_ERROR_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_COMPILE_ERROR))
#define CPG_COMPILE_ERROR_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_COMPILE_ERROR, CpgCompileErrorClass))

typedef struct _CpgCompileError		CpgCompileError;
typedef struct _CpgCompileErrorClass	CpgCompileErrorClass;
typedef struct _CpgCompileErrorPrivate	CpgCompileErrorPrivate;

struct _CpgCompileError
{
	GObject parent;

	CpgCompileErrorPrivate *priv;
};

struct _CpgCompileErrorClass
{
	GObjectClass parent_class;
};

#define CPG_COMPILE_ERROR_TYPE (cpg_compile_error_type_quark ())

/**
 * CpgCompileErrorCode:
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
typedef enum
{
	CPG_COMPILE_ERROR_PROPERTY_NOT_FOUND,
	CPG_COMPILE_ERROR_FUNCTION_NOT_FOUND,
	CPG_COMPILE_ERROR_INVALID_TOKEN,
	CPG_COMPILE_ERROR_MAXARG,
	CPG_COMPILE_ERROR_INVALID_STACK,
	CPG_COMPILE_ERROR_NUM_ERRORS
} CpgCompileErrorCode;

GType            cpg_compile_error_get_type         (void) G_GNUC_CONST;
CpgCompileError *cpg_compile_error_new              (void);

GQuark           cpg_compile_error_type_quark       (void);

void             cpg_compile_error_set              (CpgCompileError *error,
                                                     GError          *gerror,
                                                     CpgObject       *object,
                                                     CpgProperty     *property,
                                                     CpgLinkAction   *action);

GError           *cpg_compile_error_get_error       (CpgCompileError *error);
CpgObject        *cpg_compile_error_get_object      (CpgCompileError *error);
CpgProperty      *cpg_compile_error_get_property    (CpgCompileError *error);
CpgLinkAction    *cpg_compile_error_get_link_action (CpgCompileError *error);

const gchar      *cpg_compile_error_string          (CpgCompileError *error);
const gchar      *cpg_compile_error_code_string     (gint             code);
gint              cpg_compile_error_get_code        (CpgCompileError *error);
const gchar      *cpg_compile_error_get_message     (CpgCompileError *error);

G_END_DECLS

#endif /* __CPG_COMPILE_ERROR_H__ */


