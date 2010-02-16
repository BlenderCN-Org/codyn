#ifndef __CPG_EXPRESSION_H__
#define __CPG_EXPRESSION_H__

#include <stdio.h>
#include <glib-object.h>
#include "cpg-compile-context.h"

G_BEGIN_DECLS

#define CPG_TYPE_EXPRESSION	(cpg_expression_get_type())

/* Forward declaration */
struct _CpgProperty;
struct _CpgFunction;

typedef struct _CpgInstruction 		CpgInstruction;

/**
 * CpgInstructionCode:
 * @CPG_INSTRUCTION_TYPE_NONE: none
 * @CPG_INSTRUCTION_TYPE_FUNCTION: function
 * @CPG_INSTRUCTION_TYPE_NUMBER: number
 * @CPG_INSTRUCTION_TYPE_OPERATOR: operator
 * @CPG_INSTRUCTION_TYPE_PROPERTY: property 
 * @CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION: custom function 
 *
 * Enum used to indicate instruction type
 *
 **/
typedef enum {
	CPG_INSTRUCTION_TYPE_NONE,
	CPG_INSTRUCTION_TYPE_FUNCTION,
	CPG_INSTRUCTION_TYPE_NUMBER,
	CPG_INSTRUCTION_TYPE_OPERATOR,
	CPG_INSTRUCTION_TYPE_PROPERTY,
	CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION
} CpgInstructionCode;

struct _CpgInstruction
{
	CpgInstructionCode type;
};

typedef struct
{
	CpgInstruction parent;
	
	guint id;
	gchar *name;
	gint arguments;
	gboolean variable;
} CpgInstructionFunction;

typedef struct
{
	CpgInstruction parent;
	
	struct _CpgFunction *function;

	gint arguments;
} CpgInstructionCustomFunction;

typedef struct
{
	CpgInstruction parent;
	
	gdouble value;
} CpgInstructionNumber;

typedef enum
{
	CPG_INSTRUCTION_BINDING_NONE = 0,
	CPG_INSTRUCTION_BINDING_FROM,
	CPG_INSTRUCTION_BINDING_TO,
} CpgInstructionBinding;

typedef struct _CpgInstructionProperty CpgInstructionProperty;

struct _CpgInstructionProperty
{
	CpgInstruction parent;

	struct _CpgProperty *property;
	CpgInstructionBinding binding;
};

typedef struct _CpgExpression 		CpgExpression;

GType			  cpg_expression_get_type			(void);
CpgExpression 	 *cpg_expression_new				(gchar const    *expression);

GSList		 	 *cpg_expression_get_dependencies	(CpgExpression  *expression);
const gchar      *cpg_expression_get_as_string		(CpgExpression  *expression);
gint			  cpg_expression_compile			(CpgExpression  *expression, 
													 CpgCompileContext *context,
													 GError        **error);

gdouble 		  cpg_expression_evaluate			(CpgExpression  *expression);
void			  cpg_expression_set_value			(CpgExpression  *expression, 
													 gdouble         value);
void			  cpg_expression_reset				(CpgExpression  *expression);

gboolean		  cpg_expression_equal				(CpgExpression  *expression,
													 CpgExpression  *other);

void			  cpg_expression_set_from_string	(CpgExpression  *expression, 
													 gchar const    *value);
void 			  cpg_expression_reset_cache		(CpgExpression  *expression);

/* Instructions */
GSList 			 *cpg_expression_get_instructions	(CpgExpression  *expression);
gboolean          cpg_expression_set_instructions   (CpgExpression  *expression,
                                                     GSList         *instructions);

CpgInstruction   *cpg_instruction_function_new 		(guint         id,
													 gchar const  *name,
													 gint          arguments,
													 gboolean      variable);

CpgInstruction   *cpg_instruction_custom_function_new (struct _CpgFunction *function,
													   gint                 arguments);

CpgInstruction   *cpg_instruction_number_new 		(gdouble value);
CpgInstruction   *cpg_instruction_operator_new 		(guint         id,
													 gchar const  *name,
													 gint          arguments);
CpgInstruction   *cpg_instruction_property_new 		(struct _CpgProperty *property,
                                                     CpgInstructionBinding binding);
CpgInstruction   *cpg_instruction_copy 				(CpgInstruction *instruction);
void              cpg_instruction_free 				(CpgInstruction *instruction);

gchar			 *cpg_instruction_to_string			(CpgInstruction *instruction);

G_END_DECLS

#endif /* __CPG_EXPRESSION_H__ */
