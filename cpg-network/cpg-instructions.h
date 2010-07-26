#ifndef __CPG_INSTRUCTIONS_H__
#define __CPG_INSTRUCTIONS_H__

#include <glib.h>
#include <cpg-network/cpg-math.h>
#include <cpg-network/cpg-property.h>
#include <cpg-network/cpg-function.h>

G_BEGIN_DECLS

/* Forward declaration */
typedef struct _CpgInstruction                 CpgInstruction;
typedef struct _CpgInstructionProperty         CpgInstructionProperty;
typedef struct _CpgInstructionFunction         CpgInstructionFunction;
typedef struct _CpgInstructionVariadicFunction CpgInstructionVariadicFunction;
typedef struct _CpgInstructionCustomFunction   CpgInstructionCustomFunction;
typedef struct _CpgInstructionNumber           CpgInstructionNumber;

#define CPG_INSTRUCTION(x)                   ((CpgInstruction *)x)
#define CPG_INSTRUCTION_PROPERTY(x)          ((CpgInstructionProperty *)x)
#define CPG_INSTRUCTION_FUNCTION(x)          ((CpgInstructionFunction *)x)
#define CPG_INSTRUCTION_VARIADIC_FUNCTION(x) ((CpgInstructionVariadicFunction *)x)
#define CPG_INSTRUCTION_CUSTOM_FUNCTION(x)   ((CpgInstructionCustomFunction *)x)
#define CPG_INSTRUCTION_NUMBER(x)            ((CpgInstructionNumber *)x)

/**
 * CpgInstructionType:
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
typedef enum
{
	CPG_INSTRUCTION_TYPE_NONE,
	CPG_INSTRUCTION_TYPE_FUNCTION,
	CPG_INSTRUCTION_TYPE_NUMBER,
	CPG_INSTRUCTION_TYPE_OPERATOR,
	CPG_INSTRUCTION_TYPE_PROPERTY,
	CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION,
	CPG_INSTRUCTION_TYPE_VARIADIC_FUNCTION
} CpgInstructionType;

/**
 * CpgInstruction:
 * @type: the instruction type
 *
 * The base instruction. All other instructions are derived from this.
 *
 */
struct _CpgInstruction
{
	CpgInstructionType type;
};

/**
 * CpgInstructionFunction:
 * @id: The function/operator id (see #cpg_math_function_lookup)
 * @name: The function/operator name
 * @arguments: The number of arguments the function receives
 * @variable: Whether the function is called with a variable number of
 *            arguments
 *
 * The instruction class for %CPG_INSTRUCTION_TYPE_FUNCTION. Note: this
 * instruction is used both for functions and for operators!
 *
 */
struct _CpgInstructionFunction
{
	/*< private >*/
	CpgInstruction parent;

	/*< public >*/
	guint id;
	gchar *name;
	gint arguments;
	gboolean variable;
};

struct _CpgInstructionVariadicFunction
{
	/*< private >*/
	CpgInstructionFunction parent;

	/*< public >*/
	gboolean cached;
	gdouble cached_result;
};

/**
 * CpgInstructionCustomFunction:
 * @function: the custom function
 * @arguments: the number of arguments the function receives
 *
 * The instruction class for %CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION
 *
 */
struct _CpgInstructionCustomFunction
{
	/*< private >*/
	CpgInstruction parent;

	/*< public >*/
	CpgFunction *function;
	gint arguments;
};

/**
 * CpgInstructionNumber:
 * @value: the numeric value
 *
 * The instruction class for %CPG_INSTRUCTION_TYPE_NUMBER
 *
 */
struct _CpgInstructionNumber
{
	/*< private >*/
	CpgInstruction parent;

	/*< public >*/
	gdouble value;
};

/**
 * CpgInstructionBinding:
 * @CPG_INSTRUCTION_BINDING_NONE: none
 * @CPG_INSTRUCTION_BINDING_FROM: from
 * @CPG_INSTRUCTION_BINDING_TO: to
 *
 * Enum used to indicate how the property in a #CpgInstructionProperty
 * was bound when the instruction was compiled.
 *
 **/
typedef enum
{
	CPG_INSTRUCTION_BINDING_NONE = 0,
	CPG_INSTRUCTION_BINDING_FROM,
	CPG_INSTRUCTION_BINDING_TO,
} CpgInstructionBinding;

/**
 * CpgInstructionProperty:
 * @property: the property
 * @binding: the property binding
 *
 * The instruction class for %CPG_INSTRUCTION_TYPE_PROPERTY.
 *
 */
struct _CpgInstructionProperty
{
	/*< private >*/
	CpgInstruction parent;

	/*< public >*/
	CpgProperty *property;
	CpgInstructionBinding binding;
};

CpgInstruction *cpg_instruction_function_new        (guint        id,
                                                     const gchar *name,
                                                     gint         arguments,
                                                     gboolean     variable);

CpgInstruction *cpg_instruction_variadic_function_new (guint        id,
                                                       const gchar *name,
                                                       gint         arguments,
                                                       gboolean     variable);

CpgInstruction *cpg_instruction_custom_function_new (CpgFunction *function,
                                                     gint         arguments);

CpgInstruction *cpg_instruction_number_new          (gdouble      value);
CpgInstruction *cpg_instruction_operator_new        (guint        id,
                                                     const gchar *name,
                                                     gint         arguments);

CpgInstruction *cpg_instruction_property_new        (CpgProperty           *property,
                                                     CpgInstructionBinding  binding);

CpgInstruction *cpg_instruction_copy                (CpgInstruction *instruction);
void            cpg_instruction_free                (CpgInstruction *instruction);

gchar          *cpg_instruction_to_string           (CpgInstruction *instruction);

G_END_DECLS

#endif /* __CPG_INSTRUCTIONS_H__ */

