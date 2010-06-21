#ifndef __CPG_INSTRUCTIONS_H__
#define __CPG_INSTRUCTIONS_H__

#include <glib.h>
#include <cpg-network/cpg-math.h>
#include <cpg-network/cpg-property.h>
#include <cpg-network/cpg-function.h>

G_BEGIN_DECLS

/* Forward declaration */
typedef struct _CpgInstruction CpgInstruction;

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
typedef enum
{
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

	CpgFunction *function;

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

	CpgProperty *property;
	CpgInstructionBinding binding;
};

CpgInstruction *cpg_instruction_function_new        (guint        id,
                                                     gchar const *name,
                                                     gint         arguments,
                                                     gboolean     variable);

CpgInstruction *cpg_instruction_custom_function_new (CpgFunction *function,
                                                     gint         arguments);

CpgInstruction *cpg_instruction_number_new          (gdouble      value);
CpgInstruction *cpg_instruction_operator_new        (guint        id,
                                                     gchar const *name,
                                                     gint         arguments);

CpgInstruction *cpg_instruction_property_new        (CpgProperty           *property,
                                                     CpgInstructionBinding  binding);

CpgInstruction *cpg_instruction_copy                (CpgInstruction *instruction);
void            cpg_instruction_free                (CpgInstruction *instruction);

gchar          *cpg_instruction_to_string           (CpgInstruction *instruction);

G_END_DECLS

#endif /* __CPG_INSTRUCTIONS_H__ */

