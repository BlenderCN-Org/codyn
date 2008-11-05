#ifndef __CPG_EXPRESSION_PRIVATE_H__
#define __CPG_EXPRESSION_PRIVATE_H__

#include "cpg-expression.h"
#include "cpg-mutex.h"

typedef enum
{
	CPG_INSTRUCTION_TYPE_NONE,
	CPG_INSTRUCTION_TYPE_FUNCTION,
	CPG_INSTRUCTION_TYPE_NUMBER,
	CPG_INSTRUCTION_TYPE_OPERATOR,
	CPG_INSTRUCTION_TYPE_PROPERTY
} CpgInstructionType;

typedef struct _CpgInstruction CpgInstruction;

struct _CpgInstruction
{
	CpgInstructionType type;
	CpgInstruction *next;
};

typedef struct
{
	CpgInstruction parent;
	
	CpgFunctionClosure function;
	char *name;
	int arguments;
} CpgInstructionFunction;

typedef struct
{
	CpgInstruction parent;
	
	double value;
} CpgInstructionNumber;

typedef struct
{
	CpgInstruction parent;

	CpgProperty *property;
} CpgInstructionProperty;

struct _CpgExpression
{	
	// Expression to evaluate
	char *expression;
	
	CpgInstruction *instructions;
	double *output_ptr;
	double *output;
	unsigned num_output;
	
	CpgMutex *mutex;
	double cached_output;
	int has_cache;
	int instant;
};

void			cpg_expression_set				(CpgExpression *expression, char const *value);
void 			cpg_expression_reset_cache		(CpgExpression *expression);

#endif /* __CPG_EXPRESSION_PRIVATE_H__ */
