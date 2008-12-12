#ifndef __CPG_SHARED_EXPRESSION_H__
#define __CPG_SHARED_EXPRESSION_H__

#include <cpg-network/cpg-stack.h>
#include <cpg-network/cpg-types.h>
#include <cpg-network/shared/cpg-shared-utils.h>

typedef struct
{
	CpgInstructionType type;

	unsigned id; /* used for function and operator instruction types */
	double value; /* used for number instruction type */
	CpgSharedPointer property; /* used for property instruction type */
} CpgSharedInstruction;

typedef struct
{
	unsigned num_instructions;
	CpgSharedPointer instructions; /* offset to CpgSharedInstruction */

	CpgStack output;
} CpgSharedExpression;

void cpg_shared_expression_set_value		(CpgSharedExpression *expression,
											 double				  value,
											 void				 *base);

double cpg_shared_expression_evaluate		(CpgSharedExpression *expression,
											 void				 *base);

#endif /* __CPG_SHARED_EXPRESSION_H__ */

