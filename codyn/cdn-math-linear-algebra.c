#include "cdn-math-linear-algebra.h"

#include <string.h>

void
matrix_multiply (CdnStack           *stack,
                 CdnStackArgs const *argdim)
{
	gdouble *ptrA;
	gdouble *ptrB;
	gdouble *ptrC;

	gint num1 = argdim->args[1].rows * argdim->args[1].columns;
	gint num2 = argdim->args[0].rows * argdim->args[0].columns;
	gint numend = argdim->args[1].rows * argdim->args[0].columns;

	gint c;
	gint wr = 0;
	gint bc = 0;
	gint i;
	
	ptrC = cdn_stack_output_ptr (stack);
	ptrB = ptrC - num2;
	ptrA = ptrB - num1;

	// Naive implementation
	for (c = 0; c < argdim->args[0].columns; ++c)
	{
		gint r;

		for (r = 0; r < argdim->args[1].rows; ++r)
		{
			gdouble s = 0;
			gint ar = r;

			// dot product of row <r> in <A> and column <c> in <B>
			for (i = 0; i < argdim->args[0].rows; ++i)
			{
				s += ptrA[ar + i] * ptrB[bc + i];
				ar += argdim->args[1].rows;
			}

			ptrC[wr] = s;
		}

		bc += argdim->args[0].rows;
	}

	memmove (ptrA, ptrC, sizeof (gdouble) * numend);

	cdn_stack_set_output_ptr (stack,
	                          ptrA + numend);
}
