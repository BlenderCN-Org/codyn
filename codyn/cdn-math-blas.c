#include "cdn-math-linear-algebra.h"

#ifdef PLATFORM_OSX
#include <Accelerate/Accelerate.h>
#else  //PLATFORM_OSX
#include <cblas.h>
#include <string.h>
#endif //PLATFORM_OSX

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

	ptrC = cdn_stack_output_ptr (stack);
	ptrB = ptrC - num2;
	ptrA = ptrB - num1;

	cblas_dgemm (CblasColMajor,
	             CblasNoTrans,
	             CblasNoTrans,
	             argdim->args[1].rows,
	             argdim->args[0].columns,
	             argdim->args[1].columns,
	             1,
	             ptrA,
	             argdim->args[1].rows,
	             ptrB,
	             argdim->args[0].rows,
	             0,
	             ptrC,
	             argdim->args[1].rows);
	

	memmove (ptrA, ptrC, sizeof (gdouble) * numend);

	cdn_stack_set_output_ptr (stack,
	                          ptrA + numend);

}
