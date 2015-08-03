#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_LAPACK
#ifdef PLATFORM_OSX
#include <Accelerate/Accelerate.h>
#else
#include <clapack.h>
#endif
#endif

#ifdef PLATFORM_OSX
#define LP_int __CLPK_integer
#define LP_double __CLPK_doublereal
#else
#define LP_int gint
#define LP_double gdouble
#endif

#include "cdn-math.h"

#ifndef PLATFORM_OSX
extern void dgetrf_ (LP_int *,
                     LP_int *,
                     LP_double *,
                     LP_int *,
                     LP_int *,
                     LP_int *);

extern void dgetri_ (LP_int *,
                     LP_double *,
                     LP_int *,
                     LP_int *,
                     LP_double *,
                     LP_int *,
                     LP_int *);

extern void dgelsd_ (LP_int *,
                     LP_int *,
                     LP_int *,
                     LP_double *,
                     LP_int *,
                     LP_double *,
                     LP_int *,
                     LP_double *,
                     LP_double *,
                     LP_int *,
                     LP_double *,
                     LP_int *,
                     LP_int *,
                     LP_int *);

extern void dgesv_ (LP_int *,
                    LP_int *,
                    LP_double *,
                    LP_int *,
                    LP_int *,
                    LP_double *,
                    LP_int *,
                    LP_int *);

extern void dgeqrf_ (LP_int *,
                     LP_int *,
                     LP_double *,
                     LP_int *,
                     LP_double *,
                     LP_double *,
                     LP_int *,
                     LP_int *);

extern void dorgqr_ (LP_int *,
                     LP_int *,
                     LP_int *,
                     LP_double *,
                     LP_int *,
                     LP_double *,
                     LP_double *,
                     LP_int *,
                     LP_int *);
#endif


void
op_inverse (CdnStack           *stack,
            CdnStackArgs const *argdim,
            gpointer            userdata)
{
	LP_int n;
	LP_double *ptr;
	LP_int *ipiv;
	LP_int nn;
	LP_int info;
	LP_double *work;
	LP_int lwork;

	n = argdim->args[0].rows;
	nn = n * n;

	ptr = cdn_stack_output_ptr (stack) - nn;

	ipiv = (LP_int *)(cdn_stack_output_ptr (stack));
	work = cdn_stack_output_ptr (stack) + n;
	lwork = (cdn_stack_ptr (stack) + cdn_stack_size (stack)) - work;

	dgetrf_ (&n, &n, ptr, &n, ipiv, &info);
	dgetri_ (&n, ptr, &n, ipiv, work, &lwork, &info);
}

static LP_int
pseudo_inverse_iwork (CdnDimension const *dim)
{
	LP_int mindim;
	LP_int nlvl;

	mindim = dim->rows < dim->columns ? dim->rows : dim->columns;
	nlvl = (LP_int)log2(mindim / (25.0 + 1.0)) + 1;

	if (nlvl < 0)
	{
		nlvl = 0;
	}

	return 3 * mindim * nlvl + 11 * mindim;
}

#include <glib/gprintf.h>

void
op_pseudo_inverse (CdnStack           *stack,
                   CdnStackArgs const *argdim,
                   gpointer            userdata)
{
	LP_int n;
	LP_int m;
	LP_double *A;
	LP_double *b;
	LP_double *bptr;
	LP_double *s;
	LP_double *work;
	LP_int *iwork;
	LP_int liwork;
	LP_int i;
	LP_int rank;
	LP_int info;
	LP_int worksize;
	LP_double rcond = -1;
	LP_int maxdim;
	LP_int mindim;
	LP_int lb;

	m = argdim->args[0].rows;
	n = argdim->args[0].columns;

	if (m > n)
	{
		maxdim = m;
		mindim = n;
	}
	else
	{
		maxdim = n;
		mindim = m;
	}

	// Reserved space starts with b
	lb = maxdim;
	b = cdn_stack_output_ptr (stack);

	// Get where the matrix A is
	A = b - (n * m);

	// Then comes s
	s = b + lb * lb;

	// Then comes work and iwork
	liwork = pseudo_inverse_iwork (&argdim->args[0].dimension);
	iwork = (LP_int *)(s + mindim);

	work = s + mindim + liwork;
	worksize = (cdn_stack_ptr (stack) + cdn_stack_size (stack)) - work;

	bptr = b;

	memset (bptr, 0, sizeof (gdouble) * (lb * lb));

	// fill b with identity matrix
	for (i = 0; i < lb; ++i)
	{
		*bptr = 1;
		bptr += lb + 1;
	}

	rcond = 1e-6;

	dgelsd_ (&m,
	         &n,
	         &lb,
	         A,
	         &m,
	         b,
	         &lb,
	         s,
	         &rcond,
	         &rank,
	         work,
	         &worksize,
	         iwork,
	         &info);

	// copy back from b the n-by-m pseudo inverse
	for (i = 0; i < m; ++i)
	{
		memcpy (A, b, sizeof (gdouble) * n);
		A += n;
		b += lb;
	}
}

void
op_linsolve (CdnStack           *stack,
             CdnStackArgs const *argdim,
             gpointer            userdata)
{
	LP_double *ptrA;
	LP_double *ptrB;
	LP_int *ptrIpv;
	LP_int numa;
	LP_int numb;
	LP_int n;
	LP_int nrhs;
	LP_int lda;
	LP_int ldb;
	LP_int info;

	numa = cdn_stack_arg_size (argdim->args);
	numb = cdn_stack_arg_size (argdim->args + 1);

	ptrA = cdn_stack_output_ptr (stack) - numa;
	ptrB = ptrA - numb;

	// Use the extra space we allocated on the stack to write ipv
	ptrIpv = (LP_int *)cdn_stack_output_ptr (stack);

	n = argdim->args[0].rows;
	nrhs = argdim->args[1].columns;
	lda = argdim->args[0].columns;
	ldb = argdim->args[0].rows;

	dgesv_ (&n,
	        &nrhs,
	        ptrA,
	        &lda,
	        ptrIpv,
	        ptrB,
	        &ldb,
	        &info);

	cdn_stack_popn (stack, numa);
}

void
op_qr (CdnStack           *stack,
       CdnStackArgs const *argdim,
       gpointer            userdata)
{
	LP_double *ptrA;
	LP_int m;
	LP_int n;
	LP_int lwork;
	LP_double *tau;
	LP_double *work;
	LP_int mindim;
	LP_int info;
	gdouble *ptrWr;
	gdouble *ptrRd;
	gint numa;
	LP_int i;

	m = argdim->args[0].rows;
	n = argdim->args[0].columns;

	mindim = m < n ? m : n;

	numa = cdn_stack_arg_size (argdim->args);
	ptrA = cdn_stack_output_ptr (stack) - numa;
	tau = cdn_stack_output_ptr (stack) + m * m;

	// remaining work size
	work = tau + mindim;
	lwork = (cdn_stack_ptr (stack) + cdn_stack_size (stack)) - work;

	dgeqrf_ (&m,
	         &n,
	         ptrA,
	         &m,
	         tau,
	         work,
	         &lwork,
	         &info);

	ptrWr = ptrA + m * m;
	ptrRd = ptrA;

	memset (ptrWr, 0, sizeof(gdouble) * m * n);

	// copy r to after A
	for (i = 1; i <= n; ++i)
	{
		memcpy (ptrWr, ptrRd, sizeof(gdouble) * i);

		ptrWr += m;
		ptrRd += m;
	}

	// compute q in ptrA
	dorgqr_ (&m,
	         &m,
	         &n,
	         ptrA,
	         &m,
	         tau,
	         work,
	         &lwork,
	         &info);

	cdn_stack_set_output_ptr (stack, tau);
}

LP_int
inverse_work_space (CdnDimension const *dim)
{
	LP_int n;
	LP_int *ipiv;
	LP_int info;
	LP_double work;
	LP_int lwork;
	LP_double *ptr;

	n = dim->rows;

	ptr = g_new0 (LP_double, n * n);
	ipiv = g_new0 (LP_int, n);
	lwork = -1;

	dgetri_ (&n, ptr, &n, ipiv, &work, &lwork, &info);

	g_free (ptr);
	g_free (ipiv);

	return n + (LP_int)work;
}

LP_int
pseudo_inverse_work_space (CdnDimension const *dim)
{
	LP_int mindim;
	LP_int maxdim;
	LP_int iwork;
	LP_int m;
	LP_int n;
	LP_double rcond = -1;
	LP_int rank;
	LP_double wkopt;
	LP_int lwork = -1;
	LP_int info;
	LP_int lb;
	LP_int *llwork;
	LP_double *A;
	LP_double *b;
	LP_double *s;

	m = dim->rows;
	n = dim->columns;

	if (m < n)
	{
		mindim = m;
		maxdim = n;
	}
	else
	{
		mindim = n;
		maxdim = m;
	}

	iwork = pseudo_inverse_iwork (dim);
	lb = maxdim;

	A = g_new0 (LP_double, m * n);
	b = g_new0 (LP_double, lb * lb);
	s = g_new0 (LP_double, mindim);
	llwork = g_new0 (LP_int, iwork);

	// query workspace
	dgelsd_ (&m,
	         &n,
	         &lb,
	         A,
	         &m,
	         b,
	         &lb,
	         s,
	         &rcond,
	         &rank,
	         &wkopt,
	         &lwork,
	         llwork,
	         &info);

	g_free (A);
	g_free (b);
	g_free (s);
	g_free (llwork);

	// size for b (lb-by-lb), s (mindim) and work (lwork * 2)
	return lb * lb + mindim + (LP_int)wkopt + iwork;
}

LP_int
linsolve_work_space (CdnDimension const *dim)
{
	LP_int n;
	LP_int lda;
	LP_double *A;
	LP_int *ipiv;
	LP_double work;
	LP_int lwork = -1;
	LP_int info;

	ipiv = g_new0 (LP_int, dim->rows);
	A = g_new0 (LP_double, cdn_dimension_size (dim));

	n = dim->rows;
	lda = dim->rows;

	// for pivoting
	dgetri_ (&n,
	         A,
	         &lda,
	         ipiv,
	         &work,
	         &lwork,
	         &info);

	g_free (A);
	g_free (ipiv);

	return (LP_int)work + dim->rows;
}

LP_int
qr_work_space (CdnDimension const *dim)
{
	LP_int m;
	LP_int n;
	LP_double work;
	LP_double *A;
	LP_double *tau;
	LP_int lwork = -1;
	LP_int info;
	LP_int mindim;

	m = dim->rows;
	n = dim->columns;

	mindim = m < n ? m : n;

	A = g_new0 (LP_double, cdn_dimension_size (dim));
	tau = g_new0 (LP_double, mindim);

	dgeqrf_ (&m,
	         &n,
	         A,
	         &m,
	         tau,
	         &work,
	         &lwork,
	         &info);

	g_free (A);
	g_free (tau);

	// space for tau and work
	return mindim + (LP_int)work + dim->rows * dim->rows;
}
