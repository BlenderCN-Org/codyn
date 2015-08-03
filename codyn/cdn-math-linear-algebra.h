#ifndef __CDN_MATH_LINEAR_ALGEBRA_H__
#define __CDN_MATH_LINEAR_ALGEBRA_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif //HAVE_CONFIG_H

#include "cdn-math.h"

#ifdef PLATFORM_OSX
#ifndef HAVE_EIGEN
#include <Accelerate/Accelerate.h>
#define LP_int __CLPK_integer
#define LP_double __CLPK_doublereal
#else  //HAVE_EIGEN
#define LP_int gint
#define LP_double gdouble
#endif //HAVE_EIGEN
#else  //PLATFORM_OSX
#define LP_int gint
#define LP_double gdouble
#endif //PLATFORM_OSX

#ifdef __cplusplus
extern "C" {
#endif //__cplusplus

void matrix_multiply(CdnStack *stack, CdnStackArgs const *argdim);


#if defined(HAVE_LAPACK) || defined(HAVE_EIGEN)

void   op_inverse                (CdnStack           *stack,
                                  CdnStackArgs const *argdim,
                                  gpointer            userdata);
void   op_pseudo_inverse         (CdnStack           *stack,
                                  CdnStackArgs const *argdim,
                                  gpointer            userdata);
void   op_linsolve               (CdnStack           *stack,
                                  CdnStackArgs const *argdim,
                                  gpointer            userdata);
void   op_qr                     (CdnStack           *stack,
                                  CdnStackArgs const *argdim,
                                  gpointer            userdata);

LP_int inverse_work_space        (CdnDimension const *dim);
LP_int pseudo_inverse_work_space (CdnDimension const *dim);
LP_int linsolve_work_space       (CdnDimension const *dim);
LP_int qr_work_space             (CdnDimension const *dim);

#endif

#ifdef __cplusplus
}
#endif //__cplusplus


#endif //__CDN_MATH_LINEAR_ALGEBRA_H__
