/*
 * cdn-function-polynomial.h
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with codyn; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CDN_FUNCTION_POLYNOMIAL_H__
#define __CDN_FUNCTION_POLYNOMIAL_H__

#include <codyn/cdn-function.h>
#include <codyn/cdn-function-polynomial-piece.h>

G_BEGIN_DECLS

#define CDN_TYPE_FUNCTION_POLYNOMIAL			(cdn_function_polynomial_get_type ())
#define CDN_FUNCTION_POLYNOMIAL(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_FUNCTION_POLYNOMIAL, CdnFunctionPolynomial))
#define CDN_FUNCTION_POLYNOMIAL_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_FUNCTION_POLYNOMIAL, CdnFunctionPolynomial const))
#define CDN_FUNCTION_POLYNOMIAL_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_FUNCTION_POLYNOMIAL, CdnFunctionPolynomialClass))
#define CDN_IS_FUNCTION_POLYNOMIAL(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_FUNCTION_POLYNOMIAL))
#define CDN_IS_FUNCTION_POLYNOMIAL_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_FUNCTION_POLYNOMIAL))
#define CDN_FUNCTION_POLYNOMIAL_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_FUNCTION_POLYNOMIAL, CdnFunctionPolynomialClass))

typedef struct _CdnFunctionPolynomial			CdnFunctionPolynomial;
typedef struct _CdnFunctionPolynomialClass		CdnFunctionPolynomialClass;
typedef struct _CdnFunctionPolynomialPrivate	CdnFunctionPolynomialPrivate;

/**
 * CdnFunctionPolynomial:
 *
 * Custom user defined piecewise polynomial.
 *
 * This class provides a specialized custom user function which defines
 * and evaluates piecewise polynomials. The piece polynomials can be
 * specified in terms of the interval in which they are evaluated and
 * the polynomial coefficients.
 */
struct _CdnFunctionPolynomial
{
	/*< private >*/
	CdnFunction parent;

	CdnFunctionPolynomialPrivate *priv;
};

struct _CdnFunctionPolynomialClass
{
	/*< private >*/
	CdnFunctionClass parent_class;

	/*< public >*/

	/* signals */
	void (*piece_added)   (CdnFunctionPolynomial      *polynomial,
	                       CdnFunctionPolynomialPiece *piece);

	void (*piece_removed) (CdnFunctionPolynomial      *polynomial,
	                       CdnFunctionPolynomialPiece *piece);
};

GType         cdn_function_polynomial_get_type     (void) G_GNUC_CONST;

CdnFunctionPolynomial *
              cdn_function_polynomial_new          (const gchar                *name);


gboolean      cdn_function_polynomial_add          (CdnFunctionPolynomial      *function,
                                                    CdnFunctionPolynomialPiece *piece);

gboolean      cdn_function_polynomial_remove       (CdnFunctionPolynomial      *function,
                                                    CdnFunctionPolynomialPiece *piece);

void          cdn_function_polynomial_clear_pieces (CdnFunctionPolynomial      *function);

const GSList *cdn_function_polynomial_get_pieces   (CdnFunctionPolynomial      *function);

G_END_DECLS

#endif /* __CDN_FUNCTION_POLYNOMIAL_H__ */
