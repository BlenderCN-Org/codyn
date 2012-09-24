/*
 * cdn-function-polynomial-piece.h
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CDN_FUNCTION_POLYNOMIAL_PIECE_H__
#define __CDN_FUNCTION_POLYNOMIAL_PIECE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CDN_TYPE_FUNCTION_POLYNOMIAL_PIECE		(cdn_function_polynomial_piece_get_type ())
#define CDN_FUNCTION_POLYNOMIAL_PIECE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_FUNCTION_POLYNOMIAL_PIECE, CdnFunctionPolynomialPiece))
#define CDN_FUNCTION_POLYNOMIAL_PIECE_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_FUNCTION_POLYNOMIAL_PIECE, CdnFunctionPolynomialPiece const))
#define CDN_FUNCTION_POLYNOMIAL_PIECE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_FUNCTION_POLYNOMIAL_PIECE, CdnFunctionPolynomialPieceClass))
#define CDN_IS_FUNCTION_POLYNOMIAL_PIECE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_FUNCTION_POLYNOMIAL_PIECE))
#define CDN_IS_FUNCTION_POLYNOMIAL_PIECE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_FUNCTION_POLYNOMIAL_PIECE))
#define CDN_FUNCTION_POLYNOMIAL_PIECE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_FUNCTION_POLYNOMIAL_PIECE, CdnFunctionPolynomialPieceClass))

typedef struct _CdnFunctionPolynomialPiece		CdnFunctionPolynomialPiece;
typedef struct _CdnFunctionPolynomialPieceClass		CdnFunctionPolynomialPieceClass;
typedef struct _CdnFunctionPolynomialPiecePrivate	CdnFunctionPolynomialPiecePrivate;

struct _CdnFunctionPolynomialPiece
{
	GInitiallyUnowned parent;

	CdnFunctionPolynomialPiecePrivate *priv;
};

struct _CdnFunctionPolynomialPieceClass
{
	GInitiallyUnownedClass parent_class;
};

GType cdn_function_polynomial_piece_get_type (void) G_GNUC_CONST;

CdnFunctionPolynomialPiece *
               cdn_function_polynomial_piece_new              (gdouble                     begin,
                                                               gdouble                     end,
                                                               gdouble const              *coefficients,
                                                               guint                       num_coefficients);

CdnFunctionPolynomialPiece *
               cdn_function_polynomial_piece_copy             (CdnFunctionPolynomialPiece *piece);

gboolean       cdn_function_polynomial_piece_equal            (CdnFunctionPolynomialPiece *a,
                                                               CdnFunctionPolynomialPiece *b);

gdouble        cdn_function_polynomial_piece_get_begin         (CdnFunctionPolynomialPiece *piece);
gdouble        cdn_function_polynomial_piece_get_end           (CdnFunctionPolynomialPiece *piece);

void           cdn_function_polynomial_piece_set_begin         (CdnFunctionPolynomialPiece *piece,
                                                                gdouble                     begin);
void           cdn_function_polynomial_piece_set_end           (CdnFunctionPolynomialPiece *piece,
                                                                gdouble                     end);

const gdouble *cdn_function_polynomial_piece_get_coefficients  (CdnFunctionPolynomialPiece *piece,
                                                                guint                      *num);

void           cdn_function_polynomial_piece_set_coefficients  (CdnFunctionPolynomialPiece *piece,
                                                                gdouble                    *coefficients,
                                                                guint                       num);

gdouble        cdn_function_polynomial_piece_evaluate          (CdnFunctionPolynomialPiece *piece,
                                                                gdouble                     t);

CdnFunctionPolynomialPiece *
               cdn_function_polynomial_piece_get_derivative    (CdnFunctionPolynomialPiece *piece,
                                                                gint                        order);

G_END_DECLS

#endif /* __CDN_FUNCTION_POLYNOMIAL_PIECE_H__ */
