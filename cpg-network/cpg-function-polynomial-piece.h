/*
 * cpg-function-polynomial-piece.h
 * This file is part of cpg-network
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

#ifndef __CPG_FUNCTION_POLYNOMIAL_PIECE_H__
#define __CPG_FUNCTION_POLYNOMIAL_PIECE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_FUNCTION_POLYNOMIAL_PIECE		(cpg_function_polynomial_piece_get_type ())
#define CPG_FUNCTION_POLYNOMIAL_PIECE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_FUNCTION_POLYNOMIAL_PIECE, CpgFunctionPolynomialPiece))
#define CPG_FUNCTION_POLYNOMIAL_PIECE_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_FUNCTION_POLYNOMIAL_PIECE, CpgFunctionPolynomialPiece const))
#define CPG_FUNCTION_POLYNOMIAL_PIECE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_FUNCTION_POLYNOMIAL_PIECE, CpgFunctionPolynomialPieceClass))
#define CPG_IS_FUNCTION_POLYNOMIAL_PIECE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_FUNCTION_POLYNOMIAL_PIECE))
#define CPG_IS_FUNCTION_POLYNOMIAL_PIECE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_FUNCTION_POLYNOMIAL_PIECE))
#define CPG_FUNCTION_POLYNOMIAL_PIECE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_FUNCTION_POLYNOMIAL_PIECE, CpgFunctionPolynomialPieceClass))

typedef struct _CpgFunctionPolynomialPiece		CpgFunctionPolynomialPiece;
typedef struct _CpgFunctionPolynomialPieceClass		CpgFunctionPolynomialPieceClass;
typedef struct _CpgFunctionPolynomialPiecePrivate	CpgFunctionPolynomialPiecePrivate;

struct _CpgFunctionPolynomialPiece
{
	GInitiallyUnowned parent;

	CpgFunctionPolynomialPiecePrivate *priv;
};

struct _CpgFunctionPolynomialPieceClass
{
	GInitiallyUnownedClass parent_class;
};

GType cpg_function_polynomial_piece_get_type (void) G_GNUC_CONST;

CpgFunctionPolynomialPiece *
               cpg_function_polynomial_piece_new              (gdouble                     begin,
                                                               gdouble                     end,
                                                               gdouble                    *coefficients,
                                                               guint                       num_coefficients);

CpgFunctionPolynomialPiece *
               cpg_function_polynomial_piece_copy             (CpgFunctionPolynomialPiece *piece);

gdouble        cpg_function_polynomial_piece_get_begin        (CpgFunctionPolynomialPiece *piece);
gdouble        cpg_function_polynomial_piece_get_end          (CpgFunctionPolynomialPiece *piece);

void           cpg_function_polynomial_piece_set_begin        (CpgFunctionPolynomialPiece *piece,
                                                               gdouble                     begin);
void           cpg_function_polynomial_piece_set_end          (CpgFunctionPolynomialPiece *piece,
                                                               gdouble                     end);

const gdouble *cpg_function_polynomial_piece_get_coefficients (CpgFunctionPolynomialPiece *piece,
                                                               guint                      *num);

void           cpg_function_polynomial_piece_set_coefficients (CpgFunctionPolynomialPiece *piece,
                                                               gdouble                    *coefficients,
                                                               guint                       num);

const gdouble *cpg_function_polynomial_piece_get_normalization (CpgFunctionPolynomialPiece *piece,
                                                                guint                      *num);

G_END_DECLS

#endif /* __CPG_FUNCTION_POLYNOMIAL_PIECE_H__ */
