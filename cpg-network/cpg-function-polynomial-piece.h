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

gdouble const *cpg_function_polynomial_piece_get_coefficients (CpgFunctionPolynomialPiece *piece,
                                                               guint                      *num);

void           cpg_function_polynomial_piece_set_coefficients (CpgFunctionPolynomialPiece *piece,
                                                               gdouble                    *coefficients,
                                                               guint                       num);

gdouble const *cpg_function_polynomial_piece_get_normalization (CpgFunctionPolynomialPiece *piece,
                                                                guint                      *num);

G_END_DECLS

#endif /* __CPG_FUNCTION_POLYNOMIAL_PIECE_H__ */
