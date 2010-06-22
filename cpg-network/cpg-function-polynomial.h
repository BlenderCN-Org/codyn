#ifndef __CPG_FUNCTION_POLYNOMIAL_H__
#define __CPG_FUNCTION_POLYNOMIAL_H__

#include <cpg-network/cpg-function.h>

G_BEGIN_DECLS

#define CPG_TYPE_FUNCTION_POLYNOMIAL			(cpg_function_polynomial_get_type ())
#define CPG_FUNCTION_POLYNOMIAL(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_FUNCTION_POLYNOMIAL, CpgFunctionPolynomial))
#define CPG_FUNCTION_POLYNOMIAL_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_FUNCTION_POLYNOMIAL, CpgFunctionPolynomial const))
#define CPG_FUNCTION_POLYNOMIAL_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_FUNCTION_POLYNOMIAL, CpgFunctionPolynomialClass))
#define CPG_IS_FUNCTION_POLYNOMIAL(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_FUNCTION_POLYNOMIAL))
#define CPG_IS_FUNCTION_POLYNOMIAL_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_FUNCTION_POLYNOMIAL))
#define CPG_FUNCTION_POLYNOMIAL_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_FUNCTION_POLYNOMIAL, CpgFunctionPolynomialClass))

typedef struct _CpgFunctionPolynomial			CpgFunctionPolynomial;
typedef struct _CpgFunctionPolynomialClass		CpgFunctionPolynomialClass;
typedef struct _CpgFunctionPolynomialPrivate	CpgFunctionPolynomialPrivate;

typedef struct _CpgFunctionPolynomialPiece		CpgFunctionPolynomialPiece;

struct _CpgFunctionPolynomial
{
	/*< private >*/
	CpgFunction parent;

	CpgFunctionPolynomialPrivate *priv;
};

struct _CpgFunctionPolynomialClass
{
	/*< private >*/
	CpgFunctionClass parent_class;
};

GType cpg_function_polynomial_get_type (void) G_GNUC_CONST;

GType cpg_function_polynomial_piece_get_type (void);

CpgFunctionPolynomial *
			 cpg_function_polynomial_new 			(gchar const           *name);

CpgFunctionPolynomialPiece *
			 cpg_function_polynomial_piece_new		(gdouble  begin,
													 gdouble  end,
													 gdouble *coefficients,
													 guint    num_coefficients);

void		 cpg_function_polynomial_add 			(CpgFunctionPolynomial *function,
													 CpgFunctionPolynomialPiece *piece);

void 		 cpg_function_polynomial_remove 		(CpgFunctionPolynomial      *function,
													 CpgFunctionPolynomialPiece *piece);

void		 cpg_function_polynomial_clear			(CpgFunctionPolynomial *function);

GSList		*cpg_function_polynomial_get_pieces		(CpgFunctionPolynomial *function);

gdouble		 cpg_function_polynomial_piece_get_begin 	(CpgFunctionPolynomialPiece *piece);
gdouble		 cpg_function_polynomial_piece_get_end 		(CpgFunctionPolynomialPiece *piece);

void		 cpg_function_polynomial_piece_set_begin 	(CpgFunctionPolynomialPiece *piece,
														 gdouble                     begin);
void		 cpg_function_polynomial_piece_set_end 		(CpgFunctionPolynomialPiece *piece,
														 gdouble                     end);

gdouble		*cpg_function_polynomial_piece_get_coefficients (CpgFunctionPolynomialPiece *piece,
															 guint                      *num);

void		 cpg_function_polynomial_piece_set_coefficients (CpgFunctionPolynomialPiece *piece,
															 gdouble                    *coefficients,
															 guint                       num);

G_END_DECLS

#endif /* __CPG_FUNCTION_POLYNOMIAL_H__ */
