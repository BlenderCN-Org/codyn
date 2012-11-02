#ifndef __CDN_MATRIX_H__
#define __CDN_MATRIX_H__

#include <codyn/cdn-stack.h>

#ifdef __GI_SCANNER__
typedef struct
{
	gdouble *values;
	CdnDimension dimension;
} CdnMatrix;
#else
typedef struct
{
	union
	{
		gdouble *values;
		gdouble value;
	};

	CdnDimension dimension;
} CdnMatrix;
#endif

GType               cdn_matrix_get_type   (void);

CdnMatrix          *cdn_matrix_new        (gdouble const      *values,
                                           CdnDimension const *dimension);

CdnMatrix          *cdn_matrix_new_one    (gdouble             value);

void                cdn_matrix_free       (CdnMatrix          *matrix);
void                cdn_matrix_destroy    (CdnMatrix          *matrix);

gint                cdn_matrix_size       (CdnMatrix const    *matrix);

CdnDimension const *cdn_matrix_dimension  (CdnMatrix const    *matrix);

void                cdn_matrix_set        (CdnMatrix          *matrix,
                                           gdouble const      *values,
                                           CdnDimension const *dimension);

void                cdn_matrix_set_at     (CdnMatrix          *matrix,
                                           gint                r,
                                           gint                c,
                                           gdouble             value);

void                cdn_matrix_copy       (CdnMatrix          *dest,
                                           CdnMatrix const    *src);

void                cdn_matrix_copy_to    (CdnMatrix const    *matrix,
                                           gdouble            *values);

void                cdn_matrix_set_one    (CdnMatrix          *matrix,
                                           gdouble             value);

gdouble const      *cdn_matrix_get        (CdnMatrix const    *matrix);

gdouble             cdn_matrix_get_at     (CdnMatrix const    *matrix,
                                           gint                r,
                                           gint                c);

gdouble            *cdn_matrix_get_memory (CdnMatrix          *matrix);

gdouble const      *cdn_matrix_get_flat   (CdnMatrix const    *matrix,
                                           gint               *length);

void                cdn_matrix_clear      (CdnMatrix          *matrix);

#endif /* __CDN_MATRIX_H__ */

