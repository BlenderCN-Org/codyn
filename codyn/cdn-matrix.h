#ifndef __CDN_MATRIX_H__
#define __CDN_MATRIX_H__

#include <codyn/cdn-stack.h>

typedef struct
{
#ifdef __GI_SCANNER__
	double *values;
#else
	union
	{
		gdouble *values;
		gdouble value;
	};
#endif
	CdnDimension dimension;
} CdnMatrix;

GType          cdn_matrix_get_type (void);

CdnMatrix     *cdn_matrix_new      (gdouble const      *values,
                                    CdnDimension const *dimension);

CdnMatrix     *cdn_matrix_new_one  (gdouble             value);

void           cdn_matrix_free     (CdnMatrix          *matrix);
void           cdn_matrix_destroy  (CdnMatrix          *matrix);

gint           cdn_matrix_size     (CdnMatrix const    *matrix);

void           cdn_matrix_set      (CdnMatrix          *matrix,
                                    gdouble const      *values,
                                    CdnDimension const *dimension);

void           cdn_matrix_copy     (CdnMatrix          *dest,
                                    CdnMatrix const    *src);

void           cdn_matrix_copy_to  (CdnMatrix const    *matrix,
                                    gdouble            *values);

void           cdn_matrix_set_one  (CdnMatrix          *matrix,
                                    gdouble             value);

gdouble const *cdn_matrix_get      (CdnMatrix const    *matrix);

gdouble *cdn_matrix_get_memory     (CdnMatrix          *matrix);

gdouble const *cdn_matrix_get_flat (CdnMatrix const    *matrix,
                                    gint               *length);

void           cdn_matrix_clear    (CdnMatrix          *matrix);

#endif /* __CDN_MATRIX_H__ */

