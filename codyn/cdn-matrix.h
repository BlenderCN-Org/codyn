#ifndef __CDN_MATRIX_H__
#define __CDN_MATRIX_H__

#include <cdn-stack.h>

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

CdnMatrix     *cdn_matrix_new      (gdouble const *values,
                                    CdnDimension  *dimension);

CdnMatrix     *cdn_matrix_new_one  (gdouble        value);

void           cdn_matrix_free     (CdnMatrix     *matrix);
void           cdn_matrix_destroy  (CdnMatrix     *matrix);

void           cdn_matrix_set      (CdnMatrix     *matrix,
                                    gdouble const *value,
                                    CdnDimension  *dimension);

gdouble const *cdn_matrix_get      (CdnMatrix     *matrix,
                                    CdnDimension  *dimension);

gdouble const *cdn_matrix_get_flat (CdnMatrix     *matrix,
                                    gint          *length);

#endif /* __CDN_MATRIX_H__ */

