#ifndef __CDN_STACK_PRIVATE_H__
#define __CDN_STACK_PRIVATE_H__

#include "cdn-stack.h"

struct _CdnStack
{
	/*< private >*/
	gdouble *output_ptr;
	gdouble *output;
	guint size;
};

#endif /* __CDN_STACK_PRIVATE_H__ */

