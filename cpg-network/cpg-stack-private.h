#ifndef __CPG_STACK_PRIVATE_H__
#define __CPG_STACK_PRIVATE_H__

#include "cpg-stack.h"

struct _CpgStack
{
	/*< private >*/
	gdouble *output_ptr;
	gdouble *output;
	guint size;
};

#endif /* __CPG_STACK_PRIVATE_H__ */

