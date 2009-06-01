#include "cpg-debug.h"

#ifndef RTLINUX
#include <stdarg.h>
#include <stdio.h>
#endif

char const *debug_types[] =
{
	"NONE",
	"ERROR",
	"EXPRESSION",
	"ERROR, EXPRESSION",
	"EVALUATION",
	"ERROR, EVALUATION",
	"EXPRESSION, EVALUATION",
	"ERROR, EXPRESSION, EVALUATION"
};

int debug_type = CPG_DEBUG_TYPE_ERROR;

void
cpg_debug_add (CpgDebugType type)
{
	debug_type |= type;
}

#ifndef RTLINUX
void
cpg_debug_message_function (CpgDebugType  type, 
                            char const   *function, 
                            char const   *format, 
                            ...)
{
	if (!(type & debug_type))
		return;

	va_list ap;
	va_start (ap, format);

	fprintf (stderr, " ** DEBUG %s in %s: ", debug_types[type], function);
	vfprintf (stderr, format, ap);
	fprintf (stderr, "\n");
		
	va_end (ap);
}
#endif
