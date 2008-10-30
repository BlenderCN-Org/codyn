#include "cpg-debug.h"
#include <stdarg.h>
#include <stdio.h>

static char const *debug_types[] =
{
	"NONE",
	"EXPRESSION",
	"EVALUATION"
};

static int debug_type = CPG_DEBUG_TYPE_NONE;

void
cpg_debug_add(CpgDebugType type)
{
	debug_type |= type;
}

void
cpg_debug_message_function(CpgDebugType type, char const *function, char const *format, ...)
{
	if (!(type & debug_type))
		return;

	va_list ap;
	va_start(ap, format);

	fprintf(stderr, " ** DEBUG %s in %s: ", debug_types[type], function);
	vfprintf(stderr, format, ap);
	fprintf(stderr, "\n");
		
	va_end(ap);
}
