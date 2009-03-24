#include <stdio.h>
#include "utils.h"
#include <math.h>

gboolean
assert_function(gdouble expect, gdouble real, gchar const *function)
{
	if (expect - real < 0.0000000000001 && expect - real > -0.0000000000001)
		return TRUE;
	
	fprintf(stderr, "\e[31mError in evaluating:\e[0m `\e[;1m%s\e[0m', \e[31mexpected\e[0m \e[;1m%f\e[0m \e[31mbut got\e[0m \e[;1m%f\e[0m\e[31;1m...\e[0m\n", function, expect, real);
	
	return FALSE;
}
