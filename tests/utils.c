#include <stdio.h>
#include "utils.h"
#include <math.h>

int
assert_function(double expect, double real, char const *function)
{
	if (expect - real < 0.0000000000001 && expect - real > -0.0000000000001)
		return 1;
	
	fprintf(stderr, "\e[31mError in evaluating:\e[0m `\e[;1m%s\e[0m', \e[31mexpected\e[0m \e[;1m%f\e[0m \e[31mbut got\e[0m \e[;1m%f\e[0m\e[31;1m...\e[0m\n", function, expect, real);
	
	return 0;
}
