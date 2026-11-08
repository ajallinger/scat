/*----------------------------------------------------------------------
Safe division routine to prevent overflow
Based on a post to comp.lang.fortran on 7/17/1999 by Julio,
following a suggestion by William Long
A paraphrase of a code snippet -- it should not infringe any copyright.
----------------------------------------------------------------------*/
#include <stdbool.h>
#include <float.h>
#include <math.h>

bool safdiv (float numer, float denom)
{
/* Local variables */
	int ne, de;
	bool retval;
	if (0.f == denom) return false;
	if (0.f == numer) return true;
/*----------------------------------------------------------------------
Require the difference in exponents to be strictly less than ME,
to handle the case when the fractional part of the numerator
is greater than the fractional part of the denominator.
----------------------------------------------------------------------*/
	ne = ilogbf(numer);
	de = ilogbf(denom);
	retval = ne - de < FLT_MAX_EXP && fabsf(denom) >= FLT_MIN;
	return retval;
} /* end of safdiv */

/* Same thing in double precision */
bool dafdiv (double numer, double denom) {
/* Local variables */
	int ne, de;
	bool retval;
	if (0. == denom) return false;
	if (0. == numer) return true;
	ne = ilogb(numer);
	de = ilogb(denom);
	retval = (ne - de) < DBL_MAX_EXP && fabs(denom) >= DBL_MIN;
	return retval;
} /* end of dafdiv */
