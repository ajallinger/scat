/*----------------------------------------------------------------------
ISHELL by Alfred H. Morris, Jr., 1993
Translated from the NSWC Library of Mathematics Subroutines
This file is public domain.

    The Shell sorting procedure is used to reorder the elements of A
    so that A(I).LE.A(I+1) for I=1,...,N-1. It is assumed that N.GE.1.
----------------------------------------------------------------------*/
void ishell (int *a, int n)
{
/* Initialized data */
	static int k[10] = { 1,4,13,40,121,364,1093,3280,9841,29524 };
/* Local variables */
	int i, ii, imax, j, jmax, ki, l, ll, s;
/* Parameter adjustments */
	--a;
/*             Selection of the increments K(I) = (3^I-1)/2 */
	if (n < 2) return;
	imax = 1;
	for (i = 3; i <= 10; ++i) {
		if (n <= k[i-1]) break;
		++imax;
	}
/*            Stepping through the increments K(IMAX),...,K(1) */
	i = imax;
	for (ii = 1; ii <= imax; ++ii) {
		ki = k[i-1];
/*             Sorting elements that are KI positions apart
               so that A(J).LE.A(J+KI) for J=1,...,N-KI */
		jmax = n - ki;
		for (j = 1; j <= jmax; ++j) {
			l = j;
			ll = j + ki;
			s = a[ll];
L30:
			if (s >= a[l]) goto L31;
			a[ll] = a[l];
			ll = l;
			l -= ki;
			if (l > 0) goto L30;
L31:
			a[ll] = s;
		}
		--i;
	}
	return;
} /* end of ishell */
