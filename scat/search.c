/*----------------------------------------------------------------------
search.c - bisection search
by Andy Allinger, 2012-2022, released to the public domain

   Permission  to  use, copy, modify, and distribute this software and
   its documentation  for  any  purpose  and  without  fee  is  hereby
   granted,  without any conditions or restrictions.  This software is
   provided "as is" without express or implied warranty.
----------------------------------------------------------------------*/

/*----------------------------------------------------------------------
Bisection search for the first element greater than a value

___Name___Type_____In/Out___Description_________________________________
   x[n]   float*   In       Array (in increasing order)
   n      int      In       Length of x
   v      float    In       The value sought
   i      int*     Out      Index where x > v found
----------------------------------------------------------------------*/
void rbsgt (float *x, int n, float v, int *i)
{
	int il, ir;
/* Function Body */
	if (n < 1) return;
	if (v < x[0]) {
		*i = 0;
		return;
	} else if (v >= x[n-1]) {
		*i = n-1;
		return;
	}
/* Bisection search */
	il = 0;
	ir = n-1;
L10:
	*i = (il + ir) / 2;
	if (il == *i) {
		if (x[il] <= v) *i = ir;
		return;
	}
	if (x[*i] <= v) {
		il = *i;
	} else {
		ir = *i;
	}
	goto L10;
} /* end of rbsgt */

/*----------------------------------------------------------------------
Bisection search for the last element less than a value

___Name___Type_____In/Out___Description_________________________________
   x[n]   float*   In       Array (in increasing order)
   n      int      In       Length of x
   v      float    In       The value sought
   i      int*     Out      Index where x < v found
----------------------------------------------------------------------*/
void rbslt (float *x, int n, float v, int *i)
{
	int il, ir;
/* Function Body */
	if (n < 1) return;
	if (v <= x[0]) {
		*i = 0;
		return;
	} else if (v > x[n-1]) {
		*i = n-1;
		return;
	}
/* Bisection search */
	il = 0;
	ir = n-1;
L10:
	*i = (il + ir) / 2;
	if (il == *i) {
		if (x[ir] < v) *i = ir;
		return;
	}
	if (x[*i] < v) {
		il = *i;
	} else {
		ir = *i;
	}
	goto L10;
} /* end of rbslt */

/*----------------------------------------------------------------------
Bisection search for the first element greater than or equal to a value

___Name___Type___In/Out___Description___________________________________
   x[n]   int*   In       Array (in increasing order)
   n      int    In       Length of x
   v      int    In       The value sought
   i      int*   Out      Index where x >= v found
----------------------------------------------------------------------*/
void ibsge (int *x, int n, int v, int *i)
{
	int il, ir;
/* Function Body */
	if (n < 1) return;
	if (v < x[0]) {
		*i = 0;
		return;
	} else if (v > x[n-1]) {
		*i = n-1;
		return;
	}
/* Bisection search */
	il = 0;
	ir = n-1;
L10:
	*i = (il + ir) / 2;
	if (il == *i) {
		if (x[il] < v) *i = ir;
		return;
	}
	if (x[*i] < v) {
		il = *i;
	} else {
		ir = *i;
	}
	goto L10;
} /* end of ibsge */

/*----------------------------------------------------------------------
Bisection search for the last element less than or equal to a value

___Name___Type__ In/Out___Description___________________________________
   x[n]   int*   In       Array (in increasing order)
   n      int    In       Length of x
   v      int    In       The value sought
   i      int*   Out      Index where x <= v found
----------------------------------------------------------------------*/
void ibsle (int *x, int n, int v, int *i)
{
	int il, ir;
/* Function Body */
	if (n < 1) return;
	if (v < x[0]) {
		*i = 0;
		return;
	} else if (v > x[n-1]) {
		*i = n-1;
		return;
	}
/* Bisection search */
	il = 0;
	ir = n-1;
L10:
	*i = (il + ir) / 2;
	if (il == *i) {
		if (x[ir] <= v) *i = ir;
		return;
	}
	if (x[*i] <= v) {
		il = *i;
	} else {
		ir = *i;
	}
	goto L10;
} /* end of ibsle */
/*--------------------- end of file search.c -------------------------*/
