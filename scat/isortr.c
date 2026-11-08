/*----------------------------------------------------------------------
isortr - passive insertion sort
by Andy Allinger, 2019, released to the public domain.

   Permission  to  use, copy, modify, and distribute this software and
   its documentation  for  any  purpose  and  without  fee  is  hereby
   granted,  without any conditions or restrictions.  This software is
   provided "as is" without express or implied warranty.

___Name_____Type______In/Out___Description______________________________
   x[n]     float*    In       Array to be sorted
   ind[n]   int*      Out      Index array
   n        int       In       Length of arrays
----------------------------------------------------------------------*/
void isortr (float *x, int *ind, int n)
{
/* Local variables */
	int i, j, k;
	float t;
/* Parameter adjustments */
	--x;
	--ind;
/* Function Body */
	for (i = 1; i <= n; ++i) ind[i] = i;
	if (n < 2) return;
	for (j = 2; j <= n; ++j) {
		k = ind[j];
		t = x[k];
		for (i = j-1; i >= 1; --i) {
			if (x[ind[i]] <= t) {
				ind[i+1] = k;
				goto L10;
			}
			ind[i+1] = ind[i];
		}
		ind[1] = k;
L10:
		;
	}
	return;
} /* end of isortr */
