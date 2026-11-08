/*----------------------------------------------------------------------
dyrb - dynamic programming of real observations, binary theoretical states
by Andy Allinger, 2021, released to the public domain.

   Permission  to  use, copy, modify, and distribute this software and
   its documentation  for  any  purpose  and  without  fee  is  hereby
   granted,  without any conditions or restrictions.  This software is
   provided "as is" without express or implied warranty.

___Name________Type_____In/Out____Description___________________________
   r[n]        float*   In        Observations 0 ... 1
   n           int      In        Length of sequence
   base        float    In        Indifference value
   trans       float    In        Transition penalty
   b[n]        int*     Out       Optimal theoretical states {0,1}
   back[2*n]   int*     Neither   Backpointers
----------------------------------------------------------------------*/
#include <math.h>

int dyrb (float *r, int n, float base, float trans, int *b, int *back)
{
/* Local variables */
	int i, j, k, knew, kold;
	float sbest, stry, scor[4];
/* Function Body */
	if (n < 1) return 1;
	if (base <= 0.f || base >= 1.f) return 2;
	if (trans <= 0.f) return 3;
	for (k = 0; k < n; ++k) if (r[k] <= 0.f || r[k] > 1.f) return 4;
	back[0] = 0;
	back[1] = 1;
	scor[0] = -logf(r[0] / base);
	scor[1] = +logf(r[0] / base);
	scor[2] = 0.f;
	scor[3] = 0.f;
	knew = 0;
	for (k = 1; k < n; ++k) {  /* for each time step */
		kold = knew;  /* set index to odd or even */
		knew = k % 2;
		for (j = 0; j <= 1; ++j) {  /* states in present step */
			sbest = -1e36f;
			for (i = 0; i <= 1; ++i) {  /* states in prior step */
				stry = scor[i + (kold << 1)];
				if (i != j) stry -= trans;  /* penalize change */
				if (stry > sbest) {
					sbest = stry;
					back[j + (k << 1)] = i;
				}
			}
			if (0 == j) {
				scor[j + (knew << 1)] = sbest - logf(r[k] / base);
			}
			else {
				scor[j + (knew << 1)] = sbest + logf(r[k] / base);
			}
		}
	}
/* Choose highest-scoring path */
	if (scor[knew << 1] > scor[(knew << 1) + 1]) {
		j = 0;
	} else {
		j = 1;
	}
/* Trace back pointers */
	for (k = n-1; k >= 0; --k) {
		b[k] = j;
		j = back[j + (k << 1)];
	}
	return 0;
} /* end of dyrb */
