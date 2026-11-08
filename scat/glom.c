/*----------------------------------------------------------------------
glom.c - agglomerate a real sequence into segments of similar values
by Andy Allinger, 2021-2023, released to the public domain

   Permission  to  use, copy, modify, and distribute this software and
   its documentation  for  any  purpose  and  without  fee  is  hereby
   granted,  without any conditions or restrictions.  This software is
   provided "as is" without express or implied warranty.
----------------------------------------------------------------------*/
#include <math.h>
void ishell (int *a, int n);
void siftup (float *h, int *i, int *j, int n, int pos);
void siftdn (float *h, int *i, int *j, int n, int pos);

/*----------------------------------------------------------------------
Segments are merged until a stopping criterion is met:

To stop when the adjacent segment's average value differs by
some threshold, set ADPARM to the threshold value, set MDPARM to
a very large number, and set K to 1. On output, K will return
the number of segments made.

To stop when the range of values within a segment reaches some
threshold, set MDPARM to the threshold value, set ADPARM to a
very large number, and set K to 1. On output, K will return the
number of segments made.

To form a specific number of segments, set ADPARM and MDPARM to
a very large number, and set K to the desired number of
segments.

___Name________Type_____I/O_______Description___________________________
   x[n]        float*   In        Sequence
   n           int      In        Length of the sequence
   adparm      float    In        Excessive average difference
   mdparm      float    In        Excessive maximum difference
   k           int*     In/Out    Final number of segments
   bound[n]    int*     Out       Beginning each segment
   length[n]   int*     Out       Length of each segment
   ave[n]      float*   Out       Average value in segment
   least[n]    float*   Out       Minimum value in segment
   great[n]    float*   Out       Maximum value in segment
   sumx[n]     float*   Neither   Sum of segment
   dw[n]       float*   Neither   Change in square error from merging
   prior[n]    int*     Neither   Prior segment beginning
   next[n]     int*     Neither   Next segment beginning
   rind[n]     int*   Neither   Index into heap
----------------------------------------------------------------------*/
void glom (float *x, int n, float adparm, float mdparm, int *k,
           int *bound, int *length, float *ave, float *least, float *great,
           float *sumx, float *dw, int *prior, int *next, int *rind)
{
/* Local variables */
	int i, ih, ii, j, jj, kin;
	float d2, f, g, fl0, fl1, r;
/* Parameter adjustments */
	--x;
	--bound;
	--length;
	--ave;
	--least;
	--great;
	--sumx;
	--dw;
	--prior;
	--next;
	--rind;
/* Function Body */
	for (j = 1; j <= n; ++j) {
		bound[j] = j;
		length[j] = 1;
		prior[j] = j - 1;
		next[j] = j + 1;
		rind[j] = j;
		ave[j] = x[j];
		least[j] = x[j];
		great[j] = x[j];
		sumx[j] = x[j];
	}
/*----------------------------------------------------------------------
The heap-sifting routines seek a maximum value.  Thus, to find a
minimal change in sum-of-squares, negated values are entered into DW.
----------------------------------------------------------------------*/
	dw[1] = -1e36f;
	for (j = 2; j <= n; ++j) {
		r = x[j-1] - x[j];
		d2 = r * r;
		dw[j] = -d2 * .5f;
	}
	for (j = n/2; j >= 1; --j) {  /* form heap */
		siftdn (&dw[1], &bound[1], &rind[1], n, j);
	}
	kin = *k;
	*k = n;
/* Main loop */
	while (dw[1] > -9e35f && *k > kin) {
		j = bound[1];
		i = prior[j];
/* Check limits */
		f = fminf(least[i], least[j]);
		g = fmaxf(great[i], great[j]);
		if ( (fabsf(ave[i] - ave[j]) >= adparm) || (g - f >= mdparm) ) {
			dw[1] = -1e36f;
			siftdn (&dw[1], &bound[1], &rind[1], *k, 1);
			continue;
		}
/* Merge */
		length[i] += length[j];
		sumx[i] += sumx[j];
		ave[i] = sumx[i] / (float) length[i];
		least[i] = f;
		great[i] = g;
/* Delete boundary from heap */
		dw[1] = dw[*k];
		bound[1] = bound[*k];
		rind[bound[1]] = 1;
		--(*k);
		siftdn (&dw[1], &bound[1], &rind[1], *k, 1);
/* Recalculate differences */
		ii = prior[i];
		if (ii >= 1) {
			ih = rind[i];
			r = ave[ii] - ave[i];
			d2 = r * r;
			fl0 = (float) length[ii];
			fl1 = (float) length[i];
			dw[ih] = -d2 * (fl0 * fl1) / (fl0 + fl1);  /* Ward's formula */
			siftup (&dw[1], &bound[1], &rind[1], *k, ih);
			siftdn (&dw[1], &bound[1], &rind[1], *k, ih);
		}
		jj = next[j];
		next[i] = jj;
		if (jj <= n) {
			ih = rind[jj];
			r = ave[j] - ave[jj];
			d2 = r * r;
			fl0 = (float) length[j];
			fl1 = (float) length[jj];
			dw[ih] = -d2 * (fl0 * fl1) / (fl0 + fl1);
			siftup (&dw[1], &bound[1], &rind[1], *k, ih);
			siftdn (&dw[1], &bound[1], &rind[1], *k, ih);
			prior[jj] = i;
		}
	}
/* Put output arrays in order */
	ishell (&bound[1], *k);
	for (j = 1; j <= *k; ++j) {
		length[j] = length[bound[j]];
		ave[j] = ave[bound[j]];
		least[j] = least[bound[j]];
		great[j] = great[bound[j]];
	}
	for (j = 1; j <= *k; ++j) --bound[j];  /* Zero offset */
	return;
} /* end of glom */
