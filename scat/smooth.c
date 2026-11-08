/*----------------------------------------------------------------------
smooth.c - remove outliers from a signal
by Andy Allinger, 2022, released to the public domain.

   Permission  to  use, copy, modify, and distribute this software and
   its documentation  for  any  purpose  and  without  fee  is  hereby
   granted,  without any conditions or restrictions.  This software is
   provided "as is" without express or implied warranty.
----------------------------------------------------------------------*/

/*----------------------------------------------------------------------
operu - apply the "U" operation

___Name___Type_____In/Out___Description_________________________________
   x[l]   float*   In       Data sequence
   l      int      In       Length of the sequence
   n      int      In       Half-width of the filter
   y[l]   float*   Out      Filtered data
----------------------------------------------------------------------*/
void operu (float *x, int l, int n, float *y)
{
/* Local variables */
	int i, j, k, kbeg, kend;
	float great, least;
/* Function Body */
	if (n < 0) return;
	for (i = 0; i < l; ++i) {  /* main loop */
		least = +1e36f;
		for (j = i-n; j <= i; ++j) {
			great = -1e36f;
			kbeg = j;
			if (kbeg < 0) kbeg = 0;
			kend = j + n;
			if (kend >= l) kend = l - 1;
			for (k = kbeg; k <= kend; ++k) {
				if (x[k] > great) great = x[k];
			}
			if (great < least) least = great;
		}
		y[i] = least;
	}
	return;
} /* end of operu */

/*----------------------------------------------------------------------
operl - apply the "L" operation

___Name___Type_____In/Out___Description_________________________________
   x[l]   float*   In       Data sequence
   l      int      In       Length of the sequence
   n      int      In       Half-width of the filter
   y      float*   Out      Filtered data
----------------------------------------------------------------------*/
void operl (float *x, int l, int n, float *y)
{
/* Local variables */
	int i, j, k, kbeg, kend;
	float great, least;
/* Function Body */
	if (n < 0) return;
	for (i = 0; i < l; ++i) {  /* main loop */
		great = -1e36f;
		for (j = i-n; j <= i; ++j) {
			least = +1e36f;
			kbeg = j;
			if (kbeg < 0) kbeg = 0;
			kend = j + n;
			if (kend >=l) kend = l - 1;
			for (k = kbeg; k <= kend; ++k) {
				if (x[k] < least) least = x[k];
			}
			if (least > great) great = least;
		}
		y[i] = great;
	}
	return;
} /* end of operl */


/*----------------------------------------------------------------------
smooth - apply the "B" filter as described in:

     "Some Statistical Aspects of LULU Smoothers"
     Maria Dorothea Jankowitz
     Dissertation, Stellenbosch University, Dec. 2007

___Name______Type_____In/Out____Description_____________________________
   s[l]      float*   Both      Sample data, returned filtered
   l         int      In        Length of the sequence
   n         int      In        Half-width of the filter
   c[l]      float*   Neither   Ceiling sequence
   f[l]      float*   Neither   Floor sequence
   work[l]   float*   Neither   Workspace
----------------------------------------------------------------------*/
void smooth (float *s, int l, int n, float *c, float *f, float *work)
{
/* Local variables */
	int j, k;
/* Function Body */
	if (n < 1) return;
	for (j = 0; j < l; ++j) {
		c[j] = s[j];  /* C_0 = S */
		f[j] = s[j];  /* F_0 = S */
	}
	for (k = 1; k <= n; ++k) {
		operu (c, l, k, work);  /* C_n = L(U(C_n-1)) */
		operl (work, l, k, c);
		operl (f, l, k, work);  /* F_n = U(L(F_n-1)) */
		operu (work, l, k, f);
		for (j = 0; j < l; ++j) {
			if (s[j] < f[j]) {
				s[j] = f[j];
			} else if (s[j] > c[j]) {
				s[j] = c[j];
			}
		}
	}
	return;
} /* end of smooth */
