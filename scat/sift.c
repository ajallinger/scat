/*----------------------------------------------------------------------
sift.c - sift a real heap, with forwards and reverse indices.
by Andy Allinger, 2017-2019, released to the public domain

   Permission  to  use, copy, modify, and distribute this software and
   its documentation  for  any  purpose  and  without  fee  is  hereby
   granted,  without any conditions or restrictions.  This software is
   provided "as is" without express or implied warranty.

___Name___Type_____In/Out___Description_________________________________
   h[]    float*   Both     A heap
   i[]    int*     Both     Index array carried along
   j[]    int*     Both     Reverse permutation
   n      int      In       Size of heap
   pos    int      In       Position of the new element
----------------------------------------------------------------------*/
void siftup (float *h, int *i, int *j, int n, int pos)
{
	int p, stem, iswap;
	float rswap;
/* Parameter adjustments */
	--h;
	--i;
	--j;
/* Function Body */
	p = pos;
L10:  /* come here */
	stem = p / 2;
/* not at root */
	if (stem >= 1) {
/* compare to stem */
		if (h[p] > h[stem]) {
			rswap = h[p];
			h[p] = h[stem];
			h[stem] = rswap;
			iswap = i[p];
			i[p] = i[stem];
			i[stem] = iswap;
			j[i[p]] = p;
			j[i[stem]] = stem;
			p = stem;
			goto L10;
		}
	}
	return;
} /* end of siftup */

/*--------------------------------------------------------------------*/
void siftdn (float *h, int *i, int *j, int n, int pos)
{
	int p, lbran, rbran, iswap;
	float rswap;
/* Parameter adjustments */
	--h;
	--i;
	--j;
/* Function Body */
	p = pos;
L20:  /* come here */
	lbran = p << 1;
	if (lbran <= n) {
		rbran = lbran + 1;
		if (rbran <= n) {  /* right branch exists? */
			if (h[rbran] > h[lbran]) goto L30;
		}
		if (h[p] < h[lbran]) {  /* compare to left */
			rswap = h[p];
			h[p] = h[lbran];
			h[lbran] = rswap;
			iswap = i[p];
			i[p] = i[lbran];
			i[lbran] = iswap;
			j[i[p]] = p;
			j[i[lbran]] = lbran;
			p = lbran;
			goto L20;
		}
	}
	return;
L30:
	if (h[p] < h[rbran]) {  /* compare to right */
		rswap = h[p];
		h[p] = h[rbran];
		h[rbran] = rswap;
		iswap = i[p];
		i[p] = i[rbran];
		i[rbran] = iswap;
		j[i[p]] = p;
		j[i[rbran]] = rbran;
		p = rbran;
		goto L20;
	}
	return;
} /* end of siftdn */
