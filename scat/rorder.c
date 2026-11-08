/***********************************************************************

                                              Robert Renka
                                      Oak Ridge Natl. Lab.

  This routine applies a set of permutations to a vector.

Input parameters -  N - Length of A and IP.

                   IP - Vector containing the sequence of
                        integers 1,...,N permuted in the
                        same fashion that a is to be permuted.

                    A - Vector to be permuted.

N and IP are not altered by this routine.

Output parameter -  A - Reordered vector reflecting the
                        permutations defined by IP.

***********************************************************************/
void rorder (float *a, int *ip, int n)
{
/* Local variables */
	int j, k, ipj;
	float temp;

/***********************************************************************
Local parameters -

K =    Index for IP and for the first element of A in a permutation
J =    INDEX for IP and A, J .GE. K
IPJ =  IP(J)
TEMP = Temporary storage for A(K)
***********************************************************************/

/* Parameter adjustments */
	--a;
	--ip;
/* Function Body */
	if (n < 2) return;
	k = 1;
/* Loop on permutations */
L1:
	j = k;
	temp = a[k];
/* Apply permutation to A.  IP(J) is marked (made negative)
   as being included in the permutation. */
L2:
	ipj = ip[j];
	ip[j] = -ipj;
	if (ipj == k) goto L3;
	a[j] = a[ipj];
	j = ipj;
	goto L2;
L3:
	a[j] = temp;
/* Search for an unmarked element of IP */
L4:
	++k;
	if (k > n) goto L5;
	if (ip[k] > 0) goto L1;
	goto L4;
/* All permutations have been applied.  Unmark IP. */
L5:
	for (k = 1; k <= n; ++k) ip[k] = -ip[k];
	return;
} /* end of rorder */
