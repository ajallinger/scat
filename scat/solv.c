/*
     Sandia Mathematical Program Library
     Mathematical Computing Services Division 5422
     Sandia Laboratories
     P. O. Box 5800
     Albuquerque, New Mexico  87115
     Control data 6600 version 4.5, 1 November 1971

     Modified to run at NBS by D. Kahaner Div 713

     ABSTRACT
        ZEROIN searches for a zero of a function F(X) between
        the given values B and C until the width of the interval
        (B,C) has collapsed to within a tolerance specified by
        the stopping criterion, ABS(B-C) .LE. 2.*(RW*ABS(B)+AE).

     DESCRIPTION OF ARGUMENTS
        F     - Name of the real valued external function.  This name
                must be in an EXTERNAL statement in the calling
                program.  F must be a function of !![one real argument].!!
        B     - One end of the interval (B,C).  The value returned for
                B usually is the better approximation to a zero of F.
        C     - The other end of the interval (B,C)
        RE    - Relative error used for RW in the stopping criterion.
                If the requested RE is less than machine precision,
                then RW is set to approximately machine precision.
        AE    - Absolute error used in the stopping criterion.  If the
                given interval (B,C) contains the origin, then a
                nonzero value should be chosen for AE.
        IFLAG - Returns a status of the results indicating which
                of the following conditions hold.
                    A - ABS(B-C) .LE. 2.*(RW*ABS(B)+AE)
                    B - F(B) * F(C) .LT. 0.
                    C - ABS(F(B)) .LE. ABS(F(C))
                    D - ABS(F(B   )) .LE. MAX(ABS(F(B  )),ABS(F(C  )))
                               OUT                   IN          IN
                    E - Number of evaluations of F(X) .LE. 500
               =1 Indicates normal case.  All conditions above hold.
               =2 Indicates F(B) = 0.  Condition A may not hold.
               =3 Indicates conditions A, B, C, and E hold but D does
                  not.  (B,C) probably contains a singular point of f.
               =4 Indicates conditions A and E hold but B does not.
                  A local minimum of F(X) in (B,C) may have been found.
               =5 Indicates search was aborted when condition E failed.

     REFERENCES
       1.  L F Shampine AND H A Watts, ZEROIN, a root-solving code,
           SC-TM-70-631, Sept 1970.
       2.  T J Dekker, Finding a Zero by Means of Successive Linear
           Interpolation, *Constructive Aspects of the Fundamental
           Theorem of Algebra*, edited by B Dejon and P Henrici, 1969.

------------------------------------------------------------------------
 Changed and renamed to make F a function of a real argument and a pointer.
 Return zero on success.
 AJA, 3 Jan 2022
----------------------------------------------------------------------*/
#include <float.h>
#include <math.h>
int solv (float (*f)(float, void*), float *b, float *c, float re, float ae, void *garg)
{
/* Initialized data */
	static float er = 0.f;
/* Local variables */
	int ic, kount;
	float a, acbs, acmb, cmb, p, q, fa, fb, fc, fx, rw, tol;
/* Function Body */
	if (0.f == er) er = 4.f * (2.f * FLT_EPSILON);
	rw = fmaxf(re,er);
	ic = 0;
	acbs = fabsf(*b - *c);
	a = *c;
	fa = (*f)(a, garg);
	fb = (*f)(*b, garg);
	fc = fa;
	kount = 2;
	fx = fmaxf(fabsf(fb),fabsf(fc));
L1:
	if (fabsf(fc) >= fabsf(fb)) goto L2;
/* Perform interchange */
	a = *b;
	fa = fb;
	*b = *c;
	fb = fc;
	*c = a;
	fc = fa;
L2:
	cmb = 0.5f * (*c - *b);
	acmb = fabsf(cmb);
	tol = rw * fabsf(*b) + ae;
/* Test stopping criterion */
	if (acmb <= tol) goto L10;
/* Calculate new iterate implicitly as B+P/Q
   where we arrange P .GE. 0.
   The implicit form is used to prevent overflow. */
	p = (*b - a) * fb;
	q = fa - fb;
	if (p >= 0.f) goto L3;
	p = -p;
	q = -q;
/* Update A and check for satisfactory reduction
   in the size of our bounding interval. */
L3:
	a = *b;
	fa = fb;
	++ic;
	if (ic < 4) goto L4;
	if (acmb * 8.f >= acbs) goto L6;
	ic = 0;
	acbs = acmb;
/* Test for too small a change */
L4:
	if (p > fabsf(q) * tol) goto L5;
/* Increment by tolerance */
	*b += copysignf(tol, cmb);
	goto L7;
/* Root ought to be between B and (C+B)/2. */
L5:
	if (p >= cmb * q) goto L6;
/* Interpolate */
	*b += p / q;
	goto L7;
/* Bisect */
L6:
	*b = 0.5f * (*c + *b);
/* Have completed computation for new iterate B */
L7:
	fb = (*f)(*b, garg);
	if (0.f == fb) return 2;
	++kount;
	if (kount > 500) return 5;
/* Decide whether next step is interpolation or extrapolation */
	if (copysignf(1.f, fb) != copysignf(1.f, fc)) goto L1;
	*c = a;
	fc = fa;
	goto L1;
/* Finished. Process results for proper setting of IFLAG */
L10:
	if (fb * fc > 0.f) return 4;
	if (fabsf(fb) > fx) return 3;
	return 0;
} /* end of solv */
