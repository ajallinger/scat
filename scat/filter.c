/*----------------------------------------------------------------------
filter - High-order Butterworth filter.
by Andy Allinger, 2021, released to the public domain

   Permission  to  use, copy, modify, and distribute this software and
   its documentation  for  any  purpose  and  without  fee  is  hereby
   granted,  without any conditions or restrictions.  This software is
   provided "as is" without express or implied warranty.

Refer to:

    "Cookbook formulae for audio EQ biquad filter coefficients"
    Robert Bristow-Johnson, [2001]

    "The Butterworth Low-Pass Filter"
    John Stensby, 19 Oct 2005

The Butterworth poles lie on a circle.  The product of the qualities
is 2^(-1/2).

___Name_____Type_____In/Out___Description_______________________________
   s[l]     float*   Both     Sample data, returned filtered
   l        int      In       Length of s
   fsamp    float    In       Sample rate, cycles per second
   fpass    float    In       Pass frequency, cycles per second
   fstop    float    In       Stop frequency, cycles per second
   hpass    float    In       Minimum passband transmission, 0...1
   hstop    float    In       Maximum stopband transmission, 0...1
----------------------------------------------------------------------*/
#include <stdbool.h>
#include <math.h>

int filter (float *s, int l, float fsamp, float fpass, float fstop, float hpass, float hstop)
{
/* Constant */
	static float zero = 0.f;
	static float half = 0.5f;
	static float one = 1.f;
	static float pi = 3.141592654f;
/* Local variables */
	int j, k, n;
	float a0, a1, a2, b1, b2, c, d, e, fcut, o, q, r, w0, x, x1, x2, y, y1, y2;
	bool lo;
/* Function Body */
	if (l <= 0) return 1;
	if (fsamp <= zero) return 2;
	if (fpass <= zero || fpass > half * fsamp) return 3;
	if (fstop <= zero || fstop > half * fsamp) return 4;
	if (hpass <= zero || hpass >= one) return 5;
	if (hstop <= zero || hstop >= one) return 6;
	if (fpass == fstop) return 7;
/* Find coefficients */
	lo = fpass < fstop;
	d = one / hstop;
	e = sqrtf(one / (hpass * hpass) - one);
	n = ((int) fabsf(logf(e / sqrtf(d * d - one)) / logf(fpass / fstop))) + 1;
	if (n % 2) ++n;
	o = lo ? -one / (float) n : +one / (float) n;
	fcut = fstop * powf(sqrtf(d * d - one), o);
	w0 = 2.f * pi * fcut / fsamp;
	c = cos(w0);
	for (k = n / 2; k >= 1; --k) {
		q = -half / cos(pi * (float) (2 * k + n - 1) / (float) (2 * n));
		r = sin(w0) / (2.f * q);
		if (lo) {
			a1 = (one - c) / (one + r);
			a0 = +half * a1;
		} else {
			a1 = -(one + c) / (one + r);
			a0 = -half * a1;
		}
		a2 = a0;
		b1 = -2.f * c / (one + r);
		b2 = (one - r) / (one + r);
/* Process the buffer. */
		x = zero;
		x1 = zero;
		x2 = zero;
		y = zero;
		y1 = zero;
		y2 = zero;
		for (j = 0; j < l; ++j) {
			x2 = x1;
			x1 = x;
			x = s[j];
			y2 = y1;
			y1 = y;
			y = a0 * x + a1 * x1 + a2 * x2 - b1 * y1 - b2 * y2;
			s[j] = y;
		}
	}
	return 0;
} /* end of filter */
