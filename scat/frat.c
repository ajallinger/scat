/*----------------------------------------------------------------------
frat - Frequency Re-Assignment Transform
by Andy Allinger, 2019-2021, released to the public domain.

   Permission  to  use, copy, modify, and distribute this software and
   its documentation  for  any  purpose  and  without  fee  is  hereby
   granted,  without any conditions or restrictions.  This software is
   provided "as is" without express or implied warranty.


___Name_________Type______In/Out____Description_________________________
   s[l]         float*    In        Samples in the range -1 ... +1
   l            int       In        Length of series
   m            int       In        Index of coefficient pair
   n            int       In        Size of transform window, samples
   r            int       In        Sample rate
   stride       int       In        Downsampling factor
   hop          float     In        Transform increment, samples
   fram         int       In        # frames
   freq[fram]   float*    Out       Frequencies by bin, column
   ampl[fram]   float*    Out       Amplitudes by bin, column
   nef[fram]    int*      Neither   # transforms each frame
   t[2*n]       double*   Neither   Transform matrix
   w[2*n]       double*   Neither   Window function, derivative
   z[n]         double*   Neither   Sample value shifted and scaled
----------------------------------------------------------------------*/
#include <stdbool.h>
#include <math.h>
bool safdiv (float numer, float denom);
bool dafdiv (double numer, double denom);

int frat (float *s, int l, int m, int n, int r, int stride, float hop, int fram,
           float *freq, float *ampl, int *nef, double *t, double *w, double *z)
{
/* Constants */
	static double twopi = 6.2831853071795865;
	static double wc0 = 0.375, wc1 = 0.5, wc2 = 0.125;  /* Nuttall window */
/* Local variables */
	int i, j, jadd, jbeg, jcen, jend, jsub, k, q;
	float p;
	double a, admbc, b, c, corex, d, dn, dr, dx, dxj, e, f, f0, odn;
	/* double sx, sx2, sxy, sy; */
/* Function Body */
	if (l <= 0 || m <= 0 || n <= 0) return 1;
	for (j = 0; j < fram; ++j) {  /* clear freq and ampl */
		freq[j] = 0.f;
		ampl[j] = 0.f;
		nef[j] = 0;
	}
	dn = (double) (n);
	dr = (double) (r);
	f0 = dr / dn;
	odn = 1. / dn;
	p = (float) (fram) / (float) (l);
	q = (int) (l / hop);
/*----------------------------------------------------------------------
Real Discrete Fourier Transform matrix.
Refer to:

     "Calculation of a constant Q spectral transform"
     Judith C. Brown
     Journal of the Acoustic Society of America v.89 i.1, January 1991
----------------------------------------------------------------------*/
	dx = twopi / dn;
	for (j = 0; j < n; ++j) {
		dxj = m * (j + 1) * dx;
		t[(j << 1)] = cos(dxj) * odn;
		t[(j << 1) + 1] = sin(dxj) * odn;
	}
/* Window function and its derivative */
	for (j = 0; j < n; ++j) {
		dxj = dx * (j + 1);
		w[(j << 1)] = wc0 - wc1 * cos(dxj) + wc2 * cos(dxj * 2.);
		w[(j << 1) + 1] = wc1 * sin(dxj) * dx - wc2 * sin(dxj * 2.) * dx * 2.;
	}
/* Main loop */
	jadd = n * stride / 2;
	jsub = jadd;
	if (n * stride % 2 == 0) --jsub;  /* one less for even n */
	for (k = 0; k <= q; ++k) {
		jcen = lroundf((float) k * hop);
		jbeg = jcen - jsub;
		jend = jcen + jadd;
		i = 0;  /* Move samples from buffer */
		for (j = jbeg; j <= jend; j += stride) {
			if (j >= 1 && j <= l) {
				z[i] = (double) s[j-1];
			} else {
				z[i] = 0.;
			}
			++i;
		}
/* Uncomment the following lines if input is not high-pass filtered */
/* Detrend by subtracting least-squares line */
/*		sy = 0.;
		sxy = 0;
		for (j = 0; j < n; ++j) {
			sy += z[j];
			sxy += (double) (j + 1) * z[j];
		}
		sx = dn * (dn + 1.) / 2.;
		sx2 = dn * (dn + 1.) * (2. * dn + 1.) / 6.;
		d = dn * dn * (dn * dn - 1.) / 12.;
		a = (sx2 * sy - sx * sxy) / d;
		b = (dn * sxy - sx * sy) / d;
		for (j = 0; j < n; ++j) {
			z[j] = z[j] - a - b * (double) (j + 1);
		}
*/
/*----------------------------------------------------------------------
Frequency Reassignment
Refer to:

     "Programming the Phase Vocoder" by Victor Lazzarini
     in The Audio Programming Book, ed. Richard Boulanger
     and Victor Lazzarini.  MIT Press, 2011.
----------------------------------------------------------------------*/
		a = 0.;
		b = 0.;
		c = 0.;
		d = 0.;
		for (j = 0; j < n; ++j) {
			a += t[(j << 1)] * z[j] * w[(j << 1)];
			b += t[(j << 1) + 1] * z[j] * w[(j << 1)];
			c += t[(j << 1)] * z[j] * w[(j << 1) + 1];
			d += t[(j << 1) + 1] * z[j] * w[(j << 1) + 1];
		}
/* Energy and frequency */
		e = a * a + b * b;  /* energy */
		f = f0 * m;
		admbc = a * d - b * c;
		if (dafdiv (admbc, e)) {
			corex = (admbc / e) * (dr / twopi);
			if (fabs(corex) <= 0.5 * f0) {
				f += corex;
				j = (int) (jcen * p);
				if (j < 0) j = 0;
				if (j >= fram) j = fram - 1;
				++nef[j];
				freq[j] += (float) (e * f);
				ampl[j] += (float) e;
			}
		}
	}
	for (j = 0; j < fram; ++j) {
		if (safdiv (freq[j], ampl[j])) {
			freq[j] /= ampl[j];
		} else {
			f = f0 * m;
			freq[j] = (float) f;
		}
		if (nef[j] > 0) {
			ampl[j] = sqrtf(ampl[j] / (float) nef[j]);
		} else {
			ampl[j] = 0.f;
		}
	}
	return 0;
} /* end of frat */
