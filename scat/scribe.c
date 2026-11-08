/*----------------------------------------------------------------------
scribe.c - read a Portable Gray Map, write a MIDI music file
by Andy Allinger, 2006-2022, released to the public domain

   Permission  to  use, copy, modify, and distribute this software and
   its documentation  for  any  purpose  and  without  fee  is  hereby
   granted,  without any conditions or restrictions.  This software is
   provided "as is" without express or implied warranty.

_______________________OPTIONS__________________________________________
  --tune [+|-]x                   performance is sharp of standard A=440
                                    tuning by x semitones (negative for flat)
  --patch #                       select General Midi patch/program #
  --text hhh                      a comment of any type
  --copyright hhh                 legal ownership of the file
  --seqname hhh                   a title or description
----------------------------------------------------------------------*/
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

bool safdiv (float numer, float denom);
bool dafdiv (double numer, double denom);
int solv (float (*f)(float, float*), float *b, float *c, float re, float ae, float *rarg);
int dyrb (float *r, int n, float base, float trans, int *b, int *back);
void rbsgt (float *x, int n, float v, int *i);
void rbslt (float *x, int n, float v, int *i);
void smooth (float *s, int l, int n, float *c, float *f, float *work);
void glom (float *x, int n, float adparm, float mdparm, int *k,
           int *bound, int *length, float *ave, float *least, float *great,
           float *sumx, float *dw, int *prior, int *next, int *rind);

#define BPM 120
#define BUFL 1024
#define CHANEL 0
#define ELIM 144000
/* 5 min @ 60 s/min @ 240px/s @ 640 px */
#define LLIM 46080000
#define OPTL 256
#define PBR 200
#define PXPS 240
#define TIMDIV 120
#define VERT 640

/*----------------------------------------------------------------------
kernin - invert the Lanczos kernel function

___Name______Type_____In/Out___Description______________________________
   x         float    In       A position in pixels of the pre-interpolated data
   rarg[4]   float*   In       rarg[0] Position X1 after interpolation
                               rarg[1] Position X2 after interpolation
                               rarg[2] Intensity Y1 at position X1
                               rarg[3] Intensity Y2 at position X2
   kernin    float    Out      Residual
----------------------------------------------------------------------- */
float kernin (float x, float *rarg)
{
/* Constants */
	static float pi = 3.141592654f;
	static float sig = 2.f;
/* Local variables */
	float a, b, dx, q1, q2;
/* Function Body */
	dx = rarg[0] - x;
	a = dx * dx;
	b = sinf(pi * dx) * sinf(pi * dx / sig);
	q1 = safdiv(a, b) ? a / b : 1.f;
	q1 *= rarg[2];
	dx = rarg[1] - x;
	a = dx * dx;
	b = sinf(pi * dx) * sinf(pi * dx / sig);
	q2 = safdiv(a, b) ? a / b : 1.f;
	q2 *= rarg[3];
	a = q1 - q2;
	return a;
} /* end of kernin */

/*----------------------------------------------------------------------
qwm - quadratic weighted mean.  x is assumed sorted and equally spaced.

___Name______Type_____In/Out____Description_____________________________
   x[n]      float*   In        Data array
   y[n]      float*   In        Importance weights
   n         int      In        Length of arrays
   ave       float*   Out       Average
   brt       float*   Out       Brightness
   work[n]   float*   Neither   Cumulative y
----------------------------------------------------------------------- */
int qwm (float *x, float *y, int n, float *ave, float *brt,	float *work)
{
/* Local variables */
	int i, j, k;
	float a, b, g2, h, q, wjm1, xq[3], xspac;
	double swxy, swy, sy;
/* Function Body */
	if (n < 1) return -1;
	if (1 == n) {
		*ave = x[0];
		*brt = y[0];
		return 0;
	}
	sy = 0.;  /* sum of intensities */
	for (j = 0; j < n; ++j) {
		if (y[j] < 0.f) y[j] = 0.f;
		sy += y[j];
		work[j] = sy;
	}
	if (sy <= 0.f) return 1;
/* Weighted quartiles */
	xspac = (x[n-1] - x[0]) / (float) (n - 1);
	for (k = 1; k <= 3; ++k) {
		q = (float) k / 4.f * sy;
		rbsgt (work, n, q, &j);
		wjm1 = (j > 0) ? work[j-1] : 0.f;
		xq[k-1] = x[j] + ((q - wjm1) / y[j] - .5f) * xspac;
	}
/* Quadratic weighted mean */
	*ave = xq[1];
	g2 = 0.5f * (xq[2] - xq[0]);  /* semi-interquartile range */
	g2 = g2 * g2;
	swy = 0.;
	swxy = 0.;
	for (j = 0; j < n; ++j) {
		h = x[j] - *ave;
		h = h * h + g2;
		if (safdiv(y[j], h)) {
			h = y[j] / h;
		} else {
			return 2;
		}
		swy += h;
		swxy += h * (x[j] - *ave);
	}
	if (dafdiv(swxy, swy)) {
		*ave += swxy / swy;  /* weighted mean */
	} else {
		return 3;
	}
/* Maximum weight within 2g of the median */
	a = xq[1] - (xq[2] - xq[0]);
	b = xq[1] + (xq[2] - xq[0]);
	rbsgt (x, n, a, &j);
	rbslt (x, n, b, &k);
	*brt = 0.f;
	for (i = j; i <= k; ++i) if (y[i] > *brt) *brt = y[i];
	return 0;
} /* end of qwm */

/*----------------------------------------------------------------------
clef - Separate pixels into dark or light.  Make midi events.

___Name______________Type_____In/Out___Description______________________
   a[width*height]   float*   In       Bitmap of pixel intensities
   width             int      In       Width of image
   height            int      In       Height of image
   trnpos            float    In       Amount to adjust tuning
   raxbrt            float    In       Maxval from .PPM file
   et[elim]          int*     Out      Event times
   ek[elim]          int*     Out      Event Kinds (refer to WMIDI)
   ep1[elim]         int*     Out      Event parameter #1
   ep2[elim]         int*     Out      Event parameter #2
   etot              int*     Out      # events
----------------------------------------------------------------------*/
int clef (float *a, int width, int height, float trnpos, float raxbrt,
          int *et, int *ek, int *ep1, int *ep2, int *etot)
{
/* Constants */
	static float adparm = 1.f;
	static float mdparm = 3.f;
	static float bogus = -999.f;
	static float fmnnlo = 52.5f;
	static float fmnnhi = 84.5f;
	static float pi = 3.141592654f;
	static float pi2 = 9.869604401f;
	static float pip = 0.035f;
	static float re = 1.e-6f;
	static float sig = 2.f;
	static float respar = 9.765625e-4f;
	static float vcrit = 0.66f;
/* Local variables */
	int ex, exold, i, ierr, j, jmax, k, l, n, nold, pb, pbold, smoord, t, v;
	float atol, b, base, brt, bf, c, corex, fmark, fmean, fspac, fvar, parm,
	      pos, sa, sap, sap2, srrp, trans, usppx, ym;
	float rarg[4];
	bool sound;
/* Allocate arrays */
	int *bound, *length, *rind, *back;
	float *freq, *ampl, *work, *x, *y, *ave, *sumx, *dw, *great, *least;
	bound = (int *) malloc(width * sizeof(int));
	length = (int *) malloc(width * sizeof(int));
	rind = (int *) malloc(width * sizeof(int));
	back = (int *) malloc(2 * width * sizeof(int));
	freq = (float *) malloc(VERT * sizeof(float));
	ampl = (float *) malloc(VERT * sizeof(float));
	work = (float *) malloc(VERT * sizeof(float));
	x = (float *) malloc(width * sizeof(float));
	y = (float *) malloc(width * sizeof(float));
	ave = (float *) malloc(width * sizeof(float));
	sumx = (float *) malloc(width * sizeof(float));
	dw = (float *) malloc(width * sizeof(float));
	great = (float *) malloc(width * sizeof(float));
	least = (float *) malloc(width * sizeof(float));
	if (!bound || !length || !rind || !back || !freq || !ampl || !work
	 || !x || !y || !ave || !sumx || !dw || !great || !least) {
		printf("clef: couldn't allocate arrays!\n");
		return 1;
	}
/* Function Body */
	bf = 8192.f * 100.f / PBR;
	fspac = (fmnnhi - fmnnlo) / VERT;
	parm = 1.f + 1.f / respar;
	atol = 0.1f / (bf * fspac);  /* absolute error tolerance */
	fmark = fmnnlo - 0.5f * fspac;  /* frequency of zeroth pixel */
	if (width < 1 || height < 1) return 2;
	if (height != VERT) return 3;
	for (j = 0; j < VERT; ++j) freq[j] = fmark + (j + 1) * fspac + trnpos;
/* Pitch and amplitude each time step. */
	for (i = 0; i < width; ++i) {
		k = 0;
		jmax = 0;
		ym = 0.f;
		for (j = 0; j < VERT; ++j) {
			ampl[j] = a[i+j*width] / raxbrt;
			if (a[i+j*width] > 0.5f) ++k;
			if (ampl[j] > ym) {
				jmax = j;
				ym = ampl[j];
			}
		}
		if (0 == k) {  /* silence */
			x[i] = bogus;
			y[i] = 0.f;
			continue;
		} else if (1 == k) {  /* one pixel */
			x[i] = freq[jmax];
			y[i] = ym;
			continue;
		}
		sa = 0.f;  /* weighted variance */
		sap = 0.f;
		for (j = 0; j < VERT; ++j) {
			sa += ampl[j];
			sap += ampl[j] * (float) j;
		}
		fmean = sap / sa;
		sap2 = 0.f;
		for (j = 0; j < VERT; ++j) {
			sap = (float) j - fmean;
			sap2 += ampl[j] * sap * sap;
		}
		fvar = sap2 / sa;
/* De-interpolate in units of pixels and linear light. */
		if (fvar < vcrit) {  /* attempt if variance is small */
			ym = 0.f;
			jmax = 1;
			for (j = 1; j < VERT; ++j) {
				sa = ampl[j-1] + ampl[j];
				if (sa > ym) {
					ym = sa;
					jmax = j;
				}
			}
			rarg[0] = (float) jmax;
			rarg[1] = (float) (jmax + 1);
			rarg[2] = respar * (powf(parm, ampl[jmax-1]) - 1.f);
			rarg[3] = respar * (powf(parm, ampl[jmax]) - 1.f);
			b = rarg[0] + atol;
			c = rarg[1] - atol;
			pos = b;
			ierr = solv (kernin, &pos, &c, re, atol, rarg);
			if (0 == ierr || 2 == ierr) {  /* error 2 is success for present purposes */
				if (rarg[2] > rarg[3]) {
					c = rarg[0];
					ym = rarg[2];
				} else {
					c = rarg[1];
					ym = rarg[3];
				}
				sap = c - pos;
				b = pi2 * sap * sap;
				c = sig * sinf(pi * (c - pos)) * sinf(pi * (c - pos) / sig);
				brt = safdiv(b, c) ? b / c : 1.f;
				brt *= ym;
				x[i] = fmark + pos * fspac + trnpos;  /* pixel -> MIDI note */
				y[i] = logf(1.f + brt / respar) / logf(parm);
				continue;
			}
		}
/* Fallback routine in units of Midi note and log-amplitude */
		for (j = 0; j < VERT; ++j) {
			ampl[j] = respar * (powf(parm, ampl[j]) - 1.f);
		}
		ierr = qwm (freq, ampl, VERT, &pos, &brt, work);
		if (0 != ierr) {
			printf("qwm returns #%i\n", ierr);
			return 4;
		}
		brt = logf(1.f + brt / respar) / logf(parm);
		brt = fminf(fmaxf(0.f, brt), 1.f);
		x[i] = pos;
		y[i] = brt;
	}
/* Choose sounding or silent by dynamic programming. */
	for (i = 0; i < width; ++i) {  /* Limit to 0...1 */
		y[i] = fminf(fmaxf(0.f, y[i]), 1.f);
		great[i] = (y[i] * raxbrt + 1.f) / (raxbrt + 1.f);  /* can't take log of 0 */
	}
	srrp = sqrtf(respar);
	trans = logf(raxbrt) * (float) PXPS * pip;
	ierr = dyrb (great, width, srrp, trans, bound, back);
	if (0 != ierr) printf("dyrb returns#%i\n", ierr);
	for (k = 0; k < width; ++k) {
		if (0 == bound[k]) {
			x[k] = bogus;  /* force silence */
			y[k] = 0.f;
		}
	}
	for (k = width-2; k >= 1; --k) {
		if (1 == bound[k] && x[k] < 0.f) {
			x[k] = x[k+1];  /* fill in */
			y[k] = y[k+1];
		}
	}
	for (k = 1; k <= width-2; ++k) {
		if (1 == bound[k] && x[k] < 0.f) {
			x[k] = x[k-1];
			y[k] = y[k-1];
		}
	}
/* Merge contiguous frequencies into notes. */
	sound = false;  /* mark note beginnings */
	k = 0;
	for (j = 0; j < width; ++j) {
		if (!sound && x[j] >= 0.f) {
			sound = true;
			++k;
			bound[k-1] = j;
		} else if (sound && (x[j] < 0.f || j == width-1)) {
			sound = false;
			length[k-1] = j - bound[k-1];
		}
	}
/* Smoothing pass */
	smoord = (int) (PXPS * pip);
	for (j = 0; j < k; ++j) {
		smooth (&x[bound[j]], length[j], smoord, least, great, ave);
		smooth (&y[bound[j]], length[j], smoord, least, great, ave);
	}
	k = 1;
	glom (x, width, adparm, mdparm, &k, bound, length, ave,
	      least, great, sumx, dw, back, &back[width], rind);
/* max velocity per note in sumx[] */
	i = 0;
	for (j = 0; j < k; ++j) {
		ave[j] = 0.5f * (least[j] + great[j]);
		sumx[j] = 0.f;
		l = length[j];
		if (l <= 0) {
			printf("clef: warning zero length %i\n", l);
			return 5;
		}
		for (t = 0; t < l; ++t) {
			if (i >= width) {
				printf("clef: out of bounds at velocity\n");
				return 6;
			}
			if (y[i] > sumx[j]) sumx[j] = y[i];
			++i;
		}
		if (ave[j] < 0.f) sumx[j] = bogus;
	}
/* Make events */
	base = sqrt(parm);
	corex = sqrt(1.f + respar);
	usppx = 1000000.f / (float) PXPS;
	ex = 127;  /* default max for expression */
	pb = 8192;  /* default center for pitch bend */
	j = 0;
	l = 0;  /* zero offset */
	n = lroundf(bogus);
	nold = n;
	v = 0;
	for (i = 0; i < width; ++i) {
		nold = n;
		exold = ex;
		pbold = pb;
		t = lroundf((i + 1) * usppx);
		if (i >= l) {  /* advance to next segment? */
			if (j >= k) return 7;
			l = bound[j] + length[j];
			n = lroundf(ave[j]);  /* per-segment pitch */
			if (n > 127) n = 127;
/*----------------------------------------------------------------------
Quote from the MIDI specification:

    "Preferably, application of velocity to volume should be an exponential
     function. This is the suggested default action;"

Program SPECTR sets the image intensity to the logarithm of sound amplitude.
Here velocity is set directly proportional to image intensity.  Thus,
an exponential interpretation of velocity restores the original volume.
----------------------------------------------------------------------*/
			if (sumx[j] > 0.f) {
				v = lroundf(sumx[j] * 127.f);  /* per-segment velocity */
				if (v < 1) v = 1;
				if (v > 127) v = 127;
			} else {
				v = 0;
			}
			++j;
		}
/* Note Off */
		if (nold >= 0 && n != nold) {
			if (*etot >= ELIM) return 8;
			et[*etot] = t;
			ek[*etot] = 0;
			ep1[*etot] = nold;
			ep2[*etot] = 0;
			++(*etot);
		}
		if (v > 0) {  /* sounding ? */
			if (n < 0) {
				printf("clef: positive v, negative n !!\n");
				return 9;
			}
/*----------------------------------------------------------------------
Expression
The MIDI spec describes the possibility of using volume only, or volume
and expression.  The equation for volume only is:

     L = 40 log (V/127)

where L is amplitude in decibels, and V is the MIDI volume.
Program SCRIBE uses expression only.  In program SPECTR, intensity was:

            log(A + p A_max) - log(p A_max)
     Y = ------------------------------------ R
          log(A_max + p A_max) - log(p A_max)

where p is on the order of the precision, and R is the maximum brightness.
A decibel of amplitude, defined so as to avoid taking a logarithm of zero:

                   A + p A_max
     dBA = 20 log(-------------)
                      A_max

After some algebra, it appears:

     E = 127 (1 + p)^(1/2) (1/p + 1)^( (1/2) (Y/R - V/127))

----------------------------------------------------------------------*/
			ex = lroundf( 127.f * corex * powf(base, (y[i] - (float) v / 127.f)) );
			if (ex < 0) ex = 0;
			if (ex > 127) ex = 127;
			if (ex != exold) {
				if (*etot >= ELIM) return 10;
				et[*etot] = t;
				ek[*etot] = 12;
				ep1[*etot] = ex;
				++(*etot);
			}
/* Pitch bend */
			pb = 8192 + lroundf( (x[i] - (float) n) * bf );
			if (pb < 0) pb = 0;
			if (pb > 16383) pb = 16383;
			if (pb != pbold) {
				if (*etot >= ELIM) return 11;
				et[*etot] = t;
				ek[*etot] = 57;
				ep1[*etot] = pb;
				++(*etot);
			}
		}
/* Note On */
		if (n >= 0 && n != nold) {
			if (*etot >= ELIM) return 12;
			et[*etot] = t;
			ek[*etot] = 1;
			ep1[*etot] = n;
			ep2[*etot] = v;
			++(*etot);
		}
	}  /* next time step */
/* Turn off any note left sounding */
	if (nold >= 0) {
		if (*etot >= ELIM) return 13;
		et[*etot] = lroundf( (width + 1) * usppx );
		ek[*etot] = 0;
		ep1[*etot] = nold;
		ep2[*etot] = 0;
		++(*etot);
	}
/* Release memory */
	free (bound);
	free (length);
	free (rind);
	free (back);
	free (freq);
	free (ampl);
	free (work);
	free (x);
	free (y);
	free (ave);
	free (sumx);
	free (dw);
	free (great);
	free (least);
	return 0;
} /* end of clef */

/*----------------------------------------------------------------------
rpgm - read a portable gray map

___Name___________Type______In/Out___Description________________________
   ifil           char*     In       Filename
   a[horz*vert]   float**   In       Pointer to bitmap array
   horz           int*      Out      Width of image
   vert           int*      Out      Height of image
   maxbrt         int*      Out      Maximum intensity
----------------------------------------------------------------------*/
int rpgm (char *ifil, float **a, int *horz, int *vert, int *maxbrt)
{
/* Local variables */
	int c, h, hlim, i, j, k, l, llim, n, pixl, pos;
	char ftyp[3], header[32], declar[32];
	bool coment, prevw;
	FILE *iou;
	static char blank[32] = "                               ";
/* Function Body */
	iou = fopen(ifil, "rb");
	if (NULL == iou) {
		printf("trouble opening file: %s\n", ifil);
		return 1;
	}
	j = fseek(iou, -1L, SEEK_END);  /* get file length */
	if (0 != j) {
		printf("trouble getting length of file %s\n", ifil);
		return 2;
	}
	n = ftell(iou) + 1;
	rewind(iou);
/* Interpret header. */
	pos = 0;  /* position in file */
	j = 0;  /* # white space intervals */
	l = 0;  /* position in header */
	strncpy(header, blank, 32);
	prevw = false;
	coment = false;
	while (j < 4) {
		++pos;
		c = fgetc(iou);
		if (EOF == c) {
			printf("rpgm: trouble reading in while loop!\n");
			return 3;
		}
		if (35 == c) coment = true;
		if (10 == c) coment = false;
		if (coment) continue;
		if ((c >= 9 && c <= 13) || c == 32) {
			if (! prevw) {
				++j;
				header[l] = ' ';
				++l;
				prevw = true;
			}
		} else {
			prevw = false;
			header[l] = (char) c;
			++l;
		}
		if (l >= 32) {
			printf("header too long for PNM file format\n");
			return 4;
		}
	}
	c = sscanf(header, "%s %i %i %i", declar, horz, vert, maxbrt);
	if (4 != c) {
		printf("rpgm: trouble scanning header!\n");
		return 5;
	}
	ftyp[0] = declar[0];
	ftyp[1] = declar[1];
	if ((0 != strncmp(ftyp, "P2", 2)) &&
	    (0 != strncmp(ftyp, "P3", 2)) &&
	    (0 != strncmp(ftyp, "P5", 2)) &&
	    (0 != strncmp(ftyp, "P6", 2)) ) {
		return 6;
	}
	llim = *horz * *vert;
	(*a) = (float *) malloc(llim * sizeof(float));
	if (!(*a)) {
	 	printf("couldn't allocate bitmap!\n");
	 	return 7;
	}
	if ( *maxbrt > 65535 && (0 != strncmp(ftyp, "P2", 2))
	                             && (0 != strncmp(ftyp, "P3", 2)) ) {
		printf("maxbrt too big!\n");
		return 8;
	}
	hlim = (*maxbrt > 255) ? 1 : 0;
	if (0 == strncmp(ftyp, "P3", 2)) goto L30;
	if (0 == strncmp(ftyp, "P5", 2)) goto L50;
	if (0 == strncmp(ftyp, "P6", 2)) goto L60;
/* Format P2 */
	j = 0;
	l = 0;
	prevw = false;
	coment = false;
	strncpy(header, blank, 32);
	for (i = pos; i < n; ++i) {
		c = fgetc(iou);
		if (EOF == c) {
			printf("rpgm: trouble reading P2 data\n");
			return 9;
		}
		if (35 == c) coment = true;
		if (10 == c) coment = false;
		if (coment) continue;
		if ((c >= 9 && c <= 13) || c == 32) {
			if (! prevw) {
				c = sscanf(header, "%i", &h);
				if (1 != c) {
					printf("rpgm: trouble internal I/O, format P2\n");
					return 10;
				}
				if (h > *maxbrt) {
					printf("rpgm: pixel too bright!\n");
					return 11;
				}
				if (l >= llim) {
					printf("rpgm: out of memory!\n");
					return 12;
				}

				(*a)[l] = (float) h;
				++l;
				prevw = true;
				strncpy(header, blank, 32);
				j = 0;
			}
		} else {
			prevw = false;
			header[j] = (char) c;
			++j;
		}
	}
	goto L90;
/* Format P3 */
L30:
	j = 0;
	k = 0;
	l = 0;
	pixl = 0;
	prevw = false;
	coment = false;
	strncpy(header, blank, 32);
	for (i = pos; i < n; ++i) {
		c = fgetc(iou);
		if (EOF == c) {
			printf("rpgm: trouble reading P3 data\n");
			return 13;
		}
		if (35 == c) coment = true;
		if (10 == c) coment = false;
		if (coment) continue;
		if ((c >= 9 && c <= 13) || c == 32) {
			if (! prevw) {
				c = sscanf(header, "%i", &h);
				if (1 != c) {
					printf("rpgm: trouble internal I/O, format P3\n");
					return 14;
				}
				if (h > *maxbrt) {
					printf("rpgm: color too bright\n");
					return 15;
				}
				++k;
				pixl += h;
				if (3 == k) {
					if (l >= llim) {
						printf("rpgm:  out of memory!\n");
						return 16;
					}
					(*a)[l] = (float) pixl;
					++l;
					k = 0;
					pixl = 0;
				}
				prevw = true;
				strncpy(header, blank, 32);
				j = 0;
			}
		} else {
			prevw = false;
			header[j] = (char) c;
			++j;
		}
	}
	*maxbrt *= 3;
	goto L90;
/* Format P5 */
L50:
	l = 0;
/*	if (*vert * *horz > llim) {
		printf("rpgm: image too big for allocated memory!\n");
		return 17;
	}
*/
	for (j = 0; j < *vert; ++j) {
		for (i = 0; i < *horz; ++i) {
			n = 0;
			for (h = hlim; h >= 0; --h) {
				c = fgetc(iou);
				if (EOF == c) {
					printf("rpgm: error reading P5 data at %i %i %i\n", j, i, h);
					return 18;
				}
				n |= (c & 255) << (h << 3);
			}
			(*a)[l] = (float) n;
			++l;
		}
	}
	goto L90;
/* Format P6 */
L60:
	l = 0;
/*	if (*vert * *horz > llim) {
		printf("rpgm: image too big for allocated memory!\n");
		return 19;
	}
*/
	for (j = 0; j < *vert; ++j) {
		for (i = 0; i < *horz; ++i) {
			pixl = 0;
			for (k = 0; k < 3; ++k) {
				n = 0;
				for (h = hlim; h >= 0; --h) {
					c = fgetc(iou);
					if (EOF == c) {
						printf("rpgm: trouble reading P6 data %i %i %i %i\n", j, i, k, h);
						return 20;
					}
					n |= (c & 255) << (h << 3);
				}
				pixl += n;
			}
			(*a)[l] = (float) pixl;
			++l;
		}
	}
	*maxbrt *= 3;
/* Done. */
L90:
	c = fclose(iou);
	if (0 != c) {
		printf("trouble closing input file!\n");
		return 21;
	}
	return 0;
} /* end of rpgm */

/*----------------------------------------------------------------------
      Convert an integer to MIDI variable-byte format
----------------------------------------------------------------------*/
void wvarb (int n, unsigned char *varb, int *p)
{
	if (n <= 127) {
		*p = 1;
		varb[0] = (unsigned char) (n & 127);
	} else if (n <= 16383) {
		*p = 2;
		varb[0] = (unsigned char) (((n & 16256) >> 7) | 128) ;
		varb[1] = (unsigned char) (n & 127);
	} else if (n <= 2097151) {
		*p = 3;
		varb[0] = (unsigned char) (((n & 2080768) >> 14) | 128);
		varb[1] = (unsigned char) (((n & 16256) >> 7) | 128);
		varb[2] = (unsigned char) (n & 127) ;
	} else if (n <= 268435455) {
		*p = 4;
		varb[0] = (unsigned char) (((n & 266338304) >> 21) | 128);
		varb[1] = (unsigned char) (((n & 2080768) >> 14) | 128) ;
		varb[2] = (unsigned char) (((n & 16256) >> 7) | 128) ;
		varb[3] = (unsigned char) (n & 127);
	} else {
		printf("number too large for variable-format\n");
		*p = -1;
	}
	return;
} /* end of wvarb */

/*----------------------------------------------------------------------
wmidi - write a Standard MIDI file.  This subroutine was based on
the much longer version appearing in project MIXMIDI, which supports
the entire MIDI 1.0 specification.

                   SUPPORTED EVENT KINDS

___Kind___Description_____Par._1___Par._2_____MIDI_#____________________
    0     Note Off        Note #   Velocity   8
    1     Note On         Note #   Velocity   9
   12     Expression      7 bits              11-11
   55     Patch change    Patch #             12
   57     Pitch Bend      14 bits             14
   61     Text            Offset   Length     15-15-1
   62     Copyright       Offset   Length     15-15-2
   63     Sequence Name   Offset   Length     15-15-3
   76     Bend Range      Cents               RPN 0

                        I/O LIST

___Name________Type____In/Out___Description_____________________________
   fname       char*   In       Output filename
   text        char*   In       Text of name, copyright, etc.
   etot        int     In       Count of all events
   et[etot]    int*    In       Event times [microseconds]
   ek[etot]    int*    In       Event kind
   ep1[etot]   int*    In       Event parameter 1
   ep2[etot]   int*    In       Event parameter 2
----------------------------------------------------------------------*/
int wmidi (char *fname, char *text, int etot, int *et, int *ek, int *ep1, int *ep2)
{
/* Initialized data */
	static int mthd[8] = { 77, 84, 104, 100, 0, 0, 0, 6 };
	static int mtrk[4] = { 77, 84, 114, 107 };
/* Local variables */
	int b, c, g, h, i, m, p, s, kind, sprev;
	long l;
	uint32_t n;
	unsigned char varb[4];
	float tpus;
	FILE *iou;
/* Function Body */
	tpus = TIMDIV * BPM / 60.f / 1000000.f;  /* Convert times to ticks */
	for (i = 0; i <= etot; ++i) et[i] = lroundf((float)et[i] * tpus);
/* File I/O */
	iou = fopen(fname, "wb");
	if (NULL == iou) {
		printf("trouble opening file %s\n", fname);
		return 1;
	}
/* Write the header */
	for (i = 0; i < 8; ++i) {
		c = fputc(mthd[i], iou);
		if (EOF == c) return 2;
	}
/* File format / type 0 */
	if (fputc(0, iou) == EOF) return 3;
	if (fputc(0, iou) == EOF) return 4;
/* Number of tracks */
	if (fputc(0, iou) == EOF) return 5;
	if (fputc(1, iou) == EOF) return 6;
/* Time division */
	b = (TIMDIV & 65280) >> 8;
	c = TIMDIV & 255;
	if (fputc(b, iou) == EOF) return 7;
	if (fputc(c, iou) == EOF) return 8;
	i = 14;  /* at end of file header */
	g = 0;  /* no events processed */
	for (h = 0; h < 4; ++h) if (fputc(mtrk[h], iou) == EOF) return 9;  /* write 'MTrk' */
	i += 4;
/* Remember file position for track length */
	l = i;
	i += 4;
	c = fseek(iou, 4L, SEEK_CUR);
	if (0 != c) {
		printf("trouble advancing file position!\n");
		return 10;
	}
/* Process events */
	s = 0;  /* at beginning */
	for (g = 0; g < etot; ++g) {  /* for all events in track */
		kind = ek[g];
/* Write delta-time */
		sprev = s;
		s = et[g];
		m = s - sprev;
		if (m < 0) {
			printf("wmidi: negative delta-time!\n");
			return 11;
		}
		wvarb (m, varb, &p);  /* convert to variable bytes */
		if (-1 == p) {
			printf("can't convert delta time of %i\n", m);
			return 12;
		}
		for (h = 0; h < p; ++h) if (fputc((int) varb[h], iou) == EOF) return 13;
		i += p;
/* NoteOff - zero velocity NoteOn */
		if (0 == kind) {
			m = 144 | CHANEL;
			if (fputc(m, iou) == EOF) return 14;
			if (fputc(ep1[g], iou) == EOF) return 15;
			if (fputc(0, iou) == EOF) return 16;
			i += 3;
/* NoteOn */
		} else if (1 == kind) {
			m = 144 | CHANEL;
			if (fputc(m, iou) == EOF) return 17;
			if (fputc(ep1[g], iou) == EOF) return 18;
			if (fputc(ep2[g], iou) == EOF) return 19;
			i += 3;
/* Expression */
		} else if (12 == kind) {
			m = 176 | CHANEL;
			if (fputc(m, iou) == EOF) return 20;
			if (fputc(11, iou) == EOF) return 21;
			if (ep1[g] < 0) {
				printf("wmidi: negative expression!\n");
				return 22;
			}
			if (fputc(ep1[g], iou) == EOF) return 23;
			i += 3;
/* Patch change */
		} else if (55 == kind) {
			m = 192 | CHANEL;
			if (fputc(m, iou) == EOF) return 24;
			if (fputc(ep1[g], iou) == EOF) return 25;
			i += 2;
/* Pitch bend */
		} else if (57 == kind) {
			m = 224 | CHANEL;
			if (fputc(m, iou) == EOF) return 26;
			m = ep1[g] & 127;
			if (fputc(m, iou) == EOF) return 27;
			m = (ep1[g] & 16256) >> 7;
			if (fputc(m, iou) == EOF) return 28;
			i += 3;
/* Text-type events */
		} else if (kind >= 61 && kind <= 63) {
			m = kind - 60;
			if (fputc(255, iou) == EOF) return 29;  /* meta */
			if (fputc(m, iou) == EOF) return 30;
			i += 2;
			m = ep2[g];
			wvarb (m, varb, &p);
			if (-1 == p) {
				printf("can't convert text length %i\n", m);
				return 31;
			}
			for (h = 0; h < p; ++h) if (fputc((int) varb[h], iou) == EOF) return 32;  /* write length */
			i += p;
			p = ep1[g];  /* pointer */
			for (h = p; h < p+m; ++h) if (fputc((int) text[h], iou) == EOF) return 33; /* write string */
			i += m;
/* Registered parameter - pitch bend range */
		} else if (76 == kind) {
			m = 176 | CHANEL;
			if (fputc(m, iou) == EOF) return 34;  /* coarse adjust */
			if (fputc(101, iou) == EOF) return 35;
			if (fputc(0, iou) == EOF) return 36;
			if (fputc(0, iou) == EOF) return 37;  /* delta-time */
			if (fputc(m, iou) == EOF) return 38;  /* fine adjust */
			if (fputc(100, iou) == EOF) return 39;
			if (fputc(0, iou) == EOF) return 40;
			if (fputc(0, iou) == EOF) return 41;  /* delta-time */
			if (fputc(m, iou) == EOF) return 42;
			if (fputc(6, iou) == EOF) return 43;  /* coarse slider */
			if (fputc(ep1[g] / 100, iou) == EOF) return 44;
			if (fputc(0, iou) == EOF) return 45;  /* delta-time */
			if (fputc(m, iou) == EOF) return 46;
			if (fputc(38, iou) == EOF) return 47;  /* fine slider */
			if (fputc(ep1[g] % 100, iou) == EOF) return 48;
			i += 15;
		} else {
			printf("unrecognized event kind %i\n", kind);
			return 49;
		}
	}  /* next event */
/* End of track */
	if (fputc(0, iou) == EOF) return 50;
	if (fputc(255, iou) == EOF) return 51;
	if (fputc(47, iou) == EOF) return 52;
	if (fputc(0, iou) == EOF) return 53;
	i += 4;
/* Length of track events */
	n = i - l - 4;
	c = fseek(iou, l, SEEK_SET);
	if (0 != c) {
		printf("wmidi: trouble seeking to file position\n");
		return 54;
	}
	b = (n & 4278190080UL) >> 24;
	if (fputc(b, iou) == EOF) return 55;
	b = (n & 16711680) >> 16;
	if (fputc(b, iou) == EOF) return 56;
	b = (n & 65280) >> 8;
	if (fputc(b, iou) == EOF) return 57;
	b = n & 255;
	if (fputc(b, iou) == EOF) return 58;
/* done */
	i = fclose(iou);
	if (0 != i) {
		printf("wmidi: trouble closing file\n");
		return 59;
	}
	return 0;  /* success */
} /* end of wmidi */

/*----------------------------------------------------------------------
              Main program
----------------------------------------------------------------------*/
int main (int argc, char **argv)
{
/* Local variables */
	int c, etot, height, i, ierr, j, k, l, maxbrt, narg, offset, patch, width;
	float swap, raxbrt, trnpos;
	char ifil[OPTL], ofil[OPTL], opt[OPTL], text[BUFL];
	FILE *iou;
/* allocate arrays */
	int *ek, *ep1, *ep2, *et;
	float *a;
	float **ptrbuf;
	ek = (int *) malloc(ELIM * sizeof(int));
	ep1 = (int *) malloc(ELIM * sizeof(int));
	ep2 = (int *) malloc(ELIM * sizeof(int));
	et = (int *) malloc(ELIM * sizeof(int));
	if (!ek || !ep1 || !ep2 || !et) {
		printf("couldn't allocate memory!\n");
		exit(EXIT_FAILURE);
	}
/* Begin.  Read command line. */
	etot = 0;
	offset = 0;
	trnpos = 0.f;
	narg = argc - 1;
	if (narg < 2) {
		printf("files not specified\n");
		exit(EXIT_FAILURE);
	}
	if (narg > 99) {
		printf("too many arguments!\n");
		exit(EXIT_FAILURE);
	}
	strncpy(ifil, argv[narg-1], OPTL-1);  /* read file name from command line */
	strncpy(ofil, argv[narg], OPTL-1);
/* Check files for existence */
	iou = fopen(ifil, "r");
	if (NULL == iou) {
		printf("can't open input file\n");
		exit(EXIT_FAILURE);
	}
	fclose(iou);
	iou = fopen(ofil, "r");
	if (NULL != iou) {
		printf("File: %s already exists.  Please choose another name.\n", ofil);
		exit(EXIT_FAILURE);
	}
/* Options processing loop */
	for (i = 1; i < narg-1; ++i) {
		strncpy(opt, argv[i], OPTL-1);
		for (j = 0; j < OPTL; ++j) {
			if (!opt[j]) break;
			opt[j] = (char) toupper(opt[j]);
		}
/* Text events.  Beware the nonstandard code numbers. */
		if (NULL != strstr(opt, "--TEXT")) {
			strncpy(opt, argv[i+1], OPTL-1);
			l = strlen(opt);
			if (offset + l > BUFL) {
				printf("too much text\n");
				exit(EXIT_FAILURE);
			}
			et[etot] = 0;
			ek[etot] = 61;
			ep1[etot] = offset;
			ep2[etot] = l;
			++etot;
			for (k = 0; k < l; ++k) text[offset+k] = opt[k];
			offset += l;
		} else if (NULL != strstr(opt, "--COPYRIGHT")) {
			strncpy(opt, argv[i+1], OPTL-1);
			l = strlen(opt);
			if (offset + l > BUFL) {
				printf("too much text\n");
				exit(EXIT_FAILURE);
			}
			et[etot] = 0;
			ek[etot] = 62;
			ep1[etot] = offset;
			ep2[etot] = l;
			++etot;
			for (k = 0; k < l; ++k) text[offset+k] = opt[k];
			offset += l;
		} else if (NULL != strstr(opt, "--SEQNAME")) {
			strncpy(opt, argv[i+1], OPTL-1);
			l = strlen(opt);
			if (offset + l > BUFL) {
				printf("too much text\n");
				exit(EXIT_FAILURE);
			}
			et[etot] = 0;
			ek[etot] = 63;
			ep1[etot] = offset;
			ep2[etot] = l;
			++etot;
			for (k = 0; k < l; ++k) text[offset+k] = opt[k];
			offset += l;
/* Patch change */
		} else if (NULL != strstr(opt, "--PATCH")) {
			strncpy(opt, argv[i+1], OPTL-1);
			c = sscanf(opt, "%i", &patch);
			if (1 != c) {
				printf("trouble reading patch change!\n");
				exit(EXIT_FAILURE);
			} else if (patch < 0 || patch > 127) {
				printf("patch numbers are from 0 to 127\n");
				exit(EXIT_FAILURE);
			}
			et[etot] = 0;
			ek[etot] = 55;
			ep1[etot] = patch;
			++etot;
/* Tuning option */
		} else if (NULL != strstr(opt, "--TUNE")) {
			strncpy(opt, argv[i+1], OPTL-1);
			c = sscanf(opt, "%f", &trnpos);
			if (1 != c) {
				printf("trouble reading retuning option!\n");
				exit(EXIT_FAILURE);
			}
		}
	}
/* Read in a standard .PGM file */
	ptrbuf = &a;
	ierr = rpgm (ifil, &a, &width, &height, &maxbrt);
	a = *ptrbuf;  /* fewer asterisks */
	if (0 != ierr) {
		printf("error %i reading from file: %s \n", ierr, ifil);
		exit(EXIT_FAILURE);
	}
	if (height != VERT) {
		printf("scribe: height must be %i pixels!\n", VERT);
		exit(EXIT_FAILURE);
	}
	j = height;  /* flip upside-down */
	for (i = 0; i < height/2; ++i) {
		--j;
		for (k = 0; k < width; ++k) {
			swap = a[k + i * width];
			a[k + i * width] = a[k + j * width];
			a[k + j * width] = swap;
		}
	}
	raxbrt = (float) maxbrt;
	et[etot] = 0;  /* set pitch bend range */
	ek[etot] = 76;
	ep1[etot] = PBR;
	++etot;
/* Interpret */
	ierr = clef (a, width, height, trnpos, raxbrt, et, ek, ep1, ep2, &etot);
	if (0 != ierr) {
		printf("clef returns #%i\n", ierr);
		exit(EXIT_FAILURE);
	}
	if (etot >= ELIM) printf("scribe:  out of memory\n");
/* Write out */
	ierr = wmidi (ofil, text, etot, et, ek, ep1, ep2);
	if (0 != ierr) {
		printf("wmidi returns #%i\n", ierr);
		exit(EXIT_FAILURE);
	}
/* Release memory */
	free(ek);
	free(ep1);
	free(ep2);
	free(et);
	free(a);
	printf("program complete\n");
	exit(EXIT_SUCCESS);
} /* end of main */
/*--------------------- end of file scribe.c -------------------------*/
