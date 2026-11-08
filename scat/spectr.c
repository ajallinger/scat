/*----------------------------------------------------------------------
spectr.c - read digital audio, draw spectrogram as a Portable Grey Map.
by Andy Allinger, 2016-2021, released to the public domain

    Permission  to  use, copy, modify, and distribute this software and
    its documentation  for  any  purpose  and  without  fee  is  hereby
    granted,  without any conditions or restrictions.  This software is
    provided "as is" without express or implied warranty.

________________________OPTIONS________________________________________
   --tune x     transpose input up by x semitones
   --swab       reverse byte order of the input
----------------------------------------------------------------------*/
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

bool safdiv (float numer, float denom);  /* defined in safdiv.c */
bool dafdiv (double numer, double denom);
void isortr (float *x, int *ind, int n);  /* defined in isortr.c */
void rorder (float *a, int *ip, int n);  /* defined in rorder.c */
void rbsgt (float *x, int n, float v, int *i);  /* defined in search.c */
void rbslt (float *x, int n, float v, int *i);
void ibsge (int *x, int n, int v, int *i);
int filter (float *s, int l, float fsamp, float fpass, float fstop, float hpass, float hstop); /* defined in filter.c */
int frat (float *s, int l, int m, int n, int r, int stride, float hop, int fram, /* defined in frat.c */
           float *freq, float *ampl, int *nef, double *t, double *w, double *z);

#define BAND 32
#define CHLIM 256
/* 5 min @ 60 s/min @ 240 px/s */
#define FLIM 128
/* 5 min @ 60 s/min @ 48000 samp/s */
#define MAXBRT 255
#define MNNSUB 52
#define NLIM 27720  /* arbitrary */
#define PXPS 240
#define VERT 640
#define TARGN 3000

/*----------------------------------------------------------------------
Reverse the byte order of a 32-bit float
----------------------------------------------------------------------*/
void swab32 (float *r)
{
/* Local variables */
	unsigned char b0, b1, b2, b3;
	int32_t *iv;
/* Begin. */
	iv = (int32_t *) r;
	b0 = (*iv & 255);
	b1 = (*iv & 65280) >> 8;
	b2 = (*iv & 16711680) >> 16;
	b3 = (*iv & 4278190080UL) >> 24;
	*iv = b3;
	*iv |= b2 << 8;
	*iv |= b1 << 16;
	*iv |= b0 << 24;
	return;
} /* end of swab32 */

/*----------------------------------------------------------------------
rwavmo - read a wave file into mono

___Name__________Type______In/Out___Description_________________________
   ifil[CHLIM]   char*     In       Name of input file
   fillen        int       In       Length of input file in bytes
   bysw          bool      In       Reverse byte order of floats
   s[*l]         float**   Out      Buffer of samples
   l             int*      Out      Length of s
   rate          int*      Out      Samples / second
----------------------------------------------------------------------*/
int rwavmo (char *ifil, int fillen, bool bysw, float **s, int *l, int *rate)
{
/* Initialized data */
	static char idriff[4] = "RIFF";
	static char idrifx[4] = "RIFX";
	static char idwave[4] = "WAVE";
	static char idfmt[4] = "fmt ";
	static char iddata[4] = "data";
/* Local variables */
	int i, j, k;
	int blkaln, bps, bytrat, c, chanel, chnksz, ibeg, iend, ish,
	    nby, sampl, shift, stride, thresh;
	long pos;
	float divid, fch, flosam;
	bool fp;
	char tag[4];
	FILE *iou;
/* Open input file */
	*l = 0;
	*rate = 0;
	iou = fopen(ifil, "rb");
	if (NULL == iou) return 1;
/* Read RIFF */
	pos = 0;
	for (j = 0; j < 4; ++j) {
		++pos;
		if (pos > fillen) goto L90;
		c = fgetc(iou);
		if (EOF == c) {
			printf("trouble reading byte %i of RIFF\n", j);
			return 2;
		}
		tag[j] = (char) c;
	}
	if (0 == strncmp(tag, idriff, 4)) {
		stride = +1;
	} else if (0 == strncmp(tag, idrifx, 4)) {
		stride = -1;
	} else {
		return 3;
	}
/* Read file size. */
	i = 0;
	for (j = 0; j < 4 ; ++j) {
		++pos;
		if (pos > fillen) goto L90;
		c = fgetc(iou);
		if (EOF == c) {
			printf("trouble reading file size byte %i\n", j);
			return 4;
		}
		i |= (c & 255) << (j << 3);
	}
	if (i != fillen - 8) {
		printf("i is %i but measured file length is: %i\n", i, fillen);
		return 5;
	}
/* Read WAVE */
	for (j = 0; j < 4; ++j) {
		++pos;
		if (pos > fillen) goto L90;
		c = fgetc(iou);
		if (EOF == c) {
			printf("trouble reading byte %i of WAVE\n", j);
			return 6;
		}
		tag[j] = (char) c;
	}
	if (0 != strncmp(tag, idwave, 4)) return 7;
/* Read chunk descriptor */
L10:
	for (j = 0; j < 4; ++j) {
		++pos;
		if (pos > fillen) goto L90;
		c = fgetc(iou);
		if (EOF == c) {
			printf("trouble reading byte %i of fmt\n", j);
			return 8;
		}
		tag[j] = (char) c;
	}
/* Read 16 */
	i = 0;
	for (j = 0; j < 4; ++j) {
		++pos;
		if (pos > fillen) goto L90;
		c = fgetc(iou);
		if (EOF == c) {
			printf("trouble reading 16 byte %i\n", j);
			return 9;
		}
		i |= (c & 255) << (j << 3);
	}
	if (0 != strncmp(tag, idfmt, 4)) {
		pos += i;
		c = fseek(iou, pos, SEEK_SET);
		if (-1 == c) return 10;
		goto L10;
	}
	if (16 != i) return 11;
/* Read 1 for PCM */
	ish = 0;
	for (j = 0; j < 2; ++j) {
		++pos;
		if (pos > fillen) goto L90;
		c = fgetc(iou);
		if (EOF == c) {
			printf("Trouble reading PCM byte %i\n", j);
			return 12;
		}
		ish |= (c & 255) << (j << 3);
	}
	if (1 == ish) {
		fp = false;
	} else if (3 == ish) {
		fp = true;
	} else {
		printf("unsupported codec #%i (must be pcm)\n", ish);
		return 13;
	}
/* Read # channels */
	chanel = 0;
	for (j = 0; j < 2; ++j) {
		++pos;
		if (pos > fillen) goto L90;
		c = fgetc(iou);
		if (EOF == c) {
			printf("trouble reading # channels byte %i\n", j);
			return 14;
		}
		chanel |= (c & 255) << (j << 3);
	}
	if (chanel < 1 || chanel > 2) {
		printf("bad # channels!  %i\n", chanel);
		return 15;
	}
/* Read sample rate */
	*rate = 0;
	for (j = 0; j < 4; ++j) {
		++pos;
		if (pos > fillen) goto L90;
		c = fgetc(iou);
		if (EOF == c) {
			printf("trouble reading sample rate byte %i\n", j);
			return 16;
		}
		*rate |= (c & 255) << (j << 3);
	}
	if (*rate < 2205 || *rate > 1000000) {
		printf("sample rate out of bounds! %i\n", *rate);
		return 17;
	}
/* Read byte rate */
	bytrat = 0;
	for (j = 0; j < 4; ++j) {
		++pos;
		if (pos > fillen) goto L90;
		c = fgetc(iou);
		if (EOF == c) {
			printf("trouble reading byte rate, byte %i\n", j);
			return 18;
		}
		bytrat |= (c & 255) << (j << 3);
	}
/* Read block align */
	blkaln = 0;
	for (j = 0; j < 2; ++j) {
		++pos;
		if (pos > fillen) goto L90;
		c = fgetc(iou);
		if (EOF == c) {
			printf("trouble reading block align byte %i\n", j);
			return 19;
		}
		blkaln |= (c & 255) << (j << 3);
	}
/* Read bits per sample */
	bps = 0;
	for (j = 0; j < 2; ++j) {
		++pos;
		if (pos > fillen) goto L90;
		c = fgetc(iou);
		if (EOF == c) {
			printf("trouble reading bits per sample byte %i\n", j);
			return 20;
		}
		bps |= (c & 255) << (j << 3);
	}
	if (bps < 1 || bps % 8 != 0) {
		printf ("bad bits per sample: %i\n", bps);
		return 21;
	}
	nby = bps / 8;
	if (bytrat != *rate * chanel * nby) {
		printf("!!! bytrat: %i rate: %i chanel: %i bits per second: %i\n", bytrat, *rate, chanel, bps);
		return 22;
	}
	if (blkaln != chanel * nby) {
		printf("!! blkaln: %i chanel: %i bits per second: %i\n", blkaln, chanel, bps);
		return 23;
	}
/* Read descriptor */
L20:
	for (j = 0; j < 4; ++j) {
		++pos;
		if (pos > fillen) goto L90;
		c = fgetc(iou);
		if (EOF == c) {
			printf("trouble reading data chunk header byte %i\n", j);
			return 24;
		}
		tag[j] = (char) c;
	}
/* Read size of data chunk */
	chnksz = 0;
	for (j = 0; j < 4; ++j) {
		++pos;
		if (pos > fillen) goto L90;
		c = fgetc(iou);
		if (EOF == c) {
			printf("trouble reading size of data chunk byte %i\n", j);
			return 25;
		}
		chnksz |= (c & 255) << (j << 3);
	}
	if (0 != strncmp(tag, iddata, 4)) {
		pos += chnksz;
		c = fseek(iou, pos, SEEK_SET);
		if (-1 == c) return 26;
		goto L20;
	}
/* Read samples */
	*l = chnksz / chanel / nby;
	*s = (float *) malloc(*l * sizeof(float));
	if (!(*s)) {
		printf("can't allocate memory for wave!\n");
		return 27;
	}
	if (stride > 0) {
		ibeg = 0;
		iend = nby - 1;
	} else {
		ibeg = nby - 1;
		iend = 0;
	}
	if (1 == chanel) {
		fch = 1.0f;
	} else if (2 == chanel) {
		fch = 0.5f;
	} else {
		printf("number of channels: %i\n", chanel);
		return 28;
	}
	if (fp) goto L30;
	if (1 == nby) {
		thresh = 0;
		shift = 128;
		divid = 1.f / 128.f;
	} else if (2 == nby) {
		thresh = 32767;
		shift = 65536;
		divid = 1.f / 32768.f;
	} else if (3 == nby) {
		thresh = 8388607;
		shift = 16777216;
		divid = 1.f / 8388608.f;
	} else {
		printf("! nby is %i\n", nby);
		return 29;
	}
/* Main loop */
	for (k = 0; k < *l; ++k) {
		(*s)[k] = 0.f;
		for (j = 0; j < chanel; ++j) {
			sampl = 0;
			for (i = ibeg; i <= iend; i += stride) {
				++pos;
				if (pos > fillen) goto L90;
				c = fgetc(iou);
				if (EOF == c) {
					printf("trouble reading byte %li of %s\n", pos, ifil);
					return 30;
				}
				sampl |= (c & 255) << (i << 3);
			}
			if (sampl > thresh) sampl -= shift;
			(*s)[k] += (float) sampl * divid;
		}
		(*s)[k] *= fch;
	}
	goto L80;
/* Floating-point read */
L30:
	if (4 != nby) {
		printf("floating point numbers must be 32 bits\n");
		return 31;
	}
	for (k = 0; k < *l; ++k) {
		(*s)[k] = 0.f;
		for (j = 0; j < chanel; ++j) {
			pos += 4;
			if (pos > fillen) goto L90;
			c = fread(&flosam, 4, 1, iou);
			if (c < 1) {
				printf("trouble reading byte %li of %s\n", pos, ifil);
				return 32;
			}
			if (bysw) swab32 (&flosam);
			(*s)[k] += flosam;
		}
		(*s)[k] *= fch;
	}
/* Done with input file */
L80:
	c = fclose(iou);
	if (0 != c) {
		printf("trouble closing file %s\n", ifil);
		return 33;
	}
	return 0;  /* success */
L90:
	printf("file i/o error!\n");
	return 34;
} /* end of rwavmo */

/*----------------------------------------------------------------------
wpgm - write a Portable Gray Map
The maximum intensity is assumed to be 255.

___Name_____Type_____In/Out___Description_______________________________
   ofil     char*    In       File name
   y[n*m]   float*   In       Image matrix
   n        int      In       Columns
   m        int      In       Rows
----------------------------------------------------------------------*/
int wpgm (char *ofil, float *y, int n, int m)
{
/* Initialized data */
	static unsigned char fid[3] = { 80,53,10 };
	static unsigned char mxv[4] = { 50,53,53,10 };
/* Local variables */
	int c, j, k;
	long i;
	char header[32] = "                               \0";
	FILE *iou;
/* Function Body */
	iou = fopen(ofil, "wb");
	if (NULL == iou) {
		printf("trouble opening output file: %s\n", ofil);
		return 1;
	}
/* Write 'P5' */
	for (i = 0; i < 3; ++i) {
		c = fputc(fid[i], iou);
		if (EOF == c) {
			printf("trouble writing %i\n", fid[i]);
			return 2;
		}
	}
/* Time horizontal */
	c = sprintf(header, "%i", n);
	if (0 >= c) {
		printf("trouble composing header!\n");
		return 3;
	}
	j = strspn(header, " ");
	k = strlen(header);
	for (i = j; i < k; ++i) {
		c = fputc(header[i], iou);
		if (EOF == c) {
			printf("trouble writing position %li of header\n", i);
			return 4;
		}
	}
/* A single whitespace character */
	c = fputc(32, iou);  /* space */
	if (EOF == c) {
		printf("trouble writing space!\n");
		return 5;
	}
/* Frequency vertical */
	c = sprintf(header, "%i", m);
	if (0 >= c) {
		printf("trouble composing header\n");
		return 6;
	}
	j = strspn(header, " ");
	k = strlen(header);
	for (i = j; i < k; ++i) {
		c = fputc(header[i], iou);
		if (EOF == c) {
			printf("trouble writing position %li of header!!\n", i);
			return 7;
		}
	}
/* A single whitespace character */
	c = fputc(10, iou);  /* line feed */
	if (EOF == c) {
		printf("trouble writing line feed!\n");
		return 8;
	}
/* Write '255' */
	for (i = 0; i < 4; ++i) {
		c = fputc(mxv[i], iou);
		if (EOF == c) {
			printf("trouble writing %i\n", mxv[i]);
			return 9;
		}
	}
/* The data */
	for (k = m-1; k >= 0; --k) {
		for (j = 0; j < n; ++j) {
			i = lroundf(y[j + k * n]);
			if (i < 0) i = 0;
			if (i > 255) i = 255;
			c = fputc(i, iou);
			if (EOF == c) {
				printf("trouble writing bitmap at %i, %i\n", j, k);
				return 10;
			}
		}
	}
/* Done */
	c = fclose(iou);
	if (EOF == c) {
		printf("trouble closing output file!\n");
		return 11;
	}
	return 0;
} /* end of wpgm */

/*----------------------------------------------------------------------
              Main program
----------------------------------------------------------------------*/
int main (int argc, char **argv)
{
/* Constants */
	static int m = 17;
	static float fmnnhi = 84.5f,
	             fmnnlo = 52.5f,
	             olap = 6.f,
	             pi = 3.141592654f,
	             pi2 = 9.869604401f,
	             pip = 0.035f,
	             respar = 9.765625e-4f,
	             sig = 2.f,
	             tolt = 17.3123405;
	static int factr[128] = { 1,2,3,4,5,6,7,9,10,12,14,15,18,20,21,25,28,
	30,35,36,42,45,49,50,60,63,70,75,84,90,98,100,105,126,140,147,150,
	175,180,196,210,225,245,252,294,300,315,350,420,441,450,490,525,
	588,630,700,735,882,900,980,1050,1225,1260,1470,1575,1764,2100,
	2205,2450,2940,3150,3675,4410,4900,6300,7350,8820,11025,14700,
	22050,44100,44100,44100,44100,44100,44100,44100,44100,44100,44100,
	44100,44100,44100,44100,44100,44100,44100,44100,44100,44100,44100,
	44100,44100,44100,44100,44100,44100,44100,44100,44100,44100,44100,
	44100,44100,44100,44100,44100,44100,44100,44100,44100,44100,44100,
	44100,44100,44100,44100,44100 };
/* Local variables */
	int horz, i, ierr, ihi, ilo, j, k, l, n, r, rate, stride, targr;
	float a, b, dif, emin, emax, f, fmark, fpass, fsamp, fspac, fstop,
	      hop, hpass, hstop, raxbrt, tot, trnpos, u, xi;
	bool bysw;
	FILE *iunit, *ounit;
/* Local arrays */
	int ind[BAND];
	float x[BAND], y[BAND];
	char ifil[CHLIM], ofil[CHLIM], opt[CHLIM];
	int *nef;
	float *ampl, *freq, *s, *sf;
	float **ptrbuf;
	double *t, *w, *z;
/* Allocate arrays */
	t = (double *) malloc(2 * NLIM * sizeof(double));
	w = (double *) malloc(2 * NLIM * sizeof(double));
	z = (double *) malloc(NLIM * sizeof(double));
	if (!t || !w || !z) {
		printf("can't allocate memory!\n");
		exit(EXIT_FAILURE);
	}
	n = argc - 1;
	if (n < 2) {
		printf("spectr: files not specified\n");
		exit(EXIT_FAILURE);
	} else if (n > 5) {
		printf("spectr: too many arguments!\n");
		exit(EXIT_FAILURE);
	}
	strncpy(ifil, argv[n-1], CHLIM-1);
	strncpy(ofil, argv[n], CHLIM-1);
/* Options processing loop */
	trnpos = 0.f;
	bysw = false;
	for (i = 1; i <= n-2; ++i) {
		strncpy(opt, argv[i], CHLIM-1);
		for (j = 0; j < CHLIM; ++j) {
			if (!opt[j]) break;
			opt[j] = (char) toupper(opt[j]);
		}
		if (NULL != strstr(opt, "--SWAB")) bysw = true;
		if (NULL != strstr(opt, "--TUNE")) {
			strncpy(opt, argv[i+1], CHLIM-1);
			k = sscanf(opt, "%f", &trnpos);
			if (1 != k) {
				printf("trouble reading retuning option!\n");
				exit(EXIT_FAILURE);
			}
			if (trnpos < MNNSUB + BAND - 127 || trnpos > MNNSUB) {
				printf("transpose out of bounds\n");
				exit(EXIT_FAILURE);
			}
		}
	}
/* Check files for existence */
	iunit = fopen(ifil, "r");
	if (NULL == iunit) {
		printf("File %s not found.\n", ifil);
		exit(EXIT_FAILURE);
	}
	ounit = fopen(ofil, "r");
	if (NULL != ounit) {
		printf("File %s already exists.  Please choose another name.\n", ofil);
		exit(EXIT_FAILURE);
	}
	i = fseek(iunit, -1L, SEEK_END);
	if (0 != i) {
		printf("couldn't seek to end of file!\n");
		exit(EXIT_FAILURE);
	}
	n = ftell(iunit) + 1;
	if (0 >= n) {
		printf("file length is %i!\n", n);
		exit(EXIT_FAILURE);
	}
	fclose(iunit);
/* Read in samples */
	ptrbuf = &s;
	ierr = rwavmo (ifil, n, bysw, &s, &l, &rate);
	if (0 != ierr) {
		printf("trouble reading from file: %s\n", ifil);
		exit(EXIT_FAILURE);
	}
	s = *ptrbuf;  /* single deference is bad enough */
	sf = (float *) malloc(l * sizeof(float));
	if (!sf) {
		printf("couldn't allocate array!\n");
		exit(EXIT_FAILURE);
	}
	fsamp = (float) rate;
	horz = (int) ((double)l * (double)PXPS / (double)fsamp) + 1;
	nef = (int *) malloc(horz * sizeof(int));
	ampl = (float *) malloc(VERT * horz * sizeof(float));
	freq = (float *) malloc(BAND * horz * sizeof(float));
	if (!nef || !ampl || !freq) {
		printf("couldn't allocate memory!\n");
//		printf("nef: %i ampl: %i freq: %i\n", (int)nef, (int)ampl, (int)freq);
		printf("horz was: %i \n", horz);
		printf("l was %i, rate was %i\n", l, rate);
		exit(EXIT_FAILURE);
	}
	if (l < fsamp * pip) {
		printf("too short a file!\n");
		exit(EXIT_FAILURE);
	}
	if (44100 == rate) {
		k = 81;
	} else {
		k = 0;
		for (j = 1; j <= rate; ++j) {
			if (0 == (rate % j)) {
				if (k >= FLIM) {
					printf("too many factors of sampling rate!\n");
					exit(EXIT_FAILURE);
				}
				factr[k] = j;
				++k;
			}
		}
	}
/* Short-time Fourier transforms.  M @ R = N @ F */
	hpass = 1.f - respar;
	hstop = respar;
	for (j = 0; j < BAND; ++j) {
		xi = MNNSUB + (j + 1) - trnpos;
		f = 55.f * powf(2.f, (xi - 33.f) / 12.f);
		targr = lroundf(TARGN * f / m);
		ibsge (factr, k, targr, &i);
		r = factr[i];
		for (i = 0; i < l; ++i) sf[i] = s[i];
		if (r < rate) {  /* lo pass */
			fpass = f * ((2.f * m + 1.f) / (2.f * m));
			fstop = 0.5f * r;
			ierr = filter (sf, l, fsamp, fpass, fstop, hpass, hstop);
			if (0 != ierr) printf("lopass filter returns #%i\n", ierr);
		}
		fpass = f * ((2.f * m - 1.f) / (2.f * m));  /* hi pass */
		fstop = f / m;
		ierr = filter (sf, l, fsamp, fpass, fstop, hpass, hstop);
		if (0 != ierr) printf("hipass filter returns #%i\n", ierr);
		n = lroundf(m * r / f);
		if (n > NLIM) {
			printf("Window size too big!  Try a lower sample rate.\n");
			exit(EXIT_FAILURE);
		}
		stride = rate / r;
		hop = fminf(n * stride / olap, fsamp / PXPS);
		ierr = frat (sf, l, m, n, r, stride, hop, horz, &freq[j*horz], &ampl[j*horz], nef, t, w, z);
		if (0 != ierr) printf("frat returns #%i\n", ierr);
	}
/* Convert to MIDI note number. */
	for (k = 0; k < BAND; ++k) {
		for (j = 0; j < horz; ++j) {
			freq[j+k*horz] = tolt * logf(freq[j+k*horz] / 55.f) + 33.f;
		}
	}
/* Interpolate. */
	fspac = (fmnnhi - fmnnlo) / VERT;
	fmark = fmnnlo - 0.5f * fspac;
	for (k = 0; k < horz; ++k) {
		for (j = 0; j < BAND; ++j) {  /* transpose copy */
			x[j] = freq[k+j*horz];
			y[j] = ampl[k+j*horz];
		}
		isortr (x, ind, BAND);
		rorder (x, ind, BAND);
		rorder (y, ind, BAND);
		for (j = 0; j < VERT; ++j) {
			xi = fmark - trnpos + (j + 1) * fspac;
			a = xi - fspac * sig;
			b = xi + fspac * sig;
			rbsgt (x, BAND, a, &ilo);
			rbslt (x, BAND, b, &ihi);
			tot = 0.f;
			for (i = ilo; i <= ihi; ++i) {
				dif = (x[i] - xi) / fspac;
				a = sig * sinf(pi * dif) * sinf(pi * dif / sig);  /* Lanczos kernel */
				b = pi2 * (dif * dif);
				u = safdiv(a, b) ? a / b : 1.f;
				tot += y[i] * u;
			}
			ampl[k+j*horz] = tot;
		}
	}
/* Scale image intensities 0...MAXBRT */
	emin = +1e36f;  /* find extreme values */
	emax = -1e36f;
	for (k = 0; k < VERT; ++k) {
		for (j = 0; j < horz; ++j) {
			if (ampl[j+k*horz] < emin) emin = ampl[j+k*horz];
			if (ampl[j+k*horz] > emax) emax = ampl[j+k*horz];
		}
	}
/* Logarithms */
	u = emax * respar;
	if (u > 1e-36f) {
		for (k = 0; k < VERT; ++k) {
			for (j = 0; j < horz; ++j) {
				ampl[j+k*horz] = logf(fmaxf(0.f, ampl[j+k*horz]) + u);
			}
		}
		emin = logf(fmaxf(0.f, emin) + u);
		emax = logf(fmaxf(0.f, emax) + u);
	}
	raxbrt = (float) MAXBRT;
	dif = emax - emin;
	u = safdiv(raxbrt, dif) ? raxbrt / dif : 1.f;
	for (k = 0; k < VERT; ++k) {
		for (j = 0; j < horz; ++j) {
			ampl[j+k*horz] = (ampl[j+k*horz] - emin) * u;
			ampl[j+k*horz] = fminf(fmaxf(0.f, ampl[j+k*horz]), raxbrt);
		}
	}
/* write out */
	ierr = wpgm (ofil, ampl, horz, VERT);
	if (0 !=ierr) {
		printf("wpgm returns #%i\n", ierr);
		exit(EXIT_FAILURE);
	}
/* Release memory */
	free(nef);
	free(ampl);
	free(freq);
	free(s);
	free(sf);
	free(t);
	free(w);
	free(z);
	printf("program complete\n");
	exit(EXIT_SUCCESS);
} /* end of main */
/*--------------------- end of file spectr.c -------------------------*/
