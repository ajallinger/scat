scat - convert audio to image and image to midi
by Andy Allinger, 2021-2023, released to the public domain
address correspondence to:  andy@13olive.net


------------------------------- OVERVIEW -------------------------------
To convert recorded music to MIDI is a difficult problem.  One
possible method is to convert the audio to an image by taking the
spectrogram.  The image is then edited to erase overtones
and echos.  The solo line of the edited image may be converted to
MIDI.

Program SPECTR produces a high-resolution spectrogram in the alto
range.  Program SCRIBE converts the edited image to a standard
midi file.

This is a command-prompt program.  It is completely free and in
the public domain.


-------------------------------- USAGE ---------------------------------
     spectr something.wav something.pgm
     scribe something.pgm something.mid

where
     .wav is standard Microsoft wave audio
     .pgm is Portable Grey Map image file
     .mid is your output

You are free to attempt any kind of transform on the image except
that it has to remain 640 pixels in height.  Remember that SCRIBE
transcribes solos ONLY.

Both programs have an option --tune to transpose.  So you can do
something like:

     spectr --tune +24.5 deep.wav alto.pgm
     scribe --tune -24.5 alto.pgm deep.mid

That shifts a baritone voice up by two octaves and a quarter-tone
into the range of the specrogram, then transposes it back when
writing the MIDI.

Program SCRIBE has the options --patch, --text, --copyright, and 
--seqname so that

     scribe --patch 68 --seqname "Hacker's Blues" --copyright "(C)2021 Joe" --text "nobody reads the comments" blues.pgm blues.mid

sets the patch to oboe, gives the track a name, a copyright notice, and
a comment.

Program SPECTR has an option --swab which you might need if your
.WAV is a 32-bit float with byte order backwards from the
computer's default.


------------------------------- CONTENTS -------------------------------
zerora.f           Solve an equation numerically, by L F Shampine and H A Watts
solv.c             Public domain

rorder.f           Apply a permutation to an array, by Robert Renka
rorder.c           Public domain

ishell.f           Sort an array of integers, by Alfred H. Morris
ishell.c           Public domain

safdiv.f           Test if division is safe, based on a program by Julio.
safdiv.c           Public domain

dyrb.f             Routines for music transcription by Andy Allinger
dyrb.c             Public domain
filter.f
filter.c
frat.f
frat.c
glom.f
glom.c
isortr.f
isortr.c
scribe.f
scribe.c
search.f
search.c
sift.f
sift.c
smooth.f
smooth.c
spectr.f
spectr.c

both.f             Utility functions in the public domain
wrapper.f

readme.txt         This file
changelog.txt


------------------------------ DISCLAIMER ------------------------------
  The names and affiliations of authors of subroutines included in the
package are for acknowledgment only, and in no way indicates their
endorsement of this software distribution.
  This software MUST NOT be used in applications where life or property
is at stake until the user has investigated whether it meets his needs.


--------------------------- REVISION HISTORY ---------------------------
3    8 Aug 2023   Agglomerate by Ward's method. Pitch on linear scale.
2   30 Apr 2022   Nonlinear smoothing, inverse interpolation, no .exe files
1   10 Oct 2021   Add readme, FORTRAN source and .exe files
0    6 Oct 2021   Original release
