*-----------------------------------------------------------------------
* spectr.f - read digital audio, draw spectrogram as a Portable Grey Map.
* by Andy Allinger, 2016-2021, released to the public domain
*
*    Permission  to  use, copy, modify, and distribute this software and
*    its documentation  for  any  purpose  and  without  fee  is  hereby
*    granted,  without any conditions or restrictions.  This software is
*    provided "as is" without express or implied warranty.
*
*                          OPTIONS
*   --tune x                   transpose input up by x semitones
*   --swab                     reverse byte order of the input
*-----------------------------------------------------------------------
      PROGRAM SPECTR
       IMPLICIT NONE

*                Constants
       INTEGER BAND, DUR, FLIM, HIRATE, HLIM, LLIM, M, MAXBRT, MNNSUB,
     $          NLIM, PXPS, VERT
       REAL BIG, FMARK, FMNNLO, FMNNHI, FSPAC, HPASS, HSTOP, LIT,
     $       OLAP, PI, PI2, PIP, RESPAR, SIG, TARGN, TOLT

*-----------------------------------------------------------------------
* Not all of these parameters are configurable.
*-----------------------------------------------------------------------
       PARAMETER (BAND = 32,            ! number of transform bands
     $            BIG = 1.E+36,         ! arbitrary large number
     $            DUR = 5 * 60,         ! max file duration [seconds]
     $            FLIM = 128,           ! most factors
     $            HIRATE = 48000,       ! fast sample rate
     $            FMNNLO = 52.5,        ! lowest frequency as MIDI note number
     $            FMNNHI = 84.5,        ! highest frequency
     $            LIT = 1.E-36,         ! very small number
     $            MAXBRT = 255,         ! maximum value in a .pgm file
     $            M = 17,               ! multiple of fundemental frequency
     $            MNNSUB = 52)          ! MIDI note below transform range
       PARAMETER (NLIM = 27720,         ! largest window size
     $            OLAP = 6.,            ! 1/overlap of successive windows
     $            PI  = 3.141592654,
     $            PI2 = 9.869604401,    ! pi^2
     $            PIP = 0.035,          ! a brief duration
     $            PXPS = 240,           ! Pixels per second
     $            RESPAR = 1. / 1024.,  ! desired precision:  -60 dB
     $            SIG = 2.,             ! Lanczos parameter
     $            TARGN = 3000.,        ! target value for N
     $            TOLT = 17.3123405,    ! twelve over log two
     $            VERT = 640)           ! height of output image

       PARAMETER (
     $            FSPAC = (FMNNHI - FMNNLO) / VERT,  ! spacing between pixels
     $            HPASS = 1. - RESPAR,               ! passband transmission
     $            HSTOP = RESPAR,                    ! stopband transmission
     $            HLIM = DUR * PXPS,                 ! max image width
     $            LLIM = DUR * HIRATE)               ! max # samples

       PARAMETER (FMARK = FMNNLO - 0.5 * FSPAC)  ! center freq of 0th pixel

*                     Variables
       INTEGER
     $         I, J, K,          ! counters
     $         FACTR(FLIM),      ! factors of RATE
     $         HORZ,             ! image width
     $         IERR,             ! error code
     $         ILO, IHI,         ! coefficient indices
     $         IND(BAND),        ! permutation vector
     $         L,                ! length of input, samples
     $         N,                ! window size
     $         NEF(HLIM),        ! number of transforms each pixel
     $         R, RATE,          ! sample rate, input sample rate
     $         STRIDE,           ! factor to downsample by
     $         TARGR             ! target sampling rate

       REAL
     $      A, B,               ! bounds of interpolation
     $      AMPL(HLIM,VERT),    ! intensity per frequency, time
     $      DIF, EMIN, EMAX,    ! a difference, extreme values
     $      F,                  ! frequency
     $      FPASS, FSTOP,       ! passband, stopband frequencies
     $      FSAMP,              ! sample rate as real
     $      HOP,                ! # samples to advance
     $      RAXBRT,             ! MAXBRT as REAL
     $      S(LLIM), SF(LLIM),  ! samples, filtered samples
     $      TOT,                ! a total
     $      TRNPOS,             ! amount to transpose
     $      U,                  ! a scale factor
     $      FREQ(HLIM,BAND),    ! frequency of the ith coefficient
     $      X(BAND), Y(BAND),   ! workspace arrays
     $      XI                  ! frequency of pixel

       DOUBLE PRECISION
     $                  T(2,NLIM),       ! transform matrix
     $                  W(2,NLIM),       ! window function, its derivative
     $                  Z(NLIM)          ! samples in mono

       LOGICAL BYSW, LEXIST

       CHARACTER*256 IFIL, OFIL, OPT     ! filenames

*            External functions
       INTEGER IARGC, LTRIM
       LOGICAL SAFDIV

       DATA FACTR /
     $      1,     2,     3,     4,     5,     6,     7,     9,
     $     10,    12,    14,    15,    18,    20,    21,    25,
     $     28,    30,    35,    36,    42,    45,    49,    50,
     $     60,    63,    70,    75,    84,    90,    98,   100,
     $    105,   126,   140,   147,   150,   175,   180,   196,
     $    210,   225,   245,   252,   294,   300,   315,   350,
     $    420,   441,   450,   490,   525,   588,   630,   700,
     $    735,   882,   900,   980,  1050,  1225,  1260,  1470,
     $   1575,  1764,  2100,  2205,  2450,  2940,  3150,  3675,
     $   4410,  4900,  6300,  7350,  8820, 11025, 14700, 22050,
     $    48 * 44100 /

*-----------------------------------------------------------------------
*        Begin.
*-----------------------------------------------------------------------
       IERR = 0
       N = IARGC ()
       IF (N < 2) THEN
         PRINT *, 'spectr:  files not specified'
         STOP
       ELSE IF (N > 5) THEN
         PRINT *, 'spectr: too many arguments!'
         STOP
       END IF
       CALL GETARG (N-1, IFIL)            ! read file name from command line
       CALL GETARG (N, OFIL)

*          Options processing loop
       TRNPOS = 0.
       BYSW = .FALSE.
       DO I = 1, N-2
         CALL GETARG (I, OPT)
         CALL UCASE (OPT)
         IF (INDEX(OPT, '--SWAB') > 0) BYSW = .TRUE.
         IF (INDEX(OPT, '--TUNE') > 0) THEN
           CALL GETARG(I+1, OPT)
           READ (UNIT=OPT, FMT=*, IOSTAT=IERR) TRNPOS
           IF (IERR .NE. 0) PRINT *, 'trouble reading retuning option!'
           IF (TRNPOS < MNNSUB + BAND - 127 .OR. TRNPOS > MNNSUB) THEN
             PRINT *, 'transpose out of bounds'
             STOP
           END IF
         END IF
       END DO

*-----------------------------------------------------------------------
*             Check files for existence
*-----------------------------------------------------------------------
       INQUIRE (FILE=IFIL, EXIST=LEXIST, IOSTAT=IERR)
       IF (IERR .NE. 0) STOP 'trouble inquiring of input file!'
       IF (.NOT. LEXIST) THEN
         L = LTRIM(IFIL)
         PRINT *, 'File ', IFIL(1:L), ' not found.'
         STOP
       END IF
       INQUIRE (FILE=OFIL, EXIST=LEXIST, IOSTAT=IERR)
       IF (IERR .NE. 0) STOP 'trouble inquiring of output file!'
       IF (LEXIST) THEN
         L = LTRIM(OFIL)
         PRINT *, 'File:  ', OFIL(1:L), ' already exists.  ',
     $              'Please choose another name.'
         STOP
       END IF

       CALL LFIL (IFIL, N, IERR)
       IF (IERR .NE. 0) STOP 'trouble getting length of input file'

*-----------------------------------------------------------------------
*   Read in samples
*-----------------------------------------------------------------------
       CALL RWAVMO (IFIL, N, BYSW, S, LLIM, L, RATE, IERR)
       IF (IERR .NE. 0) THEN
         PRINT *, 'trouble reading from file: ', IFIL
         STOP
       END IF
       FSAMP = FLOAT(RATE)
       HORZ = INT (DBLE(L) * DBLE(PXPS) / DBLE(FSAMP)) + 1  ! width of image
       IF (L < PIP * FSAMP) STOP 'too short a file!'
       IF (L > LLIM .OR. HORZ > HLIM) STOP 'too big a file!'

       IF (RATE .EQ. 44100) THEN
         K = 81
       ELSE
         K = 0
         DO J = 1, RATE
           IF (MOD(RATE, J) .EQ. 0) THEN
             IF (K .GE. FLIM) STOP 'too many factors of sampling rate!'
             K = K + 1
             FACTR(K) = J
           END IF
         END DO
       END IF

*-----------------------------------------------------------------------
*             Short-time Fourier transforms.  M @ R = N @ F
*-----------------------------------------------------------------------
       DO J = 1, BAND
         F = 55. * 2.**((MNNSUB + J - 33 - TRNPOS) / 12.)
         TARGR = NINT(TARGN * F / M)
         CALL IBSGE (FACTR, K, TARGR, I)
         R = FACTR(I)

*             Filter.
         DO I = 1, L
           SF(I) = S(I)
         END DO
         IF (R < RATE) THEN       ! lo pass
           FPASS = F * ((2. * M + 1.) / (2. * M))
           FSTOP = 0.5 * R
           CALL FILTER (SF, L, FSAMP, FPASS, FSTOP, HPASS, HSTOP, IERR)
           IF (IERR .NE. 0) PRINT *, 'lo pass filter returns #', IERR
         END IF
         FPASS = F * ((2. * M - 1.) / (2. * M))  ! hi pass
         FSTOP = F / M
         CALL FILTER (SF, L, FSAMP, FPASS, FSTOP, HPASS, HSTOP, IERR)
         IF (IERR .NE. 0) PRINT *, 'hi pass filter returns #', IERR

         N = NINT(M * R / F)
         IF (N > NLIM) THEN
           PRINT *, 'Window size too big!  Try a lower sample rate.'
           STOP
         END IF
         STRIDE = RATE / R
         HOP = MIN(N * STRIDE / OLAP, FSAMP / PXPS)
         CALL FRAT (SF, L, M, N, R, STRIDE, HOP, HORZ,
     $               FREQ(1,J), AMPL(1,J), NEF, T, W, Z, IERR)
         IF (IERR .NE. 0) PRINT *, 'frat returns #', IERR
       END DO

*-----------------------------------------------------------------------
*             Convert to MIDI note number.
*-----------------------------------------------------------------------
       DO K = 1, BAND
         DO J = 1, HORZ
           FREQ(J,K) = TOLT * LOG(FREQ(J,K) / 55.) + 33.
         END DO
       END DO

*-----------------------------------------------------------------------
*             Interpolate.
*-----------------------------------------------------------------------
       DO K = 1, HORZ
         DO J = 1, BAND           ! transpose copy
           X(J) = FREQ(K,J)
           Y(J) = AMPL(K,J)
         END DO
         CALL ISORTR (X, IND, BAND)
         CALL RORDER (X, IND, BAND)
         CALL RORDER (Y, IND, BAND)

         DO J = 1, VERT               ! interpolate
           XI = FMARK - TRNPOS + FSPAC * J
           A = XI - FSPAC * SIG
           B = XI + FSPAC * SIG
           CALL RBSGT (X, BAND, A, ILO)
           CALL RBSLT (X, BAND, B, IHI)
           TOT = 0.
           DO I = ILO, IHI
             DIF = (X(I) - XI) / FSPAC
             A = SIG * SIN(PI * DIF) * SIN(PI * DIF / SIG)  ! Lanczos kernel
             B = PI2 * DIF**2
             IF (SAFDIV(A, B)) THEN
               U = A / B
             ELSE
               U = 1.
             END IF
             TOT = TOT + Y(I) * U
           END DO
           AMPL(K,J) = TOT
         END DO
       END DO  ! next time step

*-----------------------------------------------------------------------
*        Scale image intensities 0...MAXBRT
*-----------------------------------------------------------------------
       EMIN = +BIG                  ! find extreme values
       EMAX = -BIG
       DO K = 1, VERT
         DO J = 1, HORZ
           IF (AMPL(J,K) < EMIN) EMIN = AMPL(J,K)
           IF (AMPL(J,K) > EMAX) EMAX = AMPL(J,K)
         END DO
       END DO

*            Logarithms
       U = EMAX * RESPAR
       IF (U > LIT) THEN
         DO K = 1, VERT
           DO J = 1, HORZ
             AMPL(J,K) = LOG(MAX(0., AMPL(J,K)) + U)
           END DO
         END DO
         EMIN = LOG(MAX(0., EMIN) + U)
         EMAX = LOG(MAX(0., EMAX) + U)
       END IF

       RAXBRT = FLOAT(MAXBRT)
       DIF = EMAX - EMIN
       IF (SAFDIV(RAXBRT, DIF)) THEN
         U = RAXBRT / DIF
       ELSE
         U = 1.
       END IF
       DO K = 1, VERT
         DO J = 1, HORZ
           AMPL(J,K) = (AMPL(J,K) - EMIN) * U
           AMPL(J,K) = MIN(MAX(0., AMPL(J,K)), RAXBRT)
         END DO
       END DO

*-----------------------------------------------------------------------
*          write out
*-----------------------------------------------------------------------
       CALL WPGM (OFIL, AMPL, HLIM, HORZ, VERT, IERR)
       IF (IERR .NE. 0) PRINT *, 'wpgm returns #', IERR

       STOP 'program complete'
      END  ! of spectr


*-----------------------------------------------------------------------
* rwavmo - read a wave file into mono
*
*___Name_____Type____________In/Out____Description______________________
*   IFIL     Character*(*)   In        Name of input file
*   FILLEN   Integer         In        Length of input file in bytes
*   BYSW     Logical         In        Reverse byte order of floats
*   S(KS)    Real            Out       Buffer of samples
*   KS       Integer         In        Array bound of S
*   L        Integer         Out       Length of S
*   RATE     Integer         Out       Samples / second
*   IERR     Integer         Out       Error code
*-----------------------------------------------------------------------
      SUBROUTINE RWAVMO (IFIL, FILLEN, BYSW, S, KS, L, RATE, IERR)
       IMPLICIT NONE
       INTEGER FILLEN, KS, L, RATE, IERR
       REAL S(KS)
       LOGICAL BYSW
       CHARACTER*(*) IFIL

*             Local variables
       INTEGER*1 B

       INTEGER
     $         BLKALN,         ! channels @ bits/second / 8
     $         BPS,            ! bits/sampl
     $         BYTRAT,         ! channels @ bits/second / 8
     $         CHANEL,         ! two for stereo
     $         CHNKSZ,         ! samples @ channels @ bits/second / 8
     $         I, J, K,        ! count bytes, count samples
     $         IBEG, IEND,     ! byte positions
     $         IOU,            ! i/o unit
     $         ISH,
     $         NBY,            ! bytes per integer
     $         POS,            ! offset in bytes
     $         SAMPL,
     $         SHIFT,          ! offset for signed integers
     $         STRIDE,         ! count up or down
     $         THRESH          ! max positive value of signed integer

       REAL FLOSAM, DIVID, FCH

       LOGICAL FEXIST, FP

       CHARACTER*4
     $             IDRIFF,
     $             IDRIFX,
     $             IDWAVE,
     $             IDFMT,
     $             IDDATA,
     $             TAG

*             External function
       INTEGER IOUNIT

*             Constants
        DATA IDRIFF / 'RIFF' /
        DATA IDRIFX / 'RIFX' /
        DATA IDWAVE / 'WAVE' /
        DATA IDFMT  / 'fmt ' /
        DATA IDDATA / 'data' /

*-----------------------------------------------------------------------
* Open input file
*-----------------------------------------------------------------------
       IERR = 0
       L = 0
       RATE = 0
       INQUIRE (FILE=IFIL, EXIST=FEXIST, IOSTAT=IERR)
       IF (IERR .NE. 0) RETURN
       IF (.NOT. FEXIST) THEN
         PRINT *, 'file not found: ', IFIL
         IERR = 1
         RETURN
       END IF

       IOU = IOUNIT ()
       OPEN (UNIT=IOU, FILE=IFIL, STATUS='OLD', ACCESS='DIRECT',
     $  RECL=1, FORM='UNFORMATTED', IOSTAT=IERR)
       IF (IERR .NE. 0) RETURN

*-----------------------------------------------------------------------
*             Read RIFF
*-----------------------------------------------------------------------
       POS = 0
       TAG = '    '
       DO J = 0, 3
         POS = POS + 1
         IF (POS > FILLEN) GO TO 90
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'trouble reading byte ', J, ' of RIFF'
           RETURN
         END IF
         TAG(J+1:J+1) = CHAR(B)
       END DO

       IF (TAG .EQ. IDRIFF) THEN
         STRIDE = +1
       ELSE IF (TAG .EQ. IDRIFX) THEN
         STRIDE = -1
       ELSE
         IERR = 2
         RETURN
       END IF

*-----------------------------------------------------------------------
*             Read file size.
*-----------------------------------------------------------------------
       I = 0
       DO J = 0, 3
         POS = POS + 1
         IF (POS > FILLEN) GO TO 90
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'trouble reading file size byte ', J
           RETURN
         END IF
         CALL BYTGET (B, I, J)
       END DO

       IF (I .NE. FILLEN - 8) THEN
         PRINT *, 'i is ', I, ' but measured file length is: ', FILLEN
         IERR = 3
         RETURN
       END IF

*-----------------------------------------------------------------------
*             Read WAVE
*-----------------------------------------------------------------------
       TAG = '    '
       DO J = 0, 3
         POS = POS + 1
         IF (POS > FILLEN) GO TO 90
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'trouble reading byte ', J, ' of WAVE'
           RETURN
         END IF
         TAG(J+1:J+1) = CHAR(B)
       END DO

       IF (TAG .NE. IDWAVE) THEN
         IERR = 4
         RETURN
       END IF

*-----------------------------------------------------------------------
*             Read chunk descriptor
*-----------------------------------------------------------------------
 10    TAG = '    '
       DO J = 0, 3
         POS = POS + 1
         IF (POS > FILLEN) GO TO 90
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'trouble reading byte ', J, ' of fmt'
           RETURN
         END IF
         TAG(J+1:J+1) = CHAR(B)
       END DO

*-----------------------------------------------------------------------
*             Read 16
*-----------------------------------------------------------------------
       I = 0
       DO J = 0, 3
         POS = POS + 1
         IF (POS > FILLEN) GO TO 90
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'trouble reading 16 byte ', J
           RETURN
         END IF
         CALL BYTGET (B, I, J)
       END DO

       IF (TAG .NE. IDFMT) THEN
         POS = POS + I
         GO TO 10
       ELSE IF (I .NE. 16) THEN
         IERR = 5
         RETURN
       END IF

*-----------------------------------------------------------------------
*             Read 1 for PCM
*-----------------------------------------------------------------------
       ISH = 0
       DO J = 0, 1
         POS = POS + 1
         IF (POS > FILLEN) GO TO 90
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'Trouble reading PCM byte ', J
           RETURN
         END IF
         CALL BYTGET (B, ISH, J)
       END DO

       IF (ISH .EQ. 1) THEN
         FP = .FALSE.
       ELSE IF (ISH .EQ. 3) THEN
         FP = .TRUE.
       ELSE
         PRINT *, 'unsupported codec #', ISH, ' (must be PCM)'
         IERR = 6
         RETURN
       END IF

*-----------------------------------------------------------------------
*             Read # channels
*-----------------------------------------------------------------------
       CHANEL = 0
       DO J = 0, 1
         POS = POS + 1
         IF (POS > FILLEN) GO TO 90
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'trouble reading # channels byte ', J
           RETURN
         END IF
         CALL BYTGET (B, CHANEL, J)
       END DO

       IF (CHANEL < 1 .OR. CHANEL > 2) THEN
         PRINT *, 'bad # channels!'
         IERR = 7
         RETURN
       END IF

*-----------------------------------------------------------------------
*             Read sample rate
*-----------------------------------------------------------------------
       RATE = 0
       DO J = 0, 3
         POS = POS + 1
         IF (POS > FILLEN) GO TO 90
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'trouble reading sample rate byte ', J
           RETURN
         END IF
         CALL BYTGET (B, RATE, J)
       END DO

       IF (RATE < 2205 .OR. RATE > 1 000 000) THEN
         PRINT *, 'sample rate out of bounds!'
         IERR = 8
         RETURN
       END IF

*-----------------------------------------------------------------------
*             Read byte rate
*-----------------------------------------------------------------------
       BYTRAT = 0
       DO J = 0, 3
         POS = POS + 1
         IF (POS > FILLEN) GO TO 90
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'trouble reading byte rate, byte ', J
           RETURN
         END IF
         CALL BYTGET (B, BYTRAT, J)
       END DO

*-----------------------------------------------------------------------
*             Read block align
*-----------------------------------------------------------------------
       BLKALN = 0
       DO J = 0, 1
         POS = POS + 1
         IF (POS > FILLEN) GO TO 90
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'trouble reading block align byte ', J
           RETURN
         END IF
         CALL BYTGET (B, BLKALN, J)
       END DO

*-----------------------------------------------------------------------
*             Read bits per sample
*-----------------------------------------------------------------------
       BPS = 0
       DO J = 0, 1
         POS = POS + 1
         IF (POS > FILLEN) GO TO 90
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'trouble reading bits per sample byte ', J
           RETURN
         END IF
         CALL BYTGET (B, BPS, J)
       END DO

       IF (BPS < 1 .OR. MOD(BPS, 8) .NE. 0) THEN
         PRINT *, 'bad bits per sample: ', BPS
         IERR = 9
         RETURN
       END IF

       NBY = BPS / 8

       IF (BYTRAT .NE. RATE * CHANEL * NBY) THEN
         PRINT *, '!!! bytrat: ', BYTRAT, ' rate: ', RATE,
     $                    ' chanel: ', CHANEL, ' bits per second: ', BPS
         IERR = 10
         RETURN
       END IF

       IF (BLKALN .NE. CHANEL * NBY) THEN
         PRINT *, '!! blkaln: ', BLKALN, ' chanel: ', CHANEL,
     $                                         ' bits per second: ', BPS
         IERR = 11
         RETURN
       END IF

*-----------------------------------------------------------------------
*             Read descriptor
*-----------------------------------------------------------------------
  20   TAG = '    '
       DO J = 0, 3
         POS = POS + 1
         IF (POS > FILLEN) GO TO 90
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'trouble reading data chunk header byte ', J
           RETURN
         END IF
         TAG(J+1:J+1) = CHAR(B)
       END DO

*-----------------------------------------------------------------------
*             Read size of data chunk
*-----------------------------------------------------------------------
       CHNKSZ = 0
       DO J = 0, 3
         POS = POS + 1
         IF (POS > FILLEN) GO TO 90
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'trouble reading size of data chunk byte ', J
           RETURN
         END IF
         CALL BYTGET (B, CHNKSZ, J)
       END DO

       IF (TAG .NE. IDDATA) THEN
         POS = POS + CHNKSZ
         GO TO 20
       END IF

*-----------------------------------------------------------------------
*             Read samples
*-----------------------------------------------------------------------
       L = CHNKSZ / CHANEL / NBY
       IF (L > KS) THEN
         PRINT *, 'too big a file!'
         IERR = 12
         RETURN
       END IF
       IF (STRIDE > 0) THEN
         IBEG = 0
         IEND = NBY - 1
       ELSE
         IBEG = NBY - 1
         IEND = 0
       END IF

       IF (CHANEL .EQ. 1) THEN
         FCH = 1.
       ELSE IF (CHANEL .EQ. 2) THEN
         FCH = 0.5
       ELSE
         PRINT *, 'number of channels: ', CHANEL
         IERR = 13
         RETURN
       END IF

       IF (FP) GO TO 30
       IF (NBY .EQ. 1) THEN       ! unsigned
         THRESH = 0
         SHIFT = 128
         DIVID = 1. / 128.
       ELSE IF (NBY .EQ. 2) THEN  ! signed
         THRESH = 32767
         SHIFT = 65536
         DIVID = 1. / 32768.
       ELSE IF (NBY .EQ. 3) THEN  ! signed
         THRESH = 8388607
         SHIFT = 16777216
         DIVID = 1. / 8388608.
       ELSE
         PRINT *, '! nby is ', NBY
         IERR = 14
         RETURN
       END IF

*-----------------------------------------------------------------------
*             Main loop.
*-----------------------------------------------------------------------
       DO K = 1, L
         S(K) = 0.
         DO J = 1, CHANEL
           SAMPL = 0
           DO I = IBEG, IEND, STRIDE
             POS = POS + 1
             IF (POS > FILLEN) GO TO 90
             READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
             IF (IERR .NE. 0) THEN
               PRINT *, 'trouble reading byte ', POS, ' of ', IFIL
               RETURN
             END IF
             CALL BYTGET (B, SAMPL, I)  ! copy bits
           END DO
           IF (SAMPL > THRESH) SAMPL = SAMPL - SHIFT
           S(K) = S(K) + FLOAT(SAMPL) * DIVID
         END DO
         S(K) = S(K) * FCH
       END DO  ! loop on samples
       GO TO 80

*-----------------------------------------------------------------------
*             Floating-point read
*-----------------------------------------------------------------------
  30   CLOSE (UNIT=IOU, IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
         PRINT *, 'trouble closing file: ', IFIL
         RETURN
       END IF
       IF (NBY .NE. 4) THEN
         PRINT *, 'floating point numbers must be 32 bits'
         IERR = 15
         RETURN
       END IF

       POS = POS / 4
       IOU = IOUNIT ()
       OPEN (UNIT=IOU, FILE=IFIL, STATUS='OLD', ACCESS='DIRECT',
     $  RECL=4, FORM='UNFORMATTED', IOSTAT=IERR)

       DO K = 1, L
         S(K) = 0.
         DO J = 1, CHANEL
           POS = POS + 1
           IF (POS > FILLEN) GO TO 90
           READ (UNIT=IOU, REC=POS, IOSTAT=IERR) FLOSAM
           IF (IERR .NE. 0) THEN
             PRINT *, 'trouble reading byte ', POS, ' of ', IFIL
             RETURN
           END IF
           IF (BYSW) CALL SWAB32 (FLOSAM)
           S(K) = S(K) + FLOSAM
         END DO
         S(K) = S(K) * FCH
       END DO  ! loop on samples

*-----------------------------------------------------------------------
*         Done with input file
*-----------------------------------------------------------------------
  80   CLOSE (UNIT=IOU, IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
         PRINT *, 'trouble closing file: ', IFIL
         RETURN
       END IF
       RETURN  ! success

  90   PRINT *, 'try to read past end of file!'
       IERR = 16
       RETURN

      END  ! of rwavmo


*-----------------------------------------------------------------------
      SUBROUTINE SWAB32 (R)
       IMPLICIT NONE
       REAL R

       INTEGER*1 B0, B1, B2, B3
       INTEGER IV
       REAL RV
       EQUIVALENCE (IV, RV)

       RV = R
       CALL BYTPUT (B0, IV, 0)
       CALL BYTPUT (B1, IV, 1)
       CALL BYTPUT (B2, IV, 2)
       CALL BYTPUT (B3, IV, 3)

       CALL BYTGET (B3, IV, 0)
       CALL BYTGET (B2, IV, 1)
       CALL BYTGET (B1, IV, 2)
       CALL BYTGET (B0, IV, 3)
       R = RV

       RETURN
      END  ! of swab32


*-----------------------------------------------------------------------
* wpgm - write a Portable Gray Map
* The maximum intensity is assumed to be 255.
*
*___Name______Type_____________In/Out___Description_____________________
*   OFIL       Character*(*)   In       File name
*   Y(LDY,M)   Real            In       Image matrix
*   LDY        Integer         In       Leading dimension of Y
*   N          Integer         In       Columns
*   M          Integer         In       Rows
*   IERR       Integer         Out      Error code, 0 = no errors
*-----------------------------------------------------------------------
      SUBROUTINE WPGM (OFIL, Y, LDY, N, M, IERR)  ! write out
       IMPLICIT NONE
       INTEGER LDY, N, M, IERR
       REAL Y(LDY,M)
       CHARACTER*(*) OFIL

       INTEGER I, J, K, IOU, POS
       INTEGER*1 B, FID(3), MXV(4)
       CHARACTER*32 HEADER
       INTEGER IOUNIT, KTRIM, LTRIM              ! external functions

       DATA FID / 80, 53, 10 /                   ! 'P', '5', Line feed
       DATA MXV / 50, 53, 53, 10 /               ! '255',  Line feed

*-----------------------------------------------------------------------
*             Begin
*-----------------------------------------------------------------------
       IOU = IOUNIT ()
       OPEN (UNIT=IOU, FILE=OFIL, STATUS='NEW', ACCESS='DIRECT', RECL=1,
     $       FORM='UNFORMATTED', IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
         PRINT *, 'trouble opening output file: ', OFIL
         RETURN
       END IF

*             Write 'P5'
       POS = 0
       DO I = 1, 3
         POS = POS + 1
         WRITE (UNIT=IOU, REC=POS, IOSTAT=IERR) FID(I)
         IF (IERR .NE. 0) PRINT *, 'trouble writing ', FID(I)
       END DO

*             Time horizontal
  10   FORMAT (I9)
       WRITE (UNIT=HEADER, FMT=10, IOSTAT=IERR) N
       IF (IERR .NE. 0) THEN
         PRINT *, 'trouble composing header!'
         RETURN
       END IF
       J = KTRIM(HEADER)
       K = LTRIM(HEADER)
       DO I = J, K
         POS = POS + 1
         WRITE (UNIT=IOU, REC=POS, IOSTAT=IERR) HEADER(I:I)
         IF (IERR .NE. 0) PRINT *, 'trouble writing ', I, 'th',
     $     ' position of header'
       END DO

*             A single whitespace character
       B = 32                     ! space
       POS = POS + 1
       WRITE (UNIT=IOU, REC=POS, IOSTAT=IERR) B
       IF (IERR .NE. 0) PRINT *, 'trouble writing space!'

*             Frequency vertical
       WRITE (UNIT=HEADER, FMT=10, IOSTAT=IERR) M
       IF (IERR .NE. 0) THEN
         PRINT *, 'trouble composing header!'
         RETURN
       END IF
       J = KTRIM(HEADER)
       K = LTRIM(HEADER)
       DO I = J, K
         POS = POS + 1
         WRITE (UNIT=IOU, REC=POS, IOSTAT=IERR) HEADER(I:I)
         IF (IERR .NE. 0) PRINT *, 'trouble writing ', I, 'th',
     $     ' position of header'
       END DO

*             A single whitespace character
       B = 10                     ! line feed
       POS = POS + 1
       WRITE (UNIT=IOU, REC=POS, IOSTAT=IERR) B
       IF (IERR .NE. 0) PRINT *, 'trouble writing line feed!'

*             Write '255'
       DO I = 1, 4
         POS = POS + 1
         WRITE (UNIT=IOU, REC=POS, IOSTAT=IERR) MXV(I)
         IF (IERR .NE. 0) PRINT *, 'trouble writing ', MXV(I)
       END DO

*             The data
       DO K = M, 1, -1
         DO J = 1, N
           I = NINT(Y(J,K))
           I = MIN(MAX(0, I), 255)
           CALL BYTPUT (B, I, 0)
           POS = POS + 1
           WRITE (UNIT=IOU, REC=POS, IOSTAT=IERR) B
           IF (IERR .NE. 0) PRINT*, 'trouble writing bitmap at ', J, K
         END DO
       END DO

*               Done
       CLOSE (UNIT=IOU, IOSTAT=IERR)
       IF (IERR .NE. 0) PRINT *, 'trouble closing output file!'
       RETURN
      END  ! of wpgm
*----------------------- End of file spectr.f --------------------------
