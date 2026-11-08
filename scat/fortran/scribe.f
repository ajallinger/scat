*-----------------------------------------------------------------------
* scribe.f - read a Portable Gray Map, write a MIDI music file
* by Andy Allinger, 2019-2022, released to the public domain
*
*    Permission  to  use, copy, modify, and distribute this software and
*    its documentation  for  any  purpose  and  without  fee  is  hereby
*    granted,  without any conditions or restrictions.  This software is
*    provided "as is" without express or implied warranty.
*
*_______________________OPTIONS_________________________________________
*   --tune [+|-]x                   performance is sharp of standard A=440
*                                     tuning by x semitones (negative for flat)
*   --patch #                       select General Midi patch/program #
*   --text hhh                      a comment of any type
*   --copyright hhh                 legal ownership of the file
*   --seqname hhh                   a title or description
*-----------------------------------------------------------------------
      PROGRAM SCRIBE
       IMPLICIT NONE

*                Constants
       INTEGER DUR,     ! max file length [seconds]
     $         ELIM,    ! length of event arrays
     $         LLIM,    ! max number of pixels
     $         PBR,     ! pitch bend range [cents]
     $         PXPS,    ! Pixels per second
     $         VERT,    ! height of image [pixels]
     $         WLIM     ! max width of image [pixels]

       PARAMETER (DUR = 5 * 60,        ! five minutes
     $            ELIM = 144000,
     $            PBR = 200,           ! two semitones
     $            PXPS = 240,
     $            VERT = 640)
       PARAMETER (WLIM = DUR * PXPS)
       PARAMETER (LLIM = WLIM * VERT)  ! bound of input array

*                     Variables
       INTEGER EK(ELIM), ET(ELIM),     ! event kinds, times
     $         EP1(ELIM), EP2(ELIM),   ! event parameters
     $         ETOT,                   ! # events
     $         I,                      ! counter
     $         HEIGHT,                 ! height of image
     $         IERR,                   ! error code
     $         L,                      ! a string length
     $         MAXBRT,                 ! value of max intensity
     $         NARG,                   ! number of command line arguments
     $         OFFSET,                 ! position in text buffer
     $         PATCH,                  ! instrument to play
     $         WIDTH                   ! width of image

       REAL A(LLIM),              ! bitmap array
     $      RAXBRT,               ! maxbrt as REAL
     $      TRNPOS                ! amount to adjust tuning

       LOGICAL LEXIST    ! file exists

       CHARACTER*256 IFIL, OFIL,   ! filenames
     $               OPT           ! command line option

       CHARACTER*1024 TEXT

*            External functions
       INTEGER LTRIM, IARGC

*-----------------------------------------------------------------------
*        Begin.  Read command line.
*-----------------------------------------------------------------------
       IERR = 0
       ETOT = 0
       OFFSET = 0
       TRNPOS = 0.

       NARG = IARGC()
       IF (NARG < 2) STOP 'files not specified'
       IF (NARG > 99) STOP 'too many arguments!'
       CALL GETARG (NARG-1, IFIL)      ! read file name from command line
       CALL GETARG (NARG, OFIL)

*         Check files for existence
       INQUIRE (FILE=IFIL, EXIST=LEXIST, IOSTAT=IERR)
       IF (IERR .NE. 0) STOP 'trouble inquiring of input file!'
       IF (.NOT. LEXIST) STOP 'file not found'
       INQUIRE (FILE=OFIL, EXIST=LEXIST, IOSTAT=IERR)
       IF (IERR .NE. 0) STOP 'trouble inquiring of output file!'
       IF (LEXIST) THEN
         L = LTRIM(OFIL)
         PRINT *, 'File:  ', OFIL(1:L), ' already exists.  ',
     $              'Please choose another name.'
         STOP
       END IF

*          Options processing loop
       DO I = 1, NARG-2
         CALL GETARG (I, OPT)
         CALL UCASE (OPT)

*             Text events.  Beware the nonstandard code numbers.
         IF (INDEX(OPT, '--TEXT') > 0) THEN
           CALL TXTEVT (OPT,TEXT,I,ET,EK,EP1,EP2,ELIM,ETOT,OFFSET,61)
         ELSE IF (INDEX(OPT, '--COPYRIGHT') > 0) THEN
           CALL TXTEVT (OPT,TEXT,I,ET,EK,EP1,EP2,ELIM,ETOT,OFFSET,62)
         ELSE IF (INDEX(OPT, '--SEQNAME') > 0) THEN
           CALL TXTEVT (OPT,TEXT,I,ET,EK,EP1,EP2,ELIM,ETOT,OFFSET,63)

*             Patch change
         ELSE IF (INDEX(OPT, '--PATCH') > 0) THEN
           CALL GETARG (I+1, OPT)
           READ (UNIT=OPT, FMT=*, IOSTAT=IERR) PATCH
           IF (IERR .NE. 0) PRINT *, 'trouble reading patch change!'
           IF (PATCH < 0 .OR. PATCH > 127)
     $          STOP 'patch numbers are from 0 to 127'
           ETOT = ETOT + 1
           ET(ETOT) = 0
           EK(ETOT) = 55
           EP1(ETOT) = PATCH

         ELSE IF (INDEX(OPT, '--TUNE') > 0) THEN
           CALL GETARG (I+1, OPT)
           READ (UNIT=OPT, FMT=*, IOSTAT=IERR) TRNPOS
           IF (IERR .NE. 0) PRINT *, 'trouble reading retuning option!'
         END IF
       END DO

*-----------------------------------------------------------------------
*         Read in a standard .PGM file
*-----------------------------------------------------------------------
       CALL RPGM (IFIL, A, LLIM, WIDTH, HEIGHT, MAXBRT, IERR)
       IF (IERR .NE. 0) THEN
         PRINT *, 'trouble reading from file: ', IFIL
         STOP
       END IF
       IF (HEIGHT .NE. VERT) THEN
         PRINT *, 'scribe: height must be ', VERT, ' pixels!'
         STOP
       END IF

       CALL FLIPUD (A, WIDTH, HEIGHT)      ! turn upside-down
       RAXBRT = FLOAT(MAXBRT)

       ETOT = ETOT + 1                    ! set pitch bend range
       ET(ETOT) = 0
       EK(ETOT) = 76
       EP1(ETOT) = PBR

*-----------------------------------------------------------------------
*          Interpret
*-----------------------------------------------------------------------
       CALL CLEF (A, WIDTH, HEIGHT, TRNPOS, RAXBRT,
     $             ET, EK, EP1, EP2, ELIM, ETOT, IERR)
       IF (IERR .NE. 0) PRINT *, 'clef returns #', IERR
       IF (ETOT .GE. ELIM) PRINT *, 'scribe:  out of memory'

*-----------------------------------------------------------------------
*          Write out
*-----------------------------------------------------------------------
       CALL WMIDI (OFIL, TEXT, ETOT, ET, EK, EP1, EP2, IERR)
       IF (IERR .NE. 0) PRINT *, 'wmidi returns #', IERR

       STOP 'program complete'
      END  ! of scribe


*-----------------------------------------------------------------------
*         Make a text event
*-----------------------------------------------------------------------
      SUBROUTINE TXTEVT (OPT,TEXT,I,ET,EK,EP1,EP2,ELIM,ETOT,OFFSET,ID)
       IMPLICIT NONE
       CHARACTER*(*) OPT
       CHARACTER*(*) TEXT
       INTEGER I, ELIM, ETOT, OFFSET, ID
       INTEGER ET(ELIM), EK(ELIM), EP1(ELIM), EP2(ELIM)

       INTEGER BUFL
       PARAMETER (BUFL = 1024)
       INTEGER L          ! local variable
       INTEGER LTRIM      ! external function

       CALL GETARG (I+1, OPT)
       L = LTRIM(OPT)
       IF (OFFSET + L > BUFL) STOP 'too much text'
       ETOT = ETOT + 1
       ET(ETOT) = 0
       EK(ETOT) = ID
       EP1(ETOT) = OFFSET + 1
       EP2(ETOT) = L
       TEXT(OFFSET+1:OFFSET+L) = OPT(1:L)
       OFFSET = OFFSET + L
       RETURN
      END  ! of txtevt


*-----------------------------------------------------------------------
* flipud - flip over the image so that high frequencies have high indices
*
*___Name___________Type______In/Out___Description_______________________
*   A(HORZ,VERT)   Real      In       Bitmap array
*   HORZ           Integer   Out      Width of image
*   VERT           Integer   Out      Height of image
*-----------------------------------------------------------------------
      SUBROUTINE FLIPUD (A, HORZ, VERT)
       IMPLICIT NONE
       INTEGER HORZ, VERT
       REAL A(HORZ,VERT)

       INTEGER I, J, K
       REAL SWAP

*          Begin.
       J = VERT
       DO I = 1, VERT/2
         DO K = 1, HORZ
           SWAP = A(K,I)
           A(K,I) = A(K,J)
           A(K,J) = SWAP
         END DO
         J = J - 1
       END DO

       RETURN
      END  ! of FLIPUD


*-----------------------------------------------------------------------
* clef - Separate pixels into dark or light.  Make midi events.
*
*___Name______________Type______In/Out___Description____________________
*   A(WIDTH,HEIGHT)   Real      In       Bitmap of pixel intensities
*   WIDTH             Integer   In       Width of image
*   HEIGHT            Integer   In       Height of image
*   TRNPOS            Real      In       Amount to adjust tuning
*   RAXBRT            Real      In       Maxval from .PPM file
*   ET(ELIM)          Integer   Out      Event times
*   EK(ELIM)          Integer   Out      Event Kinds (refer to WMIDI)
*   EP1(ELIM)         Integer   Out      Event parameter #1
*   EP2(ELIM)         Integer   Out      Event parameter #2
*   ELIM              Integer   In       Length of arrays
*   ETOT              Integer   Out      # events
*   IERR              Integer   Out      Error code 0=no errors
*-----------------------------------------------------------------------
      SUBROUTINE CLEF (A, WIDTH, HEIGHT, TRNPOS, RAXBRT,
     $               ET, EK, EP1, EP2, ELIM, ETOT, IERR)
       IMPLICIT NONE
       INTEGER WIDTH, HEIGHT, ELIM
       INTEGER ET(ELIM), EK(ELIM), EP1(ELIM), EP2(ELIM), ETOT, IERR
       REAL A(WIDTH,HEIGHT), TRNPOS, RAXBRT

*       Constants - must agree with elsewhere, including program spectr
       INTEGER DUR, KLIM, PXPS, VERT
       REAL ADPARM, ATOL, BF, BOGUS, FMARK, FMNNLO, FMNNHI,
     $  FSPAC, MDPARM, PARM, PBR, PI, PI2,
     $  PIP, RE, RESPAR, SIG, USPPX, VCRIT

       PARAMETER (
     $            ADPARM = 1.,          ! difference to split notes
     $            MDPARM = 3.,          ! max difference within a note
     $            BOGUS = -999.,        ! invalid note number
     $            DUR = 5 * 60,         ! five minutes
     $            FMNNLO = 52.5,        ! note # at least frequency
     $            FMNNHI = 84.5,        ! note # at greatest frequency
     $            PBR = 200.,           ! pitch bend range in cents
     $            PI  = 3.141592654,
     $            PI2 = 9.869604401,
     $            PIP = 0.035,          ! ignore events briefer than PIP [s]
     $            PXPS = 240,           ! pixels per second
     $            RE = 1.E-6,           ! relative error for root-finding
     $            RESPAR = 1. / 1024.,  ! resolution parameter
     $            SIG = 2.0,            ! for Lanczos interpolation
     $            VERT = 640,           ! height of image
     $            VCRIT = 0.66)         ! threshhold variance
       PARAMETER (BF = 8192. * 100. / PBR,  ! pitch bend units per semitone
     $            FSPAC = (FMNNHI - FMNNLO) / VERT,   ! semitones per pixel
     $            KLIM = PXPS * DUR,
     $            PARM = 1. + 1. / RESPAR,
     $            USPPX = 1 000 000. / PXPS)     ! microseconds per pixel
       PARAMETER (ATOL = 0.1 / (BF * FSPAC),     ! absolute error tolerance
     $            FMARK = FMNNLO - 0.5 * FSPAC)  ! frequency of zeroth pixel

*          Local variables
       INTEGER
     $         BACK(0:1,KLIM),  ! back pointers
     $         BOUND(KLIM),     ! index of pixel of a note onset
     $         EX, EXOLD,       ! expression values
     $         I, J, L,         ! counters
     $         JMAX,            ! index of maximum brightness
     $         K,               ! # segments
     $         LENGTH(KLIM),    ! workspace for GLOM
     $         N, NOLD,         ! MIDI Note Number as integer
     $         PB, PBOLD,       ! pitch bend values
     $         RIND(KLIM),      ! workspace for GLOM
     $         SMOORD,          ! order of smoothing
     $         T,               ! time in microseconds
     $         V                ! midi velocity

       REAL
     $      AMPL(VERT),     ! pixel intensity in a column
     $      AVE(KLIM),      ! average pitch each note
     $      B, C,           ! bounds of search interval
     $      BASE,           ! controls diminuendo
     $      BRT,            ! brightness
     $      COREX,          ! factor in expression formula
     $      DW(KLIM),       ! workspace for GLOM
     $      FMEAN, FVAR,    ! mean and variance of frequency
     $      FREQ(VERT),     ! Midi note number in a column
     $      GREAT(KLIM),    ! workspace for GLOM
     $      LEAST(KLIM)     ! workspace for GLOM

       REAL POS,            ! fractional index of max brightness
     $      RARG(4),        ! arguments for kernel function
     $      SRRP,           ! square root of resolution parameter
     $      SUMX(KLIM),     ! workspace for GLOM
     $      SA, SAP, SAP2,  ! sums of amplitude
     $      TRANS,          ! transition penalty
     $      WORK(VERT),     ! workspace for QWM
     $      X(KLIM),        ! midi note number each time step
     $      Y(KLIM),        ! total intensity, each time step
     $      YM              ! maximum value of amplitude

       LOGICAL SOUND        ! note is sounding at a pixel

*             External functions
       REAL KERNIN
       EXTERNAL KERNIN
       LOGICAL SAFDIV

*-----------------------------------------------------------------------
*             Begin.
*-----------------------------------------------------------------------
       IF (WIDTH < 1 .OR. HEIGHT < 1) THEN
         IERR = 1
         RETURN
       ELSE IF (HEIGHT .NE. VERT) THEN
         IERR = 2
         RETURN
       END IF
       DO J = 1, VERT
         FREQ(J) = FMARK + J * FSPAC + TRNPOS
       END DO

*-----------------------------------------------------------------------
*             Pitch and amplitude each time step.
*-----------------------------------------------------------------------
       DO 10 I = 1, WIDTH
         K = 0
         JMAX = 0
         YM = 0.
         DO J = 1, VERT
           AMPL(J) = A(I,J) / RAXBRT
           IF (A(I,J) > 0.5) K = K + 1
           IF (AMPL(J) > YM) THEN
             JMAX = J
             YM = AMPL(J)
           END IF
         END DO

         IF (K .EQ. 0) THEN       ! silence
           X(I) = BOGUS
           Y(I) = 0.
           GO TO 10
         ELSE IF (K .EQ. 1) THEN  ! one pixel
           X(I) = FREQ(JMAX)
           Y(I) = YM
           GO TO 10
         END IF

         SA = 0.                  ! weighted variance
         SAP = 0.
         DO J = 1, VERT
           SA = SA + AMPL(J)
           SAP = SAP + AMPL(J) * FLOAT(J)
         END DO
         FMEAN = SAP / SA
         SAP2 = 0.
         DO J = 1, VERT
           SAP2 = SAP2 + AMPL(J) * (FLOAT(J) - FMEAN)**2
         END DO
         FVAR = SAP2 / SA

*-----------------------------------------------------------------------
*             De-interpolate in units of pixels and linear light.
*-----------------------------------------------------------------------
         IF (FVAR < VCRIT) THEN   ! attempt if variance is small
           YM = 0.
           JMAX = 2
           DO J = 2, VERT
             SA = AMPL(J-1) + AMPL(J)
             IF (SA > YM) THEN
               YM = SA
               JMAX = J
             END IF
           END DO

           RARG(1) = FLOAT(JMAX-1)
           RARG(2) = FLOAT(JMAX)
           RARG(3) = RESPAR * (PARM**AMPL(JMAX-1) - 1.)
           RARG(4) = RESPAR * (PARM**AMPL(JMAX) - 1.)
           B = RARG(1) + ATOL
           C = RARG(2) - ATOL
           POS = B
           CALL ZERORA (KERNIN, POS, C, RE, ATOL, RARG, IERR)
           IF (IERR .EQ. 1 .OR. IERR .EQ. 2) THEN
             IF (RARG(3) > RARG(4)) THEN
               C = RARG(1)
               YM = RARG(3)
             ELSE
               C = RARG(2)
               YM = RARG(4)
             END IF
             B = PI2 * (C - POS)**2
             C = SIG * SIN(PI * (C - POS)) * SIN(PI * (C - POS) / SIG)
             IF (SAFDIV(B, C)) THEN
               BRT = B / C
             ELSE
               BRT = 1.
             END IF
             BRT = BRT * YM
             X(I) = FMARK + POS * FSPAC + TRNPOS  ! pixel -> MIDI note
             Y(I) = LOG(1. + BRT / RESPAR) / LOG(PARM)
             GO TO 10
           END IF
         END IF

*-----------------------------------------------------------------------
*             Fallback routine in units of Midi note
*-----------------------------------------------------------------------
         DO J = 1, VERT
           AMPL(J) = RESPAR * (PARM**AMPL(J) - 1.)
         END DO
         CALL QWM (FREQ, AMPL, VERT, POS, BRT, WORK, IERR)
         IF (IERR .NE. 0) THEN
           PRINT *, 'qwm returns #', IERR
           RETURN
         END IF
         BRT = LOG(1. + BRT / RESPAR) / LOG(PARM)
         BRT = MIN(MAX(0., BRT), 1.)
         X(I) = POS
         Y(I) = BRT
  10   CONTINUE

*-----------------------------------------------------------------------
* Choose sounding or silent by dynamic programming.
*-----------------------------------------------------------------------
       DO I = 1, WIDTH                           ! Limit to 0...1
         Y(I) = MIN(MAX(0., Y(I)), 1.)
         GREAT(I) = (Y(I) * RAXBRT + 1.) / (RAXBRT + 1.)  ! can't take log(0)
       END DO
       SRRP = SQRT(RESPAR)
       TRANS = LOG(RAXBRT) * FLOAT(PXPS) * PIP
       CALL DYRB (GREAT, WIDTH, SRRP, TRANS, BOUND, BACK, IERR)
       IF (IERR .NE. 0) PRINT *, 'dyrb returns #', IERR

       DO K = 1, WIDTH
         IF (BOUND(K) .EQ. 0) THEN
           X(K) = BOGUS                ! force silence
           Y(K) = 0.
         END IF
       END DO

       DO K = WIDTH-1, 2, -1
         IF (BOUND(K) .EQ. 1 .AND. X(K) < 0.) THEN
           X(K) = X(K+1)               ! fill in
           Y(K) = Y(K+1)
         END IF
       END DO

       DO K = 2, WIDTH-1, +1
         IF (BOUND(K) .EQ. 1 .AND. X(K) < 0.) THEN
           X(K) = X(K-1)
           Y(K) = Y(K-1)
         END IF
       END DO

*-----------------------------------------------------------------------
*             Merge contiguous frequencies into notes.
*-----------------------------------------------------------------------
       SOUND = .FALSE.                           ! mark note beginnings
       K = 0
       DO J = 1, WIDTH
         IF (.NOT. SOUND .AND. X(J) .GE. 0.) THEN
           SOUND = .TRUE.
           K = K + 1
           BOUND(K) = J
         ELSE IF (SOUND .AND. (X(J) < 0. .OR. J .EQ. WIDTH)) THEN
           SOUND = .FALSE.
           LENGTH(K) = J - BOUND(K)
         END IF
       END DO

*             Smoothing pass
       SMOORD = INT(PXPS * PIP)
       DO J = 1, K
         CALL SMOOTH (X(BOUND(J)), LENGTH(J), SMOORD, LEAST, GREAT, AVE)
         CALL SMOOTH (Y(BOUND(J)), LENGTH(J), SMOORD, LEAST, GREAT, AVE)
       END DO

       K = 1
       CALL GLOM (X, WIDTH, ADPARM, MDPARM, K, BOUND, LENGTH, AVE,
     $        LEAST, GREAT, SUMX, DW, BACK(0,1), BACK(0,KLIM/2+1), RIND)

*             max velocity per note in SUMX
       I = 0
       DO J = 1, K
         AVE(J) = 0.5 * (LEAST(J) + GREAT(J))  ! split the difference
         SUMX(J) = 0.
         L = LENGTH(J)
         IF (L .LE. 0) PRINT *, 'clef: warning, zero length ', L
         DO T = 1, L
           I = I + 1
           IF (I > WIDTH) PRINT *, 'clef: out of bounds at velocity'
           IF (Y(I) > SUMX(J)) SUMX(J) = Y(I)
         END DO
         IF (AVE(J) < 0.) SUMX(J) = BOGUS
       END DO

*-----------------------------------------------------------------------
*             Make events
*-----------------------------------------------------------------------
       BASE = SQRT(PARM)
       COREX = SQRT(1. + RESPAR)
       EX = 127          ! default max for expression
       PB = 8192         ! default center for pitch bend
       J = 0             ! not yet to first segment
       L = 0             ! zero offset
       N = NINT(BOGUS)
       NOLD = N
       V = 0
       DO I = 1, WIDTH
         NOLD = N
         EXOLD = EX
         PBOLD = PB
         T = NINT(I * USPPX)
         IF (I .GE. L) THEN  ! advance to next segment?
           IF (J .GE. K) THEN
             IERR = 3
             RETURN
           END IF
           J = J + 1
           L = BOUND(J) + LENGTH(J)
           N = NINT(AVE(J))                      ! per-segment pitch
           N = MIN(N, 127)

*-----------------------------------------------------------------------
* Quote from the MIDI specification:
*
*     "Preferably, application of velocity to volume should be an exponential
*      function. This is the suggested default action;"
*
* Program SPECTR sets the image intensity to the logarithm of sound amplitude.
* Here velocity is set directly proportional to image intensity.  Thus,
* an exponential interpretation of velocity restores the original volume.
*-----------------------------------------------------------------------
           IF (SUMX(J) > 0.) THEN
             V = NINT(SUMX(J) * 127.)  ! per-segment velocity
             V = MIN(MAX(1, V), 127)
           ELSE
             V = 0
           END IF
         END IF

*-----------------------------------------------------------------------
*             Note Off
*-----------------------------------------------------------------------
         IF (NOLD .GE. 0 .AND. N .NE. NOLD) THEN
           IF (ETOT .GE. ELIM) RETURN
           ETOT = ETOT + 1
           ET(ETOT) = T
           EK(ETOT) = 0
           EP1(ETOT) = NOLD
           EP2(ETOT) = 0
         END IF

         IF (V > 0) THEN                         ! sounding ?
           IF (N < 0) PRINT *, 'clef: positive v, negative n !!'

*-----------------------------------------------------------------------
*             Expression
* The MIDI spec describes the possibility of using volume only, or volume
* and expression.  The equation for volume only is:
*
*      L = 40 log (V/127)
*
* where L is amplitude in decibels, and V is the MIDI volume.
* Program SCRIBE uses expression only.  In program SPECTR, intensity was:
*
*            log(A + p A_max) - log(p A_max)
*      Y = ------------------------------------ R
*           log(A_max + p A_max) - log(p A_max)
*
* where p is on the order of the precision, and R is the maximum brightness.
* A decibel of amplitude, defined so as to avoid taking a logarithm of zero:
*
*                    A + p A_max
*      dBA = 20 log(-------------)
*                       A_max
*
* After some algebra, it appears:
*
*      E = 127 (1 + p)^(1/2) (1/p + 1)^( (1/2) (Y/R - V/127))
*
*-----------------------------------------------------------------------
           EX = NINT(127. * COREX * BASE**(Y(I) - FLOAT(V) / 127.) )
           EX = MIN(MAX(0, EX), 127)
           IF (EX .NE. EXOLD) THEN
             IF (ETOT .GE. ELIM) RETURN
             ETOT = ETOT + 1
             ET(ETOT) = T
             EK(ETOT) = 12
             EP1(ETOT) = EX
           END IF

*-----------------------------------------------------------------------
*             Pitch bend
*-----------------------------------------------------------------------
           PB = 8192 + NINT( (X(I) - FLOAT(N)) * BF )
           PB = MIN(MAX(0, PB), 16383)
           IF (PB .NE. PBOLD) THEN
             IF (ETOT .GE. ELIM) RETURN
             ETOT = ETOT + 1
             ET(ETOT) = T
             EK(ETOT) = 57
             EP1(ETOT) = PB
           END IF
         END IF

*-----------------------------------------------------------------------
*             Note On
*-----------------------------------------------------------------------
         IF (N .GE. 0 .AND. N .NE. NOLD) THEN
           IF (ETOT .GE. ELIM) RETURN
           ETOT = ETOT + 1
           ET(ETOT) = T
           EK(ETOT) = 1
           EP1(ETOT) = N
           EP2(ETOT) = V
         END IF
       END DO  ! next time step

*-----------------------------------------------------------------------
*             Turn off any note left sounding
*-----------------------------------------------------------------------
       IF (NOLD .GE. 0) THEN
         IF (ETOT .GE. ELIM) RETURN
         ETOT = ETOT + 1
         ET(ETOT) = NINT((WIDTH+1) * USPPX)
         EK(ETOT) = 0
         EP1(ETOT) = NOLD
         EP2(ETOT) = 0
       END IF
       RETURN
      END  ! of clef


*-----------------------------------------------------------------------
* kernin - invert the Lanczos kernel function
*
*___Name______Type___In/Out___Description_______________________________
*   X         Real   In       A position in pixels of the pre-interpolated data
*   RARG(4)   Real   In       RARG(1) Position X1 after interpolation
*                             RARG(2) Position X2 after interpolation
*                             RARG(3) Intensity Y1 at position X1
*                             RARG(4) Intensity Y2 at position X2
*   KERNIN    Real   Out      Residual
*-----------------------------------------------------------------------
      FUNCTION KERNIN (X, RARG)
       IMPLICIT NONE
       REAL X, RARG(4), KERNIN

       REAL PI, SIG
       PARAMETER (PI = 3.141592654,
     $            SIG = 2.0)
       REAL A, B, DX, Q1, Q2
       LOGICAL SAFDIV

*-----------------------------------------------------------------------
*             Begin
*-----------------------------------------------------------------------
       DX = RARG(1) - X
       A = DX**2
       B = SIN(PI * DX) * SIN(PI * DX / SIG)
       IF (SAFDIV(A, B)) THEN
         Q1 = A / B
       ELSE
         Q1 = 1.
       END IF
       Q1 = Q1 * RARG(3)
       DX = RARG(2) - X
       A = DX**2
       B = SIN(PI * DX) * SIN(PI * DX / SIG)
       IF (SAFDIV(A, B)) THEN
         Q2 = A / B
       ELSE
         Q2 = 1.
       END IF
       Q2 = Q2 * RARG(4)
       KERNIN = Q1 - Q2

       RETURN
      END  ! of kernin


*-----------------------------------------------------------------------
* qwm - quadratic weighted mean.  X is assumed sorted and equally spaced.
*
*___Name______Type______In/Out____Description___________________________
*   X(N)      Real      In        Data array
*   Y(N)      Real      In        Importance weights
*   N         Integer   In        Length of arrays
*   AVE       Real      Out       Average
*   BRT       Real      Out       Brightness
*   WORK(N)   Real      Neither   Cumulative Y
*   IERR      Integer   Out       Error code
*-----------------------------------------------------------------------
      SUBROUTINE QWM (X, Y, N, AVE, BRT, WORK, IERR)
       IMPLICIT NONE
       INTEGER N, IERR
       REAL X(N), Y(N), AVE, BRT, WORK(N)

*             Constant
       REAL PI
       PARAMETER (PI = 3.141592654)

*             Local variables
       INTEGER I, J, K
       REAL A, B,
     $      G2,              ! Cauchy distibution parameter
     $      H,               ! temp scalar
     $      Q,               ! a fraction of total weight
     $      WJM1,            ! W(J-1)
     $      XQ(3),           ! quartile abscissas
     $      XSPAC            ! spacing of X values
       DOUBLE PRECISION SWXY, SWY, SY

       LOGICAL DAFDIV, SAFDIV        ! external functions

*-----------------------------------------------------------------------
*             Begin.
*-----------------------------------------------------------------------
       IF (N < 1) THEN
         IERR = -1
         RETURN
       END IF
       IERR = 0
       IF (N .EQ. 1) THEN
         AVE = X(1)
         BRT = Y(1)
         RETURN
       END IF

       SY = 0.D0                  ! sum of intensities
       DO J = 1, N
         IF (Y(J) < 0.) Y(J) = 0.
         SY = SY + Y(J)
         WORK(J) = SNGL(SY)
       END DO
       IF (SY .LE. 0.) THEN
         IERR = 1
         RETURN
       END IF

*             Weighted quartiles
       XSPAC = (X(N) - X(1)) / FLOAT(N - 1)
       DO K = 1, 3
         Q = FLOAT(K) / 4. * SNGL(SY)
         CALL RBSGT (WORK, N, Q, J)
         IF (J > 1) THEN
           WJM1 = WORK(J-1)
         ELSE
           WJM1 = 0.
         END IF
         XQ(K) = X(J) + ((Q - WJM1) / Y(J) - 0.5) * XSPAC
       END DO

*-----------------------------------------------------------------------
*             Quadratic weighted mean
*-----------------------------------------------------------------------
       AVE = XQ(2)
       G2 = 0.5 * (XQ(3) - XQ(1))                ! semi-interquartile range
       G2 = G2**2

       SWY = 0.D0
       SWXY = 0.D0
       DO J = 1, N
         H = (X(J) - AVE)**2 + G2
         IF (SAFDIV(Y(J), H)) THEN
           H = Y(J) / H
         ELSE
           IERR = 2
           RETURN
         END IF
         SWY = SWY + H
         SWXY = SWXY + H * (X(J) - AVE)
       END DO

       IF (DAFDIV(SWXY, SWY)) THEN
         AVE = AVE + SNGL(SWXY / SWY)            ! weighted mean
       ELSE
         IERR = 3
         RETURN
       END IF

*-----------------------------------------------------------------------
*             Maximum weight within 2g of the median
*-----------------------------------------------------------------------
       A = XQ(2) - (XQ(3) - XQ(1))
       B = XQ(2) + (XQ(3) - XQ(1))
       CALL RBSGT (X, N, A, J)
       CALL RBSLT (X, N, B, K)
       BRT = 0.
       DO I = J, K
         IF (Y(I) > BRT) BRT = Y(I)
       END DO

       RETURN
      END  ! of qwm


*-----------------------------------------------------------------------
* rpgm - read a portable gray map
*
*___Name______Type____________In/Out___Description______________________
*   IFIL      Character*(*)   In       Filename
*   A(LLIM)   Real            In       Bitmap array
*   LLIM      Integer         In       Max length of A
*   HORZ      Integer         Out      Width of image
*   VERT      Integer         Out      Height of image
*   MAXBRT    Integer         Out      Maximum intensity
*   IERR      Integer         Out      Error code
*-----------------------------------------------------------------------
      SUBROUTINE RPGM (IFIL, A, LLIM, HORZ, VERT, MAXBRT, IERR)
       IMPLICIT NONE
       INTEGER LLIM, HORZ, VERT, MAXBRT, IERR
       REAL A(LLIM)
       CHARACTER*(*) IFIL

*             Local variables
       INTEGER*1 B
       INTEGER H, I, J, K,     ! counters
     $         HLIM,           ! 0 or 1 sets 1 byte or 2 bytes per pixel
     $         IOU,            ! i/o unit number
     $         L,              ! index into A
     $         N,              ! length of file in bytes, input
     $         PIXL,           ! total intensity for pixel
     $         POS             ! offset into file
       LOGICAL COMENT,         ! this is a comment line?
     $         PREVW           ! previous character was whitespace?
       CHARACTER*2 FTYP
       CHARACTER*32 DECLAR, HEADER

*            External function
       INTEGER IOUNIT

*-----------------------------------------------------------------------
*             Begin.
*-----------------------------------------------------------------------
       IERR = 0
       CALL LFIL (IFIL, N, IERR)
       IF (IERR .NE. 0) THEN
         PRINT *, 'trouble getting length of file: ', IFIL
         RETURN
       END IF
       IOU = IOUNIT ()
       OPEN (UNIT=IOU, FILE=IFIL, STATUS='OLD', ACCESS='DIRECT',
     $   RECL=1, FORM='UNFORMATTED', IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
         PRINT *, 'trouble opening file: ', IFIL
         STOP
       END IF

*-----------------------------------------------------------------------
*             Interpret header.
*-----------------------------------------------------------------------
       POS = 0             ! position in file
       J = 0               ! # white space intervals
       L = 0               ! position in header
       HEADER = ' '
       PREVW = .FALSE.
       COMENT = .FALSE.
       DO 10 WHILE (J < 4)
         POS = POS + 1
         READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
         IF (IERR .NE. 0) PRINT *, 'rpgm: trouble in while loop!'
         IF (B .EQ. 35) COMENT = .TRUE.
         IF (B .EQ. 10) COMENT = .FALSE.
         IF (COMENT) GO TO 10
         IF ((B .GE. 9 .AND. B .LE. 13) .OR. B .EQ. 32) THEN
           IF (.NOT. PREVW) THEN
             J = J + 1
             L = L + 1
             HEADER(L:L) = ' '
             PREVW = .TRUE.
           END IF
         ELSE
           L = L + 1
           PREVW = .FALSE.
           HEADER(L:L) = CHAR(B)
         END IF
         IF (L .GE. 32) THEN
           PRINT *, 'header too long for PNM file format'
           IERR = 10
           RETURN
         END IF
  10   CONTINUE

       READ (UNIT=HEADER, FMT=*, IOSTAT=IERR)
     $      DECLAR, HORZ, VERT, MAXBRT
       IF (IERR .NE. 0) PRINT *, 'rpgm: trouble reading header!'
       FTYP = DECLAR(1:2)
       IF (      FTYP .NE. 'P2'
     $     .AND. FTYP .NE. 'P3'
     $     .AND. FTYP .NE. 'P5'
     $     .AND. FTYP .NE. 'P6') THEN
         IERR = 1
         RETURN
       ELSE IF (HORZ * VERT > LLIM) THEN
         PRINT *, 'ppm file too big!'
         IERR = 2
         RETURN
       ELSE IF (MAXBRT > 65535
     $          .AND. FTYP .NE. 'P2'
     $          .AND. FTYP .NE. 'P3') THEN
         PRINT *, 'maxbrt too big!'
         IERR = 3
         RETURN
       END IF

       IF (MAXBRT > 255) THEN
         HLIM = 1
       ELSE
         HLIM = 0
       END IF
       IF (FTYP .EQ. 'P3') GO TO 30
       IF (FTYP .EQ. 'P5') GO TO 50
       IF (FTYP .EQ. 'P6') GO TO 60

*-----------------------------------------------------------------------
*             Format P2
*-----------------------------------------------------------------------
       J = 0
       L = 0
       PREVW = .FALSE.
       COMENT = .FALSE.
       HEADER = ' '
       DO 20 I = POS+1, N
         READ (UNIT=IOU, REC=I, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'rpgm: trouble reading P2 data'
           RETURN
         END IF
         IF (B .EQ. 35) COMENT = .TRUE.
         IF (B .EQ. 10) COMENT = .FALSE.
         IF (COMENT) GO TO 20
         IF ((B .GE. 9 .AND. B .LE. 13) .OR. B .EQ. 32) THEN
           IF (.NOT. PREVW) THEN
             READ (UNIT=HEADER, FMT=*, IOSTAT=IERR) H
             IF (IERR .NE. 0) THEN
               PRINT *, 'rpgm: trouble internal I/O, format P2'
               RETURN
             END IF
             IF (H > MAXBRT) THEN
               PRINT *, 'rpgm: pixel too bright!'
               IERR = 4
               RETURN
             END IF
             L = L + 1
             IF (L > LLIM) THEN
               PRINT *, 'rpgm: out of memory!'
               IERR = 5
               RETURN
             END IF
             A(L) = FLOAT(H)
             PREVW = .TRUE.
             HEADER = ' '
             J = 0
           END IF
         ELSE
           J = J + 1
           PREVW = .FALSE.
           HEADER(J:J) = CHAR(B)
         END IF
  20   CONTINUE
       GO TO 90

*-----------------------------------------------------------------------
*             Format P3
*-----------------------------------------------------------------------
  30   J = 0
       K = 0
       L = 0
       PIXL = 0
       PREVW = .FALSE.
       COMENT = .FALSE.
       HEADER = ' '
       DO 40 I = POS+1, N
         READ (UNIT=IOU, REC=I, IOSTAT=IERR) B
         IF (IERR .NE. 0) THEN
           PRINT *, 'rpgm: trouble reading P3 data'
           RETURN
         END IF
         IF (B .EQ. 35) COMENT = .TRUE.
         IF (B .EQ. 10) COMENT = .FALSE.
         IF (COMENT) GO TO 40
         IF ((B .GE. 9 .AND. B .LE. 13) .OR. B .EQ. 32) THEN
           IF (.NOT. PREVW) THEN
             READ (UNIT=HEADER, FMT=*, IOSTAT=IERR) H
             IF (IERR .NE. 0) THEN
               PRINT *, 'rpgm: trouble internal I/O, format P3'
               RETURN
             END IF
             IF (H > MAXBRT) THEN
               PRINT *, 'rpgm: color too bright'
               IERR = 6
               RETURN
             END IF
             K = K + 1
             PIXL = PIXL + H
             IF (K .EQ. 3) THEN
               L = L + 1
               IF (L > LLIM) THEN
                 PRINT *, 'rpgm:  out of memory!'
                 IERR = 7
                 RETURN
               END IF
               A(L) = PIXL
               K = 0
               PIXL = 0
             END IF
             PREVW = .TRUE.
             HEADER = ' '
             J = 0
           END IF
         ELSE
           J = J + 1
           PREVW = .FALSE.
           HEADER(J:J) = CHAR(B)
         END IF
  40   CONTINUE
       MAXBRT = MAXBRT * 3
       GO TO 90

*-----------------------------------------------------------------------
*             Format P5
*-----------------------------------------------------------------------
  50   L = 0
       N = 0
       IF (VERT * HORZ > LLIM) THEN
         PRINT *,'rpgm: image to big for allocated memory!'
         IERR = 8
         RETURN
       END IF
       DO J = 1, VERT
         DO I = 1, HORZ
           DO H = HLIM, 0, -1
             POS = POS + 1
             READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
             IF (IERR .NE. 0) PRINT *, 'rpgm: reading P5 at ', I,J,H
             CALL BYTGET (B, N, H)
           END DO
           L = L + 1
           A(L) = FLOAT(N)
         END DO
       END DO
       GO TO 90


*-----------------------------------------------------------------------
*             Format P6
*-----------------------------------------------------------------------
  60   L = 0
       N = 0
       IF (VERT * HORZ > LLIM) THEN
         PRINT *,'rpgm: image to big for allocated memory!'
         IERR = 9
         RETURN
       END IF
       DO J = 1, VERT
         DO I = 1, HORZ
           PIXL = 0
           DO K = 1, 3
             DO H = HLIM, 0, -1
               POS = POS + 1
               READ (UNIT=IOU, REC=POS, IOSTAT=IERR) B
               IF (IERR .NE. 0) PRINT *, 'rpgm: reading P6 ', I,J,K,H
               CALL BYTGET (B, N, H)
             END DO
             PIXL = PIXL + N
           END DO
           L = L + 1
           A(L) = FLOAT(PIXL)
         END DO
       END DO
       MAXBRT = MAXBRT * 3

*-----------------------------------------------------------------------
*             Done.
*-----------------------------------------------------------------------
  90   CLOSE (UNIT=IOU, IOSTAT=IERR)
       IF (IERR .NE. 0) PRINT *, 'trouble closing output file!'
       RETURN
      END  ! of rpgm


*-----------------------------------------------------------------------
* wmidi - write a Standard MIDI file.  This subroutine was based on
* the much longer version appearing in project MIXMIDI, which supports
* the entire MIDI 1.0 specification.
*
*                        SUPPORTED EVENT KINDS
*
*___Kind___Description______Par._1________Par._2______________MIDI_#____
*    0     Note Off         Note #        Velocity            8
*    1     Note On          Note #        Velocity            9
*   12     Expression       7 bits                            11-11
*   55     Patch change     Patch #                           12
*   57     Pitch Bend       14 bits                           14
*   61     Text             Offset        Length              15-15-1
*   62     Copyright        Offset        Length              15-15-2
*   63     Sequence Name    Offset        Length              15-15-3
*   76     Bend Range       Cents                             RPN 0
*
*                              I/O LIST
*
*___Name________Type____________In/Out____Description___________________
*   FNAME       Character*(*)   In        Output filename
*   TEXT        Character*(*)   In        Text of name, copyright, etc.
*   ETOT        Integer         In        Count of all events
*   ET(ETOT)    Integer         In        Event times [microseconds]
*   EK(ETOT)    Integer         In        Event kind
*   EP1(ETOT)   Integer         In        Event parameter 1
*   EP2(ETOT)   Integer         In        Event parameter 2
*   IFAULT      Integer         Out       Error code 0=no errors
*-----------------------------------------------------------------------
      SUBROUTINE WMIDI (FNAME, TEXT, ETOT, ET, EK, EP1, EP2, IFAULT)
       IMPLICIT NONE
       INTEGER ETOT
       INTEGER ET(ETOT), EK(ETOT), EP1(ETOT), EP2(ETOT), IFAULT
       CHARACTER*(*) FNAME, TEXT

*           Local variables
       INTEGER*1
     $           B, C, D,             ! single bytes
     $           VARB(4)              ! a delta-time

       INTEGER
     $           G,                    ! count events
     $           H,                    ! misc. counter
     $           I,                    ! file position
     $           IOU,                  ! a file I/O Unit
     $           KIND,                 ! what kind of event
     $           L,                    ! position to write track length at
     $           M,                    ! temporary integer
     $           N,                    ! length of track data
     $           P,                    ! number of data bytes in an event
     $           S, SPREV              ! ticks elapsed so far

       REAL
     $      TPUS                       ! ticks per microsecond

*            External functions
       INTEGER
     $           IOUNIT                ! get a valid I/O Unit

*-----------------------------------------------------------------------
* Constants.  Timebase of 120 translates 240 pix/sec to ticks at 1:1.
*-----------------------------------------------------------------------
       INTEGER*1
     $           MTHD(8), MTRK(4),   ! file and track headings
     $           NULL
       INTEGER BPM, CHANEL, TIMDIV
       PARAMETER (BPM = 120,         ! always 120 beats per minute
     $            CHANEL = 0,        ! channel number always 0
     $            TIMDIV = 120)      ! timing resolution in ticks per Q.N.
       SAVE MTHD, MTRK, NULL
       DATA MTHD / 77, 84, 104, 100, 0, 0, 0, 6 /
       DATA MTRK / 77, 84, 114, 107 /
       DATA NULL / 0 /
       DATA B, C, D / 0, 0, 0 /  ! avoid compiler warnings

*-----------------------------------------------------------------------
*        Begin.
*-----------------------------------------------------------------------
       IFAULT = 0
       TPUS = TIMDIV * BPM / 60. / 1 000 000.    ! Convert times to ticks
       DO I = 1, ETOT
         ET(I) = NINT(FLOAT(ET(I)) * TPUS)
       END DO

*-----------------------------------------------------------------------
*               File I/O
*-----------------------------------------------------------------------
       IOU = IOUNIT()
       OPEN (UNIT=IOU, FILE=FNAME, ACCESS='DIRECT', RECL=1,
     $     FORM='UNFORMATTED', STATUS='NEW', IOSTAT=IFAULT)
         IF (IFAULT .NE. 0) PRINT *, 'trouble opening file ', FNAME

*                     Write the header
       DO I = 1, 8
         WRITE (UNIT=IOU, REC=I, ERR=90) MTHD(I)
       END DO

*                     File format / type 0
       B = 0
       C = 0
       WRITE (UNIT=IOU, REC=9, ERR=90) B
       WRITE (UNIT=IOU, REC=10, ERR=90) C

*                      Number of tracks
       B = 0
       C = 1
       WRITE (UNIT=IOU, REC=11, ERR=90) B
       WRITE (UNIT=IOU, REC=12, ERR=90) C

*                       Time division
       CALL BYTPUT (B, TIMDIV, 1)
       CALL BYTPUT (C, TIMDIV, 0)
       WRITE (UNIT=IOU, REC=13, ERR=90) B
       WRITE (UNIT=IOU, REC=14, ERR=90) C

       I = 14      ! at end of file header
       G = 0       ! no events processed

       DO H = 1, 4             ! write 'MTrk'
         I = I + 1
         WRITE (UNIT=IOU, REC=I, ERR=90) MTRK(H)
       END DO

*           Remember file position for track length
       L = I
       I = I + 4              ! skip ahead

*-----------------------------------------------------------------------
*            Process events
*-----------------------------------------------------------------------
       S = 0                      ! at beginning
       DO G = 1, ETOT          ! for all events in track
         KIND = EK(G)

*-----------------------------------------------------------------------
*                 Write delta-time
*-----------------------------------------------------------------------
         SPREV = S
         S = ET(G)
         M = S - SPREV
         IF (M < 0) PRINT *, 'wmidi: negative delta-time!'
         CALL WVARB (M, VARB, P)           ! convert to variable bytes
         IF (P .EQ. -1) PRINT *, 'cant convert delta time of ', M
         DO H = 1, P
           I = I + 1
           WRITE (UNIT=IOU, REC=I, ERR=90) VARB(H)
         END DO

*-----------------------------------------------------------------------
*               NoteOff - zero velocity NoteOn
*-----------------------------------------------------------------------
         IF (KIND .EQ. 0) THEN
           M = IOR(144, CHANEL)
           CALL BYTPUT (B, M, 0)                ! status byte
           CALL BYTPUT (C, EP1(G), 0)           ! note #
           WRITE (UNIT=IOU, REC=I+1, ERR=90) B
           WRITE (UNIT=IOU, REC=I+2, ERR=90) C
           WRITE (UNIT=IOU, REC=I+3, ERR=90) NULL
           I = I + 3

*-----------------------------------------------------------------------
*             NoteOn
*-----------------------------------------------------------------------
         ELSE IF (KIND .EQ. 1) THEN
           M = IOR(144, CHANEL)
           CALL BYTPUT (B, M, 0)                ! status byte
           CALL BYTPUT (C, EP1(G), 0)           ! note #
           CALL BYTPUT (D, EP2(G), 0)           ! velocity
           WRITE (UNIT=IOU, REC=I+1, ERR=90) B
           WRITE (UNIT=IOU, REC=I+2, ERR=90) C
           WRITE (UNIT=IOU, REC=I+3, ERR=90) D
           I = I + 3

*-----------------------------------------------------------------------
*             Expression
*-----------------------------------------------------------------------
         ELSE IF (KIND .EQ. 12) THEN              ! expression
           M = IOR(176, CHANEL)
           CALL BYTPUT (B, M, 0)
           C = 11
           CALL BYTPUT (D, EP1(G), 0)
           IF (D < 0) THEN
             PRINT *, 'WMIDI: negative expression!'
             IFAULT = 3
           END IF
           WRITE (UNIT=IOU, REC=I+1, ERR=90) B
           WRITE (UNIT=IOU, REC=I+2, ERR=90) C
           WRITE (UNIT=IOU, REC=I+3, ERR=90) D
           I = I + 3

*-----------------------------------------------------------------------
*             Patch change
*-----------------------------------------------------------------------
           ELSE IF (KIND .EQ. 55) THEN
             M = IOR(192, CHANEL)
             CALL BYTPUT (B, M, 0)
             CALL BYTPUT (C, EP1(G), 0)
             WRITE (UNIT=IOU, REC=I+1, ERR=90) B
             WRITE (UNIT=IOU, REC=I+2, ERR=90) C
             I = I + 2

*-----------------------------------------------------------------------
*             Pitch bend
*-----------------------------------------------------------------------
         ELSE IF (KIND .EQ. 57) THEN
           M = IOR(224, CHANEL)
           CALL BYTPUT (B, M, 0)
           M = IBITS(EP1(G), 0, 7)
           CALL BYTPUT (C, M, 0)
           M = IBITS(EP1(G), 7, 7)
           CALL BYTPUT (D, M, 0)
           WRITE (UNIT=IOU, REC=I+1, ERR=90) B
           WRITE (UNIT=IOU, REC=I+2, ERR=90) C
           WRITE (UNIT=IOU, REC=I+3, ERR=90) D
           I = I + 3

*-----------------------------------------------------------------------
*             Text-type events
*-----------------------------------------------------------------------
         ELSE IF (KIND .GE. 61 .AND. KIND .LE. 63) THEN
           B = -1             ! meta
           M = KIND - 60
           CALL BYTPUT (C, M, 0)
           WRITE (UNIT=IOU, REC=I+1, ERR=90) B
           WRITE (UNIT=IOU, REC=I+2, ERR=90) C
           I = I + 2
           M = EP2(G)
           CALL WVARB(M, VARB, P)
           IF (P .EQ. -1) PRINT *, 'cant convert text length ', m
           DO H = 1, P                               ! write length
             I = I + 1
             WRITE (UNIT=IOU, REC=I, ERR=90) VARB(H)
           END DO
           P = EP1(G)                              ! pointer
           DO H = P, P+M-1                         ! write string
             I = I + 1
             B = ICHAR(TEXT(H:H))
             WRITE (UNIT=IOU, REC=I, ERR=90) B
           END DO

*-----------------------------------------------------------------------
*             Registered parameter - pitch bend range
*-----------------------------------------------------------------------
         ELSE IF (KIND .EQ. 76) THEN
           M = IOR(176, CHANEL)
           CALL BYTPUT (B, M, 0)
           C = 101                            ! coarse adjust
           D = 0
           WRITE (UNIT=IOU, REC=I+1, ERR=90) B
           WRITE (UNIT=IOU, REC=I+2, ERR=90) C
           WRITE (UNIT=IOU, REC=I+3, ERR=90) D
           C = 100                            ! fine adjust
           D = 0
           WRITE (UNIT=IOU, REC=I+4, ERR=90) NULL   ! delta-time
           WRITE (UNIT=IOU, REC=I+5, ERR=90) B
           WRITE (UNIT=IOU, REC=I+6, ERR=90) C
           WRITE (UNIT=IOU, REC=I+7, ERR=90) D
           C = 6                                    ! slider
           M = EP1(G) / 100
           CALL BYTPUT (D, M, 0)
           WRITE (UNIT=IOU, REC=I+8, ERR=90) NULL
           WRITE (UNIT=IOU, REC=I+9, ERR=90) B
           WRITE (UNIT=IOU, REC=I+10, ERR=90) C
           WRITE (UNIT=IOU, REC=I+11, ERR=90) D
           C = 38
           M = MOD(EP1(G), 100)
           CALL BYTPUT (D, M, 0)
           WRITE (UNIT=IOU, REC=I+12, ERR=90) NULL
           WRITE (UNIT=IOU, REC=I+13, ERR=90) B
           WRITE (UNIT=IOU, REC=I+14, ERR=90) C
           WRITE (UNIT=IOU, REC=I+15, ERR=90) D
           I = I + 15

         ELSE
           PRINT *, 'unrecognized event kind ', KIND
           IFAULT = -7
         END IF  ! kind of event
       END DO  ! track events

*-----------------------------------------------------------------------
*             End of track
*-----------------------------------------------------------------------
       B = -1
       C = 47
       WRITE (UNIT=IOU, REC=I+1, ERR=90) NULL
       WRITE (UNIT=IOU, REC=I+2, ERR=90) B
       WRITE (UNIT=IOU, REC=I+3, ERR=90) C
       WRITE (UNIT=IOU, REC=I+4, ERR=90) NULL
       I = I + 4

*                 Length of track events
       N = I - L - 4
       CALL BYTPUT (B, N, 3)
       WRITE (UNIT=IOU, REC=L+1, ERR=90) B
       CALL BYTPUT (B, N, 2)
       WRITE (UNIT=IOU, REC=L+2, ERR=90) B
       CALL BYTPUT (B, N, 1)
       WRITE (UNIT=IOU, REC=L+3, ERR=90) B
       CALL BYTPUT (B, N, 0)
       WRITE (UNIT=IOU, REC=L+4, ERR=90) B

*                     done
       CLOSE (UNIT=IOU, IOSTAT=IFAULT)
       IF (IFAULT .NE. 0) PRINT *, 'WMIDI: trouble closing file'
       RETURN      ! success

*-----------------------------------------------------------------------
  90   CONTINUE         ! fatal file I/O errors
*-----------------------------------------------------------------------
       PRINT *, 'trouble writing to file ', FNAME, ' position ', I
       STOP 'unrecoverable error'

      END  ! of wmidi


*-----------------------------------------------------------------------
* Convert an integer to MIDI variable-byte format
*-----------------------------------------------------------------------
      SUBROUTINE WVARB (N, VARB, P)
       IMPLICIT NONE
       INTEGER N, P
       INTEGER*1 VARB(4)

*           1 byte size
       IF (N .LE. 127) THEN
         VARB(1) = IAND(N, 127)
         P = 1
         RETURN

*           2 byte size
       ELSE IF (N .LE. 16383) THEN
         VARB(1) = IBSET(ISHFT(IAND(N, 16256), -7), 7)
         VARB(2) = IAND(N, 127)
         P = 2
         RETURN

*          3 byte size
       ELSE IF (N .LE. 2097151) THEN
         VARB(1) = IBSET(ISHFT(IAND(N, 2080768), -14), 7)
         VARB(2) = IBSET(ISHFT(IAND(N, 16256), -7), 7)
         VARB(3) = IAND(N, 127)
         P = 3
         RETURN

*          4 byte size
       ELSE IF (N .LE. 268435455) THEN
         VARB(1) = IBSET(ISHFT(IAND(N, 266338304), -21), 7)
         VARB(2) = IBSET(ISHFT(IAND(N, 2080768), -14), 7)
         VARB(3) = IBSET(ISHFT(IAND(N, 16256), -7), 7)
         VARB(4) = IAND(N, 127)
         P = 4
         RETURN

*          Out of bounds
       ELSE
         PRINT *, 'number too large for variable-format'
       END IF
       P = -1
       RETURN
      END  ! of wvarb
*------------------------ End of file scribe.f -----------------------
