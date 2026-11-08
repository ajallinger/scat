*-----------------------------------------------------------------------
* filter - High-order Butterworth filter.
* by Andy Allinger, 2021, released to the public domain
*
*    Permission  to  use, copy, modify, and distribute this software and
*    its documentation  for  any  purpose  and  without  fee  is  hereby
*    granted,  without any conditions or restrictions.  This software is
*    provided "as is" without express or implied warranty.
*
* Refer to:
*
*     "Cookbook formulae for audio EQ biquad filter coefficients"
*     Robert Bristow-Johnson, [2001]
*
*     "The Butterworth Low-Pass Filter"
*     John Stensby, 19 Oct 2005
*
* The Butterworth poles lie on a circle.  The product of the qualities
* is 2^(-1/2).
*
*___Name_____Type______In/Out___Description_____________________________
*   S(L)     Real      Both     Sample data, returned filtered
*   L        Integer   In       Length of S
*   FSAMP    Real      In       Sample rate, cycles per second
*   FPASS    Real      In       Pass frequency, cycles per second
*   FSTOP    Real      In       Stop frequency, cycles per second
*   HPASS    Real      In       Minimum passband transmission, 0...1
*   HSTOP    Real      In       Maximum stopband transmission, 0...1
*   IERR     Integer   Out      Error code
*-----------------------------------------------------------------------
      SUBROUTINE FILTER (S, L, FSAMP, FPASS, FSTOP, HPASS, HSTOP, IERR)
       IMPLICIT NONE
       INTEGER L, IERR
       REAL S(L), FSAMP, FPASS, FSTOP, HPASS, HSTOP

*             Constants
       REAL ZERO, HALF, ONE, TWO, PI
       PARAMETER (ZERO = 0.,
     $            HALF = 0.5,
     $            ONE = 1.,
     $            TWO = 2.,
     $            PI = 3.141592654)

*             Local variables
       INTEGER J, K, N
       REAL A0, A1, A2, B1, B2,               ! filter coefficients
     $                  D, E,                 ! attenuation variables
     $                  FCUT,                 ! cutoff frequency
     $                  Q,                    ! quality
     $                  C, O, R, W0,          ! intermediate variables
     $                  X, X1, X2, Y, Y1, Y2  ! temporary storage
       LOGICAL LO

*-----------------------------------------------------------------------
*             Begin.
*-----------------------------------------------------------------------
       IERR = 0
       IF (L .LE. 0) THEN
         IERR = 1
         RETURN
       ELSE IF (FSAMP .LE. ZERO) THEN
         IERR = 2
         RETURN
       ELSE IF (FPASS .LE. ZERO .OR. FPASS .GT. HALF * FSAMP) THEN
         IERR = 3
         RETURN
       ELSE IF (FSTOP .LE. ZERO .OR. FSTOP .GT. HALF * FSAMP) THEN
         IERR = 4
         RETURN
       ELSE IF (HPASS .LE. ZERO .OR. HPASS .GE. ONE) THEN
         IERR = 5
         RETURN
       ELSE IF (HSTOP .LE. ZERO .OR. HSTOP .GE. ONE) THEN
         IERR = 6
         RETURN
       ELSE IF (FPASS .EQ. FSTOP) THEN
         IERR = 7
         RETURN
       END IF

*             Find coefficients
       LO = FPASS < FSTOP
       D = ONE / HSTOP
       E = SQRT(ONE / HPASS**2 - ONE)
       N = INT(ABS(LOG(E / SQRT(D**2 - ONE)) / LOG(FPASS / FSTOP))) + 1
       IF (MOD(N, 2) .NE. 0) N = N + 1
       IF (LO) THEN
         O = -ONE / FLOAT(N)
       ELSE
         O = +ONE / FLOAT(N)
       END IF
       FCUT = FSTOP * SQRT(D**2 - ONE)**O
       W0 = TWO * PI * FCUT / FSAMP
       C = COS(W0)

       DO K = N/2, 1, -1
         Q = -HALF / COS(PI * FLOAT(2 * K + N - 1) / FLOAT(2 * N))
         R = SIN(W0) / (TWO * Q)
         IF (LO) THEN
           A1 = (ONE - C) / (ONE + R)
           A0 = HALF * A1
         ELSE
           A1 = -(ONE + C) / (ONE + R)
           A0 = -HALF * A1
         END IF
         A2 = A0
         B1 = -TWO * C / (ONE + R)
         B2 = (ONE - R) / (ONE + R)

*             Process the buffer.
         X = ZERO
         X1 = ZERO
         X2 = ZERO
         Y = ZERO
         Y1 = ZERO
         Y2 = ZERO
         DO J = 1, L
           X2 = X1
           X1 = X
           X = S(J)
           Y2 = Y1
           Y1 = Y
           Y = A0 * X + A1 * X1 + A2 * X2 - B1 * Y1 - B2 * Y2
           S(J) = Y
         END DO
       END DO

       RETURN
      END  ! of filter
