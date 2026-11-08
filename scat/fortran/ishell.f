C-----------------------------------------------------------------------
C ISHELL by Alfred H. Morris, Jr., 1993
C from the Naval Surface Warfare Center Library of Mathematics Subroutines
C This file is public domain.
C
C     The Shell sorting procedure is used to reorder the elements of A
C     so that A(I).LE.A(I+1) for I=1,...,N-1. It is assumed that N.GE.1.
C-----------------------------------------------------------------------
      SUBROUTINE ISHELL (A, N)
      IMPLICIT NONE
      INTEGER N
      INTEGER A(N)
      INTEGER I, II, IMAX, J, JMAX, K(10), KI, L, LL, S
C------------------------
      DATA K / 1, 4, 13, 40, 121, 364,
     $         1093, 3280, 9841, 29524 /
C------------------------
C
C             Selection of the increments K(I) = (3^I-1)/2
C
      IF (N .LT. 2) RETURN
      IMAX = 1
      DO 10 I = 3,10
         IF (N .LE. K(I)) GO TO 20
         IMAX = IMAX + 1
   10 CONTINUE
C
C            Stepping through the increments K(IMAX),...,K(1)
C
   20 I = IMAX
      DO 40 II = 1,IMAX
         KI = K(I)
C
C             Sorting elements that are KI positions apart
C               so that A(J).LE.A(J+KI) for J=1,...,N-KI
C
         JMAX = N - KI
         DO 32 J = 1,JMAX
            L = J
            LL = J + KI
            S = A(LL)
   30          IF (S .GE. A(L)) GO TO 31
               A(LL) = A(L)
               LL = L
               L = L - KI
               IF (L .GT. 0) GO TO 30
   31    A(LL) = S
   32    CONTINUE
C
      I = I - 1
   40 CONTINUE
      RETURN
      END  ! of ishell
