C
C     Sandia Mathematical Program Library
C     Mathematical Computing Services Division 5422
C     Sandia Laboratories
C     P. O. Box 5800
C     Albuquerque, New Mexico  87115
C     Control data 6600 version 4.5, 1 November 1971
C
C     Modified to run at NBS by D. Kahaner Div 713
C
C     ABSTRACT
C        ZEROIN searches for a zero of a function F(X) between
C        the given values B and C until the width of the interval
C        (B,C) has collapsed to within a tolerance specified by
C        the stopping criterion, ABS(B-C) .LE. 2.*(RW*ABS(B)+AE).
C
C     DESCRIPTION OF ARGUMENTS
C        F     - Name of the real valued external function.  This name
C                must be in an EXTERNAL statement in the calling
C                program.  F must be a function of !![one real argument].!!
C        B     - One end of the interval (B,C).  The value returned for
C                B usually is the better approximation to a zero of F.
C        C     - The other end of the interval (B,C)
C        RE    - Relative error used for RW in the stopping criterion.
C                If the requested RE is less than machine precision,
C                then RW is set to approximately machine precision.
C        AE    - Absolute error used in the stopping criterion.  If the
C                given interval (B,C) contains the origin, then a
C                nonzero value should be chosen for AE.
C        IFLAG - Returns a status of the results indicating which
C                of the following conditions hold.
C                    A - ABS(B-C) .LE. 2.*(RW*ABS(B)+AE)
C                    B - F(B) * F(C) .LT. 0.
C                    C - ABS(F(B)) .LE. ABS(F(C))
C                    D - ABS(F(B   )) .LE. MAX(ABS(F(B  )),ABS(F(C  )))
C                               OUT                   IN          IN
C                    E - Number of evaluations of F(X) .LE. 500
C               =1 Indicates normal case.  All conditions above hold.
C               =2 Indicates F(B) = 0.  Condition A may not hold.
C               =3 Indicates conditions A, B, C, and E hold but D does
C                  not.  (B,C) probably contains a singular point of f.
C               =4 Indicates conditions A and E hold but B does not.
C                  A local minimum of F(X) in (B,C) may have been found.
C               =5 Indicates search was aborted when condition E failed.
C
C     REFERENCES
C       1.  L F Shampine AND H A Watts, ZEROIN, a root-solving code,
C           SC-TM-70-631, Sept 1970.
C       2.  T J Dekker, Finding a Zero by Means of Successive Linear
C           Interpolation, *Constructive Aspects of the Fundamental
C           Theorem of Algebra*, edited by B Dejon and P Henrici, 1969.
C
C-----------------------------------------------------------------------
C Changed and renamed to make F a function of a real argument and a real array.
C Declared all variables. Replace call to R1MACH with wrapper around intrinsic.
C AJA, 3 Jan 2022
C-----------------------------------------------------------------------
      SUBROUTINE ZERORA (F, B, C, RE, AE, RARG, IFLAG)
      IMPLICIT NONE
      INTEGER IFLAG
      REAL F, B, C, RE, AE, RARG(*)
      INTEGER IC, KOUNT
      REAL A, ACBS, ACMB, CMB, ER, FA, FB, FC, FX, P, Q, RW, TOL
      REAL SPMPAR
      SAVE ER
C     Initialize
      DATA ER/0.0/
C      IF (ER .EQ. 0.0) ER = 4. * (R1MACH (4) )
C             Replace call to R1MACH
      IF (ER .EQ. 0.0) ER = 4. * (2. * SPMPAR(1))
      RW=AMAX1(RE,ER)
      IC=0
      ACBS=ABS(B-C)
      A=C
      FA = F(A, RARG)
      FB = F(B, RARG)
      FC=FA
      KOUNT=2
      FX=AMAX1(ABS(FB),ABS(FC))
C
    1 IF (ABS(FC) .GE. ABS(FB)) GO TO 2
C     Perform interchange
      A=B
      FA=FB
      B=C
      FB=FC
      C=A
      FC=FA
C
    2 CMB=0.5*(C-B)
      ACMB=ABS(CMB)
      TOL=RW*ABS(B)+AE
C
C     Test stopping criterion
      IF (ACMB .LE. TOL) GO TO 10
C
C     Calculate new iterate implicitly as B+P/Q
C     where we arrange P .GE. 0.
C     The implicit form is used to prevent overflow.
      P=(B-A)*FB
      Q=FA-FB
      IF (P .GE. 0.) GO TO 3
      P=-P
      Q=-Q
C
C     Update A and check for satisfactory reduction
C     in the size of our bounding interval.
    3 A=B
      FA=FB
      IC=IC+1
      IF (IC .LT. 4) GO TO 4
      IF (8.*ACMB .GE. ACBS) GO TO 6
      IC=0
      ACBS=ACMB
C
C     Test for too small a change
    4 IF (P .GT. ABS(Q)*TOL) GO TO 5
C
C     Increment by tolerance
      B=B+SIGN(TOL,CMB)
      GO TO 7
C
C     Root ought to be between B and (C+B)/2.
    5 IF (P .GE. CMB*Q) GO TO 6
C
C     Interpolate
      B=B+P/Q
      GO TO 7
C
C     Bisect
    6 B=0.5*(C+B)
C
C     Have completed computation for new iterate B
    7 FB = F(B, RARG)
      IF (FB .EQ. 0.) GO TO 11
      KOUNT=KOUNT+1
      IF (KOUNT .GT. 500) GO TO 15
C
C     Decide whether next step is interpolation or extrapolation
      IF (SIGN(1.0,FB) .NE. SIGN(1.0,FC)) GO TO 1
      C=A
      FC=FA
      GO TO 1
C
C
C     Finished. Process results for proper setting of IFLAG
C
   10 IF (FB*FC .GT. 0.) GO TO 13
      IF (ABS(FB) .GT. FX) GO TO 12
      IFLAG = 1
      RETURN
   11 IFLAG = 2
      RETURN
   12 IFLAG = 3
      RETURN
   13 IFLAG = 4
      RETURN
   15 IFLAG = 5
      RETURN
      END  ! of zerora
