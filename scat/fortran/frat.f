*-----------------------------------------------------------------------
* frat - Frequency Re-Assignment Transform
* by Andy Allinger, 2019-2021, released to the public domain.
*
*    Permission  to  use, copy, modify, and distribute this software and
*    its documentation  for  any  purpose  and  without  fee  is  hereby
*    granted,  without any conditions or restrictions.  This software is
*    provided "as is" without express or implied warranty.
*
*
*___Name_________Type______In/Out____Description________________________
*   S(L)         Real      In        Samples in the range -1 ... +1
*   L            Integer   In        Length of series
*   M            Integer   In        Index of coefficient pair
*   N            Integer   In        Size of transform window, samples
*   R            Integer   In        Sample rate
*   STRIDE       Integer   In        Downsampling factor
*   HOP          Real      In        Transform increment, samples
*   FRAM         Integer   In        # frames
*   FREQ(FRAM)   Real      Out       Frequencies by bin, column
*   AMPL(FRAM)   Real      Out       Amplitudes by bin, column
*   NEF(FRAM)    Integer   Neither   # transforms each frame
*   T(2,N)       Double    Neither   Transform matrix
*   W(2,N)       Double    Neither   Window function, derivative
*   Z(N)         Double    Neither   Sample value shifted and scaled
*   IERR         Integer   Out       Error code
*-----------------------------------------------------------------------
      SUBROUTINE FRAT (S, L, M, N, R, STRIDE, HOP, FRAM,
     $                  FREQ, AMPL, NEF, T, W, Z, IERR)
       IMPLICIT NONE
       INTEGER L, M, N, R, STRIDE, FRAM, IERR
       INTEGER NEF(FRAM)
       REAL S(L), HOP, FREQ(FRAM), AMPL(FRAM)
       DOUBLE PRECISION T(2,N), W(2,N), Z(N)

*             Constants
       DOUBLE PRECISION ZERO, HALF, ONE, TWO, TWOPI, WC0, WC1, WC2
       PARAMETER (ZERO = 0.D0,
     $            HALF = 0.5D0,
     $            ONE = 1.D0,
     $            TWO = 2.D0,
     $            TWOPI = 6.2831853071795865D0,
     $            WC0 = 0.375D0,                 ! Nuttall window
     $            WC1 = 0.5D0,
     $            WC2 = 0.125D0)

*             Variables
       INTEGER I,                 ! counter
     $         JADD, JSUB,        ! # samples before and after central time
     $         JBEG, JCEN, JEND,  ! offsets into S
     $         J,                 ! count samples
     $         K,                 ! count hops
     $         Q                  ! how many transforms to perform

       REAL P                     ! frames per sample

       DOUBLE PRECISION A, B, C, D,  ! transform coefficients
     $                  ADMBC,       ! ad - bc
     $                  COREX,       ! correction to frequency
     $                  DX, DXJ,     ! 2 pi / n, 2 pi j / n
     $                  DN, DR,      ! N and R as double precision
     $                  E,           ! energy
     $                  F, F0,       ! bin frequency, fundemental freq
     $                  ODN          ! 1 / n
*       DOUBLE PRECISION SX, SX2,    ! least-squares sums
*     $                 SXY, SY

*             External functions
       LOGICAL SAFDIV, DAFDIV

*-----------------------------------------------------------------------
*             Begin.
*-----------------------------------------------------------------------
       IERR = 0
       IF (L .LE. 0 .OR. M .LE. 0 .OR. N .LE. 0 ) THEN
         IERR = 1
         RETURN
       END IF

       DO J = 1, FRAM               ! clear FREQ and AMPL
         FREQ(J) = 0.
         AMPL(J) = 0.
         NEF(J) = 0
       END DO

       DN = DBLE(N)
       DR = DBLE(R)
       F0 = DR / DN
       ODN = ONE / DN
       P = FLOAT(FRAM) / FLOAT(L)
       Q = INT(L / HOP)

*-----------------------------------------------------------------------
* Real Discrete Fourier Transform matrix.
* Refer to:
*
*     "Calculation of a constant Q spectral transform"
*     Judith C. Brown
*     Journal of the Acoustic Society of America v.89 i.1, January 1991
*-----------------------------------------------------------------------
       DX = TWOPI / DN
       DO J = 1, N
         DXJ = M * J * DX
         T(1,J) = COS(DXJ) * ODN
         T(2,J) = SIN(DXJ) * ODN
       END DO

*-----------------------------------------------------------------------
*             Window function and its derivative
*-----------------------------------------------------------------------
       DO J = 1, N
         DXJ = DX * J
         W(1,J) = WC0 - WC1 * COS(DXJ) + WC2 * COS(DXJ * TWO)
         W(2,J) = WC1 * SIN(DXJ) * DX - WC2 * SIN(DXJ * TWO) * DX * TWO
       END DO

*-----------------------------------------------------------------------
*             Main loop
*-----------------------------------------------------------------------
       JADD = N * STRIDE / 2
       JSUB = JADD
       IF (MOD(N * STRIDE, 2) .EQ. 0) JSUB = JSUB - 1  ! one less for even N
       DO K = 0, Q
         JCEN = NINT(FLOAT(K) * HOP)
         JBEG = JCEN - JSUB
         JEND = JCEN + JADD
         I = 0                       ! Move samples from buffer
         DO J = JBEG, JEND, STRIDE
           I = I + 1
           IF (J .GE. 1 .AND. J .LE. L) THEN
             Z(I) = DBLE(S(J))
           ELSE
             Z(I) = ZERO
           END IF
         END DO

*-----------------------------------------------------------------------
* Uncomment the following lines if input is not high-pass filtered.
*-----------------------------------------------------------------------
*         SY = ZERO         ! Detrend by subtracting least-squares line.
*         SXY = ZERO
*         DO J = 1, N
*           SY = SY + Z(J)
*           SXY = SXY + DBLE(J) * Z(J)
*         END DO
*         SX = DN * (DN + ONE) / TWO
*         SX2 = DN * (DN + ONE) * (TWO * DN + ONE) / 6.D0
*         D = DN**2 * (DN**2 - ONE) / 12.D0
*         A = (SX2 * SY - SX * SXY) / D
*         B = (DN * SXY - SX * SY) / D
*         DO J = 1, N
*           Z(J) = Z(J) - A - B * DBLE(J)
*         END DO

*-----------------------------------------------------------------------
* Frequency Reassignment
* Refer to:
*
*      "Programming the Phase Vocoder" by Victor Lazzarini
*      in The Audio Programming Book, ed. Richard Boulanger
*      and Victor Lazzarini.  MIT Press, 2011.
*-----------------------------------------------------------------------
         A = ZERO
         B = ZERO
         C = ZERO
         D = ZERO
         DO J = 1, N
           A = A + T(1,J) * Z(J) * W(1,J)
           B = B + T(2,J) * Z(J) * W(1,J)
           C = C + T(1,J) * Z(J) * W(2,J)
           D = D + T(2,J) * Z(J) * W(2,J)
         END DO

*           Energy and frequency
         E = A**2 + B**2   ! energy
         F = F0 * M
         ADMBC = A * D - B * C
         IF (DAFDIV(ADMBC, E)) THEN
           COREX = (ADMBC / E) * (DR / TWOPI)
           IF (ABS(COREX) .LE. HALF * F0) THEN
             F = F + COREX
             J = INT(JCEN * P) + 1              ! Store in FREQ and AMPL
             J = MIN(MAX(1, J), FRAM)
             NEF(J) = NEF(J) + 1
             FREQ(J) = FREQ(J) + SNGL(E * F)
             AMPL(J) = AMPL(J) + SNGL(E)
           END IF
         END IF
       END DO  ! next K

       DO J = 1, FRAM
         IF (SAFDIV(FREQ(J), AMPL(J))) THEN
           FREQ(J) = FREQ(J) / AMPL(J)
         ELSE
           F = F0 * M
           FREQ(J) = SNGL(F)
         END IF
         IF (NEF(J) > 0) THEN
           AMPL(J) = SQRT(AMPL(J) / FLOAT(NEF(J)))
         ELSE
           AMPL(J) = 0.
         END IF
       END DO

       RETURN
      END  ! of frat
