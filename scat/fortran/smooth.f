*-----------------------------------------------------------------------
* smooth.f - remove outliers from a signal
* by Andy Allinger, 2022, released to the public domain.
*
*    Permission  to  use, copy, modify, and distribute this software and
*    its documentation  for  any  purpose  and  without  fee  is  hereby
*    granted,  without any conditions or restrictions.  This software is
*    provided "as is" without express or implied warranty.
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
* smooth - apply the "B" filter as described in:
*
*      "Some Statistical Aspects of LULU Smoothers"
*      Maria Dorothea Jankowitz
*      Dissertation, Stellenbosch University, Dec. 2007
*
*___Name______Type______In/Out____Description___________________________
*   S(L)      Real      Both      Sample data, returned filtered
*   L         Integer   In        Length of the sequence
*   N         Integer   In        Half-width of the filter
*   C(L)      Real      Neither   Ceiling sequence
*   F(L)      Real      Neither   Floor sequence
*   WORK(L)   Real      Neither   Workspace
*-----------------------------------------------------------------------
      SUBROUTINE SMOOTH (S, L, N, C, F, WORK)
       IMPLICIT NONE
       INTEGER L, N
       REAL S(L), C(L), F(L), WORK(L)

       INTEGER J, K

*             Begin.
       IF (N < 1) RETURN
       DO J = 1, L
         C(J) = S(J)                             ! C_0 = S
         F(J) = S(J)                             ! F_0 = S
       END DO

       DO K = 1, N
         CALL OPERU (C, L, K, WORK)              ! C_n = L(U(C_n-1))
         CALL OPERL (WORK, L, K, C)
         CALL OPERL (F, L, K, WORK)              ! F_n = U(L(F_n-1))
         CALL OPERU (WORK, L, K, F)

         DO J = 1, L
           IF (S(J) < F(J)) THEN
             S(J) = F(J)
           ELSE IF (S(J) > C(J)) THEN
             S(J) = C(J)
           END IF
         END DO
       END DO

       RETURN
      END  ! of smooth


*-----------------------------------------------------------------------
* operu - apply the "U" operation
*
*___Name___Type______In/Out___Description_______________________________
*   X(L)   Real      In       Data sequence
*   L      Integer   In       Length of the sequence
*   N      Integer   In       Half-width of the filter
*   Y(L)   Real      Out      Filtered data
*-----------------------------------------------------------------------
      SUBROUTINE OPERU (X, L, N, Y)
       IMPLICIT NONE
       INTEGER L, N
       REAL X(L), Y(L)

       REAL BIG
       PARAMETER (BIG = 1.E36)

       INTEGER I, J, K, KBEG, KEND
       REAL LEAST, GREAT

*             Begin
       IF (N < 0) RETURN
       DO I = 1, L                ! main loop
         LEAST = +BIG
         DO J = I-N, I
           GREAT = -BIG
           KBEG = MAX(1, J)
           KEND = MIN(J+N, L)
           DO K = KBEG, KEND
             IF (X(K) > GREAT) GREAT = X(K)
           END DO
           IF (GREAT < LEAST) LEAST = GREAT
         END DO
         Y(I) = LEAST
       END DO

       RETURN
      END  ! of operu


*-----------------------------------------------------------------------
* operl - apply the "L" operation
*
*___Name___Type______In/Out___Description_______________________________
*   X(L)   Real      In       Data sequence
*   L      Integer   In       Length of the sequence
*   N      Integer   In       Half-width of the filter
*   Y(L)   Real      Out      Filtered data
*-----------------------------------------------------------------------
      SUBROUTINE OPERL (X, L, N, Y)
       IMPLICIT NONE
       INTEGER L, N
       REAL X(L), Y(L)

       REAL BIG
       PARAMETER (BIG = 1.E36)

       INTEGER I, J, K, KBEG, KEND
       REAL LEAST, GREAT

*             Begin
       IF (N < 0) RETURN
       DO I = 1, L                ! main loop
         GREAT = -BIG
         DO J = I-N, I
           LEAST = +BIG
           KBEG = MAX(1, J)
           KEND = MIN(J+N, L)
           DO K = KBEG, KEND
             IF (X(K) < LEAST) LEAST = X(K)
           END DO
           IF (LEAST > GREAT) GREAT = LEAST
         END DO
         Y(I) = GREAT
       END DO

       RETURN
      END  ! of operl
