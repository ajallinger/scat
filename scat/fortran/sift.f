*-----------------------------------------------------------------------
* sift.f - sift a real heap, with forwards and reverse indices.
* by Andy Allinger, 2017-2019, released to the public domain
*
*    Permission  to  use, copy, modify, and distribute this software and
*    its documentation  for  any  purpose  and  without  fee  is  hereby
*    granted,  without any conditions or restrictions.  This software is
*    provided "as is" without express or implied warranty.
*
*___Name____Type______In/Out___Description______________________________
*   H(L)    Real      Both     A heap
*   I(L)    Integer   Both     Index array carried along
*   J(L)    Integer   Both     Reverse permutation
*   L       Integer   In       Array bound
*   N       Integer   In       Size of heap
*   POS     Integer   In       Position of the new element
*-----------------------------------------------------------------------
      SUBROUTINE SIFTUP (H, I, J, L, N, POS)                   ! sift up
       IMPLICIT NONE
       INTEGER L
       INTEGER I(L), J(L), N, POS
       REAL H(L)

       INTEGER ISWAP, P, STEM
       REAL RSWAP

*          Begin.
       IF (N > L) RETURN               ! validate
       P = POS
  10   STEM = P / 2                    ! come here
       IF (STEM .GE. 1) THEN           ! not at root
         IF (H(P) > H(STEM)) THEN      ! compare to stem
           RSWAP = H(P)
           H(P) = H(STEM)
           H(STEM) = RSWAP
           ISWAP = I(P)
           I(P) = I(STEM)
           I(STEM) = ISWAP
           J(I(P)) = P
           J(I(STEM)) = STEM
           P = STEM
           GO TO 10
         END IF
       END IF
       RETURN
      END  ! of siftup


*-----------------------------------------------------------------------
      SUBROUTINE SIFTDN (H, I, J, L, N, POS)                 ! sift down
       IMPLICIT NONE
       INTEGER L
       INTEGER I(L), J(L), N, POS
       REAL H(L)

       INTEGER ISWAP, P, LBRAN, RBRAN
       REAL RSWAP

*          Begin.
       P = POS
  20   LBRAN = P * 2                       ! come here
       IF (LBRAN .LE. N) THEN
         RBRAN = LBRAN + 1
         IF (RBRAN .LE. N) THEN
           IF (H(RBRAN) > H(LBRAN)) GO TO 30
         END IF  ! right branch exists?
         IF (H(P) < H(LBRAN)) THEN  ! compare to left
           RSWAP = H(P)
           H(P) = H(LBRAN)
           H(LBRAN) = RSWAP
           ISWAP = I(P)
           I(P) = I(LBRAN)
           I(LBRAN) = ISWAP
           J(I(P)) = P
           J(I(LBRAN)) = LBRAN
           P = LBRAN
           GO TO 20
         END IF
       END IF
       RETURN

  30   IF (H(P) < H(RBRAN)) THEN  ! compare to right
         RSWAP = H(P)
         H(P) = H(RBRAN)
         H(RBRAN) = RSWAP
         ISWAP = I(P)
         I(P) = I(RBRAN)
         I(RBRAN) = ISWAP
         J(I(P)) = P
         J(I(RBRAN)) = RBRAN
         P = RBRAN
         GO TO 20
       END IF
       RETURN
      END  ! of siftdn
*-------------------------- End of file sift.f --------------------------
