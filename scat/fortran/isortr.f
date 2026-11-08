*-----------------------------------------------------------------------
* isortr - passive insertion sort
* by Andy Allinger, 2019, released to the public domain.
*
*    Permission  to  use, copy, modify, and distribute this software and
*    its documentation  for  any  purpose  and  without  fee  is  hereby
*    granted,  without any conditions or restrictions.  This software is
*    provided "as is" without express or implied warranty.
*
*___Name_____Type______In/Out___Description___________________________
*   X(N)     Real      In       Array to be sorted
*   IND(N)   Integer   Out      Index array
*   N        Integer   In       Length of arrays
*-----------------------------------------------------------------------
      SUBROUTINE ISORTR (X, IND, N)
       IMPLICIT NONE
       INTEGER N
       INTEGER IND(N)
       REAL X(N)
       INTEGER I, J, K
       REAL T

*           Begin.
       DO I = 1, N
         IND(I) = I
       END DO
       IF (N < 2) RETURN

       DO 10 J = 2, N, +1
         K = IND(J)
         T = X(K)
         DO I = J-1, 1, -1
           IF (X(IND(I)) .LE. T) THEN
             IND(I+1) = K
             GO TO 10
           END IF
           IND(I+1) = IND(I)
         END DO
         IND(1) = K
  10   CONTINUE
       RETURN
      END  ! of isortr
