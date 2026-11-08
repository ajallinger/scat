*-----------------------------------------------------------------------
* search.f - bisection search
* by Andy Allinger, 2012-2022, released to the public domain
*
*    Permission  to  use, copy, modify, and distribute this software and
*    its documentation  for  any  purpose  and  without  fee  is  hereby
*    granted,  without any conditions or restrictions.  This software is
*    provided "as is" without express or implied warranty.
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
* Bisection search for the first element greater than a value
*
*___Name___Type______In/Out___Description_______________________________
*   X(N)   Real      In       Array (in increasing order)
*   N      Integer   In       Length of X
*   V      Real      In       The value sought
*   I      Integer   Out      Index where X > V found
*-----------------------------------------------------------------------
      SUBROUTINE RBSGT (X, N, V, I)
       IMPLICIT NONE
       INTEGER N, I
       REAL X(N), V

       INTEGER IL, IR

*             Begin.
       IF (N < 1) RETURN
       IF (V < X(1)) THEN
         I = 1
         RETURN
       ELSE IF (V .GE. X(N)) THEN
         I = N
         RETURN
       END IF

*             Bisection search
       IL = 1
       IR = N

  10   I = (IL + IR) / 2
       IF (I .EQ. IL) THEN
         IF (X(IL) .LE. V) I = IR
         RETURN
       END IF

       IF (X(I) .LE. V) THEN
         IL = I
       ELSE
         IR = I
       END IF
       GO TO 10
      END  ! of rbsgt


*-----------------------------------------------------------------------
* Bisection search for the last element less than a value
*
*___Name___Type______In/Out___Description_______________________________
*   X(N)   Real      In       Array (in increasing order)
*   N      Integer   In       Length of X
*   V      Real      In       The value sought
*   I      Integer   Out      Index where X < V found
*-----------------------------------------------------------------------
      SUBROUTINE RBSLT (X, N, V, I)
       IMPLICIT NONE
       INTEGER N, I
       REAL X(N), V

       INTEGER IL, IR

*             Begin
       IF (N < 1) RETURN
       IF (V .LE. X(1)) THEN
         I = 1
         RETURN
       ELSE IF (V > X(N)) THEN
         I = N
         RETURN
       END IF

*             Bisection search
       IL = 1
       IR = N

  10   I = (IL + IR) / 2
       IF (I .EQ. IL) THEN
         IF (X(IR) < V) I = IR
         RETURN
       END IF

       IF (X(I) < V) THEN
         IL = I
       ELSE
         IR = I
       END IF
       GO TO 10
      END  ! of rbslt


*-----------------------------------------------------------------------
* Bisection search for the first element greater than or equal to a value
*
*___Name___Type______In/Out___Description_______________________________
*   X(N)   Integer   In       Array (in increasing order)
*   N      Integer   In       Length of X
*   V      Integer   In       The value sought
*   I      Integer   Out      Index where X .GE. V found
*-----------------------------------------------------------------------
      SUBROUTINE IBSGE (X, N, V, I)
       IMPLICIT NONE
       INTEGER N
       INTEGER X(N), V, I

       INTEGER IL, IR

*             Begin
       IF (N < 1) RETURN
       IF (V < X(1)) THEN
         I = 1
         RETURN
       ELSE IF (V > X(N)) THEN
         I = N
         RETURN
       END IF

*             Bisection search
       IL = 1
       IR = N

  10   I = (IL + IR) / 2
       IF (I .EQ. IL) THEN
         IF (X(IL) < V) I = IR
         RETURN
       END IF

       IF (X(I) < V) THEN
         IL = I
       ELSE
         IR = I
       END IF
       GO TO 10
      END  ! of ibsge


*-----------------------------------------------------------------------
* Bisection search for the last element less than or equal to a value
*
*___Name___Type______In/Out___Description_______________________________
*   X(N)   Integer   In       Array (in increasing order)
*   N      Integer   In       Length of X
*   V      Integer   In       The value sought
*   I      Integer   Out      Index where X .LE. V found
*-----------------------------------------------------------------------
      SUBROUTINE IBSLE (X, N, V, I)
       IMPLICIT NONE
       INTEGER N
       INTEGER X(N), V, I

       INTEGER IL, IR

*             Begin
       IF (N < 1) RETURN
       IF (V < X(1)) THEN
         I = 1
         RETURN
       ELSE IF (V > X(N)) THEN
         I = N
         RETURN
       END IF

*             Bisection search
       IL = 1
       IR = N

  10   I = (IL + IR) / 2
       IF (I .EQ. IL) THEN
         IF (X(IR) .LE. V) I = IR
         RETURN
       END IF

       IF (X(I) .LE. V) THEN
         IL = I
       ELSE
         IR = I
       END IF
       GO TO 10

      END  ! of ibsle
*---------------------- End of file search.f ---------------------------
