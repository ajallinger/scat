*-----------------------------------------------------------------------
* dyrb - dynamic programming of real observations, binary theoretical states
* by Andy Allinger, 2021, released to the public domain.
*
*    Permission  to  use, copy, modify, and distribute this software and
*    its documentation  for  any  purpose  and  without  fee  is  hereby
*    granted,  without any conditions or restrictions.  This software is
*    provided "as is" without express or implied warranty.
*
*___Name____________Type______In/Out____Description_____________________
*   R(N)            Real      In        Observations 0 ... 1
*   N               Integer   In        Length of sequence
*   BASE            Real      In        Indifference value
*   TRANS           Real      In        Transition penalty
*   B(N)            Integer   Out       Optimal theoretical states
*   BACK(0:1,N)     Integer   Neither   Backpointers
*   IERR            Integer   Out       0 = no errors
*-----------------------------------------------------------------------
      SUBROUTINE DYRB (R, N, BASE, TRANS, B, BACK, IERR)
       IMPLICIT NONE
       INTEGER N
       INTEGER B(N), BACK(0:1,N), IERR
       REAL R(N), BASE, TRANS

*             Constant
       REAL BIG
       PARAMETER (BIG = 1.E36)    ! arbitrary large number

*             Local variables
       INTEGER
     $         I, J, K,           ! counters
     $         KNEW, KOLD         ! indices into SCOR()

       REAL
     $      SBEST, STRY,          ! scores in dynamic programming
     $      SCOR(0:1,0:1)

*-----------------------------------------------------------------------
*             Begin.
*-----------------------------------------------------------------------
       IERR = 0
       IF (N < 1) THEN
         IERR = 1
         RETURN
       ELSE IF (BASE .LE. 0. .OR. BASE .GE. 1.) THEN
         IERR = 2
         RETURN
       ELSE IF (TRANS .LE. 0.) THEN
         IERR = 3
         RETURN
       END IF
       DO K = 1, N
         IF (R(K) .LE. 0. .OR. R(K) > 1.) THEN
           IERR = 4
           RETURN
         END IF
       END DO

       BACK(0,1) = 0
       BACK(1,1) = 1
       SCOR(0,0) = 0.
       SCOR(1,0) = 0.
       SCOR(0,1) = -LOG(R(1) / BASE)
       SCOR(1,1) = +LOG(R(1) / BASE)

       KNEW = 1
       DO K = 2, N                ! for each time step
         KOLD = KNEW              ! set index to odd or even
         KNEW = MOD(K, 2)

         DO J = 0, 1              ! states in present step
           SBEST = -BIG
           DO I = 0, 1            ! states in prior step
             STRY = SCOR(I,KOLD)
             IF (I .NE. J) STRY = STRY - TRANS   ! penalize change
             IF (STRY > SBEST) THEN
               SBEST = STRY
               BACK(J,K) = I
             END IF
           END DO
           IF (J .EQ. 0) THEN
             SCOR(J,KNEW) = SBEST - LOG(R(K) / BASE)
           ELSE
             SCOR(J,KNEW) = SBEST + LOG(R(K) / BASE)
           END IF
         END DO
       END DO

*             Choose highest-scoring path
       IF (SCOR(0,KNEW) > SCOR(1,KNEW)) THEN
         J = 0
       ELSE
         J = 1
       END IF

*             Trace back pointers
       DO K = N, 1, -1
         B(K) = J
         J = BACK(J,K)
       END DO

       RETURN
      END  ! of dyrb
