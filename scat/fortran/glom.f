*-----------------------------------------------------------------------
* glom - agglomerate a real sequence into segments of similar values
* by Andy Allinger, 2021-2023, released to the public domain
*
*    Permission  to  use, copy, modify, and distribute this software and
*    its documentation  for  any  purpose  and  without  fee  is  hereby
*    granted,  without any conditions or restrictions.  This software is
*    provided "as is" without express or implied warranty.
*
* Segments are merged until a stopping criterion is met:
*
* To stop when the adjacent segment's average value differs by
* some threshold, set ADPARM to the threshold value, set MDPARM to
* a very large number, and set K to 1. On output, K will return
* the number of segments made.
*
* To stop when the range of values within a segment reaches some
* threshold, set MDPARM to the threshold value, set ADPARM to a
* very large number, and set K to 1. On output, K will return the
* number of segments made.
*
* To form a specific number of segments, set ADPARM and MDPARM to
* a very large number, and set K to the desired number of
* segments.
*
*___Name________Type______I/O_______Description_________________________
*   X(N)        Real      In        Sequence
*   N           Integer   In        Length of the sequence
*   ADPARM      Real      In        Excessive average difference
*   MDPARM      Real      In        Excessive maximum difference
*   K           Integer   In/Out    Final number of segments
*   BOUND(N)    Integer   Out       Beginning each segment
*   LENGTH(N)   Integer   Out       Length of each segment
*   AVE(N)      Real      Out       Average value in segment
*   LEAST(N)    Real      Out       Minimum value in segment
*   GREAT(N)    Real      Out       Maximum value in segment
*   SUMX(N)     Real      Neither   Sum of segment
*   DW(N)       Real      Neither   Change in square error from merging
*   PRIOR(N)    Integer   Neither   Prior segment beginning
*   NEXT(N)     Integer   Neither   Next segment beginning
*   RIND(N)     Integer   Neither   Index into heap
*-----------------------------------------------------------------------
      SUBROUTINE GLOM (X, N, ADPARM, MDPARM, K, BOUND, LENGTH, AVE,
     $                  LEAST, GREAT, SUMX, DW, PRIOR, NEXT, RIND)
       IMPLICIT NONE
       INTEGER N
       INTEGER K, BOUND(N), LENGTH(N), PRIOR(N), NEXT(N), RIND(N)
       REAL X(N), ADPARM, MDPARM, AVE(N), LEAST(N), GREAT(N), SUMX(N),
     $       DW(N)

*         Constants
       REAL ALMBIG, BIG
       PARAMETER (ALMBIG = 9.E35,
     $            BIG = 1.E36)

*         Local variables
       INTEGER I, II, J, JJ,  ! array indices
     $         IH,            ! heap index
     $         KIN            ! input K

       REAL
     $      D2,        ! squared distance
     $      F, G,      ! min, max
     $      FL0, FL1   ! segment lengths as floats

*-----------------------------------------------------------------------
*             Begin
*-----------------------------------------------------------------------
       DO J = 1, N
         BOUND(J) = J
         LENGTH(J) = 1
         PRIOR(J) = J - 1
         NEXT(J) = J + 1
         RIND(J) = J
         AVE(J) = X(J)
         LEAST(J) = X(J)
         GREAT(J) = X(J)
         SUMX(J) = X(J)
       END DO

*-----------------------------------------------------------------------
* The heap-sifting routines seek a maximum value.  Thus, to find a
* minimal change in sum-of-squares, negated values are entered into DW.
*-----------------------------------------------------------------------
       DW(1) = -BIG
       DO J = 2, N
         D2 = (X(J-1) - X(J))**2
         DW(J) = -D2 * 0.5
       END DO
       DO J = N/2, 1, -1                                     ! form heap
         CALL SIFTDN (DW, BOUND, RIND, N, N, J)
       END DO
       KIN = K
       K = N

*-----------------------------------------------------------------------
*             Main loop
*-----------------------------------------------------------------------
       DO 10 WHILE (DW(1) > -ALMBIG .AND. K > KIN)
         J = BOUND(1)
         I = PRIOR(J)

*           Check limits
         F = MIN(LEAST(I), LEAST(J))
         G = MAX(GREAT(I), GREAT(J))
         IF ( ABS(AVE(I) - AVE(J)) .GE. ADPARM .OR.
     $        (G - F) .GE. MDPARM ) THEN
           DW(1) = -BIG
           CALL SIFTDN (DW, BOUND, RIND, N, K, 1)
           GO TO 10
         END IF

*             Merge
         LENGTH(I) = LENGTH(I) + LENGTH(J)
         SUMX(I) = SUMX(I) + SUMX(J)
         AVE(I) = SUMX(I) / FLOAT(LENGTH(I))
         LEAST(I) = F
         GREAT(I) = G

*             Delete boundary from heap
         DW(1) = DW(K)
         BOUND(1) = BOUND(K)
         RIND(BOUND(1)) = 1
         K = K - 1
         CALL SIFTDN (DW, BOUND, RIND, N, K, 1)

*             Recalculate differences
         II = PRIOR(I)
         IF (II .GE. 1) THEN
           IH = RIND(I)
           D2 = (AVE(II) - AVE(I))**2
           FL0 = FLOAT(LENGTH(II))
           FL1 = FLOAT(LENGTH(I))
           DW(IH) = -D2 * (FL0 * FL1) / (FL0 + FL1)     ! Ward's formula
           CALL SIFTUP (DW, BOUND, RIND, N, K, IH)
           CALL SIFTDN (DW, BOUND, RIND, N, K, IH)
         END IF
         JJ = NEXT(J)
         NEXT(I) = JJ

         IF (JJ .LE. N) THEN
           IH = RIND(JJ)
           D2 = (AVE(J) - AVE(JJ))**2
           FL0 = FLOAT(LENGTH(J))
           FL1 = FLOAT(LENGTH(JJ))
           DW(IH) = -D2 * (FL0 * FL1) / (FL0 + FL1)
           CALL SIFTUP (DW, BOUND, RIND, N, K, IH)
           CALL SIFTDN (DW, BOUND, RIND, N, K, IH)
           PRIOR(JJ) = I
         END IF
  10   CONTINUE

*-----------------------------------------------------------------------
*             Put output arrays in order
*-----------------------------------------------------------------------
       CALL ISHELL (BOUND, K)
       DO J = 1, K
         LENGTH(J) = LENGTH(BOUND(J))
         AVE(J) = AVE(BOUND(J))
         LEAST(J) = LEAST(BOUND(J))
         GREAT(J) = GREAT(BOUND(J))
       END DO

       RETURN
      END  ! of glom
