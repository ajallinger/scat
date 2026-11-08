C***********************************************************************
C
C                                               Robert Renka
C                                       Oak Ridge Natl. Lab.
C
C   This routine applies a set of permutations to a vector.
C
C Input parameters -  N - Length of A and IP.
C
C                    IP - Vector containing the sequence of
C                         integers 1,...,N permuted in the
C                         same fashion that a is to be permuted.
C
C                     A - Vector to be permuted.
C
C N and IP are not altered by this routine.
C
C Output parameter -  A - Reordered vector reflecting the
C                         permutations defined by IP.
C
C***********************************************************************
      SUBROUTINE RORDER (A, IP, N)
      INTEGER N, IP(N)
      REAL    A(N)
      INTEGER NN, K, J, IPJ
      REAL    TEMP
C
C Local parameters -
C
C NN =   Local copy of N
C K =    Index for IP and for the first element of A in a permutation
C J =    Index for IP and A, J .GE. K
C IPJ =  IP(J)
C TEMP = Temporary storage for A(K)
C
      NN = N
      IF (NN .LT. 2) RETURN
      K = 1
C
C Loop on permutations
C
    1 J = K
      TEMP = A(K)
C
C Apply permutation to A.  IP(J) is marked (made negative)
C   as being included in the permutation.
C
    2 IPJ = IP(J)
      IP(J) = -IPJ
      IF (IPJ .EQ. K) GO TO 3
      A(J) = A(IPJ)
      J = IPJ
      GO TO 2
    3 A(J) = TEMP
C
C Search for an unmarked element of IP
C
    4 K = K + 1
      IF (K .GT. NN) GO TO 5
      IF (IP(K) .GT. 0) GO TO 1
      GO TO 4
C
C All permutations have been applied.  Unmark IP.
C
    5 DO 6 K = 1,NN
        IP(K) = -IP(K)
    6 CONTINUE
      RETURN
      END  ! of RORDER
