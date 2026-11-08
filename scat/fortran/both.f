*-----------------------------------------------------------------------
* both.f - functions used in both SPECTR and SCRIBE
* by Andy Allinger, 2012-2019, released to the public domain
*
*    Permission  to  use, copy, modify, and distribute this software and
*    its documentation  for  any  purpose  and  without  fee  is  hereby
*    granted,  without any conditions or restrictions.  This software is
*    provided "as is" without express or implied warranty.
*
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
*        Find an avaiable I/O unit
*-----------------------------------------------------------------------
      FUNCTION IOUNIT ()
       IMPLICIT NONE
       INTEGER IOUNIT

       INTEGER J, IFAULT
       LOGICAL ISOPEN, ISXIST

       IOUNIT = -1
       DO 10 J = 1, 99
         IF (J .EQ. 5 .OR. J .EQ. 6 .OR. J .EQ. 7) GO TO 10
         INQUIRE (UNIT=J, EXIST=ISXIST, OPENED=ISOPEN, IOSTAT=IFAULT)
         IF (IFAULT .EQ. 0 .AND. ISXIST .AND. .NOT. ISOPEN) THEN
           IOUNIT = J
           RETURN  ! success
         END IF
  10   CONTINUE
       RETURN  ! failure
      END  ! of iounit


*-----------------------------------------------------------------------
*         Convert to upper case
*-----------------------------------------------------------------------
      SUBROUTINE UCASE (STRING)
       IMPLICIT NONE
       CHARACTER*(*) STRING
       INTEGER I, J, LTRIM
       CHARACTER*26 LOWER, UPPER
       SAVE LOWER, UPPER
       DATA LOWER / 'abcdefghijklmnopqrstuvwxyz' /
       DATA UPPER / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /

       DO I = 1, LTRIM(STRING)
         J = INDEX(LOWER, STRING(I:I))
         IF (J .NE. 0) STRING(I:I) = UPPER(J:J)
       END DO
       RETURN
      END  ! of ucase


*-----------------------------------------------------------------------
*        first non-blank position in character variable
*-----------------------------------------------------------------------
      FUNCTION KTRIM (STRING)
       IMPLICIT NONE
       CHARACTER*(*) STRING
       INTEGER KTRIM
       INTEGER J, L
       L = LEN(STRING)
       DO 50 J = 1, L, +1
         IF (STRING(J:J) .EQ. ' ') GO TO 50
         KTRIM = J
         RETURN
  50   CONTINUE
       KTRIM = 0
       RETURN
      END  ! of ktrim


*-----------------------------------------------------------------------
*       substitute for LEN_TRIM for old compilers
*-----------------------------------------------------------------------
      FUNCTION LTRIM (STRING)
       IMPLICIT NONE
       CHARACTER*(*) STRING
       INTEGER LTRIM
       INTEGER J, L

       L = LEN(STRING)
       DO 50 J = L, 1, -1
         IF (STRING(J:J) .EQ. ' ') GO TO 50
         LTRIM = J
         RETURN
  50   CONTINUE
       LTRIM = 0
       RETURN
      END  ! of ltrim


*-----------------------------------------------------------------------
*  Copy 8 bits from a INTEGER*1 into a default INTEGER
*-----------------------------------------------------------------------
      SUBROUTINE BYTGET (B, N, POS)
       IMPLICIT NONE
       INTEGER*1 B
       INTEGER N, POS

       INTEGER I, P8

       P8 = POS * 8
       DO I = 0, 7
         IF (BTEST(B, I)) THEN
           N = IBSET(N, I + P8)
         ELSE
           N = IBCLR(N, I + P8)
         END IF
       END DO

       RETURN
      END ! of bytget


*-----------------------------------------------------------------------
* Copy 8 bits from a default integer into an INTEGER*1
*-----------------------------------------------------------------------
      SUBROUTINE BYTPUT (B, N, POS)
       IMPLICIT NONE
       INTEGER*1 B
       INTEGER N, POS

       INTEGER I, P8

       P8 = POS * 8
       DO I = 0, 7
         IF (BTEST(N, I + P8)) THEN
           B = IBSET(B, I)
         ELSE
           B = IBCLR(B, I)
         END IF
       END DO

       RETURN
      END  ! of bytput
*------------------------ End of file both.f ---------------------------
