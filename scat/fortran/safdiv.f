*-----------------------------------------------------------------------
* Safe division routine to prevent overflow
* Based on a post to comp.lang.fortran on 7/17/1999 by Julio,
* following a suggestion by William Long
* A paraphrase of a code snippet -- it should not infringe any copyright.
*-----------------------------------------------------------------------
      FUNCTION SAFDIV (NUMER, DENOM)
       IMPLICIT NONE
       REAL NUMER, DENOM
       LOGICAL SAFDIV

       INTEGER ME
       REAL LIT
       LOGICAL FIRST

       SAVE ME, LIT, FIRST
       DATA FIRST / .TRUE. /

*           Begin
       IF (FIRST) THEN
         FIRST = .FALSE.
         LIT = TINY(LIT)
         ME = MAXEXPONENT(LIT)
       END IF

*-----------------------------------------------------------------------
* Require the difference in exponents to be strictly less than ME,
* to handle the case when the fractional part of the numerator
* is greater than the fractional part of the denominator.
*-----------------------------------------------------------------------
       SAFDIV = EXPONENT(NUMER) - EXPONENT(DENOM) < ME
     $           .AND. ABS(DENOM) .GE. LIT
       RETURN
      END  ! of safdiv


*-----------------------------------------------------------------------
* Same thing in double precision.
*-----------------------------------------------------------------------
      FUNCTION DAFDIV (NUMER, DENOM)
       IMPLICIT NONE
       DOUBLE PRECISION NUMER, DENOM
       LOGICAL DAFDIV

       INTEGER ME
       DOUBLE PRECISION LIT
       LOGICAL FIRST

       SAVE ME, LIT, FIRST
       DATA FIRST / .TRUE. /

*           Begin
       IF (FIRST) THEN
         FIRST = .FALSE.
         LIT = TINY(LIT)
         ME = MAXEXPONENT(LIT)
       END IF
       DAFDIV = EXPONENT(NUMER) - EXPONENT(DENOM) < ME
     $           .AND. ABS(DENOM) .GE. LIT
       RETURN
      END  ! of dafdiv
