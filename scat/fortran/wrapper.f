*-----------------------------------------------------------------------
* wrapper.f - interfaces to Linux and Fortran 95 intrinsics
* by Andy Allinger, 2011 - 2019, released to the public domain
*
*    Permission  to  use, copy, modify, and distribute this software and
*    its documentation  for  any  purpose  and  without  fee  is  hereby
*    granted,  without any conditions or restrictions.  This software is
*    provided "as is" without express or implied warranty.
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
* wrapper around FORTRAN 95 single precision machine constants
*-----------------------------------------------------------------------
      FUNCTION SPMPAR (INPT)
       IMPLICIT NONE
       INTEGER INPT
       REAL SPMPAR, S
       SAVE S
       DATA S / 1. /

       SPMPAR = 0.
       IF (INPT .EQ. 1) THEN
         SPMPAR = EPSILON(S)  ! machine precision
       ELSE IF (INPT .EQ. 2) THEN
         SPMPAR = TINY(S)     ! smallest positive number
       ELSE IF (INPT .EQ. 3) THEN
         SPMPAR = HUGE(S)     ! largest positive number
       END IF
       RETURN
      END  ! of SPMPAR


*-----------------------------------------------------------------------
*        Find the length of a given file (wrapper around STAT)
*-----------------------------------------------------------------------
      SUBROUTINE LFIL (FILNAM, N, IERR)
       IMPLICIT NONE
       CHARACTER*(*) FILNAM
       INTEGER N, IERR

*         Local variables
       INTEGER SARRAY(13)

*           ask system for file length
       CALL STAT (FILNAM, SARRAY, IERR)
       IF (IERR .NE. 0) THEN
         PRINT *, 'trouble getting length of file: ', FILNAM
         RETURN
       END IF
       N = SARRAY(8)
       RETURN
      END  ! of LFIL
