!===--- array_property.F90 -------------------------------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
!
!
!===------------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

MODULE example
  REAL(8), ALLOCATABLE :: A(:)
  !$omp declare target (A)
CONTAINS
  SUBROUTINE initialize(len)
    INTEGER,INTENT(in) :: len
    ALLOCATE(A(len))
    !$omp target enter data map(alloc:A)
  END SUBROUTINE initialize

  SUBROUTINE finalize()
    !$omp target exit data map(delete:A)
    DEALLOCATE(A)
  END SUBROUTINE finalize
END MODULE example

PROGRAM fmain
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  USE example
  implicit none
  INTEGER:: x
  OMPVV_TEST_OFFLOADING
  OMPVV_TEST_SHARED_ENVIRONMENT

  CALL initialize(N)

  A(:) = 1.0

  !$omp target update to(A)

  !$omp target map(A)
  CALL modify()
  !$omp end target

  !$omp target update from(A)

  DO x = 1, N
     OMPVV_TEST_VERBOSE(A(x) .ne. 2.0)
  END DO
  
  CALL finalize()
  OMPVV_REPORT_AND_RETURN()
END PROGRAM fmain

SUBROUTINE modify()
  use example
  implicit none
  !$omp declare target
  A(:) = 2.0
END SUBROUTINE modify
