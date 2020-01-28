! @@name:        Example_array_issues.1b.f90
! @@type:        F-free
! @@compilable:  yes
! @@linkable:    no
! @@expect:      success
MODULE example

  ! ADDED OMP DECLARE - Missing from online example.
  !$omp declare target (A)
  REAL(8), ALLOCATABLE :: A(:)

CONTAINS
  SUBROUTINE initialize(N)
    INTEGER :: N

    ALLOCATE(A(N))
    !$omp target enter data map(alloc:A)

  END SUBROUTINE initialize

  SUBROUTINE finalize()

    !$omp target exit data map(delete:A)
    DEALLOCATE(A)

  END SUBROUTINE finalize
END MODULE example


PROGRAM fmain
  use example

  implicit none

  CALL initialize(5)

  A(:) = 1.0

  PRINT *, "host, before: ", A(:)

  !$omp target update to(A)

  !$omp target map(A)
  CALL foo()
  !$omp end target

  !$omp target update from(A)

  PRINT *, "Host, after: ", A(:)

  CALL finalize()
END PROGRAM fmain

SUBROUTINE foo()
  use example
  implicit none

  !$omp declare target

  ! This write doesn't work.
  WRITE (*,*) "Device, before:", A(:)
  A(:) = 2.0
  A(1) = 2.0
  ! This write doesn't work.
  WRITE (*,*) "Device, after: ", A(:)
  ! This one works.
  WRITE (*,*) "Device, after: ", A(1)
  RETURN

END SUBROUTINE foo
