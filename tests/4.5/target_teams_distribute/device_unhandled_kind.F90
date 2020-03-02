#define N 1024

PROGRAM test_target_teams_distribute_device
  USE iso_fortran_env
  USE omp_lib
  implicit none

  PRINT *, test_multiple_devices()

CONTAINS
  INTEGER FUNCTION test_multiple_devices()
    INTEGER :: x, dev, num_devices
    INTEGER, ALLOCATABLE :: c(:, :)

    num_devices = omp_get_num_devices()

    ALLOCATE(c(N, num_devices))

    DO x = 1, N
       c(x, :) = 0
    END DO

    DO dev = 1, num_devices
       !$omp target enter data map(to: c(1:N, dev:dev)) &
       !$omp& device(dev - 1)
    END DO

    ! DO dev = 1, num_devices
    !    !$omp target teams distribute map(alloc: &
    !    !$omp& c(1:N, dev:dev)) device(dev - 1)
    !    DO x = 1, N
    !       c(x, dev) = x
    !    END DO
    ! END DO

    DO dev = 1, num_devices
       !$omp target exit data map(from: c(1:N, dev:dev)) &
       !$omp& device(dev - 1)
    END DO

    test_multiple_devices = 0
  END FUNCTION test_multiple_devices
END PROGRAM test_target_teams_distribute_device
