!===--- test_target_teams_distribute_shared.F90-----------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses the shared clause on a target teams distribute directive and
! tests in a few ways that the variable is shared between the teams.  In the
! first test, the atomic directive is used to indicate that all operations on
! the variable should be done atomicly.  If the value is the correct value at
! the end of the region, then all teams operated on the same variable.
!
! The second test sets the value of the shared value to a range of values.
! Since the variable is being updated by each team, it is impossible to know
! which value will be the last to be assigned to the variable.  However, we
! test to make sure that the variable is assigned by one of the values that
! could result from the operation.
!
! The third test, instead of writing to the variable, only reads from the
! variable.  This tests that the value of the shared variable has not been
! initiallized improperly or privatized.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

      PROGRAM test_target_teams_distribute_device
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none
        INTEGER :: errors
        errors = 0

        OMPVV_TEST_OFFLOADING()

        OMPVV_TEST_VERBOSE(test_atomic_share() .ne. 0)
        OMPVV_TEST_VERBOSE(test_data_race_share() .ne. 0)
        OMPVV_TEST_VERBOSE(test_read_only_share() .ne. 0)

        OMPVV_REPORT_AND_RETURN()
      CONTAINS
        INTEGER FUNCTION test_atomic_share()
          INTEGER:: x, errors, num_teams, share
          INTEGER,DIMENSION(N):: a

          DO x = 1, N
            a(x) = x
          END DO

          !$omp target teams distribute shared(share) map(to: a(1:N)) &
          !$omp& map(tofrom: num_teams)
          DO x = 1, N
            num_teams = omp_get_num_teams()
            !$omp atomic
            share = share + a(x)
          END DO

          DO x = 1, N
            share = share - x
          END DO

          IF (share .ne. 0) THEN
            errors = errors + 1
          END IF
          IF (num_teams .eq. 1) THEN
            OMPVV_WARNING("Test ran on too few teams to test data sharing")
          END IF
          test_atomic_share = errors
        END FUNCTION test_atomic_share

        INTEGER FUNCTION test_data_race_share()
          INTEGER,DIMENSION(N):: a
          INTEGER:: share, x, errors
          errors = 0
          share = 0

          !$omp target teams distribute shared(share) map(to:a(1:N))
          DO x = 1, N
            share = a(x)
          END DO

          IF ((share .lt. 0) .or. (share .gt. N)) THEN
            errors = errors + 1
          END IF

          test_data_race_share = errors
        END FUNCTION test_data_race_share

        INTEGER FUNCTION test_read_only_share()
          INTEGER,DIMENSION(N):: a
          INTEGER:: x, errors, share
          errors = 0
          share = 5

          DO x = 1, N
            a(x) = x
          END DO

          !$omp target teams distribute map(tofrom: a(1:N)) shared(share)
          DO x = 1, N
            a(x) = a(x) + share
          END DO

          DO x = 1, N
            IF (a(x) .ne. x + 5) THEN
              errors = errors + 1
            END IF
          END DO

          test_read_only_share = errors
        END FUNCTION test_read_only_share
      END PROGRAM
