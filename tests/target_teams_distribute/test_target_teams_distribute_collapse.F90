!===--- test_target_teams_distribute.F90------------------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses the collapse clause and tests that for loops out of the scope
! of the collapsed loops are not parallelized.  This test tests using one and
! two collapsed loops.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 512

      PROGRAM test_target_teams_distribute_collapse
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none
        INTEGER :: errors
        OMPVV_TEST_OFFLOADING()
        errors = 0

        OMPVV_TEST_VERBOSE(test_collapse1() .ne. 0)
        OMPVV_TEST_VERBOSE(test_collapse2() .ne. 0)
        OMPVV_REPORT_AND_RETURN()

        CONTAINS
          INTEGER FUNCTION test_collapse1()
            INTEGER :: a(N, N), b(N, N + 1)
            INTEGER :: errors
            INTEGER :: temp_total
            INTEGER :: num_teams
            INTEGER :: x, y

            DO x = 1, N
              b(x, 1) = 0
              DO y = 1, N
                a(x, y) = x + y
              END DO
            END DO

            !Collapse is only for one loop.  Second loop should be sequential

            !$omp target teams distribute map(to: a(1:N, 1:N)) map(tofrom: &
            !$omp& b(1:N, 1:N+1)) map(from: num_teams) collapse(1)
            DO x = 1, N
              DO y = 1, N
                b(x, y + 1) = b(x, y) + a(x, y)
                num_teams = omp_get_num_teams()
              END DO
            END DO
            !$omp end target teams distribute

            DO x = 1, N
              temp_total = 0
              DO y = 1, N + 1
                OMPVV_TEST_AND_SET(errors, temp_total-b(x, y) .ne. 0)
                IF (y .ne. N + 1) THEN
                  temp_total = temp_total + a(x, y)
                END IF
              END DO
            END DO

            IF (num_teams .eq. 1) THEN
              OMPVV_WARNING("Test operated with one team. Parallelism of teams")
              OMPVV_WARNING("distribute can't be guarunteed.")
            END IF
            test_collapse1 = errors
          END FUNCTION test_collapse1

          INTEGER FUNCTION test_collapse2()
            INTEGER :: a(N, N, N), b(N, N, N+1)
            INTEGER :: errors
            INTEGER :: x, y, z
            INTEGER :: temp_total

            DO x = 1, N
              DO y = 1, N
                b(x, y, 1) = 0
                DO z = 1, N
                  a(x, y, z) = x + y + z
                END DO
              END DO
            END DO

            !$omp target teams distribute map(to: a(1:N, 1:N, 1:N)) &
            !$omp& map(from: b(1:N, 1:N, 1:N+1)) collapse(2)
            DO x = 1, N
              DO y = 1, N
                DO z = 1, N
                  b(x, y, z+1) = b(x, y, z) + a(x, y, z)
                END DO
              END DO
            END DO

            DO x = 1, N
              DO y = 1, N
                temp_total = 0
                DO z = 1, N
                  OMPVV_TEST_AND_SET(errors, (temp_total - b(x, y, z)) .ne. 0)
                  IF (z .ne. N + 1) THEN
                    temp_total = temp_total + a(x, y, z)
                  END IF
                END DO
              END DO
            END DO

            test_collapse2 = errors
          END FUNCTION test_collapse2
      END PROGRAM test_target_teams_distribute_collapse
