!===--- test_target_teams_distribute.F90------------------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses the target teams distribute directive and tests to validate
! that computation inside the region executes properly.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

      PROGRAM test_target_teams_distribute
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none

        OMPVV_TEST_VERBOSE(test_distribute() .ne. 0)

        OMPVV_REPORT_AND_RETURN()
      CONTAINS
        INTEGER FUNCTION test_distribute()
					INTEGER,DIMENSION(N):: a, b
					INTEGER:: num_teams, errors, x
					LOGICAL:: is_host

					errors = 0

					DO x = 1, N
						a(x) = 1
						b(x) = x
					END DO

					!$omp target teams distribute map(tofrom: a(1:N)) map(to: b(1:N)) &
					!$omp& map(from: num_teams, is_host)
					DO x = 1, N
						is_host = omp_is_initial_device()
						num_teams = omp_get_num_teams()
						a(x) = a(x) + b(x)
					END DO

					DO x = 1, N
						IF (a(x) .ne. (1 + b(x))) THEN
							errors = errors + 1
						END IF
					END DO

					IF (num_teams .eq. 1) THEN
						OMPVV_WARNING("Test ran on one team.  Distribution of teams ")
						OMPVV_WARNING("cannot occur")
					END IF

					If (is_host) THEN
						OMPVV_WARNING("Test ran on host. Target execution did not occur")
					END IF

					test_distribute = errors
				END FUNCTION test_distribute
			END PROGRAM test_target_teams_distribute
