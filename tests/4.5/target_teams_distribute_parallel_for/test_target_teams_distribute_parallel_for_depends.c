#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1000

int test_all_dependencies() {
  OMPVV_INFOMSG("test_all_dependencies");

  int errors = 0;
  int dep_1[N], dep_2[N];

  // Initialize dep_1 and dep_2
  for (int i = 0; i < N; ++i) {
    dep_1[i] = 0;
    dep_2[i] = 0;
  }

  // testing depend(out... )
#pragma omp target teams distribute parallel for \
  depend(out: dep_1) map(tofrom: dep_1[0:N])
  for (int i = 0; i < N; i++) {
    dep_1[i] += 10;
  }

#pragma omp target teams distribute parallel for \
  depend(out: dep_2) map(tofrom: dep_2[0:N])
    for (int i = 0; i < N; i++) {
      dep_2[i] += 10;
    }

  // testing depend(inout... ) with host
#pragma omp task depend(inout: dep_1) \
  depend(inout: dep_2) shared(dep_1, dep_2)
  {
    for (int i = 0; i < N; ++i) {
      dep_1[i] *= 10;
      dep_2[i] *= 10;
    }
  }

  // Testing depend(inout... ) with device 
#pragma omp target teams distribute parallel for \
  depend(inout: dep_1, dep_2) map(tofrom: dep_1[0:N], dep_2[0:N])
  for (int i = 0; i < N; i++) {
    dep_1[i] += 100;
    dep_2[i] += 100;
  }

  // Testing depend(in... )
#pragma omp target teams distribute parallel for \
  depend(in: dep_1) map(tofrom: dep_1[0:N])
  for (int i = 0; i < N; i++) {
    dep_1[i] *= 10;
  }
  
#pragma omp target teams distribute parallel for \
  depend(in: dep_2) map(tofrom: dep_2[0:N])  
  for (int i = 0; i < N; i++) {
    dep_2[i] *= 10;
  }

#pragma omp taskwait
 
  // Checking the results 
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET(errors, dep_1[i] != 2000);
    OMPVV_TEST_AND_SET(errors, dep_2[i] != 2000);    
  }

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_all_dependencies());

  OMPVV_REPORT_AND_RETURN(errors);
}

