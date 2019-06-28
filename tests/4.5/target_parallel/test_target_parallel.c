//===---- test_target_parallel.c - combined consutrct target and parallel  -------------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// This test checks for the combined construct target and parallel. It allows to create a
// parallel region inside of the target device. 
//
//===----------------------------------------------------------------------------------===//
//
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

int test_target_parallel() {
  OMPVV_INFOMSG("test_target_parallel");

  int default_num_threads;
  int *num_threads;
  int errors = 0;

  // We obtain the default number of threads in the target region
#pragma omp target parallel map(from:default_num_threads)
  {
#pragma omp master
    default_num_threads = omp_get_num_threads();
  }

  OMPVV_INFOMSG("Num threads = %d", default_num_threads);
  // Warning if threads is just 1. Parallel does nothing
  OMPVV_WARNING_IF(default_num_threads == 1, "The number of threads was 1. This is not a specifications error but we could not confirm the parallel region");

  num_threads = (int *) malloc (sizeof(int) * default_num_threads); 
#pragma omp target parallel map(from:num_threads[0:default_num_threads])
  {
    int thread_id = omp_get_thread_num();
    num_threads[thread_id] = omp_get_num_threads();
  }

  for (int i = 0; i < default_num_threads; ++i) {
    OMPVV_TEST_AND_SET(errors, num_threads[i] != default_num_threads);
    OMPVV_ERROR_IF(num_threads[i] != default_num_threads, "The number of threads in threadID = %d was %d. Expected was %d", i, num_threads[i], default_num_threads);
  }

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;

  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_parallel());

  OMPVV_REPORT_AND_RETURN(errors);
}
