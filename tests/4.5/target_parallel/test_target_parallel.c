//===---- test_target_parallel.c - combined consutrct target and parallel  -------------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// This test checks for the combined construct target and parallel. It creates a parallel
// region inside of the target device. 
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

  int *max_threads;
  int *thread_limit;
  int *dynamic;

  int errors = 0;

  // We obtain the default number of threads in the target region
#pragma omp target parallel map(from:default_num_threads)
  {
#pragma omp master
    default_num_threads = omp_get_num_threads();
  }

  //default_num_threads = 1024;

  OMPVV_INFOMSG("Num threads = %d", default_num_threads);

  OMPVV_INFOMSG("Max threads as reported at toplevel = %d", omp_get_max_threads());
  OMPVV_INFOMSG("Thread limit as reported at toplevel = %d", omp_get_thread_limit());
  OMPVV_INFOMSG("Dynamic thread adjustment as reported at toplevel = %d", omp_get_dynamic());

  // Warning if threads is just 1. Parallel does nothing
  OMPVV_WARNING_IF(default_num_threads == 1, "The number of threads was 1. This is not a specifications error but we could not confirm the parallel region");

  num_threads = (int *) malloc (sizeof(int) * default_num_threads);

  max_threads = (int *) malloc (sizeof(int) * default_num_threads);
  thread_limit = (int *) malloc (sizeof(int) * default_num_threads);
  dynamic = (int *) malloc (sizeof(int) * default_num_threads);

#pragma omp target parallel map(from:num_threads[0:default_num_threads], max_threads[0:default_num_threads], thread_limit[0:default_num_threads], dynamic[0:default_num_threads])
  {
    int thread_id = omp_get_thread_num();
    num_threads[thread_id] = omp_get_num_threads();
    
    max_threads[thread_id] = omp_get_max_threads();
    thread_limit[thread_id] = omp_get_thread_limit();
    dynamic[thread_id] = omp_get_dynamic();

  }

  for (int i = 0; i < default_num_threads; ++i) {
    OMPVV_TEST_AND_SET(errors, num_threads[i] != default_num_threads);
    OMPVV_ERROR_IF(num_threads[i] != default_num_threads, "The number of threads in threadID = %d was %d. Expected was %d.", i, num_threads[i], default_num_threads);
    
    OMPVV_INFOMSG("The max threads reported by threadID = %d was %d.", i, max_threads[i]);
    OMPVV_INFOMSG("The thread limit reported by threadID = %d was %d.", i, thread_limit[i]);
    OMPVV_INFOMSG("The get_dynamic reported by threadID = %d was %d.", i, dynamic[i]);

  }

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;

  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_parallel());

  OMPVV_REPORT_AND_RETURN(errors);
}
