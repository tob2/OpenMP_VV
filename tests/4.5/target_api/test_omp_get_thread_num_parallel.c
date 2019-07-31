//===---- max_threads_bug_mwe.c - min working example of clang/xlc max_threads bug ----===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// This MWE demonstrates the behavior of num_threads and max_threads within two
// consecutive parallel regions on the target device. 
//
//===----------------------------------------------------------------------------------===//
//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define NTHREADS 256

int default_num_threads() {
  OMPVV_INFOMSG("Testing omp_get_thread_num() for target parallel regions with default num threads")
  int errors = 0;
  int max_threads = 0;
  int actual_num_threads = 0;
  int *threads_arr;

// Obtain the max_threads to allocate the array
#pragma omp target map(from:max_threads)
  {
       max_threads = omp_get_max_threads();
  }

  OMPVV_INFOMSG("Running with %d max_num_threads", max_threads);
  OMPVV_WARNING_IF(max_threads == 1, "Single number of threads allowed by max_threads, Reduced test coverage")

  // Create the arrays according to the number of threads by default
  threads_arr = (int *) malloc(max_threads*sizeof(int));

  // Fill the arrays with zero 
  for (int i = 0; i < max_threads; i ++) {
    threads_arr[i]=0;
  }

  // Check if the thread_id is distributed between 0 and num_threads-1. inside of the device
#pragma omp target parallel map(from:threads_arr[0:max_threads], actual_num_threads)
  {
    int thread_id = omp_get_thread_num();
#pragma omp master
    actual_num_threads = omp_get_num_threads();
    // Only one thread should be modifying it, but if there is an error,
    // and there is more than one thread with the same ID we want to catch this 
    // atomically. 
#pragma omp atomic
    threads_arr[thread_id]++;
  }

  OMPVV_INFOMSG("Running with %d actual num threads", actual_num_threads);
  OMPVV_TEST_AND_SET_VERBOSE(errors, actual_num_threads > max_threads);
  OMPVV_WARNING_IF(actual_num_threads == 1, "actual number of threads in the parallel region is 1. Reduced test coverage")

  for (int i = 0; i < max_threads; i++) {
    // Threads_arr should be 1, for every position that is less than 
    if (i < actual_num_threads) {
      OMPVV_TEST_AND_SET_VERBOSE(errors, threads_arr[i] != 1);
      OMPVV_ERROR_IF(threads_arr[i] != 1, "Error in thread %d, expected 1, received = %d ", i, threads_arr[i]);
    } else {
      OMPVV_ERROR_IF(threads_arr[i] != 0, "thread %d out of range (actual number of threads = %d)", i, actual_num_threads);
      OMPVV_TEST_AND_SET_VERBOSE(errors, threads_arr[i] != 0);
    }
  }
 
  free(threads_arr);
  return errors;
}

int set_num_threads() {
  OMPVV_INFOMSG("Testing omp_get_thread_num() for target parallel regions with a set of num threads")
  int errors = 0;
  int actual_num_threads = 0;
  int threads_arr[NTHREADS];

  // Fill the arrays with zero 
  for (int i = 0; i < NTHREADS; i ++) {
    threads_arr[i]=0;
  }

  // Check if the thread_id is distributed between 0 and num_threads-1. inside of the device
#pragma omp target parallel map(from:threads_arr, actual_num_threads) num_threads(NTHREADS)
  {
    int thread_id = omp_get_thread_num();
#pragma omp master
    actual_num_threads = omp_get_num_threads();
    // Only one thread should be modifying it, but if there is an error,
    // and there is more than one thread with the same ID we want to catch this 
    // atomically. 
#pragma omp atomic
    threads_arr[thread_id]++;
  }

  OMPVV_INFOMSG("Running with %d actual num threads", actual_num_threads);
  OMPVV_WARNING_IF(actual_num_threads == 1, "actual number of threads in the parallel region is 1. Reduced test coverage")

  for (int i = 0; i < NTHREADS; i++) {
    // Threads_arr should be 1, for every position that is less than 
    if (i < actual_num_threads) {
      OMPVV_TEST_AND_SET_VERBOSE(errors, threads_arr[i] != 1);
      OMPVV_ERROR_IF(threads_arr[i] != 1, "Error in thread %d, expected 1, received = %d ", i, threads_arr[i]);
    } else {
      OMPVV_TEST_AND_SET_VERBOSE(errors, threads_arr[i] != 0);
      OMPVV_ERROR_IF(threads_arr[i] != 0, "thread %d out of range (actual number of threads = %d)", i, actual_num_threads);
    }
  }
 
  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, default_num_threads());
  OMPVV_TEST_AND_SET_VERBOSE(errors, set_num_threads());

  OMPVV_REPORT_AND_RETURN(errors);
}
