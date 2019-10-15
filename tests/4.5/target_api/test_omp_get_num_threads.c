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

#define NTHREADS 1024
#define NTEAMS 128

int default_num_threads() {
  int errors = 0;
  int max_threads = 0;
  int def_num_teams = 0;
  int *threads_arr;
  int *num_threads_arr;

// Obtain the max_threads to allocate the array
#pragma omp target map(from:max_threads)
  {
       max_threads = omp_get_max_threads();
  }

  // Create the arrays according to the number of threads by default
  threads_arr = (int *) malloc(max_threads*sizeof(int));
  num_threads_arr = (int *) malloc(max_threads*sizeof(int));

  // Fill the arrays with zero 
  for (int i = 0; i < max_threads; i ++) {
    threads_arr[i]=0;
    num_threads_arr[i] = 0;
  }


  // Check if the thread_id is distributed between 0 and num_threads-1. inside of the device
#pragma omp target parallel map(from:num_threads_arr[0:first_max_threads],  threads_arr[0:first_max_threads])
  {
    int thread_id = omp_get_thread_num();
    // Only one thread should be modifying it, but if there is an error,
    // and there is more than one thread with the same ID we want to catch this 
    // atomically. 
    #pragma omp atomic
    threads_arr[thread_id]++;
    num_threads_arr[thread_id] = omp_get_num_threads();
  }

  // Check that all the threads see the same num_threads
  for (int i = 0; i < max_threads; i++) {
    // Assume that num_threads_arr[0] has the right value
    // If it is not the case it shold produce an error indirectly
    if (i < num_threads_arr[i]
    OMPVV_TEST_AND_SET(num_threads_arr[i] 
  }


  for (int i = 0; i < max_threads; i++) {
    // Threads_arr should be 1, for every position that is less than 
    OMPVV_TEST_AND_SET(threads_arr[i] && 
  }
#pragma omp target teams map(from:max_threads)
  {
       def_num_teams = omp_get_num_teams();
  }
  num_teams_arr = (int *) malloc( max_threads*sizeof(int));
  /*
  */
}

 


int main() {
  printf("omp_get_thread_num api\n");


  return errors;
}
