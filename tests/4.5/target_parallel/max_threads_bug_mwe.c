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

int main() {
  printf("max_threads_bug_mwe.c\n");

  int errors = 0;
  int first_max_threads, first_num_threads, first_dynamic;
  int second_num_threads;
  int *num_threads_arr;
  int *max_threads_arr;
  int *dynamic_arr;

  //Check num_ and max_threads ICVs in target region 1
#pragma omp target parallel map(from:first_num_threads, first_max_threads)
  {
#pragma omp master
    {
      first_num_threads = omp_get_num_threads();
      first_max_threads = omp_get_max_threads();
      first_dynamic = omp_get_dynamic();
    }
  }

  num_threads_arr = (int *) malloc(first_num_threads*sizeof(int));
  max_threads_arr = (int *) malloc(first_num_threads*sizeof(int));
  dynamic_arr = (int *) malloc(first_num_threads*sizeof(int));

  //Check num_ and max_threads ICVs in target region 2
#pragma omp target parallel map(from:num_threads_arr[0:first_num_threads], max_threads_arr[0:first_num_threads], dynamic_arr[0:first_num_threads])
  {
    int thread_id = omp_get_thread_num();
    num_threads_arr[thread_id] = omp_get_num_threads();
    max_threads_arr[thread_id] = omp_get_max_threads();
    dynamic_arr[thread_id] = omp_get_dynamic();
  }

  /*
  for (int i = 0; i < first_num_threads; i++) {
    if (num_threads_arr[i] != first_num_threads) {
      errors++;
      printf("Thread %d reads %d threads, should be %d\n", i, num_threads_arr[i], first_num_threads);
    }
  }
  */
  
  printf("Num threads r1: %d\n", first_num_threads);
  printf("Max threads r1: %d\n", first_max_threads);
  printf("Dynamic r1: %d\n", first_dynamic);
  printf("Num threads r2: %d\n", num_threads_arr[0]);
  printf("Max threads r2: %d\n", max_threads_arr[0]);
  printf("Dynamic r2: %d\n", dynamic_arr[0]);

  return errors;
}
