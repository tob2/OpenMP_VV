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
  int first_max_threads, first_num_threads;
  int second_max_threads, second_num_threads;
  int *num_threads_arr;

  //Check num_ and max_threads ICVs in target region 1
#pragma omp target parallel map(from:first_num_threads, first_max_threads)
  {
#pragma omp master
    {
      first_num_threads = omp_get_num_threads();
      first_max_threads = omp_get_max_threads();
    }
  }

  printf("Num threads in first target region = %d\n", first_num_threads);
  printf("Max threads in first target region = %d\n", first_max_threads);

  num_threads_arr = (int *) malloc(first_num_threads*sizeof(int));

  //Check num_ and max_threads ICVs in target region 2
#pragma omp target parallel map(from:num_threads_arr[0:first_num_threads], second_num_threads, second_max_threads)
  {
    int thread_id = omp_get_thread_num();
    num_threads_arr[thread_id] = omp_get_num_threads();
#pragma omp master
    {
      second_num_threads = omp_get_num_threads();
      second_max_threads = omp_get_max_threads();
    }
  }

  printf("Num threads in second target region = %d\n", second_num_threads);
  printf("Max threads in second target region = %d\n", second_max_threads);
  
  for (int i = 0; i < first_num_threads; i++) {
    if (num_threads_arr[i] != first_num_threads) {
      errors = 1;
      printf("Thread %d reads %d threads, should be %d/n", i, num_threads_arr[i], first_num_threads);
    }
  }

  if (first_num_threads != second_num_threads) {
    errors = 1;
  }
  
  if (errors > 0) {
    printf("Num threads changed between the two regions./n");
  }

  return errors;
}
