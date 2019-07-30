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
  int *threads_arr;
  int *num_threads_arr;
  int *max_threads_arr;
  int *dynamic_arr;

  //Check num_ and max_threads ICVs in target region 1
#pragma omp target map(from:first_num_threads, first_max_threads, first_dynamic)
  {
      first_num_threads = omp_get_num_threads();
      first_max_threads = omp_get_max_threads();
      first_dynamic = omp_get_dynamic();
  }

  threads_arr = (int *) malloc(first_max_threads*sizeof(int));
  num_threads_arr = (int *) malloc(first_max_threads*sizeof(int));
  max_threads_arr = (int *) malloc(first_max_threads*sizeof(int));
  dynamic_arr = (int *) malloc(first_max_threads*sizeof(int));

  for (int i = 0; i < first_max_threads; i ++) {
    threads_arr[i]=0;
    num_threads_arr[i] = 0;
    max_threads_arr[i] = 0;
    dynamic_arr[i] = 0;
  }
  //Check num_ and max_threads ICVs in target region 2
#pragma omp target parallel map(from:num_threads_arr[0:first_max_threads], max_threads_arr[0:first_max_threads], dynamic_arr[0:first_max_threads], threads_arr[0:first_max_threads])
  {
    int thread_id = omp_get_thread_num();
    #pragma omp atomic
    threads_arr[thread_id]++;
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
  printf("threads r2: ");
  for (int i = 0; i < first_max_threads; i ++)
    printf("%d  ", threads_arr[i]);
  printf("\nNum threads r2: ");
  for (int i = 0; i < first_max_threads; i ++)
    printf("%d  ", num_threads_arr[i]);
  printf("\nMax threads r2: ");
  for (int i = 0; i < first_max_threads; i ++)
    printf("%d  ", max_threads_arr[i]);
  printf("\nDynamic r2: ");
  for (int i = 0; i < first_max_threads; i ++)
    printf("%d  ", dynamic_arr[i]);

  return errors;
}
