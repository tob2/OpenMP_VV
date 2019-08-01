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
#define NTEAMS 256

int default_num_teams() {
  OMPVV_INFOMSG("Testing omp_get_thread_num() for target parallel teams regions with default num threads and num teams");
  int errors = 0;
  int *max_threads;
  int max_max_num_threads;
  int *actual_num_threads = 0;
  int *threads_arr;
  int def_num_teams = 0;


  // Get the default number of teams
#pragma omp target teams map(from:def_num_teams)
  {
    if (omp_get_team_num() == 0)
      def_num_teams = omp_get_num_teams();
  }
  // Warning if single team
  OMPVV_INFOMSG("Running with %d teams by default", def_num_teams);
  OMPVV_WARNING_IF(def_num_teams == 1, "single team created by default. Reduced test coverage");

  // Allocate arrays
  max_threads = (int *) malloc( def_num_teams*sizeof(int));
  actual_num_threads = (int *) malloc( def_num_teams*sizeof(int));
  
  // Get the max_number_of_threads per team
#pragma omp target teams map(tofrom:max_threads[0:def_num_teams])
  {
    int team_id = omp_get_team_num();
    max_threads[team_id] = omp_get_max_threads(); 
  }

  // Obtain the max, max num of threads
  max_max_num_threads = max_threads[0];
  for (int i = 0; i < def_num_teams; i ++) {
    if (max_threads[i] > max_max_num_threads) {
      max_max_num_threads = max_threads[i];
    }
  }

  // Allocate threads_array and initialize to zero
  threads_arr = (int *) malloc(max_max_num_threads*def_num_teams*sizeof(int));
  for (int i = 0; i < def_num_teams; i ++) {
    max_threads[i] = 0; 
    actual_num_threads[i] = 0;
    for (int j = 0; j < max_max_num_threads; j ++) {
      threads_arr[i*def_num_teams + j] = 0;
    }
  }
  OMPVV_INFOMSG("The max number of threads across all teams is %d", max_max_num_threads);

#pragma omp target teams map(from:threads_arr[0:def_num_teams*max_max_num_threads], actual_num_threads[0:def_num_teams])
  {
#pragma omp parallel 
    {
    int team_id = omp_get_team_num();
    int thread_id = omp_get_thread_num();
#pragma omp master
      actual_num_threads[team_id] = omp_get_num_threads();
    
#pragma omp atomic
      threads_arr[team_id*def_num_teams + thread_id]++;
    }
  }

  // Check the number of threads is less than the reported max
  for (int i = 0; i < def_num_teams; i ++) {
    OMPVV_INFOMSG("Team %d Running with %d threads by default", i, def_num_teams);
    OMPVV_TEST_AND_SET_VERBOSE(errors, actual_num_threads[i] > max_threads[i]);
    OMPVV_WARNING_IF(actual_num_threads[i] == 1, "Single threaded team executed. Reduced test covarage");
  }

  // Chech results there should be only 0 and 1
  for (int i = 0; i < def_num_teams; i ++) {
    for (int j = 0; j < max_max_num_threads; j ++) {
      if (j < actual_num_threads[i]) {
        OMPVV_TEST_AND_SET_VERBOSE(errors, threads_arr[i*def_num_teams + j] != 1);
      }
      else {
        OMPVV_TEST_AND_SET_VERBOSE(errors, threads_arr[i*def_num_teams * j] != 0);
      }
    }
  }
 
  free(threads_arr);
  free(max_threads);
  free(actual_num_threads);
  return errors;  
}

int set_num_teams() {
  OMPVV_INFOMSG("Testing omp_get_thread_num() for target parallel teams regions when defining the number of teams and number of threads");
  int errors = 0;
  int actual_num_threads[NTEAMS];
  int threads_arr[NTEAMS][NTHREADS];

  for (int i = 0; i < NTEAMS; i ++) {
    actual_num_threads[i] = 0;
    for (int j = 0; j < NTHREADS; j ++) {
      threads_arr[i][j] = 0;
    }
  }

#pragma omp target teams map(from:threads_arr, actual_num_threads) num_teams(NTEAMS)
  {
#pragma omp parallel num_threads(NTHREADS)
    {
    int team_id = omp_get_team_num();
    int thread_id = omp_get_thread_num();
#pragma omp master
      actual_num_threads[team_id] = omp_get_num_threads();
    
#pragma omp atomic
      threads_arr[team_id][thread_id]++;
    }
  }

  // Check the number of threads is less than the reported max
  for (int i = 0; i < NTEAMS; i ++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, actual_num_threads[i] > NTHREADS);
    OMPVV_WARNING_IF(actual_num_threads[i] == 1, "Single threaded team executed. Reduced test covarage");
  }

  // Chech results there should be only 0 and 1
  for (int i = 0; i < NTEAMS; i ++) {
    for (int j = 0; j < NTHREADS; j ++) {
      if (j < actual_num_threads[i]) {
        OMPVV_TEST_AND_SET_VERBOSE(errors, threads_arr[i][j] != 1);
        OMPVV_ERROR_IF(threads_arr[i][j] != 1, "Error in thread %d, expected 1, received = %d ", i, threads_arr[i][j]);
      } else {
        OMPVV_TEST_AND_SET_VERBOSE(errors, threads_arr[i][j] != 0);
        OMPVV_ERROR_IF(threads_arr[i][j] != 0, "thread %d, team %d out of range (actual number of threads = %d)", i, j, actual_num_threads[i]);
      }
    }
  }
 
  return errors;  
}
int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, set_num_teams());

  OMPVV_REPORT_AND_RETURN(errors);
}
