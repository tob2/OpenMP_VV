//===--- test_target_teams_distribute_default_none.c-------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This tests uses the default(none) clause on a target teams distribute test.
// The test aims to validate that all values will not have default data sharing
// attributes.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define SIZE_THRESHOLD 512

int main() {
  int isOffloading = 0;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  OMPVV_WARNING("Test only uses default(none) clause and does not guarantee that the default(none) is enforced.");
  int a[1024];
  int b[1024];
  int c[1024];
  int d[1024];
  int privatized;
  int share = 0;
  int x;
  int errors = 0;

  for (int x = 0; x < 1024; ++x) {
      a[x] = 1;
      b[x] = x;
      c[x] = 2*x;
      d[x] = 0;
  }

  #pragma omp target data map(from: d[0:1024]) map(to: a[0:1024], b[0:1024], c[0:1024])
  {
      #pragma omp target teams distribute default(none) shared(a, b, c, d) private(x, privatized)
      for (x = 0; x < 1024; ++x){
          privatized = 0;
          for (int y = 0; y < a[x] + b[x]; ++y){
              privatized++;
          }
          d[x] = c[x] * privatized;
      }
  }

  for (x = 0; x < 1024; ++x){
      OMPVV_TEST_AND_SET_VERBOSE(errors, (d[x] != (1 + x)*2*x));
      if (d[x] != (1 + x)*2*x){
          break;
      }
  }

  #pragma omp target data map(to: b[0:1024]) map(tofrom: share)
  {
      #pragma omp target teams distribute default(none) private(x) shared(share, b)
      for (x = 0; x < 1024; ++x){
          #pragma omp atomic
          share = share + b[x];
      }
  }
  for (int x = 0; x < 1024; ++x){
      share = share - x;
  }
  OMPVV_TEST_AND_SET_VERBOSE(errors, (share != 0));

  OMPVV_REPORT_AND_RETURN(errors);
}
