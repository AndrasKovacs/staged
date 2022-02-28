
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int64_t foo(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, int64_t f, int64_t g, int64_t h,
	    int64_t i, int64_t j, int64_t k, int64_t l, int64_t m, int64_t n, int64_t o, int64_t p){
  return a * b * c * d * e * f * g * h * i * j * k * l * m * n * o * p;
}

/* int main(){ */
/*   printf("%ld\n", foo(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)); */
/*   return 0; */
/* } */


/* int64_t foo(int64_t a, int64_t b, int64_t c, int64_t d){ */
/*   return a + b + c + d; */
/* } */

/* int main(){ */
/*   printf("%ld\n", foo(1, 2, 3, 4)); */
/*   return 0; */
/* } */

/* int64_t fact(int64_t n){ */
/*   if (n == 0){ */
/*     return 1; */
/*   } else { */
/*     return n * fact(n - 1); */
/*   } */
/* } */

int64_t fact_go(int64_t n, int64_t acc){
  if (n == 0) {
    return acc;
  } else {
    int64_t x = n - 1;
    int64_t y = n * acc;
    int64_t res = fact_go(x, y);
    return res;
  }
}


/* define dso_local cc 10 i64 @fact_go(i64, i64) #0 { */
/*   %cmp = icmp eq i64 %0, 0 */
/*   br i1 %cmp, label %Finish, label %Rec */
/*   Finish: */
/*     ret i64 %1 */
/*   Rec: */
/*     %x = sub nsw i64 %0, 1 */
/*     %y = mul nsw i64 %0, %1 */
/*     %res = tail call i64 @fact_go(i64 %x, i64 %y) */
/*     ret i64 %res */
/* } */
