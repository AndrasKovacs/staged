#include <stdio.h>
#include <inttypes.h>

/*
Solution for putting vars in .text:

https://stackoverflow.com/questions/58455300/assembler-warning-with-gcc-warning-when-placing-data-in-text

*/

/* char c1 = 'a'; */
/* /\* int f1(int n) {return n + 200;} *\/ */
/* char c2 = 'b'; */
/* char c3 = 'c'; */

/* long long mynum1 __attribute__ ((section (".text#"))) = 1200000; */
/* long long mynum2 __attribute__ ((section (".text#"))) = 999999; */

typedef struct {
  int arity;
  int foobar;
} myinfo;

uint64_t fun1_info __attribute__ ((section (".text#"))) = 41;
uint64_t fun1(uint64_t x){
  return x + 100 + x;
}

uint64_t fun2_info __attribute__ ((section (".text#"))) = 42;
uint64_t fun2(uint64_t x){
  return x + 300 + x;
}

myinfo fun3_info __attribute__ ((section (".text#"))) = {.arity = 100, .foobar = 0};
uint64_t fun3(uint64_t x){
  return x + 300 + x;
}

int64_t get_info_table(void* fun){
  return *((int64_t*)(fun - 8));
  }

int main(){
  printf("%ld\n", get_info_table((void*)(&fun2)));
}
