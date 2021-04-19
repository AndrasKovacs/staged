
#include <inttypes.h>
#include <stdio.h>

register uint64_t* Hp asm("r12");



int kek(uint64_t* ptr, uint64_t x){
  *ptr = x;
  return 0;
}

uint64_t foo() {

  register uint64_t x asm("rbx") = 0;
  register uint64_t y asm("rcx") = 20;

  asm volatile
     ("addq %1, %0; addq %1, %0"
      : "=r" (y)
      : "r"  (x));

  return x;
}

uint64_t bar(uint64_t x){
  return x + 100;
}

int main(){
  printf("%ld\n", foo());
}
