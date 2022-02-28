
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

extern void unknown (uint64_t* inp);
/* __attribute__((noinline)) uint64_t foo (){ */
/*   return 0; */
/* } */

extern void unknown2 ();

uint64_t foo (uint64_t* buf){
  /* buf[1] = buf[0]; */
  /* asm volatile("nop" : : : ); */
  uint64_t foobar = *buf;
  unknown2();
  return *buf + foobar;
}
