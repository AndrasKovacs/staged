
/* -fno-unwind-tables -fno-exceptions -fno-stack-protector -fomit-frame-pointer */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define INLINE static inline __attribute__((always_inline))

// heap objects
/* -------------------------------------------------------------------------------- */

// layout:
// pointer: 45 | num_words: 6 | num_objs: 6 | tag: 7

typedef struct {
  uint64_t out;
} obj;

INLINE obj* get_ptr(obj x)  {
  return (obj*)((((int64_t) x.out) >> 16) & (-8));
}

INLINE uint64_t get_tag(obj x) {
  return x.out & 127;
}

INLINE uint64_t get_num_words(obj x){
  return (x.out >> 13) & 63;
}

INLINE uint64_t get_num_objs(obj x){
  return (x.out >> 7) & 63;
}

INLINE obj pack_obj(obj* ptr, uint64_t nwords, uint64_t nobjs, uint64_t tag){
  obj res = {(((uint64_t) ptr) << 16) | (nwords << 13) | (nobjs << 7) | tag};
  return res;
}

/* -------------------------------------------------------------------------------- */

// A generic stack frame.

struct frame;
typedef struct __attribute__ ((__packed__)) frame {
  struct frame* prev; // can be null
  uint64_t num_objs;
  obj objs[0];
} frame;

// A word on the heap. Either an object or unboxed data.
typedef union {
  uint64_t word;
  obj obj;
} hp_word;

register frame*   fr     asm ("r13");   // frame pointer
register hp_word* hp     asm ("r14");   // heap pointer       (start of free area)
register hp_word* hp_lim asm ("r15");   // heap limit pointer (end of free area)

void perform_gc() __attribute__ ((cold));

INLINE hp_word* hp_alloc(uint64_t size){
  hp_word* res = hp;
  hp += size;
  if (hp >= hp_lim) perform_gc();
  return res;
}

#define FR_PUSH(size) ({\
  uint64_t* new_fr = alloca((size+2)*sizeof(uint64_t));	\
  new_fr[0] = (uint64_t) fr;\
  new_fr[1] = (uint64_t) size;\
  fr = (frame*)new_fr;\
  memset(new_fr + 2, 0, size*sizeof(uint64_t));\
  (frame*)new_fr;})

INLINE void fr_pop(frame*

int test(){
  frame* f1 = FR_ALLOC(2);
  return 20;
}


/* -------------------------------------------------------------------------------- */


int main(){
  int foo = 20;
  fr = NULL;
  printf("%d\n", foo);
}
