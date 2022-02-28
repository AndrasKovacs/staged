
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct {
  uint8_t  tag     ;  // 6
  uint8_t  objtype ;  // 3
  uint8_t  nobjs   ;  // 5
  uint8_t  nwords  ;  // 5
  uint64_t ptr     ;  // 45
} unpacked_obj;

typedef struct {
  uint64_t payload;
} packed_obj;

static inline packed_obj pack_obj(unpacked_obj obj){
  packed_obj res = {
       (((uint64_t)obj.tag)     << 58)
     | (((uint64_t)obj.objtype) << 55)
     | (((uint64_t)obj.nobjs)   << 50)
     | (((uint64_t)obj.nwords)  << 45)
     | obj.ptr};
  return res;
}

static inline void* to_ptr(packed_obj obj){
  return (void*)(((int64_t)(obj.payload) << 19) >> 16);
}

static inline uint8_t get_tag(packed_obj obj){
  return (uint8_t)((obj.payload) >> 58);
}

static inline uint64_t compr_ptr(void* ptr){
  return (((uint64_t) ptr) << 16) >> 19;
}

int main() {
  int* ptr = malloc(sizeof(int));
  *ptr = 100;
  unpacked_obj test = {10, 0, 0, 0, compr_ptr(ptr)};
  printf("%d\n", *((int*)(to_ptr(pack_obj(test)))));
  return 0;
}
