
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>


typedef struct {
  uint64_t payload;
} obj;

typedef struct {
  uint64_t* hp;
  uint64_t* hplim;
} hp_info;

extern hp_info perform_gc(hp_info hpi, uint64_t* frame) __attribute__ ((cold));

/* int64_t foo(uint64_t* hp, uint64_t* hplim, uint64_t* frame, int64_t a, int64_t b){ */
/*   return a * b; */
/* } */

static inline uint64_t get_tag(obj obj){
  return obj.payload >> 58;
}

static inline uint64_t* to_ptr(obj obj){
  return (uint64_t*)(((int64_t)(obj.payload) << 19) >> 16);
}

static inline obj pack_obj(uint64_t tag, uint64_t* ptr){
  obj res = {((uint64_t)ptr >> 3) | (tag << 58)};
  return res;
}

/* data List = Nil | Cons I64 List */

static inline obj nil(){
  return pack_obj(0, NULL);
}

typedef struct {
  obj res;
  hp_info hpi;
} obj_res;

static inline obj_res cons(hp_info hpi, uint64_t* frame, uint64_t x, obj* xs){
  hp_info hpi2;
  if (hpi.hp + 2 >= hpi.hplim){
    hpi2 = perform_gc(hpi, frame);
  }
  *(hpi2.hp)     = x;
  *(hpi2.hp + 1) = xs->payload;
  obj_res res = {pack_obj(1, hpi2.hp), hpi2};
  return res;
}

obj_res map(hp_info hpi, uint64_t* frame, obj* xsptr){
  uint64_t  tag = get_tag(*xsptr);
  if (tag == 0){
    obj_res res = {nil(), hpi};
    return res;
  } else {
    uint64_t* ptr = to_ptr(*xsptr);
    uint64_t head = *ptr;
    obj tail      = {*(ptr + 1)};
    obj_res res1  = map(hpi, frame, &tail);
    obj_res res2  = cons(res1.hpi, frame, head, &(res1.res));
    obj_res res3  = cons(res2.hpi, frame, 100, &(res2.res));
    return res3;
  }
}
