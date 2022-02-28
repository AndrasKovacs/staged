
#include <stdint.h>

typedef struct {
  int64_t a;
  int64_t b;
  /* int64_t c; */
  /* int64_t d; */
  /* int64_t e; */
} mystruct;

mystruct fun(int64_t a, int64_t b, int64_t c){
  mystruct res = {a * b, b * c};
  return res;
}
