
#include <stdint.h>

void foo(uint64_t f) {
	__attribute__ ((musttail)) return ((void (*)(uint64_t))f)(100);
}

int main(){return 0;}
