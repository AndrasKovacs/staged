
/* -O0 -Wall -fverbose-asm -fno-asynchronous-unwind-tables -fno-stack-protector -foptimize-sibling-calls -fno-plt -fomit-frame-pointer -finline-small-functions -fstrict-aliasing */

#define _GNU_SOURCE
#include <errno.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/sysinfo.h>
/* #include <time.h> */
#include <unistd.h>

// shorthands
/* -------------------------------------------------------------------------------- */

// stringify a macro
#define STR1(x) #x
#define STR(x) STR1(x)

#define INLINE static inline __attribute__((always_inline))

typedef uint64_t u64;
typedef int64_t  i64;

#define PACKED_STRUCT struct __attribute__ ((__packed__))


// heap objects
/* -------------------------------------------------------------------------------- */

// layout:
// pointer: 45 | num_words: 6 | num_objs: 6 | tag: 7

typedef struct {
  u64 out;
} obj;

INLINE void* get_ptr(obj x)  {
  return (void*)((((i64) x.out) >> 16) & (-8));
}

INLINE u64 get_tag(obj x) {
  return x.out & 127;
}

INLINE u64 get_num_words(obj x){
  return (x.out >> 13) & 63;
}

INLINE u64 get_num_objs(obj x){
  return (x.out >> 7) & 63;
}

INLINE obj pack_obj(void* ptr, u64 nwords, u64 nobjs, u64 tag){
  obj res = {(((u64) ptr) << 16) | (nwords << 13) | (nobjs << 7) | tag};
  return res;
}


/* -------------------------------------------------------------------------------- */


// A generic stack frame.
struct frame;
typedef
struct __attribute__ ((__packed__)) frame {
  struct frame* prev;     // can be null
  uint64_t num_objs;
  obj objs[0];
} frame;


// RTS constants and state
/* -------------------------------------------------------------------------------- */

#define HP_REG_ASM      r12       // heap pointer register in asm notation
#define HP_REG_GREGS    REG_R12   // "gregs" name of the heap register
#define HP_CHECK_INSTR  movq      // heap check instruction

static const u64 hp_check_instr_size = 1; // heap check instruction size in bytes

// GC stats
u64 bytes_allocated = 0;
u64 num_gc_runs = 0;

// Pointer to the beginning of the heap
u64* hp_start;

// Pointer to the end of the heap,
// which is also the start of the write-protected guard page.
u64* hp_end;

// Pointer to first free word of the heap.
register u64* hp asm(STR(HP_REG_ASM));

// SIGSEGV handler for GC events
struct sigaction gc_sigaction = {0};

// native page size
u64 page_size;


/* -------------------------------------------------------------------------------- */

// TODO
u64* perform_gc(frame* frame_ptr){
  printf("TODO: perform GC\n");
  exit(EXIT_SUCCESS);
}

// Handle a segfault arising from heap overflow, by running garbage collection.
// If the segfault did not actually arise from heap overflow, we abort the program.
void handle_gc_signal(int nSignum, siginfo_t* si, void* vcontext) {

  // handle_gc_signal should be only set to catch SIGSEGV
  if (nSignum != SIGSEGV){
    fprintf(stderr, "handle_gc_signal caught a signal which is not SIGSEGV\n");
    exit(EXIT_FAILURE);
  }

  // Check that the heap pointer actually points to the guard page.
  if (!(hp_end <= hp && hp < (hp_end + page_size))) {
    fprintf(stderr, "segfault occurred at address: %p\n", si->si_addr);
    exit(EXIT_FAILURE);
  }

  ucontext_t* context = (ucontext_t*)vcontext;
  frame* frame_ptr    = (frame*) context->uc_mcontext.gregs[REG_RDI];

  printf("handled gc signal, frame pointer is: %p\n", frame_ptr);

  // This updates the top level hp_start, hp_end, and hp
  perform_gc(frame_ptr);

  // We have to update the context with the new hp, otherwise execution would
  // resume with the old value.
  context->uc_mcontext.gregs[HP_REG_GREGS] = (long long int) hp;

  // We skip the faulting instruction.
  context->uc_mcontext.gregs[REG_RIP] += hp_check_instr_size;
}

void init_rts(){
  printf("DEBUG: initializing RTS\n");

  // get available RAM
  struct sysinfo si;
  if (sysinfo(&si) == -1) {
    fprintf(stderr, "failed to get system memory size");
    exit(EXIT_FAILURE);
  }

  printf("DEBUG: available RAM: %ld\n", si.totalram);

  // set stack size to half of physical memory
  u64 stack_limit = si.totalram / 2;
  struct rlimit rlim = {stack_limit, stack_limit};
  if (setrlimit(RLIMIT_STACK, &rlim) == -1){
    fprintf(stderr, "failed to set stack limit");
    exit(EXIT_FAILURE);
  }

  // install GC handler
  gc_sigaction.sa_flags     = SA_SIGINFO;
  gc_sigaction.sa_sigaction = handle_gc_signal;

  if (sigaction(SIGSEGV, &gc_sigaction, NULL) == -1){
    fprintf(stderr, "failed to install GC handler\n");
    exit(EXIT_FAILURE);
  }

  printf("DEBUG: GC handler installed\n");

  // get page size
  page_size = sysconf(_SC_PAGESIZE);
  if (page_size == -1) {
    fprintf(stderr, "failed to get page size\n");
    exit(EXIT_FAILURE);
  }

  printf("DEBUG: page size: %ld\n", page_size);

  // allocate initial heap (TODO: GC, factor this code out)
  /* ------------------------------------------------------------ */

  u64 heap_size = 1 << 26; // 64 MB

  printf("DEBUG: initial heap size: %ld\n", heap_size);

  void* start = aligned_alloc(page_size, heap_size + page_size);

  if (start == 0) {
    fprintf(stderr, "failed to allocate memory\n");
    exit(EXIT_FAILURE);
  }

  void* end = start + heap_size;

  // protect guard page
  if (mprotect(end, page_size, PROT_NONE) == -1){
    fprintf(stderr, "failed to set guard page protection");
    exit(EXIT_FAILURE);
  }

  hp_start = start;
  hp_end = end;
  hp = start;
}

// Allocation
/* -------------------------------------------------------------------------------- */

// Allocate n bytes on the heap. "n" must be smaller than page_size! Otherwise we go beyond the guard page.
// TODO: For larger "n", we need another function which checks hp_end.
#define HP_ALLOC(frame, n)						\
  asm inline volatile("leaq "STR(n)"(%0), %0; movq %0, (%0)":"+r"(hp):"D"(frame):"memory")

/* typedef struct { */
/*   frame* prev; */
/*   u64 num_objs; */
/*   obj objs[2]; */
/* } frame2; */

/* // Allocate a stack frame for "n" number of objects. */
/* #define FRAME_ALLOC2(fr) ({\ */
/*   frame2 new_fr = {fr, 2};\ */
/*   (frame*) &new_fr;}) */

/* // Allocate a stack frame for "n" number of objects. */
/* #define FRAME_ALLOC(fr, n) ({\ */
/*   frame* new_fr = alloca((n+2)*sizeof(u64));\ */
/*   new_fr->prev = fr;\ */
/*   new_fr->num_objs = n;\ */
/*   memset(new_fr->objs, 0, n*sizeof(u64));\ */
/*   new_fr;}) */

/* #define FRAME_ALLOC(fr, n) ({\ */
/*   struct {frame* prev; u64 num_objs; obj objs[n];} new_fr = {(frame*) fr, n};\ */
/*   &new_fr;}) */


// Utility
/* -------------------------------------------------------------------------------- */

void trace(){
  printf("\nRTS info\n");
  printf("bytes allocated: %ld\n", bytes_allocated);
  printf("number of gc runs: %ld\n", num_gc_runs);
  printf("start of heap: %p\n", hp_start);
  printf("end of heap: %p\n", hp_end);
  printf("size of current heap: %ld\n", (hp_end - hp_start));
  printf("page size: %ld\n\n", page_size);
}

// testing stuff
/* ------------------------------------------------------------ */

// this fails if stack limit is not set
void test_stack_limit(){
  u64 stack_test[10000000] = {0};
  printf("test_stack_limit: %ld\n", stack_test[10]);
}

void test_gc(){
  hp = hp_end;
  printf("test_gc start\n");
  HP_ALLOC(0, 10);
  printf("test_gc end\n");
}


/* -------------------------------------------------------------------------------- */

INLINE obj nil(){
  return pack_obj(0, 0, 0, 0);
}

obj cons(void* frame, u64 n, obj ns){
  HP_ALLOC(frame, 16);
  *(hp-2) = n;
  *(hp-1) = ns.out;
  return pack_obj(hp-2, 1, 1, 1);
}

obj mapsuc(void* frame, obj ns){
  void* ns_ptr = get_ptr(ns);
  u64  ns_tag = get_tag(ns);
  if (ns_tag == 0) {
    return nil();
  } else {
    PACKED_STRUCT {u64 head; obj tail;}* ns_cons = ns_ptr;
    obj ns2 = mapsuc(frame, ns_cons->tail);
    return cons(frame, ns_cons->head + 1, ns2);
  }
}

obj reverse_go(void* frame, obj ns, obj acc){
  void* ns_ptr = get_ptr(ns);
  u64  ns_tag = get_tag(ns);
  if (ns_tag == 0){
    return acc;
  } else {
    PACKED_STRUCT {u64 head; obj tail;}* ns_cons = ns_ptr;
    PACKED_STRUCT {void* prev; u64 num_objs; obj ns_tail;} frame2 = {frame, 1};
    u64 head = ns_cons->head;
    frame2.ns_tail = ns_cons->tail;
    obj acc2 = cons(&frame2, head, frame2.ns_tail);
    return reverse_go(frame, frame2.ns_tail, acc2);
  }
}


/* -------------------------------------------------------------------------------- */

int main() {
  PACKED_STRUCT {void* prev; u64 num_objs; obj x; obj y;} frame = {0, 2};
  printf("%ld\n", frame.num_objs);


  /* /\* frame* fr = 0; *\/ */
  /* init_rts(); */
  /* hp = hp_end; */
  /* HP_ALLOC(0, 20); */
  /* HP_ALLOC(0, 30); */
  /* printf("testtt\n"); */
  /* /\* test_gc(); *\/ */
  /* /\* hp = hp_end; *\/ */
  /* /\* HP_ALLOC(10, fr); *\/ */
  /* /\* printf("hp : %p\n", hp); *\/ */
  /* /\* init_rts(); *\/ */
  /* /\* test_stack_limit(); *\/ */
  /* /\* test_gc(); *\/ */
  /* /\* return 0; *\/ */
}
