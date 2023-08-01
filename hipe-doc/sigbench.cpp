#include <assert.h>
#include <chrono>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <linux/userfaultfd.h>
#include <unistd.h>
#include <string.h>

////////////////////////////////////////////////////////////////////
// userfaultfd Write Protection Test
//
// Aditya Mandaleeka
// July 28 2020, Hackathon
//
// This is a sample program I wrote to learn more about the
// userfaultfd write protection capabilities introduced
// recently in the mainline Linux kernel.
//
// In addition to learning, I also wanted to compare how UFFD
// WP compares to using the traditional PROT_NONE+SEGV handler
// trick in order to catch dirtied memory pages. This is useful
// for specific VM/GC usecases; one example is for cross-
// generational write tracking, and another is safepoint polls.
//
// Using signal handlers for such control flow "works", but it
// has problems:
//     - Handling signals is very expensive and involves a lot
//       of work on the kernel side every time a signal is
//       raised. Handling it all in userspace can thus be a
//       performance win.
//     - Modifying access protections on memory regions involves
//       touching the VMA entries in the mm subsystem, which not
//       only involves taking some low-level locks, but also
//       leads to a proliferation of VMAs when dealing with a
//       large address region with permissions that can be dis-
//       contiguous over the span of the process's lifetime.
//       The UFFD code avoids the need to deal with VMAs entirely.
//     - Signal handlers are touchy about what's actually safe
//       to do inside them, so we have to be extra careful when
//       doing interesting things from the context of the SEGV
//       handler, often jumping through extra hoops to make it
//       safe.
//
// Note: all of this was done in an afternoon/evening as part of
// a hackathon, so the code cuts a few corners, but I believe the
// concept is sound and would like to hear if the test can be
// improved!
//
////////////////////////////////////////////////////////////////////

enum test_mode {
    M_UFFD_WP,
    M_UFFD_WP_SIGBUS,
    M_MPROTECT,
};

static const int PAGE_SIZE = 4096;
static const int PAGE_COUNT = 4096;
static const uint64_t ALLOC_SIZE = (uint64_t)PAGE_SIZE * PAGE_COUNT;

// Use a char per page for now. The wasted memory isn't really of interest
// for this experiment;
static char PAGE_TRACKER[PAGE_COUNT];

// Used for tracking the start of the desired region. Think of this like
// a heap base for GC.
// TODO: turn all the dirty tracking stuff into a class and get rid of
// these globals.
static void* REGION_BASE;

// Choose one to use for measuring elapsed time
#define USE_RDTSC 1
#define USE_CHRONO 0
static_assert(USE_RDTSC, "Must specify RDTSC for delay tracking!");

struct fault_delay {
    uint64_t worker_write;
    uint64_t uffd_receive;
    uint64_t uffd_resolve;
    uint64_t worker_finish;
};
static struct fault_delay FAULT_DELAY[PAGE_COUNT];

// Aren't they?
#define EXTRA_CHECKS_ARE_FUN 0

uint64_t rdtsc(){
    unsigned int lo,hi;
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
}

void* allocate_mem_with_mmap(size_t num_bytes)
{
    void* addr = ::mmap(0,
                  num_bytes,
                  PROT_READ|PROT_WRITE,
                  MAP_PRIVATE|MAP_ANONYMOUS,
                  0 /* fd */,
                  0 /* offset*/);
    if (addr == MAP_FAILED)
    {
        printf("ERROR: mmap failed. errno: %d\n", errno);
        exit(-1);
    }

    return addr;
}

int fill_with_pattern_seq(void* addr, char pattern, uint64_t pages,
                          bool timestamp)
{
    char* cur = (char*)addr;
    for (uint64_t i = 0; i < pages; i++)
    {
        if (timestamp)
            FAULT_DELAY[i].worker_write = rdtsc();
        *cur = pattern;
        if (timestamp)
            FAULT_DELAY[i].worker_finish = rdtsc();
        cur += PAGE_SIZE;
    }

    return 0;
}

// Protect a range with UFFD WP
int protect_range(int uffd, void* addr, uint64_t length)
{
    struct uffdio_range range = {
        (__u64)addr,
        length
        };
    struct uffdio_writeprotect wp_args = {range, UFFDIO_WRITEPROTECT_MODE_WP};
    return ioctl(uffd, UFFDIO_WRITEPROTECT, &wp_args);
}

// Protect a range with mprotect
int protect_range_with_mprotect(void* addr, uint64_t length)
{
    int ret = mprotect(addr, length, PROT_NONE);
    if (ret != 0)
    {
        printf("mprotect failed to set PROT_NONE with %d. errno: %d\n", ret, errno);
    }

    return ret;
}

void* align_to_page_boundary(void* addr)
{
    // TODO: make this better
    return (void*)((uint64_t)addr & ~(PAGE_SIZE-1));
}

// Resume without the WP mode
// TODO: Check if there is a canonical way to re-protect it so that any future
// writes to this page are also caught.
int resume_without_wp(int uffd, void* addr, uint64_t length)
{
    addr = align_to_page_boundary(addr);
    struct uffdio_range range = {
        (__u64)addr,
        length
        };
    struct uffdio_writeprotect wp_args = {range, 0};
    return ioctl(uffd, UFFDIO_WRITEPROTECT, &wp_args);
}

int resume_with_mprotect_rw(void* addr, uint64_t length)
{
    addr = align_to_page_boundary(addr);

    int ret = mprotect(addr, length, PROT_READ|PROT_WRITE);
    if (ret != 0)
    {
        printf("mprotect failed to set RW with %d. errno: %d\n", ret, errno);
    }

    return ret;
}

// Register a range of memory for use with UFFD WP
int register_range_with_wp(int uffd, void* addr, uint64_t length,
                           bool sigbus)
{
    struct uffdio_range range = {
        (__u64)addr,
        length
        };

    unsigned int ctls = 0;
    struct uffdio_register reg_args = {
        range,
        UFFDIO_REGISTER_MODE_WP,
        ctls
    };

    int ret = ioctl(uffd, UFFDIO_REGISTER, &reg_args);

    if (ret != 0)
    {
        printf("UFFDIO_REGISTER failed. errno: %d\n", errno);
        exit(-1);
    }

    return ret;
}

static int page_number_global;
static int uffd_global;

// In a real runtime, this would have to do some extra work to determine whether
// this is a "real" segfault or just the write tracking signal and handle it
// accordingly. We're skipping all that stuff here since we always assume this
// is going to be a write protection fault we caused intentionally.
void sig_handler(int code, siginfo_t *siginfo, void *context)
{
    size_t addr;
    int page_number;

    if (code == SIGSEGV) {
        addr = (size_t)siginfo->si_addr;
        page_number = (addr - (uint64_t)REGION_BASE) / PAGE_SIZE;
    } else if (code == SIGBUS) {
        addr = (uint64_t)REGION_BASE + PAGE_SIZE * page_number_global;
        page_number = page_number_global++;
    }

    // Dirty the page in our tracker
    FAULT_DELAY[page_number].uffd_receive = rdtsc();
    PAGE_TRACKER[page_number] = (char)1;

    if (code == SIGSEGV) {
        resume_with_mprotect_rw((void*)addr, PAGE_SIZE);
    } else if (code == SIGBUS) {
        resume_without_wp(uffd_global, (void*)addr, PAGE_SIZE);
    }
    FAULT_DELAY[page_number].uffd_resolve = rdtsc();
}

void register_handler(int sig)
{
    struct sigaction action;

    action.sa_flags = SA_RESTART;
    action.sa_handler = NULL;
    action.sa_sigaction = sig_handler;
    action.sa_flags |= SA_SIGINFO;
    sigemptyset(&action.sa_mask);

    int ret = sigaction(sig, &action, nullptr);
    if (ret != 0)
    {
        printf("sigaction failed with %d. errno: %d\n", ret, errno);
        exit(-1);
    }
}

#if EXTRA_CHECKS_ARE_FUN
const int SENTINEL_VALUE = 0xBABA;
#endif

// This just holds the things we need to pass to the listener proc
struct uffd_wp_info
{
    int fd;

#if EXTRA_CHECKS_ARE_FUN
    int sentinel;
#endif
};

// TODO: make this better. Using a global for now to make lifetimes simpler.
static struct uffd_wp_info g_wp_info = {0};

void* listener_proc(void* arg)
{
    uffd_wp_info* wp_info = static_cast<uffd_wp_info*>(arg);
    int uffd = wp_info->fd;

#if EXTRA_CHECKS_ARE_FUN
    if (wp_info->sentinel != SENTINEL_VALUE)
    {
        printf("ERROR: Failed sentinel check!\n");
        exit(-1);
    }
#endif

    struct pollfd evt = {
        uffd,
        POLLIN,
        0
        };

    while (poll(&evt, 1, 10) > 0)
    {
        if (evt.revents & (POLLERR | POLLHUP))
        {
            printf("ERROR: Poll Error!\n");
            exit(-1);
        }

        struct uffd_msg fault_msg = {0};
        int ret = read(uffd, &fault_msg, sizeof(fault_msg));
        if (ret != sizeof(fault_msg))
        {
            printf("ERROR: Failed to read a UFFD event! read() returned %d, errno: %d, fd: %d\n", ret, errno, uffd);
        }

        if (fault_msg.event == UFFD_EVENT_PAGEFAULT)
        {
            uint64_t fault_addr = fault_msg.arg.pagefault.address;
            // printf("Yay, UFFD caught a write at %p\n", (void*)fault_addr);

            // Dirty the page in our tracker
            int page_number = (fault_addr - (uint64_t)REGION_BASE) / (uint64_t)PAGE_SIZE;
            PAGE_TRACKER[page_number] = (char)1;

            FAULT_DELAY[page_number].uffd_receive = rdtsc();

            // Now you send ioctl(uffd, UFFDIO_WRITEPROTECT, struct *uffdio_writeprotect)
            // again while pagefault.mode does not have UFFDIO_WRITEPROTECT_MODE_WP set.
            // This wakes up the thread which will continue to run with writes. This allows
            // you to do the bookkeeping about the write in the uffd reading thread before
            // the ioctl.
            ret = resume_without_wp(uffd, (void*)fault_addr, PAGE_SIZE);
            if (ret != 0)
            {
                printf("Resume failed!: %d\n", ret);
            }
            FAULT_DELAY[page_number].uffd_resolve = rdtsc();
        }
        else
        {
            printf("Unexpected event received: %d\n", fault_msg.event);
        }
    }

    return nullptr;
}

// TODO: proper handling of ret values
int set_up_way(void* buf, uint64_t alloc_size)
{
    register_handler(SIGSEGV);
    REGION_BASE = buf;
    return protect_range_with_mprotect(buf, alloc_size);
}

// TODO: proper handling of ret values
int set_up_uffd_way(void* buf, uint64_t alloc_size, bool sigbus)
{
    REGION_BASE = buf;
    int fd = 0;
    if ((fd = syscall(SYS_userfaultfd, O_NONBLOCK)) == -1)
    {
        printf("ERROR: Initial syscall failed!\n");
        return -1;
    }

    // https://www.kernel.org/doc/Documentation/admin-guide/mm/userfaultfd.rst
    // When first opened the ``userfaultfd`` must be enabled invoking the
    // ``UFFDIO_API`` ioctl specifying a ``uffdio_api.api`` value set to ``UFFD_API`` (or
    // a later API version) which will specify the ``read/POLLIN`` protocol
    // userland intends to speak on the ``UFFD`` and the ``uffdio_api.features``
    // userland requires.
    struct uffdio_api api = {
        .api = UFFD_API,
        .features = (unsigned long long)(sigbus ? UFFD_FEATURE_SIGBUS : 0),
    };

    // The ``UFFDIO_API`` ioctl if successful (i.e. if the
    // requested ``uffdio_api.api`` is spoken also by the running kernel and the
    // requested features are going to be enabled) will return into
    // ``uffdio_api.features`` and ``uffdio_api.ioctls`` two 64bit bitmasks of
    // respectively all the available features of the read(2) protocol and
    // the generic ioctl available.
    if (ioctl(fd, UFFDIO_API, &api))
    {
        printf("ERROR: Couldn't get supported UFFD features!\n");
        return -1;
    }

    // The ``uffdio_api.features`` bitmask returned by the ``UFFDIO_API`` ioctl
    // defines what memory types are supported by the ``userfaultfd`` and what
    // events, except page fault notifications, may be generated.
    uint64_t supp_features = api.features;

    // bool supports_missing_shmem = supp_features & UFFD_FEATURE_MISSING_SHMEM;
    // bool supports_hugetlbfs     = supp_features & UFFD_FEATURE_MISSING_HUGETLBFS;
    bool supports_pagefault_wp  = supp_features & UFFD_FEATURE_PAGEFAULT_FLAG_WP;

    if (!supports_pagefault_wp)
    {
        printf("ERROR: Kernel doesn't support WP with UFFD!\n");
        exit(-1);
    }

    int ret = register_range_with_wp(fd, buf, alloc_size, sigbus);
    ret = protect_range(fd, buf, alloc_size);

    if (ret) {
        return ret;
    }

    if (sigbus)
    {
        register_handler(SIGBUS);
        REGION_BASE = buf;
        uffd_global = fd;
    }
    else
    {
        g_wp_info = {
            fd
#if EXTRA_CHECKS_ARE_FUN
            , SENTINEL_VALUE
#endif
        };

        pthread_t thread = {0};
        if (pthread_create(&thread, NULL, listener_proc, &g_wp_info))
        {
            printf("ERROR: listener thread creation failed!\n");
            exit(-1);
        }
    }

    return ret;
}

int initialize_page_tracker()
{
    for (int i = 0; i < PAGE_COUNT; i++)
    {
        PAGE_TRACKER[i] = 0;
        FAULT_DELAY[i] = { 0 };
    }

    return 0;
}

// Putting all the tests in here for now...
class Tests
{
public:
    static bool ensure_writes_succeeded(void* buf)
    {
        for (uint64_t i = 0; i < PAGE_COUNT; i++)
        {
            char* cur = (char*)buf + (PAGE_SIZE * i);
            if (*cur != (char)0xAD)
            {
                printf("Sanity test ensure_writes_succeeded failed! Buffer wasn't set correctly!\n");
                printf("i = %llu, value = %d\n", i, cur[i]);
                return false;
            }
        }
        return true;
    }

    static bool ensure_pages_dirtied(uint64_t len)
    {
        for (uint64_t i = 0; i < len; i++)
        {
            if (PAGE_TRACKER[i] != (char)1)
            {
                printf("Sanity test ensure_pages_dirtied failed! Page wasn't dirtied!\n");
                return false;
            }
        }

        return true;
    }

    static bool perform_full_write_checks(void* buf)
    {
        return ensure_writes_succeeded(buf) && ensure_pages_dirtied(PAGE_COUNT);
    }
};

uint64_t sequential_write_experiment(enum test_mode mode)
{
    void* buf = allocate_mem_with_mmap(ALLOC_SIZE);
    initialize_page_tracker();

    fill_with_pattern_seq(buf, 0xEE, PAGE_COUNT, false);

    int setup_ret;
    if (mode == M_UFFD_WP)
    {
        setup_ret = set_up_uffd_way(buf, ALLOC_SIZE, false);
    }
    else if (mode == M_MPROTECT)
    {
        setup_ret = set_up_way(buf, ALLOC_SIZE);
    }
    else if (mode == M_UFFD_WP_SIGBUS)
    {
        setup_ret = set_up_uffd_way(buf, ALLOC_SIZE, true);
    }

    if (setup_ret != 0)
    {
        printf("ERROR: setup failed!\n");
        exit(-1);
    }

#if USE_RDTSC
    uint64_t time_start = rdtsc();
#else
    auto time_start = std::chrono::high_resolution_clock::now();
#endif

    // Perform sequential writes in the protected region.
    fill_with_pattern_seq(buf, 0xAD, PAGE_COUNT, true);

#if USE_RDTSC
    uint64_t time_end = rdtsc();
    uint64_t elapsed = time_end-time_start;
#else
    auto time_end = std::chrono::high_resolution_clock::now();
    uint64_t elapsed = std::chrono::duration_cast<std::chrono::microseconds>(time_end - time_start).count();
#endif

    // Make sure it actually did what it was supposed to.
    if (!Tests::perform_full_write_checks(buf))
    {
        printf("ERROR: Sanity checks failed!");
    }

    int ret = ::munmap(buf, ALLOC_SIZE);
    if (ret != 0)
    {
        printf("ERROR: munmap Failed!\n");
    }

    return elapsed;
}

void write_data(const char *name)
{
    int i;
    FILE *file;
    char fname[128];
    struct fault_delay *p;

    snprintf(fname, 127, "result-%s.txt", name);

    file = fopen(fname, "w");
    if (!file) {
        printf("ERROR: open file %s failed\n", fname);
        return;
    }

    for (i = 0; i < PAGE_COUNT; i++) {
        p = &FAULT_DELAY[i];
        fprintf(file, "%ld, %ld, %ld\n",
                (long)p->uffd_receive - (long)p->worker_write,
                (long)p->uffd_resolve - (long)p->uffd_receive,
                (long)p->worker_finish - (long)p->uffd_resolve);
    }

    fclose(file);

    printf("Data output to file '%s'\n", fname);
}

void usage(char *name)
{
    printf("usage: %s <uffd-wp|uffd-wp-sigbus|mprotect>\n", name);
    exit(1);
}

int main(int argc, char *argv[])
{
    enum test_mode mode;
    uint64_t delay;

    if (argc == 1) {
        usage(argv[0]);
    }

    if (!strcmp(argv[1], "uffd-wp")) {
        mode = M_UFFD_WP;
        printf("Testing with uffd-wp\n");
    } else if (!strcmp(argv[1], "mprotect")) {
        mode = M_MPROTECT;
        printf("Testing with mprotect\n");
    } else if (!strcmp(argv[1], "uffd-wp-sigbus")) {
        mode = M_UFFD_WP_SIGBUS;
        printf("Testing with uffd-wp-sigbus\n");
    } else {
        usage(argv[0]);
    }

    delay = sequential_write_experiment(mode);

    printf("total used time: %ld\n", delay);

    write_data(argv[1]);
    return 0;
}
