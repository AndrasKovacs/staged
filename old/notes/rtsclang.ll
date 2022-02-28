; ModuleID = 'rtsclang.c'
source_filename = "rtsclang.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.sigaction = type { %union.anon, %struct.__sigset_t, i32, void ()* }
%union.anon = type { void (i32)* }
%struct.__sigset_t = type { [16 x i64] }
%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, %struct._IO_codecvt*, %struct._IO_wide_data*, %struct._IO_FILE*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type opaque
%struct._IO_codecvt = type opaque
%struct._IO_wide_data = type opaque
%struct.frame = type { %struct.frame*, i64, [0 x %struct.obj] }
%struct.obj = type { i64 }
%struct.siginfo_t = type { i32, i32, i32, i32, %union.anon.0 }
%union.anon.0 = type { %struct.anon.3, [80 x i8] }
%struct.anon.3 = type { i32, i32, i32, i64, i64 }
%struct.sysinfo = type { i64, [3 x i64], i64, i64, i64, i64, i64, i64, i16, i16, i64, i64, i32, [0 x i8] }
%struct.rlimit = type { i64, i64 }
%struct.anon.11 = type { i8*, i64, %struct.obj }

@bytes_allocated = dso_local local_unnamed_addr global i64 0, align 8
@num_gc_runs = dso_local local_unnamed_addr global i64 0, align 8
@gc_sigaction = dso_local global %struct.sigaction zeroinitializer, align 8
@stderr = external dso_local local_unnamed_addr global %struct._IO_FILE*, align 8
@.str.1 = private unnamed_addr constant [55 x i8] c"handle_gc_signal caught a signal which is not SIGSEGV\0A\00", align 1
@hp_end = dso_local local_unnamed_addr global i64* null, align 8
@hp = dso_local local_unnamed_addr global i64* null, align 8
@page_size = dso_local local_unnamed_addr global i64 0, align 8
@.str.2 = private unnamed_addr constant [34 x i8] c"segfault occurred at address: %p\0A\00", align 1
@.str.3 = private unnamed_addr constant [41 x i8] c"handled gc signal, frame pointer is: %p\0A\00", align 1
@.str.5 = private unnamed_addr constant [33 x i8] c"failed to get system memory size\00", align 1
@.str.6 = private unnamed_addr constant [27 x i8] c"DEBUG: available RAM: %ld\0A\00", align 1
@.str.7 = private unnamed_addr constant [26 x i8] c"failed to set stack limit\00", align 1
@.str.8 = private unnamed_addr constant [30 x i8] c"failed to install GC handler\0A\00", align 1
@.str.10 = private unnamed_addr constant [25 x i8] c"failed to get page size\0A\00", align 1
@.str.11 = private unnamed_addr constant [23 x i8] c"DEBUG: page size: %ld\0A\00", align 1
@.str.12 = private unnamed_addr constant [31 x i8] c"DEBUG: initial heap size: %ld\0A\00", align 1
@.str.13 = private unnamed_addr constant [27 x i8] c"failed to allocate memory\0A\00", align 1
@.str.14 = private unnamed_addr constant [36 x i8] c"failed to set guard page protection\00", align 1
@hp_start = dso_local local_unnamed_addr global i64* null, align 8
@.str.16 = private unnamed_addr constant [22 x i8] c"bytes allocated: %ld\0A\00", align 1
@.str.17 = private unnamed_addr constant [24 x i8] c"number of gc runs: %ld\0A\00", align 1
@.str.18 = private unnamed_addr constant [19 x i8] c"start of heap: %p\0A\00", align 1
@.str.19 = private unnamed_addr constant [17 x i8] c"end of heap: %p\0A\00", align 1
@.str.20 = private unnamed_addr constant [27 x i8] c"size of current heap: %ld\0A\00", align 1
@.str.21 = private unnamed_addr constant [17 x i8] c"page size: %ld\0A\0A\00", align 1
@.str.22 = private unnamed_addr constant [23 x i8] c"test_stack_limit: %ld\0A\00", align 1
@.str.25 = private unnamed_addr constant [5 x i8] c"%ld\0A\00", align 1
@str = private unnamed_addr constant [17 x i8] c"TODO: perform GC\00", align 1
@str.26 = private unnamed_addr constant [24 x i8] c"DEBUG: initializing RTS\00", align 1
@str.27 = private unnamed_addr constant [28 x i8] c"DEBUG: GC handler installed\00", align 1
@str.28 = private unnamed_addr constant [10 x i8] c"\0ARTS info\00", align 1
@str.29 = private unnamed_addr constant [14 x i8] c"test_gc start\00", align 1
@str.30 = private unnamed_addr constant [12 x i8] c"test_gc end\00", align 1

; Function Attrs: noreturn nounwind
define dso_local noalias nonnull i64* @perform_gc(%struct.frame* nocapture readnone %0) local_unnamed_addr #0 {
  %2 = tail call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([17 x i8], [17 x i8]* @str, i64 0, i64 0))
  tail call void @exit(i32 0) #8
  unreachable
}

; Function Attrs: nofree nounwind
declare dso_local i32 @printf(i8* nocapture readonly, ...) local_unnamed_addr #1

; Function Attrs: noreturn nounwind
declare dso_local void @exit(i32) local_unnamed_addr #2

; Function Attrs: noreturn nounwind
define dso_local void @handle_gc_signal(i32 %0, %struct.siginfo_t* nocapture readonly %1, i8* nocapture readonly %2) #0 {
  %4 = icmp eq i32 %0, 11
  br i1 %4, label %8, label %5

5:                                                ; preds = %3
  %6 = load %struct._IO_FILE*, %struct._IO_FILE** @stderr, align 8, !tbaa !2
  %7 = tail call i64 @fwrite(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.1, i64 0, i64 0), i64 54, i64 1, %struct._IO_FILE* %6) #9
  tail call void @exit(i32 1) #8
  unreachable

8:                                                ; preds = %3
  %9 = load i64*, i64** @hp_end, align 8, !tbaa !2
  %10 = load i64*, i64** @hp, align 8, !tbaa !2
  %11 = icmp ugt i64* %9, %10
  br i1 %11, label %16, label %12

12:                                               ; preds = %8
  %13 = load i64, i64* @page_size, align 8, !tbaa !6
  %14 = getelementptr inbounds i64, i64* %9, i64 %13
  %15 = icmp ult i64* %10, %14
  br i1 %15, label %22, label %16

16:                                               ; preds = %8, %12
  %17 = load %struct._IO_FILE*, %struct._IO_FILE** @stderr, align 8, !tbaa !2
  %18 = getelementptr inbounds %struct.siginfo_t, %struct.siginfo_t* %1, i64 0, i32 4
  %19 = bitcast %union.anon.0* %18 to i8**
  %20 = load i8*, i8** %19, align 8, !tbaa !8
  %21 = tail call i32 (%struct._IO_FILE*, i8*, ...) @fprintf(%struct._IO_FILE* %17, i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.2, i64 0, i64 0), i8* %20) #9
  tail call void @exit(i32 1) #8
  unreachable

22:                                               ; preds = %12
  %23 = getelementptr inbounds i8, i8* %2, i64 104
  %24 = bitcast i8* %23 to %struct.frame**
  %25 = load %struct.frame*, %struct.frame** %24, align 8, !tbaa !9
  %26 = tail call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([41 x i8], [41 x i8]* @.str.3, i64 0, i64 0), %struct.frame* %25)
  %27 = tail call i64* @perform_gc(%struct.frame* undef)
  unreachable
}

; Function Attrs: nofree nounwind
declare dso_local i32 @fprintf(%struct._IO_FILE* nocapture, i8* nocapture readonly, ...) local_unnamed_addr #1

; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #3

; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #3

; Function Attrs: nounwind
define dso_local void @init_rts() local_unnamed_addr #4 {
  %1 = alloca %struct.sysinfo, align 8
  %2 = alloca %struct.rlimit, align 8
  %3 = tail call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([24 x i8], [24 x i8]* @str.26, i64 0, i64 0))
  %4 = bitcast %struct.sysinfo* %1 to i8*
  call void @llvm.lifetime.start.p0i8(i64 112, i8* nonnull %4) #10
  %5 = call i32 @sysinfo(%struct.sysinfo* nonnull %1) #10
  %6 = icmp eq i32 %5, -1
  br i1 %6, label %7, label %10

7:                                                ; preds = %0
  %8 = load %struct._IO_FILE*, %struct._IO_FILE** @stderr, align 8, !tbaa !2
  %9 = call i64 @fwrite(i8* getelementptr inbounds ([33 x i8], [33 x i8]* @.str.5, i64 0, i64 0), i64 32, i64 1, %struct._IO_FILE* %8) #9
  call void @exit(i32 1) #8
  unreachable

10:                                               ; preds = %0
  %11 = getelementptr inbounds %struct.sysinfo, %struct.sysinfo* %1, i64 0, i32 2
  %12 = load i64, i64* %11, align 8, !tbaa !11
  %13 = call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([27 x i8], [27 x i8]* @.str.6, i64 0, i64 0), i64 %12)
  %14 = load i64, i64* %11, align 8, !tbaa !11
  %15 = lshr i64 %14, 1
  %16 = bitcast %struct.rlimit* %2 to i8*
  call void @llvm.lifetime.start.p0i8(i64 16, i8* nonnull %16) #10
  %17 = getelementptr inbounds %struct.rlimit, %struct.rlimit* %2, i64 0, i32 0
  store i64 %15, i64* %17, align 8, !tbaa !15
  %18 = getelementptr inbounds %struct.rlimit, %struct.rlimit* %2, i64 0, i32 1
  store i64 %15, i64* %18, align 8, !tbaa !17
  %19 = call i32 @setrlimit(i32 3, %struct.rlimit* nonnull %2) #10
  %20 = icmp eq i32 %19, -1
  br i1 %20, label %21, label %24

21:                                               ; preds = %10
  %22 = load %struct._IO_FILE*, %struct._IO_FILE** @stderr, align 8, !tbaa !2
  %23 = call i64 @fwrite(i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.7, i64 0, i64 0), i64 25, i64 1, %struct._IO_FILE* %22) #9
  call void @exit(i32 1) #8
  unreachable

24:                                               ; preds = %10
  store i32 4, i32* getelementptr inbounds (%struct.sigaction, %struct.sigaction* @gc_sigaction, i64 0, i32 2), align 8, !tbaa !18
  store void (i32, %struct.siginfo_t*, i8*)* @handle_gc_signal, void (i32, %struct.siginfo_t*, i8*)** bitcast (%struct.sigaction* @gc_sigaction to void (i32, %struct.siginfo_t*, i8*)**), align 8, !tbaa !8
  %25 = call i32 @sigaction(i32 11, %struct.sigaction* nonnull @gc_sigaction, %struct.sigaction* null) #10
  %26 = icmp eq i32 %25, -1
  br i1 %26, label %27, label %30

27:                                               ; preds = %24
  %28 = load %struct._IO_FILE*, %struct._IO_FILE** @stderr, align 8, !tbaa !2
  %29 = call i64 @fwrite(i8* getelementptr inbounds ([30 x i8], [30 x i8]* @.str.8, i64 0, i64 0), i64 29, i64 1, %struct._IO_FILE* %28) #9
  call void @exit(i32 1) #8
  unreachable

30:                                               ; preds = %24
  %31 = call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([28 x i8], [28 x i8]* @str.27, i64 0, i64 0))
  %32 = call i64 @sysconf(i32 30) #10
  store i64 %32, i64* @page_size, align 8, !tbaa !6
  %33 = icmp eq i64 %32, -1
  br i1 %33, label %34, label %37

34:                                               ; preds = %30
  %35 = load %struct._IO_FILE*, %struct._IO_FILE** @stderr, align 8, !tbaa !2
  %36 = call i64 @fwrite(i8* getelementptr inbounds ([25 x i8], [25 x i8]* @.str.10, i64 0, i64 0), i64 24, i64 1, %struct._IO_FILE* %35) #9
  call void @exit(i32 1) #8
  unreachable

37:                                               ; preds = %30
  %38 = call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([23 x i8], [23 x i8]* @.str.11, i64 0, i64 0), i64 %32)
  %39 = call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([31 x i8], [31 x i8]* @.str.12, i64 0, i64 0), i64 67108864)
  %40 = load i64, i64* @page_size, align 8, !tbaa !6
  %41 = add i64 %40, 67108864
  %42 = call noalias i8* @aligned_alloc(i64 %40, i64 %41) #10
  %43 = icmp eq i8* %42, null
  br i1 %43, label %44, label %47

44:                                               ; preds = %37
  %45 = load %struct._IO_FILE*, %struct._IO_FILE** @stderr, align 8, !tbaa !2
  %46 = call i64 @fwrite(i8* getelementptr inbounds ([27 x i8], [27 x i8]* @.str.13, i64 0, i64 0), i64 26, i64 1, %struct._IO_FILE* %45) #9
  call void @exit(i32 1) #8
  unreachable

47:                                               ; preds = %37
  %48 = getelementptr i8, i8* %42, i64 67108864
  %49 = call i32 @mprotect(i8* %48, i64 %40, i32 0) #10
  %50 = icmp eq i32 %49, -1
  br i1 %50, label %51, label %54

51:                                               ; preds = %47
  %52 = load %struct._IO_FILE*, %struct._IO_FILE** @stderr, align 8, !tbaa !2
  %53 = call i64 @fwrite(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.14, i64 0, i64 0), i64 35, i64 1, %struct._IO_FILE* %52) #9
  call void @exit(i32 1) #8
  unreachable

54:                                               ; preds = %47
  store i8* %42, i8** bitcast (i64** @hp_start to i8**), align 8, !tbaa !2
  store i8* %48, i8** bitcast (i64** @hp_end to i8**), align 8, !tbaa !2
  store i8* %42, i8** bitcast (i64** @hp to i8**), align 8, !tbaa !2
  call void @llvm.lifetime.end.p0i8(i64 16, i8* nonnull %16) #10
  call void @llvm.lifetime.end.p0i8(i64 112, i8* nonnull %4) #10
  ret void
}

; Function Attrs: nounwind
declare dso_local i32 @sysinfo(%struct.sysinfo*) local_unnamed_addr #5

; Function Attrs: nounwind
declare dso_local i32 @setrlimit(i32, %struct.rlimit*) local_unnamed_addr #5

; Function Attrs: nounwind
declare dso_local i32 @sigaction(i32, %struct.sigaction*, %struct.sigaction*) local_unnamed_addr #5

; Function Attrs: nounwind
declare dso_local i64 @sysconf(i32) local_unnamed_addr #5

; Function Attrs: nofree nounwind
declare dso_local noalias i8* @aligned_alloc(i64, i64) local_unnamed_addr #1

; Function Attrs: nounwind
declare dso_local i32 @mprotect(i8*, i64, i32) local_unnamed_addr #5

; Function Attrs: nofree nounwind
define dso_local void @trace() local_unnamed_addr #6 {
  %1 = tail call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([10 x i8], [10 x i8]* @str.28, i64 0, i64 0))
  %2 = load i64, i64* @bytes_allocated, align 8, !tbaa !6
  %3 = tail call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([22 x i8], [22 x i8]* @.str.16, i64 0, i64 0), i64 %2)
  %4 = load i64, i64* @num_gc_runs, align 8, !tbaa !6
  %5 = tail call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([24 x i8], [24 x i8]* @.str.17, i64 0, i64 0), i64 %4)
  %6 = load i64*, i64** @hp_start, align 8, !tbaa !2
  %7 = tail call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([19 x i8], [19 x i8]* @.str.18, i64 0, i64 0), i64* %6)
  %8 = load i64*, i64** @hp_end, align 8, !tbaa !2
  %9 = tail call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([17 x i8], [17 x i8]* @.str.19, i64 0, i64 0), i64* %8)
  %10 = load i64, i64* bitcast (i64** @hp_end to i64*), align 8, !tbaa !2
  %11 = load i64, i64* bitcast (i64** @hp_start to i64*), align 8, !tbaa !2
  %12 = sub i64 %10, %11
  %13 = ashr exact i64 %12, 3
  %14 = tail call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([27 x i8], [27 x i8]* @.str.20, i64 0, i64 0), i64 %13)
  %15 = load i64, i64* @page_size, align 8, !tbaa !6
  %16 = tail call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([17 x i8], [17 x i8]* @.str.21, i64 0, i64 0), i64 %15)
  ret void
}

; Function Attrs: nofree nounwind
define dso_local void @test_stack_limit() local_unnamed_addr #6 {
  %1 = tail call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([23 x i8], [23 x i8]* @.str.22, i64 0, i64 0), i64 0)
  ret void
}

; Function Attrs: nounwind
define dso_local void @test_gc() local_unnamed_addr #4 {
  %1 = load i64, i64* bitcast (i64** @hp_end to i64*), align 8, !tbaa !2
  store i64 %1, i64* bitcast (i64** @hp to i64*), align 8, !tbaa !2
  %2 = tail call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([14 x i8], [14 x i8]* @str.29, i64 0, i64 0))
  %3 = load i64*, i64** @hp, align 8, !tbaa !2
  %4 = tail call i64* asm sideeffect "leaq 10($0), $0; movq $0, ($0)", "=r,{di},0,~{memory},~{dirflag},~{fpsr},~{flags}"(i32 0, i64* %3) #10, !srcloc !21
  store i64* %4, i64** @hp, align 8, !tbaa !2
  %5 = tail call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([12 x i8], [12 x i8]* @str.30, i64 0, i64 0))
  ret void
}

; Function Attrs: nounwind
define dso_local i64 @cons(i8* %0, i64 %1, i64 %2) local_unnamed_addr #4 {
  %4 = load i64*, i64** @hp, align 8, !tbaa !2
  %5 = tail call i64* asm sideeffect "leaq 16($0), $0; movq $0, ($0)", "=r,{di},0,~{memory},~{dirflag},~{fpsr},~{flags}"(i8* %0, i64* %4) #10, !srcloc !22
  store i64* %5, i64** @hp, align 8, !tbaa !2
  %6 = getelementptr inbounds i64, i64* %5, i64 -2
  store i64 %1, i64* %6, align 8, !tbaa !6
  %7 = getelementptr inbounds i64, i64* %5, i64 -1
  store i64 %2, i64* %7, align 8, !tbaa !6
  %8 = ptrtoint i64* %6 to i64
  %9 = shl i64 %8, 16
  %10 = or i64 %9, 8321
  ret i64 %10
}

; Function Attrs: nounwind
define dso_local i64 @mapsuc(i8* %0, i64 %1) local_unnamed_addr #4 {
  %3 = ashr i64 %1, 16
  %4 = and i64 %3, -8
  %5 = and i64 %1, 127
  %6 = icmp eq i64 %5, 0
  br i1 %6, label %23, label %7

7:                                                ; preds = %2
  %8 = inttoptr i64 %4 to i8*
  %9 = getelementptr inbounds i8, i8* %8, i64 8
  %10 = bitcast i8* %9 to i64*
  %11 = load i64, i64* %10, align 8
  %12 = tail call i64 @mapsuc(i8* %0, i64 %11)
  %13 = inttoptr i64 %4 to i64*
  %14 = load i64, i64* %13, align 8, !tbaa !23
  %15 = add i64 %14, 1
  %16 = load i64*, i64** @hp, align 8, !tbaa !2
  %17 = tail call i64* asm sideeffect "leaq 16($0), $0; movq $0, ($0)", "=r,{di},0,~{memory},~{dirflag},~{fpsr},~{flags}"(i8* %0, i64* %16) #10, !srcloc !22
  store i64* %17, i64** @hp, align 8, !tbaa !2
  %18 = getelementptr inbounds i64, i64* %17, i64 -2
  store i64 %15, i64* %18, align 8, !tbaa !6
  %19 = getelementptr inbounds i64, i64* %17, i64 -1
  store i64 %12, i64* %19, align 8, !tbaa !6
  %20 = ptrtoint i64* %18 to i64
  %21 = shl i64 %20, 16
  %22 = or i64 %21, 8321
  ret i64 %22

23:                                               ; preds = %2
  ret i64 0
}

; Function Attrs: nounwind
define dso_local i64 @reverse_go(i8* %0, i64 %1, i64 %2) local_unnamed_addr #4 {
  %4 = alloca %struct.anon.11, align 8
  %5 = ashr i64 %1, 16
  %6 = and i64 %5, -8
  %7 = and i64 %1, 127
  %8 = icmp eq i64 %7, 0
  br i1 %8, label %30, label %9

9:                                                ; preds = %3
  %10 = inttoptr i64 %6 to i8*
  %11 = bitcast %struct.anon.11* %4 to i8*
  ; call void @llvm.lifetime.start.p0i8(i64 24, i8* nonnull %11) #10
  %12 = getelementptr inbounds %struct.anon.11, %struct.anon.11* %4, i64 0, i32 0
  store i8* %0, i8** %12, align 8, !tbaa !26
  %13 = getelementptr inbounds %struct.anon.11, %struct.anon.11* %4, i64 0, i32 1
  %14 = getelementptr inbounds %struct.anon.11, %struct.anon.11* %4, i64 0, i32 2, i32 0
  %15 = bitcast i64* %13 to <2 x i64>*
  store <2 x i64> <i64 1, i64 0>, <2 x i64>* %15, align 8
  %16 = inttoptr i64 %6 to i64*
  %17 = load i64, i64* %16, align 8, !tbaa !23
  %18 = getelementptr inbounds i8, i8* %10, i64 8
  %19 = bitcast i8* %18 to i64*
  %20 = load i64, i64* %19, align 8, !tbaa !6
  store i64 %20, i64* %14, align 8, !tbaa !6
  %21 = load i64*, i64** @hp, align 8, !tbaa !2
  %22 = call i64* asm sideeffect "leaq 16($0), $0; movq $0, ($0)", "=r,{di},0,~{memory},~{dirflag},~{fpsr},~{flags}"(i8* nonnull %11, i64* %21) #10, !srcloc !22
  store i64* %22, i64** @hp, align 8, !tbaa !2
  %23 = getelementptr inbounds i64, i64* %22, i64 -2
  store i64 %17, i64* %23, align 8, !tbaa !6
  %24 = getelementptr inbounds i64, i64* %22, i64 -1
  store i64 %20, i64* %24, align 8, !tbaa !6
  %25 = ptrtoint i64* %23 to i64
  %26 = shl i64 %25, 16
  %27 = or i64 %26, 8321
  %28 = load i64, i64* %14, align 8
  ; call void @llvm.lifetime.end.p0i8(i64 24, i8* nonnull %11) #10
  %29 = tail call i64 @reverse_go(i8* %0, i64 %28, i64 %27)
  ret i64 %29

30:                                               ; preds = %3, %9
  %31 = phi i64 [ %29, %9 ], [ %2, %3 ]
  ret i64 %31
}

; Function Attrs: nofree nounwind
define dso_local i32 @main() local_unnamed_addr #6 {
  %1 = tail call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([5 x i8], [5 x i8]* @.str.25, i64 0, i64 0), i64 2)
  ret i32 0
}

; Function Attrs: nofree nounwind
declare i32 @puts(i8* nocapture readonly) local_unnamed_addr #7

; Function Attrs: nofree nounwind
declare i64 @fwrite(i8* nocapture, i64, i64, %struct._IO_FILE* nocapture) local_unnamed_addr #7

attributes #0 = { noreturn nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nofree nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noreturn nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { argmemonly nounwind willreturn }
attributes #4 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { nofree nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { nofree nounwind }
attributes #8 = { noreturn nounwind }
attributes #9 = { cold }
attributes #10 = { nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"Ubuntu clang version 11.0.0-2~ubuntu20.04.1"}
!2 = !{!3, !3, i64 0}
!3 = !{!"any pointer", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{!7, !7, i64 0}
!7 = !{!"long", !4, i64 0}
!8 = !{!4, !4, i64 0}
!9 = !{!10, !10, i64 0}
!10 = !{!"long long", !4, i64 0}
!11 = !{!12, !7, i64 32}
!12 = !{!"sysinfo", !7, i64 0, !4, i64 8, !7, i64 32, !7, i64 40, !7, i64 48, !7, i64 56, !7, i64 64, !7, i64 72, !13, i64 80, !13, i64 82, !7, i64 88, !7, i64 96, !14, i64 104, !4, i64 108}
!13 = !{!"short", !4, i64 0}
!14 = !{!"int", !4, i64 0}
!15 = !{!16, !7, i64 0}
!16 = !{!"rlimit", !7, i64 0, !7, i64 8}
!17 = !{!16, !7, i64 8}
!18 = !{!19, !14, i64 136}
!19 = !{!"sigaction", !4, i64 0, !20, i64 8, !14, i64 136, !3, i64 144}
!20 = !{!"", !4, i64 0}
!21 = !{i32 -2146917996}
!22 = !{i32 -2146917838}
!23 = !{!24, !7, i64 0}
!24 = !{!"", !7, i64 0, !25, i64 8}
!25 = !{!"", !7, i64 0}
!26 = !{!27, !3, i64 0}
!27 = !{!"", !3, i64 0, !7, i64 8, !25, i64 16}
