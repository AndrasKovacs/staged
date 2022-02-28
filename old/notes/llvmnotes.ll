; ModuleID = 'llvmnotes.c'
source_filename = "llvmnotes.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define dso_local cc 10 i64 @bar(i64, i64) #0 {
  %res = mul nsw i64 %0, %1
  ret i64 %res
}


define dso_local cc 10 i64 @foo(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) #0 {
  %17 = mul nsw i64 %1, %0
  %18 = mul nsw i64 %17, %2
  %19 = mul nsw i64 %18, %3
  %20 = mul nsw i64 %19, %4
  %21 = mul nsw i64 %20, %5
  %22 = mul nsw i64 %21, %6
  %23 = mul nsw i64 %22, %7
  %24 = mul nsw i64 %23, %8
  %25 = mul nsw i64 %24, %9
  %26 = mul nsw i64 %25, %10
  %27 = mul nsw i64 %26, %11
  %28 = mul nsw i64 %27, %12
  %29 = mul nsw i64 %28, %13
  %30 = mul nsw i64 %29, %14
  %31 = mul nsw i64 %30, %15
  ret i64 %31
}

; Function Attrs: nounwind readnone uwtable
define dso_local i64 @fact_go(i64, i64) local_unnamed_addr #1 {
  %3 = icmp eq i64 %0, 0
  br i1 %3, label %10, label %4

; <label>:4:                                      ; preds = %2, %4
  %5 = phi i64 [ %8, %4 ], [ %1, %2 ]
  %6 = phi i64 [ %7, %4 ], [ %0, %2 ]
  %7 = add nsw i64 %6, -1
  %8 = mul nsw i64 %5, %6
  %9 = icmp eq i64 %7, 0
  br i1 %9, label %10, label %4

; <label>:10:                                     ; preds = %4, %2
  %11 = phi i64 [ %1, %2 ], [ %8, %4 ]
  ret i64 %11
}

define i64 @fuckghc(i64, i64) {
  %res = mul nsw i64 %0, %1
  ret i64 %res
}

define i64 @fuckghc2(i64, i64) {
  %res1 = call i64 @fuckghc(i64 %0, i64 %1)
  %res2 = mul nsw i64 %res1, %0
  ret i64 %res2
}

attributes #0 = { norecurse nounwind  }
attributes #1 = { nounwind readnone  }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 7.1.0 (tags/RELEASE_710/final)"}
