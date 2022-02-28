; ModuleID = 'structret.c'
source_filename = "structret.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"


; Function Attrs: norecurse nounwind readnone uwtable
define private cc 10 { i64, i64, i64 } @fun3(i64, i64, i64) local_unnamed_addr #0 {
  %r1 = insertvalue { i64, i64, i64 } undef, i64 %0, 0
  %r2 = insertvalue { i64, i64, i64 } %r1  , i64 %1, 1
  %r3 = insertvalue { i64, i64, i64 } %r2  , i64 %2, 2
  ret { i64, i64, i64 } %r3
}

; Function Attrs: norecurse nounwind readnone uwtable
define private cc 10 { i64, i64, i64, i64 } @fun(i64, i64, i64, i64) local_unnamed_addr #0 {
  %r1 = insertvalue { i64, i64, i64, i64 } undef, i64 %0, 0
  %r2 = insertvalue { i64, i64, i64, i64 } %r1  , i64 %1, 1
  %r3 = insertvalue { i64, i64, i64, i64 } %r2  , i64 %2, 2
  %r4 = insertvalue { i64, i64, i64, i64 } %r3  , i64 %3, 3
  ret { i64, i64, i64, i64 } %r4
}


attributes #0 = { norecurse nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 7.1.0 (tags/RELEASE_710/final)"}
