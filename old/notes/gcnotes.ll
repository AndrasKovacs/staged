; ModuleID = 'gcnotes.c'
source_filename = "gcnotes.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.obj_res = type { %struct.obj, %struct.hp_info }
%struct.obj = type { i64 }
%struct.hp_info = type { i64*, i64* }

; Function Attrs: nounwind
define dso_local cc 10 void @map(%struct.obj_res* noalias nocapture sret, i64*, i64*, i64*, %struct.obj* nocapture readonly) local_unnamed_addr #0 {
  %6 = alloca %struct.obj, align 8
  %7 = alloca %struct.obj_res, align 8
  %8 = getelementptr inbounds %struct.obj, %struct.obj* %4, i64 0, i32 0
  %9 = load i64, i64* %8, align 8
  %10 = icmp ult i64 %9, 288230376151711744
  br i1 %10, label %11, label %15

; <label>:11:                                     ; preds = %5
  %12 = getelementptr inbounds %struct.obj_res, %struct.obj_res* %0, i64 0, i32 0, i32 0
  store i64 0, i64* %12, align 8
  %13 = getelementptr inbounds %struct.obj_res, %struct.obj_res* %0, i64 0, i32 1, i32 0
  store i64* %1, i64** %13, align 8
  %14 = getelementptr inbounds %struct.obj_res, %struct.obj_res* %0, i64 0, i32 1, i32 1
  store i64* %2, i64** %14, align 8
  br label %60

; <label>:15:                                     ; preds = %5
  %16 = shl i64 %9, 19
  %17 = ashr exact i64 %16, 16
  %18 = inttoptr i64 %17 to i64*
  %19 = load i64, i64* %18, align 8, !tbaa !2
  %20 = bitcast %struct.obj* %6 to i8*
  call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %20) #3
  %21 = getelementptr inbounds %struct.obj, %struct.obj* %6, i64 0, i32 0
  %22 = getelementptr inbounds i64, i64* %18, i64 1
  %23 = load i64, i64* %22, align 8, !tbaa !2
  store i64 %23, i64* %21, align 8, !tbaa !6
  %24 = bitcast %struct.obj_res* %7 to i8*
  call void @llvm.lifetime.start.p0i8(i64 24, i8* nonnull %24) #3
  call void @map(%struct.obj_res* nonnull sret %7, i64* %1, i64* %2, i64* %3, %struct.obj* nonnull %6)
  %25 = getelementptr inbounds %struct.obj_res, %struct.obj_res* %7, i64 0, i32 1, i32 0
  %26 = load i64*, i64** %25, align 8
  %27 = getelementptr inbounds %struct.obj_res, %struct.obj_res* %7, i64 0, i32 1, i32 1
  %28 = load i64*, i64** %27, align 8
  %29 = getelementptr inbounds i64, i64* %26, i64 2
  %30 = icmp ult i64* %29, %28
  br i1 %30, label %35, label %31

; <label>:31:                                     ; preds = %15
  %32 = tail call { i64*, i64* } @perform_gc(i64* %26, i64* %28, i64* %3) #4, !noalias !8
  %33 = extractvalue { i64*, i64* } %32, 0
  %34 = extractvalue { i64*, i64* } %32, 1
  br label %35

; <label>:35:                                     ; preds = %15, %31
  %36 = phi i64* [ %33, %31 ], [ undef, %15 ]
  %37 = phi i64* [ %34, %31 ], [ undef, %15 ]
  store i64 %19, i64* %36, align 8, !tbaa !2, !noalias !8
  %38 = getelementptr inbounds %struct.obj_res, %struct.obj_res* %7, i64 0, i32 0, i32 0
  %39 = load i64, i64* %38, align 8, !tbaa !6, !noalias !8
  %40 = getelementptr inbounds i64, i64* %36, i64 1
  store i64 %39, i64* %40, align 8, !tbaa !2, !noalias !8
  %41 = ptrtoint i64* %36 to i64
  %42 = lshr i64 %41, 3
  %43 = or i64 %42, 288230376151711744
  %44 = getelementptr inbounds i64, i64* %36, i64 2
  %45 = icmp ult i64* %44, %37
  br i1 %45, label %50, label %46

; <label>:46:                                     ; preds = %35
  %47 = tail call { i64*, i64* } @perform_gc(i64* %36, i64* %37, i64* %3) #4, !noalias !11
  %48 = extractvalue { i64*, i64* } %47, 0
  %49 = extractvalue { i64*, i64* } %47, 1
  br label %50

; <label>:50:                                     ; preds = %35, %46
  %51 = phi i64* [ %48, %46 ], [ undef, %35 ]
  %52 = phi i64* [ %49, %46 ], [ undef, %35 ]
  store i64 100, i64* %51, align 8, !tbaa !2, !noalias !11
  %53 = getelementptr inbounds i64, i64* %51, i64 1
  store i64 %43, i64* %53, align 8, !tbaa !2, !noalias !11
  %54 = ptrtoint i64* %51 to i64
  %55 = lshr i64 %54, 3
  %56 = or i64 %55, 288230376151711744
  %57 = getelementptr inbounds %struct.obj_res, %struct.obj_res* %0, i64 0, i32 0, i32 0
  store i64 %56, i64* %57, align 8, !alias.scope !11
  %58 = getelementptr inbounds %struct.obj_res, %struct.obj_res* %0, i64 0, i32 1, i32 0
  store i64* %51, i64** %58, align 8, !alias.scope !11
  %59 = getelementptr inbounds %struct.obj_res, %struct.obj_res* %0, i64 0, i32 1, i32 1
  store i64* %52, i64** %59, align 8, !alias.scope !11
  call void @llvm.lifetime.end.p0i8(i64 24, i8* nonnull %24) #3
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %20) #3
  br label %60

; <label>:60:                                     ; preds = %50, %11
  ret void
}

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.start.p0i8(i64, i8* nocapture) #1

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.end.p0i8(i64, i8* nocapture) #1

; Function Attrs: cold
declare dso_local { i64*, i64* } @perform_gc(i64*, i64*, i64*) local_unnamed_addr #2

attributes #0 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind }
attributes #2 = { cold "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }
attributes #4 = { cold nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 7.1.0 (tags/RELEASE_710/final)"}
!2 = !{!3, !3, i64 0}
!3 = !{!"long", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{!7, !3, i64 0}
!7 = !{!"", !3, i64 0}
!8 = !{!9}
!9 = distinct !{!9, !10, !"cons: argument 0"}
!10 = distinct !{!10, !"cons"}
!11 = !{!12}
!12 = distinct !{!12, !13, !"cons: argument 0"}
!13 = distinct !{!13, !"cons"}
