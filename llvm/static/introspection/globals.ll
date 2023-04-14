; ModuleID = 'globals.c'
source_filename = "globals.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

%struct.__introspection_global_entry_t = type { ptr, i64, i32, ptr }
%struct.__introspection_module_entry_t = type { ptr, i64, ptr }

@entries = dso_local global %struct.__introspection_global_entry_t zeroinitializer, align 8
@module = dso_local global %struct.__introspection_module_entry_t { ptr null, i64 0, ptr @entries }, align 8

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{i32 7, !"frame-pointer", i32 1}
!5 = !{!"Ubuntu clang version 15.0.7"}
