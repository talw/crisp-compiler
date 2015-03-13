; ModuleID = 'constants.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@TRUE_VALUE = constant i64 111, align 8
@FALSE_VALUE = constant i64 47, align 8
@NIL_VALUE = constant i64 63, align 8
@FIXNUM_TAG = constant i32 0, align 4
@FIXNUM_TAG_LEN = constant i32 2, align 4
@CHAR_TAG = constant i32 15, align 4
@CHAR_TAG_LEN = constant i32 8, align 4
@PAIR_TAG = constant i32 1, align 4
@PAIR_TAG_LEN = constant i32 3, align 4
@VECTOR_TAG = constant i32 5, align 4
@VECTOR_TAG_LEN = constant i32 3, align 4
@STRING_TAG = constant i32 6, align 4
@STRING_TAG_LEN = constant i32 3, align 4

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.1 (tags/RELEASE_34/dot1-final)"}
