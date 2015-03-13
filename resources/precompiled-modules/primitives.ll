; ModuleID = 'primitives.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@FALSE_VALUE = common global i64 0, align 8
@TRUE_VALUE = common global i64 0, align 8
@NIL_VALUE = common global i64 0, align 8
@CHAR_TAG = common global i32 0, align 4
@CHAR_TAG_LEN = common global i32 0, align 4
@FIXNUM_TAG = common global i32 0, align 4
@FIXNUM_TAG_LEN = common global i32 0, align 4
@PAIR_TAG = common global i32 0, align 4
@PAIR_TAG_LEN = common global i32 0, align 4
@VECTOR_TAG = common global i32 0, align 4
@VECTOR_TAG_LEN = common global i32 0, align 4
@STRING_TAG = common global i32 0, align 4
@STRING_TAG_LEN = common global i32 0, align 4

; Function Attrs: nounwind uwtable
define i64 @isBoolean(i64 %val) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %val, i64* %2, align 8
  %3 = load i64* %2, align 8
  %4 = load i64* @FALSE_VALUE, align 8
  %5 = icmp eq i64 %3, %4
  br i1 %5, label %10, label %6

; <label>:6                                       ; preds = %0
  %7 = load i64* %2, align 8
  %8 = load i64* @TRUE_VALUE, align 8
  %9 = icmp eq i64 %7, %8
  br i1 %9, label %10, label %12

; <label>:10                                      ; preds = %6, %0
  %11 = load i64* @TRUE_VALUE, align 8
  store i64 %11, i64* %1
  br label %14

; <label>:12                                      ; preds = %6
  %13 = load i64* @FALSE_VALUE, align 8
  store i64 %13, i64* %1
  br label %14

; <label>:14                                      ; preds = %12, %10
  %15 = load i64* %1
  ret i64 %15
}

; Function Attrs: nounwind uwtable
define i64 @isNull(i64 %val) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %val, i64* %2, align 8
  %3 = load i64* %2, align 8
  %4 = load i64* @NIL_VALUE, align 8
  %5 = icmp eq i64 %3, %4
  br i1 %5, label %6, label %8

; <label>:6                                       ; preds = %0
  %7 = load i64* @TRUE_VALUE, align 8
  store i64 %7, i64* %1
  br label %10

; <label>:8                                       ; preds = %0
  %9 = load i64* @FALSE_VALUE, align 8
  store i64 %9, i64* %1
  br label %10

; <label>:10                                      ; preds = %8, %6
  %11 = load i64* %1
  ret i64 %11
}

; Function Attrs: nounwind uwtable
define i64 @isChar(i64 %val) #0 {
  %1 = alloca i64, align 8
  store i64 %val, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = load i32* @CHAR_TAG, align 4
  %4 = load i32* @CHAR_TAG_LEN, align 4
  %5 = call i64 @isTag(i64 %2, i32 %3, i32 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define internal i64 @isTag(i64 %val, i32 %tag, i32 %tagLen) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %mask = alloca i32, align 4
  store i64 %val, i64* %2, align 8
  store i32 %tag, i32* %3, align 4
  store i32 %tagLen, i32* %4, align 4
  %5 = load i32* %4, align 4
  %6 = uitofp i32 %5 to double
  %7 = call double @pow(double 2.000000e+00, double %6) #2
  %8 = fsub double %7, 1.000000e+00
  %9 = fptoui double %8 to i32
  store i32 %9, i32* %mask, align 4
  %10 = load i64* %2, align 8
  %11 = load i32* %mask, align 4
  %12 = zext i32 %11 to i64
  %13 = and i64 %10, %12
  %14 = load i32* %3, align 4
  %15 = zext i32 %14 to i64
  %16 = icmp eq i64 %13, %15
  br i1 %16, label %17, label %19

; <label>:17                                      ; preds = %0
  %18 = load i64* @TRUE_VALUE, align 8
  store i64 %18, i64* %1
  br label %21

; <label>:19                                      ; preds = %0
  %20 = load i64* @FALSE_VALUE, align 8
  store i64 %20, i64* %1
  br label %21

; <label>:21                                      ; preds = %19, %17
  %22 = load i64* %1
  ret i64 %22
}

; Function Attrs: nounwind uwtable
define i64 @isNumber(i64 %val) #0 {
  %1 = alloca i64, align 8
  store i64 %val, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = load i32* @FIXNUM_TAG, align 4
  %4 = load i32* @FIXNUM_TAG_LEN, align 4
  %5 = call i64 @isTag(i64 %2, i32 %3, i32 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @isPair(i64 %val) #0 {
  %1 = alloca i64, align 8
  store i64 %val, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = load i32* @PAIR_TAG, align 4
  %4 = load i32* @PAIR_TAG_LEN, align 4
  %5 = call i64 @isTag(i64 %2, i32 %3, i32 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @cons(i64 %elem1, i64 %elem2) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %ptr = alloca i64*, align 8
  store i64 %elem1, i64* %1, align 8
  store i64 %elem2, i64* %2, align 8
  %3 = call noalias i8* @memalign(i64 1, i64 16) #2
  %4 = bitcast i8* %3 to i64*
  store i64* %4, i64** %ptr, align 8
  %5 = load i64* %1, align 8
  %6 = load i64** %ptr, align 8
  store i64 %5, i64* %6, align 8
  %7 = load i64* %2, align 8
  %8 = load i64** %ptr, align 8
  %9 = getelementptr inbounds i64* %8, i64 1
  store i64 %7, i64* %9, align 8
  %10 = load i64** %ptr, align 8
  %11 = ptrtoint i64* %10 to i64
  %12 = load i32* @PAIR_TAG, align 4
  %13 = sext i32 %12 to i64
  %14 = add i64 %11, %13
  ret i64 %14
}

; Function Attrs: nounwind
declare noalias i8* @memalign(i64, i64) #1

; Function Attrs: nounwind uwtable
define i64 @car(i64 %val) #0 {
  %1 = alloca i64, align 8
  %ptr = alloca i64*, align 8
  store i64 %val, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = load i32* @PAIR_TAG, align 4
  %4 = sext i32 %3 to i64
  %5 = sub i64 %2, %4
  %6 = inttoptr i64 %5 to i64*
  store i64* %6, i64** %ptr, align 8
  %7 = load i64** %ptr, align 8
  %8 = load i64* %7, align 8
  ret i64 %8
}

; Function Attrs: nounwind uwtable
define i64 @cdr(i64 %val) #0 {
  %1 = alloca i64, align 8
  %ptr = alloca i64*, align 8
  store i64 %val, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = load i32* @PAIR_TAG, align 4
  %4 = sext i32 %3 to i64
  %5 = sub i64 %2, %4
  %6 = inttoptr i64 %5 to i64*
  store i64* %6, i64** %ptr, align 8
  %7 = load i64** %ptr, align 8
  %8 = getelementptr inbounds i64* %7, i64 1
  %9 = load i64* %8, align 8
  ret i64 %9
}

; Function Attrs: nounwind uwtable
define i64 @carSet(i64 %pair, i64 %val) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %ptr = alloca i64*, align 8
  store i64 %pair, i64* %1, align 8
  store i64 %val, i64* %2, align 8
  %3 = load i64* %1, align 8
  %4 = load i32* @PAIR_TAG, align 4
  %5 = sext i32 %4 to i64
  %6 = sub i64 %3, %5
  %7 = inttoptr i64 %6 to i64*
  store i64* %7, i64** %ptr, align 8
  %8 = load i64* %2, align 8
  %9 = load i64** %ptr, align 8
  store i64 %8, i64* %9, align 8
  %10 = load i64* @NIL_VALUE, align 8
  ret i64 %10
}

; Function Attrs: nounwind uwtable
define i64 @cdrSet(i64 %pair, i64 %val) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %ptr = alloca i64*, align 8
  store i64 %pair, i64* %1, align 8
  store i64 %val, i64* %2, align 8
  %3 = load i64* %1, align 8
  %4 = load i32* @PAIR_TAG, align 4
  %5 = sext i32 %4 to i64
  %6 = sub i64 %3, %5
  %7 = inttoptr i64 %6 to i64*
  store i64* %7, i64** %ptr, align 8
  %8 = load i64* %2, align 8
  %9 = load i64** %ptr, align 8
  %10 = getelementptr inbounds i64* %9, i64 1
  store i64 %8, i64* %10, align 8
  %11 = load i64* @NIL_VALUE, align 8
  ret i64 %11
}

; Function Attrs: nounwind uwtable
define i8* @getArrayPtr(i64 %val, i32 %index, i32 %tag, i8 signext %elemSize) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i8, align 1
  %ptr = alloca i8*, align 8
  store i64 %val, i64* %1, align 8
  store i32 %index, i32* %2, align 4
  store i32 %tag, i32* %3, align 4
  store i8 %elemSize, i8* %4, align 1
  %5 = load i32* %2, align 4
  %6 = load i32* @FIXNUM_TAG_LEN, align 4
  %7 = lshr i32 %5, %6
  store i32 %7, i32* %2, align 4
  %8 = load i64* %1, align 8
  %9 = load i32* %3, align 4
  %10 = sext i32 %9 to i64
  %11 = sub i64 %8, %10
  %12 = inttoptr i64 %11 to i8*
  store i8* %12, i8** %ptr, align 8
  %13 = load i8** %ptr, align 8
  %14 = getelementptr inbounds i8* %13, i64 8
  %15 = load i32* %2, align 4
  %16 = load i8* %4, align 1
  %17 = sext i8 %16 to i32
  %18 = mul i32 %15, %17
  %19 = zext i32 %18 to i64
  %20 = getelementptr inbounds i8* %14, i64 %19
  ret i8* %20
}

; Function Attrs: nounwind uwtable
define i64 @arrayLength(i64 %val, i32 %tag) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i32, align 4
  %ptr = alloca i64*, align 8
  store i64 %val, i64* %1, align 8
  store i32 %tag, i32* %2, align 4
  %3 = load i64* %1, align 8
  %4 = load i32* %2, align 4
  %5 = sext i32 %4 to i64
  %6 = sub i64 %3, %5
  %7 = inttoptr i64 %6 to i64*
  store i64* %7, i64** %ptr, align 8
  %8 = load i64** %ptr, align 8
  %9 = load i64* %8, align 8
  ret i64 %9
}

; Function Attrs: nounwind uwtable
define i64 @isVector(i64 %val) #0 {
  %1 = alloca i64, align 8
  store i64 %val, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = load i32* @VECTOR_TAG, align 4
  %4 = load i32* @VECTOR_TAG_LEN, align 4
  %5 = call i64 @isTag(i64 %2, i32 %3, i32 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @vectorLength(i64 %val) #0 {
  %1 = alloca i64, align 8
  store i64 %val, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = load i32* @VECTOR_TAG, align 4
  %4 = call i64 @arrayLength(i64 %2, i32 %3)
  ret i64 %4
}

; Function Attrs: nounwind uwtable
define i64 @vectorRef(i64 %val, i64 %index) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %ptr = alloca i64*, align 8
  store i64 %val, i64* %1, align 8
  store i64 %index, i64* %2, align 8
  %3 = load i64* %1, align 8
  %4 = load i64* %2, align 8
  %5 = trunc i64 %4 to i32
  %6 = load i32* @VECTOR_TAG, align 4
  %7 = call i8* @getArrayPtr(i64 %3, i32 %5, i32 %6, i8 signext 8)
  %8 = bitcast i8* %7 to i64*
  store i64* %8, i64** %ptr, align 8
  %9 = load i64** %ptr, align 8
  %10 = load i64* %9, align 8
  ret i64 %10
}

; Function Attrs: nounwind uwtable
define i64 @vectorSet(i64 %vec, i64 %index, i64 %val) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %ptr = alloca i64*, align 8
  store i64 %vec, i64* %1, align 8
  store i64 %index, i64* %2, align 8
  store i64 %val, i64* %3, align 8
  %4 = load i64* %2, align 8
  %5 = load i32* @FIXNUM_TAG_LEN, align 4
  %6 = zext i32 %5 to i64
  %7 = lshr i64 %4, %6
  store i64 %7, i64* %2, align 8
  %8 = load i64* %1, align 8
  %9 = load i32* @VECTOR_TAG, align 4
  %10 = sext i32 %9 to i64
  %11 = sub i64 %8, %10
  %12 = inttoptr i64 %11 to i64*
  store i64* %12, i64** %ptr, align 8
  %13 = load i64* %3, align 8
  %14 = load i64** %ptr, align 8
  %15 = load i64* %2, align 8
  %16 = getelementptr inbounds i64* %14, i64 %15
  %17 = getelementptr inbounds i64* %16, i64 1
  store i64 %13, i64* %17, align 8
  %18 = load i64* @NIL_VALUE, align 8
  ret i64 %18
}

; Function Attrs: nounwind uwtable
define i64 @makeVector(i64 %size, i64 %elem) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %ptr = alloca i64*, align 8
  %i = alloca i32, align 4
  store i64 %size, i64* %1, align 8
  store i64 %elem, i64* %2, align 8
  %3 = load i64* %1, align 8
  %4 = load i32* @FIXNUM_TAG_LEN, align 4
  %5 = zext i32 %4 to i64
  %6 = lshr i64 %3, %5
  store i64 %6, i64* %1, align 8
  %7 = load i64* %1, align 8
  %8 = add i64 %7, 1
  %9 = mul i64 %8, 8
  %10 = call noalias i8* @memalign(i64 1, i64 %9) #2
  %11 = bitcast i8* %10 to i64*
  store i64* %11, i64** %ptr, align 8
  %12 = load i64* %1, align 8
  %13 = load i64** %ptr, align 8
  store i64 %12, i64* %13, align 8
  store i32 1, i32* %i, align 4
  br label %14

; <label>:14                                      ; preds = %25, %0
  %15 = load i32* %i, align 4
  %16 = sext i32 %15 to i64
  %17 = load i64* %1, align 8
  %18 = icmp ule i64 %16, %17
  br i1 %18, label %19, label %28

; <label>:19                                      ; preds = %14
  %20 = load i64* %2, align 8
  %21 = load i64** %ptr, align 8
  %22 = load i32* %i, align 4
  %23 = sext i32 %22 to i64
  %24 = getelementptr inbounds i64* %21, i64 %23
  store i64 %20, i64* %24, align 8
  br label %25

; <label>:25                                      ; preds = %19
  %26 = load i32* %i, align 4
  %27 = add nsw i32 %26, 1
  store i32 %27, i32* %i, align 4
  br label %14

; <label>:28                                      ; preds = %14
  %29 = load i64** %ptr, align 8
  %30 = ptrtoint i64* %29 to i64
  %31 = load i32* @VECTOR_TAG, align 4
  %32 = sext i32 %31 to i64
  %33 = add i64 %30, %32
  ret i64 %33
}

; Function Attrs: nounwind uwtable
define i64 @isString(i64 %val) #0 {
  %1 = alloca i64, align 8
  store i64 %val, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = load i32* @STRING_TAG, align 4
  %4 = load i32* @STRING_TAG_LEN, align 4
  %5 = call i64 @isTag(i64 %2, i32 %3, i32 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @stringLength(i64 %val) #0 {
  %1 = alloca i64, align 8
  store i64 %val, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = load i32* @STRING_TAG, align 4
  %4 = call i64 @arrayLength(i64 %2, i32 %3)
  ret i64 %4
}

; Function Attrs: nounwind uwtable
define zeroext i8 @stringRef(i64 %val, i64 %index) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %ptr = alloca i8*, align 8
  store i64 %val, i64* %1, align 8
  store i64 %index, i64* %2, align 8
  %3 = load i64* %1, align 8
  %4 = load i64* %2, align 8
  %5 = trunc i64 %4 to i32
  %6 = load i32* @STRING_TAG, align 4
  %7 = call i8* @getArrayPtr(i64 %3, i32 %5, i32 %6, i8 signext 1)
  store i8* %7, i8** %ptr, align 8
  %8 = load i8** %ptr, align 8
  %9 = load i8* %8, align 1
  ret i8 %9
}

; Function Attrs: nounwind
declare double @pow(double, double) #1

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.1 (tags/RELEASE_34/dot1-final)"}
