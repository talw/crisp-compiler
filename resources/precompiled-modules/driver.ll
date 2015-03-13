; ModuleID = 'driver.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@TRUE_VALUE = common global i64 0, align 8
@.str = private unnamed_addr constant [2 x i8] c" \00", align 1
@NIL_VALUE = common global i64 0, align 8
@.str1 = private unnamed_addr constant [4 x i8] c" . \00", align 1
@STRING_TAG = common global i32 0, align 4
@.str2 = private unnamed_addr constant [2 x i8] c"\22\00", align 1
@FIXNUM_TAG_LEN = common global i32 0, align 4
@.str3 = private unnamed_addr constant [3 x i8] c"%c\00", align 1
@.str4 = private unnamed_addr constant [3 x i8] c"#(\00", align 1
@.str5 = private unnamed_addr constant [2 x i8] c")\00", align 1
@.str6 = private unnamed_addr constant [3 x i8] c"#t\00", align 1
@FALSE_VALUE = common global i64 0, align 8
@.str7 = private unnamed_addr constant [3 x i8] c"#f\00", align 1
@.str8 = private unnamed_addr constant [3 x i8] c"()\00", align 1
@.str9 = private unnamed_addr constant [5 x i8] c"#\5C%c\00", align 1
@CHAR_TAG_LEN = common global i32 0, align 4
@.str10 = private unnamed_addr constant [4 x i8] c"%lu\00", align 1
@.str11 = private unnamed_addr constant [2 x i8] c"(\00", align 1
@.str12 = private unnamed_addr constant [19 x i8] c"Unrecognized value\00", align 1
@.str13 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@FIXNUM_TAG = common global i32 0, align 4
@CHAR_TAG = common global i32 0, align 4
@PAIR_TAG = common global i32 0, align 4
@PAIR_TAG_LEN = common global i32 0, align 4
@VECTOR_TAG = common global i32 0, align 4
@VECTOR_TAG_LEN = common global i32 0, align 4
@STRING_TAG_LEN = common global i32 0, align 4

; Function Attrs: nounwind uwtable
define void @showCons(i64 %val) #0 {
  %1 = alloca i64, align 8
  store i64 %val, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = call i64 @car(i64 %2)
  call void @showImmediate(i64 %3)
  %4 = load i64* %1, align 8
  %5 = call i64 @cdr(i64 %4)
  %6 = call i64 @isPair(i64 %5)
  %7 = load i64* @TRUE_VALUE, align 8
  %8 = icmp eq i64 %6, %7
  br i1 %8, label %9, label %13

; <label>:9                                       ; preds = %0
  %10 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8]* @.str, i32 0, i32 0))
  %11 = load i64* %1, align 8
  %12 = call i64 @cdr(i64 %11)
  call void @showCons(i64 %12)
  br label %24

; <label>:13                                      ; preds = %0
  %14 = load i64* %1, align 8
  %15 = call i64 @cdr(i64 %14)
  %16 = load i64* @NIL_VALUE, align 8
  %17 = icmp eq i64 %15, %16
  br i1 %17, label %18, label %19

; <label>:18                                      ; preds = %13
  br label %24

; <label>:19                                      ; preds = %13
  %20 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str1, i32 0, i32 0))
  %21 = load i64* %1, align 8
  %22 = call i64 @cdr(i64 %21)
  call void @showImmediate(i64 %22)
  br label %23

; <label>:23                                      ; preds = %19
  br label %24

; <label>:24                                      ; preds = %18, %23, %9
  ret void
}

; Function Attrs: nounwind uwtable
define void @showImmediate(i64 %val) #0 {
  %1 = alloca i64, align 8
  store i64 %val, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = load i64* @TRUE_VALUE, align 8
  %4 = icmp eq i64 %2, %3
  br i1 %4, label %5, label %7

; <label>:5                                       ; preds = %0
  %6 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str6, i32 0, i32 0))
  br label %73

; <label>:7                                       ; preds = %0
  %8 = load i64* %1, align 8
  %9 = load i64* @FALSE_VALUE, align 8
  %10 = icmp eq i64 %8, %9
  br i1 %10, label %11, label %13

; <label>:11                                      ; preds = %7
  %12 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str7, i32 0, i32 0))
  br label %72

; <label>:13                                      ; preds = %7
  %14 = load i64* %1, align 8
  %15 = load i64* @NIL_VALUE, align 8
  %16 = icmp eq i64 %14, %15
  br i1 %16, label %17, label %19

; <label>:17                                      ; preds = %13
  %18 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str8, i32 0, i32 0))
  br label %71

; <label>:19                                      ; preds = %13
  %20 = load i64* %1, align 8
  %21 = call i64 @isChar(i64 %20)
  %22 = load i64* @TRUE_VALUE, align 8
  %23 = icmp eq i64 %21, %22
  br i1 %23, label %24, label %30

; <label>:24                                      ; preds = %19
  %25 = load i64* %1, align 8
  %26 = load i32* @CHAR_TAG_LEN, align 4
  %27 = zext i32 %26 to i64
  %28 = lshr i64 %25, %27
  %29 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str9, i32 0, i32 0), i64 %28)
  br label %70

; <label>:30                                      ; preds = %19
  %31 = load i64* %1, align 8
  %32 = call i64 @isNumber(i64 %31)
  %33 = load i64* @TRUE_VALUE, align 8
  %34 = icmp eq i64 %32, %33
  br i1 %34, label %35, label %41

; <label>:35                                      ; preds = %30
  %36 = load i64* %1, align 8
  %37 = load i32* @FIXNUM_TAG_LEN, align 4
  %38 = zext i32 %37 to i64
  %39 = lshr i64 %36, %38
  %40 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str10, i32 0, i32 0), i64 %39)
  br label %69

; <label>:41                                      ; preds = %30
  %42 = load i64* %1, align 8
  %43 = call i64 @isPair(i64 %42)
  %44 = load i64* @TRUE_VALUE, align 8
  %45 = icmp eq i64 %43, %44
  br i1 %45, label %46, label %50

; <label>:46                                      ; preds = %41
  %47 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8]* @.str11, i32 0, i32 0))
  %48 = load i64* %1, align 8
  call void @showCons(i64 %48)
  %49 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8]* @.str5, i32 0, i32 0))
  br label %68

; <label>:50                                      ; preds = %41
  %51 = load i64* %1, align 8
  %52 = call i64 @isVector(i64 %51)
  %53 = load i64* @TRUE_VALUE, align 8
  %54 = icmp eq i64 %52, %53
  br i1 %54, label %55, label %57

; <label>:55                                      ; preds = %50
  %56 = load i64* %1, align 8
  call void @showVector(i64 %56)
  br label %67

; <label>:57                                      ; preds = %50
  %58 = load i64* %1, align 8
  %59 = call i64 @isString(i64 %58)
  %60 = load i64* @TRUE_VALUE, align 8
  %61 = icmp eq i64 %59, %60
  br i1 %61, label %62, label %64

; <label>:62                                      ; preds = %57
  %63 = load i64* %1, align 8
  call void @showString(i64 %63)
  br label %66

; <label>:64                                      ; preds = %57
  %65 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([19 x i8]* @.str12, i32 0, i32 0))
  br label %66

; <label>:66                                      ; preds = %64, %62
  br label %67

; <label>:67                                      ; preds = %66, %55
  br label %68

; <label>:68                                      ; preds = %67, %46
  br label %69

; <label>:69                                      ; preds = %68, %35
  br label %70

; <label>:70                                      ; preds = %69, %24
  br label %71

; <label>:71                                      ; preds = %70, %17
  br label %72

; <label>:72                                      ; preds = %71, %11
  br label %73

; <label>:73                                      ; preds = %72, %5
  ret void
}

declare i64 @car(i64) #1

declare i64 @isPair(i64) #1

declare i64 @cdr(i64) #1

declare i32 @printf(i8*, ...) #1

; Function Attrs: nounwind uwtable
define void @showString(i64 %val) #0 {
  %1 = alloca i64, align 8
  %length = alloca i64, align 8
  %i = alloca i32, align 4
  %bla = alloca i8, align 1
  store i64 %val, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = load i32* @STRING_TAG, align 4
  %4 = call i32 (i64, i32, ...)* bitcast (i32 (...)* @stringLength to i32 (i64, i32, ...)*)(i64 %2, i32 %3)
  %5 = sext i32 %4 to i64
  store i64 %5, i64* %length, align 8
  %6 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8]* @.str2, i32 0, i32 0))
  store i32 0, i32* %i, align 4
  br label %7

; <label>:7                                       ; preds = %22, %0
  %8 = load i32* %i, align 4
  %9 = zext i32 %8 to i64
  %10 = load i64* %length, align 8
  %11 = icmp ult i64 %9, %10
  br i1 %11, label %12, label %25

; <label>:12                                      ; preds = %7
  %13 = load i64* %1, align 8
  %14 = load i32* %i, align 4
  %15 = load i32* @FIXNUM_TAG_LEN, align 4
  %16 = shl i32 %14, %15
  %17 = call i32 (i64, i32, ...)* bitcast (i32 (...)* @stringRef to i32 (i64, i32, ...)*)(i64 %13, i32 %16)
  %18 = trunc i32 %17 to i8
  store i8 %18, i8* %bla, align 1
  %19 = load i8* %bla, align 1
  %20 = zext i8 %19 to i32
  %21 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str3, i32 0, i32 0), i32 %20)
  br label %22

; <label>:22                                      ; preds = %12
  %23 = load i32* %i, align 4
  %24 = add i32 %23, 1
  store i32 %24, i32* %i, align 4
  br label %7

; <label>:25                                      ; preds = %7
  %26 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8]* @.str2, i32 0, i32 0))
  ret void
}

declare i32 @stringLength(...) #1

declare i32 @stringRef(...) #1

; Function Attrs: nounwind uwtable
define void @showVector(i64 %val) #0 {
  %1 = alloca i64, align 8
  %length = alloca i64, align 8
  %i = alloca i32, align 4
  store i64 %val, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = call i64 @vectorLength(i64 %2)
  store i64 %3, i64* %length, align 8
  %4 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str4, i32 0, i32 0))
  store i32 0, i32* %i, align 4
  br label %5

; <label>:5                                       ; preds = %24, %0
  %6 = load i32* %i, align 4
  %7 = zext i32 %6 to i64
  %8 = load i64* %length, align 8
  %9 = icmp ult i64 %7, %8
  br i1 %9, label %10, label %27

; <label>:10                                      ; preds = %5
  %11 = load i64* %1, align 8
  %12 = load i32* %i, align 4
  %13 = load i32* @FIXNUM_TAG_LEN, align 4
  %14 = shl i32 %12, %13
  %15 = call i64 @vectorRef(i64 %11, i32 %14)
  call void @showImmediate(i64 %15)
  %16 = load i32* %i, align 4
  %17 = zext i32 %16 to i64
  %18 = load i64* %length, align 8
  %19 = sub i64 %18, 1
  %20 = icmp ult i64 %17, %19
  br i1 %20, label %21, label %23

; <label>:21                                      ; preds = %10
  %22 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8]* @.str, i32 0, i32 0))
  br label %23

; <label>:23                                      ; preds = %21, %10
  br label %24

; <label>:24                                      ; preds = %23
  %25 = load i32* %i, align 4
  %26 = add i32 %25, 1
  store i32 %26, i32* %i, align 4
  br label %5

; <label>:27                                      ; preds = %5
  %28 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8]* @.str5, i32 0, i32 0))
  ret void
}

declare i64 @vectorLength(i64) #1

declare i64 @vectorRef(i64, i32) #1

declare i64 @isChar(i64) #1

declare i64 @isNumber(i64) #1

declare i64 @isVector(i64) #1

declare i64 @isString(i64) #1

; Function Attrs: nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8**, align 8
  %retVal = alloca i64, align 8
  store i32 %argc, i32* %1, align 4
  store i8** %argv, i8*** %2, align 8
  %3 = call i32 (...)* @entryFunc()
  %4 = zext i32 %3 to i64
  store i64 %4, i64* %retVal, align 8
  %5 = load i64* %retVal, align 8
  call void @showImmediate(i64 %5)
  %6 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8]* @.str13, i32 0, i32 0))
  ret i32 0
}

declare i32 @entryFunc(...) #1

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.1 (tags/RELEASE_34/dot1-final)"}
