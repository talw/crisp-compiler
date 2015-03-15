# crisp-compiler

*This isn't a production-grade compiler. It is merely a personal, educational
haskell/llvm/compiler/static-vs-dynamic learning exercise.
As such, I apologize in advance for atrocities such as mostly missing comments
and/or lack of unit tests (only integration tests were written).

## Introduction
A compiler for Crisp (Lisp/Scheme subset) in Haskell, with an LLVM backend.

I initially wanted Crisp to be inspired by Scheme, and branch off it's own
path. However, due to recent time constraints, and how bringing the compiler to it's
current state took more time than I anticipated,
I settled on a subset of Scheme for now.

## Example
Here's an example of implementing merge-sort in crisp.
~~~ {.scm}
(define empty? (lambda(x) (= x '())))

(define list-length
  (lambda (lst)
    (if (empty? lst)
      0
      (+ 1 (list-length (cdr lst))))))

(define take
  (lambda (n lst)
    (if (= n 0)
      '()
      (cons (car lst)
            (take (- n 1) (cdr lst))))))

(define drop
  (lambda (n lst)
    (if (= n 0)
      lst
      (drop (- n 1) (cdr lst)))))

(define split
  (lambda (n lst)
    (cons (take n lst) (drop n lst))))

(define merge-sorted
  (lambda (lstA lstB)
    (cond ((empty? lstA)
            lstB)
          ((empty? lstB)
            lstA)
          ((< (car lstA) (car lstB))
            (cons (car lstA) (merge-sorted (cdr lstA) lstB)))
          (else
            (cons (car lstB) (merge-sorted lstA (cdr lstB)))))))

(define merge-sort
  (lambda (lst)
    (let ((len (list-length lst)))
      (if (< len 2)
        lst
        (let ((halves-pair (split (/ len 2) lst)))
          (merge-sorted (merge-sort (car halves-pair))
                        (merge-sort (cdr halves-pair))))))))

(merge-sort '(34 87 24 90 74 10 47))
~~~

Compile and execute:
~~~ {.bash}
$ crc -i merge-sort.scm -o merge-sort
Compiled successfully.
$ ./merge-sort
(10 24 34 47 74 87 90)
~~~

Here is what portions of the LLVM assembly for this module looks like:
This is what merge-sort compiled to, not including it's generated inner-lambdas.
~~~ {.ll}
define i64 @initGlobals-lambda6(i64 %__env, i64 %lst) {
entry:
  %0 = alloca i64
  store i64 %__env, i64* %0
  %1 = alloca i64
  store i64 %lst, i64* %1
  %2 = inttoptr i64 %__env to <{}>*
  %3 = call i64 @memalign(i64 1, i64 16)
  %4 = inttoptr i64 %3 to <{ i64, i64 }>*
  %5 = call i64 @memalign(i64 1, i64 8)
  %6 = inttoptr i64 %5 to <{ i64 }>*
  %7 = load i64* %1
  %8 = call i64 @memalign(i64 1, i64 8)
  %9 = inttoptr i64 %8 to i64*
  store i64 %7, i64* %9
  %10 = getelementptr inbounds <{ i64 }>* %6, i32 0, i32 0
  store i64 %8, i64* %10
  %11 = getelementptr inbounds <{ i64, i64 }>* %4, i32 0, i32 0
  store i64 %5, i64* %11
  %12 = getelementptr inbounds <{ i64, i64 }>* %4, i32 0, i32 1
  store i64 ptrtoint (i64 (i64, i64)* @initGlobals-lambda6-lambda to i64), i64* %12
  %13 = inttoptr i64 %3 to <{ i64, i64 }>*
  %14 = getelementptr inbounds <{ i64, i64 }>* %13, i32 0, i32 0
  %15 = load i64* %14
  %16 = getelementptr inbounds <{ i64, i64 }>* %13, i32 0, i32 1
  %17 = load i64* %16
  %18 = inttoptr i64 %17 to i64 (i64, i64)*
  %19 = load i64* @list-length
  %20 = inttoptr i64 %19 to <{ i64, i64 }>*
  %21 = getelementptr inbounds <{ i64, i64 }>* %20, i32 0, i32 0
  %22 = load i64* %21
  %23 = getelementptr inbounds <{ i64, i64 }>* %20, i32 0, i32 1
  %24 = load i64* %23
  %25 = inttoptr i64 %24 to i64 (i64, i64)*
  %26 = load i64* %9
  %27 = call i64 %25(i64 %22, i64 %26)
  %28 = call i64 %18(i64 %15, i64 %27)
  ret i64 %28
}
~~~

## Features

Implemented features, as of this writing:
- Crisp
  - Data types
      - Simple types
          - numbers, chars, bools
          - Functions such as:
              number?, char?, boolean?, +, -, *, /, =, <, <=, >, >=
      - Composite types
          - strings, lists, vectors
          - Example literals:
              "A string", '(1, #t, "a list item"), #(1, #t, "a vector item")
          - Functions such as:
              cons, car, cdr, set-car!, make-vector, vector-ref, vector-set!
      - Special types
          - lambdas, closures
          - Example:
          ~~~ {.scm}
              (let ((x 5))
                (lambda (y) (+ y x)))
          ~~~
  - Keywords
      such as: define, lambda, if, cond(else), and, or, not, let, set!
- Compiler
  - Normal compilation mode
  - REPL mode

- Missing features
    - Negative numbers, floating point numbers.
        - They can be implemented without too much difficulty and wouldn't
        benefit me much educationally.
        It would be mainly fiddling with how to do this in LLVM.
    - A standard library
        - Helper functions such as map or accumulate, which could easily be implemented
        in crisp itself and linked as a standard library of sorts, are missing,
        as I didn't find implementing them, nor attempting to be able to compile
        every conceivable scheme file, conducive to this learning exercise.

## Usage

The crc (crisp-compiler) is a command-line application.
The options are:
~~~ {.bash}
-i FILEPATH  --input-filepath=FILEPATH   Path of input file. REQUIRED if not in repl-mode
-o FILEPATH  --output-filepath=FILEPATH  Path of output file. Default: ./a.out
-r           --repl-mode                 Interactive REPL mode. Default: false
-p           --print-llvm                Output resulting LLVM IR. Default: false
-h           --help                      Show help
~~~

### Option details
The compiler has 2 modes of operation:
- File compilation mode (-i, -o)
    - A crisp file is passed as an input, and an output my be passed as well. (otherwise ./a.out is used)
    - The file is compiled and a binary file is written in output.
    - The target machine is the one running the compiler.
- REPL mode (-r)
    - This works like your standard REPL (read-eval-print-loop) session.
    - This mode utilizes LLVM's JIT capabilities.
    - -i and -o flags will be ignored in this mode.
- Both modes allow for the resulting LLVM IR, that is a natural side product
of the compilation process, to be sent to stdout.
    - Out of convenience, the LLVM IR that is printed is before optimization passes.
    Having said that, during a repl session the resulting LLVM IR will consist of
    all definitions up to that point, thus definitions from prior evaluations will
    be displayed optimized. However, code resulting from the latest invocation or
    definition will not.

### Requirements
- The Haskell compiler, GHC is required, as some GHC extensions are
used in the compiler source files.
- LLVM bindings to version 3.4.1 were used, as that is what was (still is?)
available at the time of writing the bulk of crc. Make sure it is installed
on your system: http://llvm.org/releases/download.html#3.4.2
It should be available in your favorite package manager.
If you're using Arch Linux, do note that already at the time of this writing,
pacman is offering a newer version, which isn't compatible with the
llvm-haskell bindings. You can use the Arch Rollback Machine.
http://seblu.net/a/arm/packages/l/llvm/llvm-3.4.1-2-x86_64.pkg.tar.xz
- Seems superfluous to mention but, you need gcc (GNU's C Compiler).
Why? Because compiling the LLVM modules result in object files. Those files
are not executable, the gcc linker will set the main function as an entry point
appropriately and render the file executable (this should happen automatically
from running this compiler).
- Although this repository already contains the neccessary primitive functions
compiled as llvm-modules, if you wish to recompile them, you'd need clang
as they are .c files which should reside as LLVM modules come link-time.
Your best bet is getting a version which matches the LLVM's, through the link
given above.
And for Arch linux users:
http://seblu.net/a/arm/packages/c/clang/clang-3.4.1-2-x86_64.pkg.tar.xz

## Tests
- The test-suite runs a test per scheme (.scm) file in the resources/tests folder
(which should reside in a different folder after you cabal installed),
- The tests compiles x.scm, runs a gcc linker over it, executes it, and
asserts the result equals to the text in x.ans.
- test-cond.scm and test-cond.ans files for example:
~~~ {.scm}
(define empty? (lambda(x) (= x '())))

(define merge-sort (lambda (lstA lstB)
  (cond ((empty? lstA) lstB)
        ((empty? lstB) lstA)
        ((< (car lstA) (car lstB)) (cons (car lstA) (merge-sort (cdr lstA) lstB)))
        (else (cons (car lstB) (merge-sort lstA (cdr lstB))))
  )
))

(merge-sort '(2 6 9) '(3 5 10))
~~~
~~~ {.scm}
(2 3 5 6 9 10)
~~~

- The HUnit tests are integration tests. And require that the binary be installed
(doesn't matter if globally or 'sandbox'ically, it uses the binDir that is
cabal-autogenerated).

## Install

###Option 1: Install package from hackage###
Package not yet uploaded to Hackage.

###Option 2: clone from Github###

To install:
~~~ {.bash}
$ git clone https://github.com/talw/crisp-compiler.git
$ cd crisp-compiler
$ cabal sandbox init
$ cabal install
~~~

After which you should have a crc in
.cabal-sandbox/bin/ where you 'cabal install'ed.
