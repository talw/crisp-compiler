(define x 5)
(define f (lambda(x)(* x 2)))
(
  let ((y 4))
    (set! x (f x))
)
(set! x (+ x 1))
(
  let ((y 5) (f (lambda(x) (+ x 1))) (g f))
    (set! x (g x))
)
x
