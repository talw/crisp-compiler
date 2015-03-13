(let
  (
    (f (lambda (x) 2 x))
    (g (lambda (x) 3 (+ x 1)))
  )
  (f 5)
  (g 5)
  (f 5)
  (g 5)
)
