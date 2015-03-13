(let
  (
    (x 5)
  )
  (set! x 6)
  (
    let ((y 4))
      (set! x 7)
  )
  (set! x (+ x 1))
  (
    let ((y 5))
      (set! x (+ x 1))
  )
  x
)
