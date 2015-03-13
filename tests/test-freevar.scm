(
  (lambda(x)
    (+ x
       ((lambda(y)(+ y x)) 2)
    ))
  5
)
