(let
  (
    (x 5)
  )
  (set! x 6)
  (
    let (
         (f ((lambda(y) (set! x 7)) 4))
         (g ((lambda(y) x) 5))
         )
    f
    g
  )
)
