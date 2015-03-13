(let
  (
    (v (make-vector 4 #t))
  )
  (vector-set! v 0 #f)
  (vector-set! v 2 #f)
  (if (not (= (vector-ref v 0) (vector-ref v 1))) #t #f)
)
