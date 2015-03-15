(define empty? (lambda(x) (= x '())))

(define merge-sort (lambda (lstA lstB)
  (cond ((empty? lstA) lstB)
        ((empty? lstB) lstA)
        ((< (car lstA) (car lstB)) (cons (car lstA) (merge-sort (cdr lstA) lstB)))
        (else (cons (car lstB) (merge-sort lstA (cdr lstB))))
  )
))

(merge-sort '(2 6 9) '(3 5 10))
