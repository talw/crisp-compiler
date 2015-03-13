(let ( (f (lambda(g)(g (let ((r (lambda(x w) (w (* 2 x))))) r)))) (y 2) ) (f (lambda(d)(d 3 (lambda(u)(+ u 1))))))
