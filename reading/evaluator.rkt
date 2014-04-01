#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (interp (exp : ArithC)) : number
  (type-case ArithC exp
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

(test (interp (numC 4)) 4)
(test (interp (plusC (numC 1) (numC 2))) 3)
(test (interp (multC (numC 5) (numC 2))) 10)
(test (interp (multC (plusC (numC 3) (numC 2)) (numC 5))) 25)

