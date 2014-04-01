#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [ifC (t : ArithC) (a : ArithC) (b : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (interp (exp : ArithC)) : number
  (type-case ArithC exp
    [numC (n) n]
    [ifC (t a b) (if (= 0 (interp t)) (interp b) (interp a))]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))


(test (interp (ifC (numC 0) (numC 99) (numC 22))) 22)
(test (interp (ifC (numC 1) (numC 99) (numC 22))) 99)

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define (desugar (exp : ArithS)) : ArithC
  (type-case ArithS exp
    [numS (n) (numC n)]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [multS (l r) (multC (desugar l) (desugar r))]))

(test (interp (desugar (bminusS (numS 5) (numS 2)))) 3)
(test (interp (desugar (uminusS (numS 5)))) -5)









