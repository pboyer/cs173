#lang plai-typed

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC 
  [numC (n : number)]
  [idC (n : symbol)]
  [appC (f : symbol) (a : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])
  
(define (interp (a : ExprC) (fds : (listof FunDefC))) : number
  (type-case ExprC a
    [numC (n) n]
    [idC (n) (error 'interp "Symbol not defined")]
    [appC (f a) (local ((define fd (get-fundef f fds))) 
                          (interp (subst a (fdC-arg fd) (fdC-body fd)) fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]))

(define (get-fundef (n : symbol) (in : (listof FunDefC))) : FunDefC
  (cond 
    ((empty? in) (error 'get-fundef "Could not find definition"))
    ((symbol=? n (fdC-name (first in))) (first in))
    (else (get-fundef n (rest in)))))

(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    (numC (n) in)
    (idC (n) (if (symbol=? n for) what in))
    (appC (f a) (appC f (subst what for a)))
    (plusC (l r) (plusC (subst what for l) (subst what for r)))
    (multC (l r) (multC (subst what for l) (subst what for r)))))

(test (interp (numC 4) (list)) 4)
(test (interp (plusC (numC 1) (numC 2)) (list)) 3)
(test (interp (multC (numC 5) (numC 2)) (list)) 10)
(test (interp (appC 'double (numC 20)) (list (fdC 'double 'x (plusC (idC 'x) (idC 'x))))) 40)
