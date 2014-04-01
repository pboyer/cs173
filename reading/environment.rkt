#lang plai-typed

(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC 
  [numC (n : number)]
  [idC (n : symbol)]
  [appC (f : symbol) (a : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])
  
(define (interp (a : ExprC) (env : Env) (fds : (listof FunDefC))) : number
  (type-case ExprC a
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f a) (local ((define fd (get-fundef f fds))) (interp 
                                                  (fdC-body fd) 
                                                  (extend-env (bind (fdC-arg fd) (interp a env fds)) mt-env) fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]))

(define (lookup (n : symbol) (in : Env)) : number
  (cond 
    ((empty? in) (error 'lookup "Could not find value"))
    ((symbol=? n (bind-name (first in))) (bind-val (first in)))
    (else (lookup n (rest in)))))

(define (get-fundef (n : symbol) (in : (listof FunDefC))) : FunDefC
  (cond 
    ((empty? in) (error 'get-fundef "Could not find definition"))
    ((symbol=? n (fdC-name (first in))) (first in))
    (else (get-fundef n (rest in)))))

(test (interp (numC 4) (list) (list)) 4)
(test (interp (plusC (numC 1) (numC 2)) (list) (list)) 3)
(test (interp (multC (numC 5) (numC 2)) (list) (list)) 10)
(test (interp (appC 'double (numC 20)) (list) (list (fdC 'double 'x (plusC (idC 'x) (idC 'x))))) 40)
(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5))))
      15)
 
(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)
 
(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)