#lang plai-typed

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Value
  [numV (n : number)]
  [funV (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC 
  [numC (n : number)]
  [idC (n : symbol)]
  [appC (f : ExprC) (a : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])
  
(define (interp (a : ExprC) (env : Env)) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [appC (f a) (local ((define fd (interp f env))) 
                  (interp (funV-body fd) (extend-env (bind (funV-arg fd) (interp a env)) mt-env)))]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [fdC (n a b) (funV n a b)]))

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
    [else (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r)))]
    [else (error 'num+ "one argument was not a number")]))

(define (lookup (n : symbol) (in : Env)) : Value
  (cond 
    ((empty? in) (error 'lookup "name not found"))
    ((symbol=? n (bind-name (first in))) (bind-val (first in)))
    (else (lookup n (rest in)))))

(test (interp (plusC (numC 10) (appC (fdC 'const5 '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))
 
(test/exn (interp (appC (fdC 'f1 'x (appC (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3))
                  mt-env)
          "lookup: name not found")