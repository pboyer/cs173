#lang plai-typed

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (f : ExprC) (a : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)
    
(define (lookup [for : symbol] [env : Env]) : Value
  (cond 
    [(empty? env) (error 'lookup "Name not found")]
    [else (cond 
            [(symbol=? for (bind-name (first env))) (bind-val (first env))]
            [else (lookup for (rest env))])]))

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define (num+ (l : Value) (r : Value)) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
    [else (error 'num+ "Input args were not both numbers")]))

(define (num* (l : Value) (r : Value)) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r)))]
    [else (error 'num* "Input args were not both numbers")]))

(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [lamC (a b) (closV a b env)]
    [appC (f a) 
       (local ([define f-value (interp f env)])
          (interp (closV-body f-value)
                  (extend-env (bind (closV-arg f-value) (interp a env)) 
                              (closV-env f-value))))]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]))

(test (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))

(test (interp (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3))
                  mt-env)
         (numV 7))

(test (interp (appC (idC 'f) (numC 7)) (extend-env (bind 'f (interp (lamC 'a (plusC (idC 'a) (numC 5))) mt-env)) mt-env)) (numV 12))


