#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (f : ExprC) (a : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [boxC (arg: ExprC)]
  [unboxC (arg: ExprC)]
  [setboxC (b: ExprC) (v : ExprC)]
  [seqC (b1: ExprC) (b2 : ExprC)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV (l : Location)])

(define-type Binding
  [bind (name : symbol) (val : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)
    
(define-type Storage
  [cell (location : Location) (val : Value)])
 
(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define (lookup [for : symbol] [env : Env]) : Location
  (cond 
    [(empty? env) (error 'lookup "Name not found")]
    [else (cond 
            [(symbol=? for (bind-name (first env))) (bind-val (first env))]
            [else (lookup for (rest env))])]))

(define (fetch [for : Location] [store : Store]) : Value
  (cond 
    [(empty? store) (error fetch "Name not found")]
    [else (cond 
            [(symbol=? for (cell-location (first store))) (cell-val (first store))]
            [else (lookup for (rest store))])]))

(define (lookup [for : symbol] [env : Env]) : Value
  (cond 
    [(empty? env) (error 'lookup "Name not found")]
    [else (cond 
            [(symbol=? for (bind-name (first env))) (bind-val (first env))]
            [else (lookup for (rest env))])]))

(define (num+ (l : Value) (r : Value)) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
    [else (error 'num+ "Input args were not both numbers")]))

(define (num* (l : Value) (r : Value)) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r)))]
    [else (error 'num* "Input args were not both numbers")]))

(define-type Result
  [v*s (v : Value) (s : Store)])

(define (interp [expr : ExprC] [env : Env] [sto: Store]) : Result
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [idC (n) (v*s (fetch (lookup n env) sto) sto)]
    [lamC (a b) (v*s (closV a b env) sto)]
    [appC (f a) 
          (type-case Result (interp f env sto)
            [v*s (f-v f-s) 
                 (type-case Result (interp f env f-s)
                   [v*s (a-v a-s)
                        (let ([where (new-loc)])
                          (interp (closV-body f-v) 
                               (extend-env (bind (closV-arg f-v) a-v) where) 
                               (override-store (cell where a-v) a-s)))])])]
    [plusC (l r) (type-case Result (interp l env sto) 
                   [v*s (l-v l-s) 
                        (type-case Result (interp r env l-s)
                        [v*s (r-v r-s) 
                             (v*s (num+ l-v l-s) r-s)])])]
    [multC (l r) (type-case Result (interp l env sto) 
                   [v*s (l-v l-s) 
                        (type-case Result (interp r env l-s)
                        [v*s (r-v r-s) 
                             (v*s (num* l-v l-s) r-s)])])]
    [boxC (a) (type-case Result (interp a env sto) 
                   [v*s (v-a s-a) 
                        (let ([where (new-loc)])
                          (v*s (boxV where)
                               (override-store (cell where v-a) s-a)))])]
    [unboxC (a) (type-case Result (interp a env sto) 
                   [v*s (v-a s-a) 
                        (v*s (fetch (boxV-l v-a) s-a) s-a)])]
    [setboxC (b v) (type-case Result (interp b env sto)
                     [v*s (v-b s-b) 
                          (type-case Result (interp v env s-b)
                            [v*s (v-v s-v)
                                 (v*s v-v (override-store (cell (boxV-l v-b) v-v) s-v))])])]
    [seqC (b1 b2) (type-case Result (interp b1 env sto) [v*s (v-b1 s-b1) (interp b2 env s-b1)])]))

(test (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))

(test (interp (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3))
                  mt-env)
         (numV 7))

(test (interp (appC (idC 'f) (numC 7)) (extend-env (bind 'f (interp (lamC 'a (plusC (idC 'a) (numC 5))) mt-env)) mt-env)) (numV 12))


