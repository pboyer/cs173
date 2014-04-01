#lang plai-typed


(define-type-alias Location number)

(define-type Binding
  [bind (name : symbol) (loc : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (loc : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define (lookup (n : symbol) (in : Env)) : Location
  (cond 
    ((empty? in) (error 'lookup "name not found"))
    ((symbol=? n (bind-name (first in))) (bind-loc (first in)))
    (else (lookup n (rest in)))))

(define (fetch (loc : Location) (sto : Store)) : Value
  (cond 
    ((empty? sto) (error 'lookup "location not found"))
    ((= loc (cell-loc (first sto))) (cell-val (first sto)))
    (else (fetch loc (rest sto)))))

(define (override-store* (c : Storage) (sto : Store)) : Store
  (cond
    ((empty? sto) (cons c sto))
    (else 
      (cond
        ((= (cell-loc c) (cell-loc (first sto))) (cons c sto))
        (else (cons (first sto) (override-store* c (rest sto))))))))

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define-type Result
  [v*s (v : Value) (s : Store)])
  
(define-type ExprC 
  [numC (n : number)]
  [idC (n : symbol)]
  [appC (f : ExprC) (a : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [setC (var : symbol) (val : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)])
  
(define (interp (a : ExprC) (env : Env) (sto : Store)) : Result
  (type-case ExprC a
    [numC (n) (v*s (numV n) sto)]
    [idC (n) (v*s (fetch (lookup n env) sto) sto)]
    [appC (f a) (type-case Result (interp f env sto)
                   (v*s (v-f s-f) 
                        (type-case Result (interp a env s-f)
                          (v*s (v-a s-a)
                               (let ((where (new-loc)))
                                  (interp (closV-body v-f) 
                                          (extend-env (bind (closV-arg v-f) where) (closV-env v-f))
                                          (override-store (cell where v-a) s-a)))))))]
    [plusC (l r) (type-case Result (interp l env sto)
                   (v*s (v-l s-l) 
                        (type-case Result (interp r env s-l)
                          (v*s (v-r s-r)
                               (v*s (num+ v-l v-r) s-r)))))]
    [multC (l r) (type-case Result (interp l env sto)
                   (v*s (v-l s-l) 
                        (type-case Result (interp r env s-l)
                          (v*s (v-r s-r)
                               (v*s (num* v-l v-r) s-r)))))]
    [lamC (a b) (v*s (closV a b env) sto)]
    [setC (var val) (type-case Result (interp val env sto)
                  [v*s (v-val s-val)
                     (let ([where (lookup var env)])
                       (v*s v-val (override-store (cell where v-val) s-val)))])]
    [seqC (b1 b2) (type-case Result (interp b1 env sto) 
                    (v*s (v-b1 s-b1) (interp b2 env s-b1)))]))

(define new-loc
  (local ([define last-loc (box 99)])
    (lambda ()
      (begin
        (set-box! last-loc (+ 1 (unbox last-loc)))
        (unbox last-loc)))))

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
    [else (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r)))]
    [else (error 'num+ "one argument was not a number")]))

(test (v*s-v (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10))) mt-env mt-store)) (numV 15))
 
(test (v*s-v (interp (appC (lamC 'x 
               (appC (lamC 'y (plusC (idC 'x) (idC 'y))) (numC 4)))
                        (numC 3))
                  mt-env mt-store)) (numV 7))
