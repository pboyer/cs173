#lang plai-typed

(require "typed-lang2.rkt")
(require (typed-in racket/base (number->string : (number -> string))))
(require (typed-in racket/base (display : (string -> void))))

(define (lookup (what : symbol) (in : Env)) : Binding
  (cond 
    ((empty? in) (interp-error (string-append "Unbound identifier: "
                                       (symbol->string what))))
    ((symbol=? what (bind-name (first in))) (first in))
    (else (lookup what (rest in)))))

(define (fetch (what : Binding) (in : Store)) : ValueC
   (cond 
    ((empty? in) (interp-error (string-append "Unbound identifier: "
                                       (symbol->string (bind-name what)))))
    ((= (bind-value what) (cell-location (first in))) (cell-value (first in)))
    (else (fetch what (rest in)))))

(define-type Cell
  [cell (location : Location) (value : ValueC)])

(define (override-store* (c : Cell) (sto : Store)) : Store
  (cond
    ((empty? sto) (cons c sto))
    (else 
      (cond
        ((= (cell-location c) (cell-location (first sto))) (cons c sto))
        (else (cons (first sto) (override-store* c (rest sto))))))))

(define-type-alias Store (listof Cell))

(define mt-env empty)
(define extend-env cons)

(define new-loc
  (local ([define last-loc (box 99)])
    (lambda ()
      (begin
        (set-box! last-loc (+ 1 (unbox last-loc)))
        (unbox last-loc)))))

(define-type Result
  [v*s (value : ValueC) (store : Store)])

(define (interp-full [exprC : ExprC] [env : Env] [store : Store]) : Result
  (type-case ExprC exprC
    
    [IdC (s) (v*s (fetch (lookup s env) store) store)]
    
    [NumC (n) (v*s (NumV n) store)]
    [StrC (s) (v*s (StrV s) store)]
    [TrueC () (v*s (TrueV) store)]
    [FalseC () (v*s (FalseV) store)]
    
    ;; TODO: should extend to check type of s
    [ErrorC (s) (interp-error (StrC-s s))]
    
    [IfC (c t e) (type-case Result (interp-full c env store)
                   (v*s (v-c s-c)
                        (type-case ValueC v-c
                          [FalseV () (interp-full e env s-c)]
                          {else (interp-full t env s-c)})))]
    
    [SeqC (e1 e2) (type-case Result (interp-full e1 env store)
                    (v*s (v-e1 s-e1) 
                         (type-case Result (interp-full e2 env s-e1)
                           (v*s (v-e2 s-e2) (v*s v-e2 s-e2)))))]
    
    [Set!C (id e) (type-case Result (interp-full e env store)
                     (v*s (v-e s-e) 
                          (v*s v-e (override-store* (cell (bind-value (lookup id env)) v-e) s-e))))]
    
    [LetC (id e body) (type-case Result (interp-full e env store)
                        (v*s (v-e s-e)
                             (let ((loc (new-loc)))
                               (interp-full body 
                                            (extend-env (bind id loc) env)
                                            (override-store* (cell loc v-e) s-e)))))]
    
    [AppC (f a) (interp-apply f a env store)]
    [FuncC (as b) (v*s (ClosureV as b env) store)]
    
    [ObjectC (fs) (type-case FieldEvalContext (interp-fields fs env store)
                    (f*s (fvs s-fvs)
                         (v*s (make-object fvs) s-fvs)))]
    
    [GetFieldC (o f) (type-case Result (interp-full o env store)
                       (v*s (v-o s-o)
                            (type-case Result (interp-full f env s-o)
                              (v*s (v-f s-f)
                                   (type-case ValueC v-o
                                     [ObjectV (ofvs) 
                                              (type-case ValueC v-f
                                                [StrV (s)
                                                         (v*s (lookup-field ofvs s) s-f)]
                                                [else (interp-error (string-append "Non-string in field lookup: " (pretty v-f)))])]
                                     [else (interp-error (string-append "Non-object in field lookup: " (pretty v-o)))])))))]
                                   
    
    [SetFieldC (o f v) (type-case Result (interp-full o env store)
                         (v*s (v-o s-o)
                            (type-case Result (interp-full f env s-o)
                              (v*s (v-f s-f)
                                (type-case Result (interp-full v env s-f)
                                   (v*s (v-v s-v)
                                        (type-case ValueC v-o
                                          [ObjectV (ofvs) 
                                                   (type-case ValueC v-f
                                                     [StrV (s)
                                                           (v*s (ObjectV (set-field ofvs s v-v)) s-v)]
                                                     [else (interp-error (string-append "Non-string in field update: " (pretty v-f)))])]
                                          [else (interp-error (string-append "Non-object in field update: " (pretty v-o)))])))))))]
    
    [Prim1C (o a) (type-case Result (interp-full a env store)
                    (v*s (v-a s-a)
                         (case o
                           
                               ('print (v*s (let ((val (pretty v-a)))
                                                  (begin 
                                                    (display val)
                                                    v-a)) s-a))
                               ('tagof (v*s (StrV (tagof v-a)) s-a)))))]
    
    [Prim2C (o a1 a2) (type-case Result (interp-full a1 env store)
                         (v*s (v-a1 s-a1)
                              (type-case Result (interp-full a2 env store)
                                (v*s (v-a2 s-a2)
                                     (case o 
                                       ['string+ (v*s (string+ v-a1 v-a2) s-a2)]
                                       ['num+ (v*s (num+ v-a1 v-a2) s-a2)]
                                       ['num- (v*s (num- v-a1 v-a2) s-a2)]
                                       ['== (v*s (to-boolv (equal v-a1 v-a2)) s-a2)]
                                       ['< (v*s (less-than v-a1 v-a2) s-a2)]
                                       ['> (v*s (greater-than v-a1 v-a2) s-a2)])))))]))



(define (make-object (fields : (listof FieldV))) : ValueC
    (ObjectV (build-fields (list) fields)))

(define (contains? (fields : (listof FieldV)) (field : FieldV)) : boolean
  (cond
    ((empty? fields) #f)
    ((string=? (fieldV-name (first fields)) (fieldV-name field)) #t)
    (else (contains? (rest fields) field))))

(define (build-fields (current : (listof FieldV)) (toadd : (listof FieldV))) : (listof FieldV)
  (cond 
    ((empty? toadd) current)
    (else (if (contains? current (first toadd)) 
              
              (interp-error "Multiply-defined fields")
              (build-fields (cons (first toadd) current) (rest toadd))))))

(define (greater-than (a : ValueC) (b : ValueC)) : ValueC
  (if (both-of-type "number" a b)
      (to-boolv (> (NumV-n a) (NumV-n b)))
      (interp-error "Both arguments to > must be numbers")))

(define (less-than (a : ValueC) (b : ValueC)) : ValueC
  (if (both-of-type "number" a b)
      (to-boolv (< (NumV-n a) (NumV-n b)))
      (interp-error "Both arguments to < must be numbers")))

(define (both-of-type (s : string) (a : ValueC) (b : ValueC)) : boolean
  (let ((ta (tagof a)) (tb (tagof b)))
    (if (string=? s ta)
        (if (string=? s tb)
            #t
            #f)
        #f)))

(define (bool-equal (a : ValueC) (b : ValueC)) : boolean
  (string=? (pretty a) (pretty b)))

(define (field-equal (a : FieldV) (b : FieldV)) : boolean
  (and (string=? (fieldV-name a) (fieldV-name b))
       (equal (fieldV-value b) (fieldV-value b))))

(define (object-fields-equal (a : (listof FieldV)) (b : (listof FieldV))) : boolean
  (cond
    [(empty? a) (empty? b)]
    [(field-equal (first a) (first b)) (object-fields-equal (rest a) (rest b))]
    [else #f]))
    
(define (object-equal (a : ValueC) (b : ValueC)) : boolean
  (object-fields-equal (ObjectV-fields a) (ObjectV-fields b)))

(define (to-boolv (v : boolean)) : ValueC
  (if v (TrueV) (FalseV)))

(define (equal (a : ValueC) (b : ValueC)) : boolean
  (cond
    [(both-of-type "string" a b) (string=? (StrV-s a) (StrV-s b))]
    [(both-of-type "number" a b) (= (NumV-n a) (NumV-n b))]
    [(both-of-type "boolean" a b) (bool-equal a b)]
    [(both-of-type "object" a b) (object-equal a b)]
    [(both-of-type "function" a b) #f]
    [else #f]))

(define (num- (a : ValueC) (b : ValueC)) : ValueC
  (type-case ValueC a
    [NumV (n-a)
          (type-case ValueC b
            [NumV (n-b) (NumV (- n-a n-b))]
            [else (interp-error "The second operand to - must be a number")])]
    [else (interp-error "The first operand to - must be a number")]))

(define (num+ (a : ValueC) (b : ValueC)) : ValueC
  (type-case ValueC a
    [NumV (n-a)
          (type-case ValueC b
            [NumV (n-b) (NumV (+ n-a n-b))]
            [else (interp-error "The second operand to + must be a number")])]
    [else (interp-error "The first operand to + must be a number")]))

(define (string+ (a : ValueC) (b : ValueC)) : ValueC
  (type-case ValueC a
    [StrV (n-a)
          (type-case ValueC b
            [StrV (n-b) (StrV (string-append n-a n-b))]
            [else (interp-error "The second operand to + must be a string")])]
    [else (interp-error "The first operand to + must be a string")]))

(define (tagof (v : ValueC)) : string
  (type-case ValueC v
    [ObjectV (fs) "object"]
    [ClosureV (a b e) "function"]
    [NumV (n) "number"]
    [StrV (s) "string"]
    [TrueV () "boolean"]
    [FalseV () "boolean"]))

(define (pretty (v : ValueC)) : string
  (type-case ValueC v
    [ObjectV (fs) "object"]
    [ClosureV (a b e) "function"]
    [NumV (n) (number->string n)] 
    [StrV (s) s]
    [TrueV () "true"]
    [FalseV () "false"]))
                               
(define (set-field (fs : (listof FieldV)) (s : string) (v : ValueC)) : (listof FieldV)
  (cond
    ((empty? fs) (list (fieldV s v)))
    ((string=? s (fieldV-name (first fs))) (cons (fieldV s v) (rest fs)))
    (else (cons (first fs) (set-field (rest fs) s v)))))
                                  
(define (lookup-field (fs : (listof FieldV)) (s : string)) : ValueC
  (cond
    ((empty? fs) (interp-error (string-append "Field not found: " s)))
    ((string=? s (fieldV-name (first fs))) (fieldV-value (first fs)))
    (else (lookup-field (rest fs) s))))
                            
(define (interp [exprC : ExprC]) : ValueC
  (type-case Result (interp-full exprC empty empty)
    [v*s (value store) value]))

(define-type Context 
  [e*s (env : Env) (store : Store)])

(define-type FieldEvalContext 
  [f*s (fields : (listof FieldV)) (store : Store)])

(define (interp-fields (fs : (listof FieldC)) (env : Env) (store : Store)) : FieldEvalContext
  (cond 
    ((empty? fs) (f*s (list) store))
    (else (type-case Result (interp-full (fieldC-value (first fs)) env store)
            (v*s (v-e s-e)
                 (type-case FieldEvalContext (interp-fields (rest fs) env s-e)
                   (f*s (fvs s-n)
                           (f*s (cons (fieldV (fieldC-name (first fs)) v-e) fvs) s-n))))))))

;; for each arg, extend the env and add to the store
(define (cascade-extend (ids : (listof symbol)) (es : (listof ExprC)) (env : Env) (store : Store)) : Context
  (cond 
    ((and (empty? es) (not (empty? ids))) (interp-error "Application failed with arity mismatch"))
    ((and (not (empty? es)) (empty? ids)) (interp-error "Application failed with arity mismatch"))
    ((and (empty? es) (empty? ids)) (e*s env store))
    (else (type-case Result (interp-full (first es) env store)
            (v*s (v-e s-e)
                 (let ((loc (new-loc)))
                   (cascade-extend (rest ids)
                                   (rest es)
                                   (extend-env (bind (first ids) loc) env)
                                   (override-store* (cell loc v-e) s-e))))))))


(define (extend-with-env (env-add : Env) (env : Env)) : Env
  (cond
    ((empty? env-add) env)
    (else (cons (first env-add) (extend-with-env (rest env-add) env)))))

(define (interp-apply (f : ExprC) (args : (listof ExprC)) (env : Env) (store : Store)) : Result
  (type-case Result (interp-full f env store)
    (v*s (v-f s-f)
         (type-case ValueC v-f
           [ClosureV (fa fb fe)
                     (type-case Context (cascade-extend fa args env s-f) 
                       (e*s (e-a s-a) (interp-full fb (extend-with-env fe e-a) s-a)))]
           [else (interp-error (string-append "Applied a non-function: " (pretty v-f)))]))))
                 
                