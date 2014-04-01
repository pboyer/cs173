#lang plai-typed

(require "../parseltongue-lang-py/typed-lang.rkt" "interp-core-py.rkt")

(require (typed-in racket/base (raise-user-error : (string -> ValueC))))

(define (interp (exprC : ExprC)) : ValueC
  (type-case AnswerC (interp-env exprC mt-env)
    [ExceptionA (exn-val store) exn-val]
    [ValueA (val store) val]))

(define (interp-env (exprC : ExprC) (env : Env)) : AnswerC
  (type-case ExprC exprC
    
    [UndefinedC () (ValueA (UndefinedV) env)]
    
    [TryCatchC (b p c) (type-case AnswerC (interp-env b env)
                         [ValueA (v s) (ValueA v s)]
                         [ExceptionA (v s) (interp-env c (extend-env p v s))])]
    
    [ErrorC (e) (type-case AnswerC (interp-env e env)
                  [ValueA (v s) (ExceptionA v s)]
                  [ExceptionA (v s) (ExceptionA v s)])]

    [IdC (s) (fetch s env)]
    
    [NumC (n) (ValueA (NumV n) env)]
    [StrC (s) (ValueA (StrV s) env)]
    [TrueC () (ValueA (TrueV) env)]
    [FalseC () (ValueA (FalseV) env)]
    
    [IfC (c t e) (type-case AnswerC (interp-env c env)
                   [ValueA (v-c s-c)
                        (type-case ValueC v-c
                          [FalseV () (interp-env e s-c)]
                          {else (interp-env t s-c)})]
                   [ExceptionA (v-c s-c) (ExceptionA v-c s-c)])]
    
    ;; check here - are we doing a set! in the first position?
    ;; if so, is the variable already created in env?
    ;; evaluate e1 - then evaluate e2 in the extended env
    [SeqC (e1 e2) (type-case AnswerC (interp-env e1 env)
                    [ValueA (v-e1 e-e1) (interp-env e2 e-e1)]
                    [ExceptionA (v-e1 e-e1) (ExceptionA v-e1 e-e1)])]
    
    ;; we're assigning a value to a variable already defined in the environment
    [Set!C (id e) (type-case AnswerC (interp-env e env)
                    [ValueA (v-e new-env) (ValueA v-e (extend-env id v-e new-env))]
                    [ExceptionA (v-e s-e) (ExceptionA v-e s-e)])]
    
    [LetC (id e body) (type-case AnswerC (interp-env e env)
                        [ValueA (v-e s-e) (interp-env body (extend-env id v-e s-e))]
                        [ExceptionA (v-e s-e) (ExceptionA v-e s-e)])]
    
    [AppC (f a) (type-case AnswerC (interp-apply f a env)
                  [ValueA (v e) (ValueA v env)] ;; do not allow inner func to modify env
                  [ExceptionA (v e) (ExceptionA v e)])]
    
    [FuncC (as b) (ValueA (ClosureV as b env) env)]
    
    [ObjectC (fs) (type-case FieldEvalContext (interp-fields fs env)
                    [ExceptionFEC (ex) ex]
                    [GoodFEC (fvs s-fvs)
                         (ValueA (ObjectV fvs) s-fvs)])]
    
    [GetFieldC (o f) (type-case AnswerC (interp-env o env)
                       [ValueA (v-o s-o)
                            (type-case AnswerC (interp-env f s-o)
                              [ValueA (v-f s-f)
                                   (type-case ValueC v-o
                                     [ObjectV (ofvs) 
                                              (type-case ValueC v-f
                                                [StrV (s) (lookup-field ofvs s s-f)]
                                                [else (interp-error (string-append "Non-string in field lookup: " (pretty v-f)) s-f)])]
                                     [else (interp-error (string-append "Non-object in field lookup: " (pretty v-o)) s-f)])]
                              [ExceptionA (v-f s-f) (ExceptionA v-f s-f)])]
                       [ExceptionA (v-o s-o) (ExceptionA v-o s-o)])]
    
    [SetFieldC (o f v) (type-case AnswerC (interp-env o env)
                         [ValueA (v-o s-o)
                            (type-case AnswerC (interp-env f s-o)
                              [ValueA (v-f s-f)
                                (type-case AnswerC (interp-env v s-o)
                                   [ValueA (v-v s-v)
                                        (type-case ValueC v-o
                                          [ObjectV (ofvs) 
                                                   (type-case ValueC v-f
                                                     [StrV (s)
                                                           (ValueA (ObjectV (set-field ofvs s v-v)) s-v)]
                                                     [else (interp-error (string-append "Non-string in field update: " (pretty v-f)) s-v)])]
                                          [else (interp-error (string-append "Non-object in field update: " (pretty v-o)) s-v)])]
                                  [ExceptionA (v-v s-v) (ExceptionA v-v s-v)])]
                              [ExceptionA (v-f s-f) (ExceptionA v-f s-f)])]
                         [ExceptionA (v-o s-o) (ExceptionA v-o s-o)])]
    
    [Prim1C (o a) (type-case AnswerC (interp-env a env)
                    [ValueA (v-a s-a)
                         (case o
                               ('print (ValueA (let ((val (pretty v-a)))
                                                  (begin 
                                                    (display val)
                                                    v-a)) s-a))
                               ('tagof (ValueA (StrV (tagof v-a)) s-a)))]
                    [ExceptionA (v-a s-a) (ExceptionA v-a s-a)])]
    
    [Prim2C (o a1 a2) (type-case AnswerC (interp-env a1 env)
                         [ValueA (v-a1 s-a1)
                              (type-case AnswerC (interp-env a2 s-a1)
                                [ValueA (v-a2 s-a2)
                                     (case o 
                                       ['string+ (string+ v-a1 v-a2 s-a2)]
                                       ['num+ (num+ v-a1 v-a2 s-a2)]
                                       ['num- (num- v-a1 v-a2 s-a2)]
                                       ['== (ValueA (to-boolv (equal v-a1 v-a2)) s-a2)]
                                       ['< (less-than v-a1 v-a2 s-a2)]
                                       ['> (greater-than v-a1 v-a2 s-a2)])]
                                [ExceptionA (v-a2 s-a2) (ExceptionA v-a2 s-a2)])]
                        [ExceptionA (v-a1 s-a1) (ExceptionA v-a1 s-a1)])]))


(define (interp-fields (fs : (listof FieldC)) (env : Env)) : FieldEvalContext
  (cond 
    ((empty? fs) (GoodFEC (list) env))
    (else (type-case AnswerC (interp-env (fieldC-value (first fs)) env)
            [ValueA (v-e s-e)
                 (type-case FieldEvalContext (interp-fields (rest fs) s-e)
                   (GoodFEC (fvs s-n)
                           (GoodFEC (cons (fieldV (fieldC-name (first fs)) v-e) fvs) s-n))
                   [ExceptionFEC (ex) (ExceptionFEC ex)])]
            [ExceptionA (ex s-e) (ExceptionFEC (ExceptionA ex s-e))]))))

;; for each arg, extend the env and add to the store
(define (cascade-extend (ids : (listof symbol)) (es : (listof ExprC)) (env : Env)) : Context
  (cond 
    ((and (empty? es) (not (empty? ids))) (ExceptionContext (interp-error "Application failed with arity mismatch" env)))
    ((and (not (empty? es)) (empty? ids)) (ExceptionContext (interp-error "Application failed with arity mismatch" env)))
    ((and (empty? es) (empty? ids)) (GoodContext env))
    (else (type-case AnswerC (interp-env (first es) env)
            [ValueA (v-e s-e)
                   (cascade-extend (rest ids)
                                   (rest es)
                                   (extend-env (first ids) v-e s-e))]
            [ExceptionA (v-e s-e) (ExceptionContext (ExceptionA v-e s-e))]))))

(define (extend-with-env (env-add : Env) (env : Env)) : Env
  (extend-with-env-inner (hash-keys env-add) env-add env))

(define (extend-with-env-inner (keys : (listof symbol)) (env-add : Env) (env : Env)) : Env
  (cond
    ((empty? keys) env)
    (else 
       (type-case (optionof ValueC) (hash-ref env-add (first keys))
         [some (s) (extend-with-env-inner (rest keys) 
                                 env-add 
                                 (extend-env (first keys) s env))]
         [none () env]))))
  
(define (interp-apply (f : ExprC) (args : (listof ExprC)) (env : Env)) : AnswerC
  (type-case AnswerC (interp-env f env)
    [ValueA (v-f s-f)
         (type-case ValueC v-f
           [ClosureV (fa fb fe)
                     (type-case Context (cascade-extend fa args s-f) 
                       [GoodContext (e-a) (interp-env fb (extend-with-env fe e-a))]
                       [ExceptionContext (ex) ex])]
           [else (interp-error (string-append "Applied a non-function: " (pretty v-f)) env)])]
    [ExceptionA (v-f s-f) (ExceptionA v-f s-f)]))                 
                