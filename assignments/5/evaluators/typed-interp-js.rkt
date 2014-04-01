#lang plai-typed

(require "../parseltongue-lang/typed-lang.rkt")
(require "interp-core-js.rkt")

(require (typed-in racket/base (raise-user-error : (string -> ValueC))))

(define (interp (exprC : ExprC)) : ValueC
  (type-case AnswerC (interp-env exprC mt-env mt-store)
    [ExceptionA (exn-val store) exn-val]
    [ValueA (val store) val]))

(define (interp-env (exprC : ExprC) (env : Env) (store : Store)) : AnswerC
  (type-case ExprC exprC
    
    [UndefinedC () (ValueA (UndefinedV) store)]
    
    [TryCatchC (b p c) (type-case AnswerC (interp-env b env store)
                         [ValueA (v s) (ValueA v s)]
                         [ExceptionA (v s) (let ((loc (new-loc))) 
                                             (interp-env c 
                                                         (cons (bind p loc) env) 
                                                         (override-store* loc v s)))])]
    
    [ErrorC (e) (type-case AnswerC (interp-env e env store)
                  [ValueA (v s) (ExceptionA v s)]
                  [ExceptionA (v s) (ExceptionA v s)])]

    [IdC (s) (lookup-answer s env store)]
    
    [NumC (n) (ValueA (NumV n) store)]
    [StrC (s) (ValueA (StrV s) store)]
    [TrueC () (ValueA (TrueV) store)]
    [FalseC () (ValueA (FalseV) store)]
    
    [IfC (c t e) (type-case AnswerC (interp-env c env store)
                   [ValueA (v-c s-c)
                        (type-case ValueC v-c
                          [FalseV () (interp-env e env s-c)]
                          {else (interp-env t env s-c)})]
                   [ExceptionA (v-c s-c) (ExceptionA v-c s-c)])]
    
    [SeqC (e1 e2) (type-case AnswerC (interp-env e1 env store)
                    [ValueA (v-e1 s-e1) (interp-env e2 env s-e1)]
                    [ExceptionA (v-e1 s-e1) (ExceptionA v-e1 s-e1)])]
    
    [Set!C (id e) (type-case AnswerC (interp-env e env store)
                    [ValueA (v-e s-e)
                            (type-case LookupResult (lookup id env store)
                              [GoodLookup (loc) (ValueA v-e (override-store* loc v-e s-e))]
                              [BadLookup (ex) ex])]
                    [ExceptionA (v-e s-e) (ExceptionA v-e s-e)])]
    
    [LetC (id e body) (type-case AnswerC (interp-env e env store)
                        [ValueA (v-e s-e)
                             (let ((loc (new-loc)))
                               (interp-env body 
                                            (cons (bind id loc) env)
                                            (override-store* loc v-e s-e)))]
                        [ExceptionA (v-e s-e) (ExceptionA v-e s-e)])]
    
    [AppC (f a) (interp-apply f a env store)]
    
    [FuncC (as b) (ValueA (ClosureV as b env) store)]
    
    [ObjectC (fs) (type-case FieldEvalContext (interp-fields fs env store)
                    [ExceptionFEC (ex) ex]
                    [GoodFEC (fvs s-fvs)
                         (ValueA (ObjectV fvs) s-fvs)])]
    
    [GetFieldC (o f) (type-case AnswerC (interp-env o env store)
                       [ValueA (v-o s-o)
                            (type-case AnswerC (interp-env f env s-o)
                              [ValueA (v-f s-f)
                                   (type-case ValueC v-o
                                     [ObjectV (ofvs) 
                                              (type-case ValueC v-f
                                                [StrV (s) (lookup-field ofvs s s-f)]
                                                [else (interp-error (string-append "Non-string in field lookup: " (pretty v-f)) s-f)])]
                                     [else (interp-error (string-append "Non-object in field lookup: " (pretty v-o)) s-f)])]
                              [ExceptionA (v-f s-f) (ExceptionA v-f s-f)])]
                       [ExceptionA (v-o s-o) (ExceptionA v-o s-o)])]
    
    [SetFieldC (o f v) (type-case AnswerC (interp-env o env store)
                         [ValueA (v-o s-o)
                            (type-case AnswerC (interp-env f env s-o)
                              [ValueA (v-f s-f)
                                (type-case AnswerC (interp-env v env s-f)
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
    
    [Prim1C (o a) (type-case AnswerC (interp-env a env store)
                    [ValueA (v-a s-a)
                         (case o
                           
                               ('print (ValueA (let ((val (pretty v-a)))
                                                  (begin 
                                                    (display val)
                                                    v-a)) s-a))
                               ('tagof (ValueA (StrV (tagof v-a)) s-a)))]
                    [ExceptionA (v-a s-a) (ExceptionA v-a s-a)])]
    
    [Prim2C (o a1 a2) (type-case AnswerC (interp-env a1 env store)
                         [ValueA (v-a1 s-a1)
                              (type-case AnswerC (interp-env a2 env store)
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


(define (interp-fields (fs : (listof FieldC)) (env : Env) (store : Store)) : FieldEvalContext
  (cond 
    ((empty? fs) (GoodFEC (list) store))
    (else (type-case AnswerC (interp-env (fieldC-value (first fs)) env store)
            [ValueA (v-e s-e)
                 (type-case FieldEvalContext (interp-fields (rest fs) env s-e)
                   (GoodFEC (fvs s-n)
                           (GoodFEC (cons (fieldV (fieldC-name (first fs)) v-e) fvs) s-n))
                   [ExceptionFEC (ex) (ExceptionFEC ex)])]
            [ExceptionA (ex s-e) (ExceptionFEC (ExceptionA ex s-e))]))))

;; for each arg, extend the env and add to the store
(define (cascade-extend (ids : (listof symbol)) (es : (listof ExprC)) (env : Env) (store : Store)) : Context
  (cond 
    ((and (empty? es) (not (empty? ids))) (ExceptionContext (interp-error "Application failed with arity mismatch" store)))
    ((and (not (empty? es)) (empty? ids)) (ExceptionContext (interp-error "Application failed with arity mismatch" store)))
    ((and (empty? es) (empty? ids)) (GoodContext env store))
    (else (type-case AnswerC (interp-env (first es) env store)
            [ValueA (v-e s-e)
                 (let ((loc (new-loc)))
                   (cascade-extend (rest ids)
                                   (rest es)
                                   (cons (bind (first ids) loc) env)
                                   (override-store* loc v-e s-e)))]
            [ExceptionA (v-e s-e) (ExceptionContext (ExceptionA v-e s-e))]))))

(define (extend-with-env (env-add : Env) (env : Env)) : Env
  (cond
    ((empty? env-add) env)
    (else (cons (first env-add) (extend-with-env (rest env-add) env)))))

(define (interp-apply (f : ExprC) (args : (listof ExprC)) (env : Env) (store : Store)) : AnswerC
  (type-case AnswerC (interp-env f env store)
    [ValueA (v-f s-f)
         (type-case ValueC v-f
           [ClosureV (fa fb fe)
                     (type-case Context (cascade-extend fa args env s-f) 
                       [GoodContext (e-a s-a) (interp-env fb (extend-with-env fe e-a) s-a)]
                       [ExceptionContext (ex) ex])]
           [else (interp-error (string-append "Applied a non-function: " (pretty v-f)) store)])]
    [ExceptionA (v-f s-f) (ExceptionA v-f s-f)]))                 
                