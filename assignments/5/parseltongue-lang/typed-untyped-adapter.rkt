#lang racket/base

(require racket/function racket/match racket/runtime-path)
(require "lang.rkt")
(require "typed-lang.rkt")
(provide untyped-value->typed-value typed-value->untyped-value
         untyped-core->typed-core typed-core->untyped-core
         untyped-psl->typed-psl)
          

(define (untyped-value->typed-value v)
  (define ut->t untyped-value->typed-value)
  (define (ut->t/field f)
    (fieldV (car f) (ut->t (cdr f))))
  (define (ut->t/binding name value)
    (bind name value))
  (match v
    [(CNum n) (NumV n)]
    [(CStr s) (StrC s)]
    [(CTrue) (TrueV)]
    [(CFalse) (FalseV)]
    [(ObjVal fields) (ObjectV (map ut->t/field fields))]
    [(Closure (CFunc args body) env)
     (ClosureV args
               (untyped-core->typed-core body)
               (hash-map env ut->t/binding))]))

(define (typed-value->untyped-value v)
  (define t->ut typed-value->untyped-value)
  (define (ut->t/field f)
    (cons (fieldV-name f) (t->ut (fieldV-value f))))
  (define (ut->t/binding b)
    (cons (bind-name b) (bind-value b)))
  (cond
    [(NumV? v) (CNum (NumV-n v))]
    [(StrV? v) (CStr (StrV-s v))]
    [(TrueV? v) (CTrue)]
    [(FalseV? v) (CFalse)]
    [(ObjectV? v) (ObjVal (map ut->t/field (ObjectV-fields v)))]
    [(ClosureV? v)
     (Closure (CFunc (ClosureV-args v) (ClosureV-body v))
              (make-immutable-hash (map ut->t/binding (ClosureV-env v))))]))

(define (untyped-core->typed-core e)
  (define ut->t untyped-core->typed-core)
  (define (ut->t/field f) (fieldC (car f) (ut->t (cdr f))))
  (match e
    [(CNum n) (NumC n)]
    [(CStr s) (StrC s)]
    [(CTrue) (TrueC)]
    [(CFalse) (FalseC)]

    [(CObject fields) (ObjectC (map ut->t/field fields))]
    [(CGetField obj field) (GetFieldC (ut->t obj) (ut->t field))]
    [(CSetField obj field newval)
     (SetFieldC (ut->t obj) (ut->t field) (ut->t newval))]

    [(CFunc args body) (FuncC args (ut->t body))]
    [(CApp func args) (AppC (ut->t func) (map ut->t args))]
    [(CLet id bind body) (LetC id (ut->t bind) (ut->t body))]
    [(CId id) (IdC id)]

    [(CSet! id value) (Set!C id (ut->t value))]

    [(CSeq e1 e2) (SeqC (ut->t e1) (ut->t e2))]
    [(CIf cond then else)
     (IfC (ut->t cond) (ut->t then) (ut->t else))]

    [(CError expr) (ErrorC (ut->t expr))]

    [(CPrim1 op arg1) (Prim1C op (ut->t arg1))]
    [(CPrim2 op arg1 arg2) (Prim2C op (ut->t arg1) (ut->t arg2))]))

;; typed-core->untyped-core : CExpr -> ExprC
(define (typed-core->untyped-core e)

  (define t->ut typed-core->untyped-core)
  (define (t->ut/field f)
    (cons (fieldC-name f) (t->ut (fieldC-value f))))
  (cond
    [(NumC? e) (CNum (NumC-n e))]
    [(StrC? e) (CStr (StrC-s e))]
    [(TrueC? e) (CTrue)]
    [(FalseC? e) (CFalse)]

    [(ObjectC? e) (CObject (map t->ut/field (ObjectC-fields e)))]
    [(GetFieldC? e)
     (CGetField (t->ut (GetFieldC-obj e))
                (t->ut (GetFieldC-field e)))]
    [(SetFieldC? e)
     (CSetField (t->ut (SetFieldC-obj e))
                (t->ut (SetFieldC-field e))
                (t->ut (SetFieldC-value e)))]

    [(FuncC? e)
     (CFunc (FuncC-args e) (t->ut (FuncC-body e)))]
    [(AppC? e)
     (CApp (t->ut (AppC-func e)) (map t->ut (AppC-args e)))]
    [(LetC? e)
     (CLet (LetC-id e) (t->ut (LetC-bind e)) (t->ut (LetC-body e)))]
    [(IdC? e) (CId (IdC-id e))]
    [(Set!C? e) (CSet! (Set!C-id e) (t->ut (Set!C-value e)))]
    [(IfC? e)
     (CIf (t->ut (IfC-cond e))
          (t->ut (IfC-then e))
          (t->ut (IfC-else e)))]
    [(SeqC? e)
     (CSeq (t->ut (SeqC-e1 e)) (t->ut (SeqC-e2 e)))]


    [(ErrorC? e) (CError (t->ut (ErrorC-expr e)))]

  ; The core operations are 'string+ 'num+ 'num- '== 'print 'tagof
    [(Prim1C? e)
     (CPrim1 (Prim1C-op e) (t->ut (Prim1C-arg e)))]
    [(Prim2C? e)
     (CPrim2 (Prim2C-op e)
             (t->ut (Prim2C-arg1 e))
             (t->ut (Prim2C-arg2 e)))]))


;; untyped-psl->typed-psl : Expr -> ExprP
(define (untyped-psl->typed-psl expr)

  (define ut->t untyped-psl->typed-psl)
  (define (ut->t/lhs e)
    (match e
      [(Bracket obj fld) (BracketLHS (ut->t obj) (ut->t fld))]
      [(Dot obj fld) (DotLHS (ut->t obj) fld)]
      [(Id x) (IdLHS x)]))
  (match expr
    [(Num n) (NumP n)]
    [(Str s) (StrP s)]
    [(True) (TrueP)]
    [(False) (FalseP)]

    [(Id x) (IdP x)]

    [(Prim op args) (PrimP op (map ut->t args))]

    [(If test then else) (IfP (ut->t test) (ut->t then) (ut->t else))]
    [(Seq exprs) (SeqP (map ut->t exprs))]

    [(Object fields)
     (ObjectP (map (λ (pair) (fieldP (car pair) (ut->t (cdr pair)))) fields))]

    [(Bracket obj fld)
     (BracketP (ut->t obj) (ut->t fld))]
    [(Dot obj fld)
     (DotP (ut->t obj) fld)]
    [(BrackMethod obj fld args)
     (BrackMethodP (ut->t obj) (ut->t fld) (map ut->t args))]
    [(DotMethod obj fld args)
     (DotMethodP (ut->t obj) fld (map ut->t args))]

    [(Var id value) (VarP id (ut->t value))]
    [(Assign lhs rhs) (AssignP (ut->t/lhs lhs) (ut->t rhs))]

    [(Func args body) (FuncP args (ut->t body))]
    [(App func exprs) (AppP (ut->t func) (map ut->t exprs))]

    [(Defvar id bind body) (DefvarP id (ut->t bind) (ut->t body))]
    [(Deffun name ids funbody body)
     (DeffunP name ids (ut->t funbody) (ut->t body))]

    [(While test body) (WhileP (ut->t test) (ut->t body))]
    [(For init test update body)
     (ForP (ut->t init) (ut->t test) (ut->t update) (ut->t body))]

    [(TryCatch body param catch)
     (TryCatchP (ut->t body) param (ut->t catch))]
    [(Raise exn) (RaiseP (ut->t exn))]
    
    [(PreInc (Id x)) (PreIncP x)]
    [(PostInc (Id x)) (PostIncP x)]
    [(PreDec (Id x)) (PreDecP x)]
    [(PostDec (Id x)) (PostDecP x)]

    [(PrimAssign op lhs newval) (PrimAssignP op (ut->t/lhs lhs) (ut->t newval))]))

