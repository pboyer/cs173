#lang plai-typed

(define-type FieldP
  [fieldP (name : string) (value : ExprP)])

(define-type LHS
  [BracketLHS (obj : ExprP) (field : ExprP)]
  [DotLHS (obj : ExprP) (field : symbol)]
  [IdLHS (id : symbol)])

;; ExprPs are ParselTongue's toplevel:
(define-type ExprP
  [ObjectP (fields : (listof FieldP))]
  [DotP (obj : ExprP) (field : symbol)]
  [BracketP (obj : ExprP) (field : ExprP)]
  [DotMethodP (obj : ExprP) (field : symbol) (args : (listof ExprP))]
  [BrackMethodP (obj : ExprP) (field : ExprP) (args : (listof ExprP))]

  [FuncP (args : (listof symbol)) (body : ExprP)]
  [AppP (func : ExprP) (args : (listof ExprP))]
  [DefvarP (id : symbol) (bind : ExprP) (body : ExprP)]
  [DeffunP (name : symbol) (ids : (listof symbol)) (funbody : ExprP) (body : ExprP)]
  [IdP (name : symbol)]

  [WhileP (test : ExprP) (body : ExprP)]
  [ForP (init : ExprP) (test : ExprP) (update : ExprP) (body : ExprP)]

  [TryCatchP (body : ExprP) (param : symbol) (catch : ExprP)]
  [RaiseP (exn : ExprP)]

  [VarP (id : symbol) (value : ExprP)]
  [AssignP (lhs : LHS) (value : ExprP)]

  [SeqP (es : (listof ExprP))]
  [IfP (cond : ExprP) (then : ExprP) (else : ExprP)]

  [NumP (n : number)]
  [StrP (s : string)]
  [TrueP]
  [FalseP]

; An op is one of '+ '- '== 'print
  [PrimP (op : symbol) (args : (listof ExprP))]
; A PrimAssign op is one of '+ '-
  [PrimAssignP (op : symbol) (lhs : LHS) (value : ExprP)]

  [PreIncP (lhs : symbol)]
  [PostIncP (lhs : symbol)]
  [PreDecP (lhs : symbol)]
  [PostDecP (lhs : symbol)])

(define-type FieldC
  [fieldC (name : string) (value : ExprC)])

(define-type ExprC

  [ObjectC (fields : (listof FieldC))]
  [GetFieldC (obj : ExprC) (field : ExprC)]
  [SetFieldC (obj : ExprC) (field : ExprC) (value : ExprC)]

  [FuncC (args : (listof symbol)) (body : ExprC)]
  [AppC (func : ExprC) (args : (listof ExprC))]
  [LetC (id : symbol) (bind : ExprC) (body : ExprC)]
  [IdC (id : symbol)]
  [Set!C (id : symbol) (value : ExprC)]

  [IfC (cond : ExprC) (then : ExprC) (else : ExprC)]
  [SeqC (e1 : ExprC) (e2 : ExprC)]

  [TryCatchC (body : ExprC) (param : symbol) (catch : ExprC)]
  [ErrorC (expr : ExprC)]

  [NumC (n : number)]
  [StrC (s : string)]
  [TrueC]
  [FalseC]
  [UndefinedC]

; The core operations are 'string+ 'num+ 'num- '== '< '> 'print 'tagof
  [Prim1C (op : symbol) (arg : ExprC)]
  [Prim2C (op : symbol) (arg1 : ExprC) (arg2 : ExprC)])

(define-type-alias Env (hashof symbol ValueC))

(define-type FieldV
  [fieldV (name : string) (value : ValueC)])

(define-type AnswerC
  [ValueA (value : ValueC) (env : Env)]
  [ExceptionA (exn : ValueC) (env : Env)])

(define-type ValueC
  [UndefinedV]
  [ObjectV (fields : (listof FieldV))]
  [ClosureV (args : (listof symbol)) (body : ExprC) (env : Env)]
  [NumV (n : number)]
  [StrV (s : string)]
  [TrueV]
  [FalseV])

(define (pretty-object2 (fs : (listof FieldV))) : string
  (foldl string-append "" (map (lambda (n) (pretty-value (fieldV-value n))) fs)))

(define (pretty-value (v : ValueC)) : string
  (type-case ValueC v
    [UndefinedV () "undefined"]
    [ObjectV (fs) (pretty-object2 fs)]
    [ClosureV (a b e) "function"]
    [NumV (n) (to-string n)]
    [StrV (s) s]
    [TrueV () "true"]
    [FalseV () "false"]))

(define (interp-error str store)
  (ExceptionA (ObjectV (list (fieldV "message" (StrV str))
                             (fieldV "type" (StrV "PSL")))) store))
(define (desugar-error str)
  (ErrorC (ObjectC (list (fieldC "message" (StrC str))
                         (fieldC "type" (StrC "PSL"))))))

