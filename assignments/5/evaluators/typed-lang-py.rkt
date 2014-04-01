#lang plai-typed

(require "../parseltongue-lang/typed-lang.rkt")
(require (typed-in racket/base (number->string : (number -> string))))
(require (typed-in racket/base (display : (string -> void))))
(require (typed-in racket/base (raise-user-error : (string -> ValueC))))

(define-type FieldV
  [fieldV (name : string) (value : ValueC)])

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