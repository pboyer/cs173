#lang racket

;; Exprs are in the top-level language (which HAS a cool name -
;; ParselTongue) (26 forms):

(provide (all-defined-out))

(struct Object (fields) #:transparent)
(struct Bracket (obj field) #:transparent)
(struct Dot (obj field) #:transparent)
(struct DotMethod (obj field args) #:transparent)
(struct BrackMethod (obj field args) #:transparent)

(struct Func (args body) #:transparent)
(struct App (func args) #:transparent)
(struct Let (id bind body) #:transparent)
(struct Defvar (id bind body) #:transparent)
(struct Deffun (name ids funbody body) #:transparent)
(struct Id (name) #:transparent)

(struct While (test body) #:transparent)
(struct For (init test update body) #:transparent)

(struct TryCatch (body exn catch) #:transparent)
(struct Raise (exn) #:transparent)

(struct Var (id value) #:transparent)
(struct Assign (lhs value) #:transparent)

(struct Seq (es) #:transparent)
(struct If (cond then else) #:transparent)

(struct Num (n) #:transparent)
(struct Str (s) #:transparent)
(struct True () #:transparent)
(struct False () #:transparent)

;(define-type Op (U '+ '- '== 'print))
(struct Prim (op args) #:transparent)
(struct PrimAssign (op lhs value) #:transparent)

(struct PreInc (lhs) #:transparent)
(struct PostInc (lhs) #:transparent)
(struct PreDec (lhs) #:transparent)
(struct PostDec (lhs) #:transparent)


;; CExps are the core language (17 forms):

(struct CObject (fields ) #:transparent)
(struct CGetField (obj field) #:transparent)
(struct CSetField (obj field value) #:transparent)

(struct CFunc (args body) #:transparent)
(struct CApp (func args) #:transparent)
(struct CLet (id bind body) #:transparent)
(struct CId (id) #:transparent)

(struct CSet! (id value) #:transparent)

(struct CSeq (e1 e2) #:transparent)
(struct CIf (cond then else) #:transparent)

(struct CTryCatch (e1 exn body) #:transparent)
(struct CRaise (e) #:transparent)

(struct CNum (n) #:transparent)
(struct CStr (s) #:transparent)
(struct CTrue () #:transparent)
(struct CFalse () #:transparent)

(struct CError (expr) #:transparent)

;(define-type COp (U 'string+ 'num+ 'num- '== 'print))
(struct CPrim1 (op arg1) #:transparent)
(struct CPrim2 (op arg1 arg2) #:transparent)

;; These need to be shared between all interpreters

(struct Closure (fun env) #:transparent)

;; Objects can have strings or numbers as field names
(struct ObjVal (fields) #:transparent)
(struct Loc (l) #:transparent)

(struct exn:interp-failure exn (message) #:transparent)

