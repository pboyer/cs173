#lang plai-typed

(require "../parseltongue-lang/typed-lang.rkt")
(require (typed-in racket/base (number->string : (number -> string))))
(require (typed-in racket/base (display : (string -> void))))
(require (typed-in racket/base (raise-user-error : (string -> ValueC))))

(define-type FieldV
  [fieldV (name : string) (value : ValueC)])

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

(define-type AnswerC
  [ValueA (value : ValueC) (store : Store)]
  [ExceptionA (exn : ValueC) (store : Store)])

(define (interp-error str store)
  (ExceptionA (ObjectV (list (fieldV "message" (StrV str))
                             (fieldV "type" (StrV "PSL")))) store))
(define (desugar-error str)
  (ErrorC (ObjectC (list (fieldC "message" (StrC str))
                         (fieldC "type" (StrC "PSL"))))))

(define-type-alias Env (listof Binding))
(define-type Binding
  [bind (name : symbol) (value : Location)])

(define-type-alias Location number)

(define-type-alias Store (hashof Location ValueC))

(define (lookup-answer (what : symbol) (in : Env) (sto : Store)) : AnswerC
  (type-case LookupResult (lookup what in sto)
    [GoodLookup (loc) (fetch (bind what loc) sto)]
    [BadLookup (ex) ex]))

(define-type LookupResult
  [GoodLookup (location : Location)]
  [BadLookup (exception : AnswerC)])

(define (lookup (what : symbol) (in : Env) (sto : Store)) : LookupResult
  (cond 
    [(empty? in) (BadLookup (interp-error (string-append "Unbound identifier: " (symbol->string what)) sto))]
    [(symbol=? what (bind-name (first in))) (GoodLookup (bind-value (first in)))]
    [else (lookup what (rest in) sto)]))

(define (fetch (where : Binding) (in : Store)) : AnswerC
  (type-case (optionof ValueC) (hash-ref in (bind-value where))
    [some (s) (ValueA s in)]
    [none () (interp-error (string-append "Unbound identifier: " (symbol->string (bind-name where))) in)]))

(define (override-store* (where : Location) (what : ValueC) (in : Store)) : Store
  (hash-set in where what))

(define mt-store (hash (list (values 0 (UndefinedV)))))
(define mt-env empty)
(define extend-env cons)

(define new-loc
  (local ([define last-loc (box 99)])
    (lambda ()
      (begin
        (set-box! last-loc (+ 1 (unbox last-loc)))
        (unbox last-loc)))))

(define (make-object (fields : (listof FieldV))) : ValueC
    (build-fields (list) fields))

(define (contains? (fields : (listof FieldV)) (field : FieldV)) : boolean
  (cond
    ((empty? fields) #f)
    ((string=? (fieldV-name (first fields)) (fieldV-name field)) #t)
    (else (contains? (rest fields) field))))

(define (build-fields (current : (listof FieldV)) (toadd : (listof FieldV))) : ValueC
  (cond 
    ((empty? toadd) (ObjectV current))
    (else (if (contains? current (first toadd)) 
              (raise-user-error "Multiply-defined fields")
              (ObjectV (ObjectV-fields (build-fields (cons (first toadd) current) (rest toadd))))))))

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


(define (greater-than (a : ValueC) (b : ValueC) (s : Store)) : AnswerC
  (if (both-of-type "number" a b)
      (ValueA (to-boolv (> (NumV-n a) (NumV-n b))) s)
      (interp-error "Both arguments to > must be numbers" s)))

(define (less-than (a : ValueC) (b : ValueC) (s : Store)) : AnswerC
  (if (both-of-type "number" a b)
      (ValueA (to-boolv (< (NumV-n a) (NumV-n b))) s)
      (interp-error "Both arguments to < must be numbers" s)))

(define (num- (a : ValueC) (b : ValueC) (s : Store)) : AnswerC
  (type-case ValueC a
    [NumV (n-a)
          (type-case ValueC b
            [NumV (n-b) (ValueA (NumV (- n-a n-b)) s)]
            [else (interp-error "The second operand to - must be a number" s)])]
    [else (interp-error "The first operand to - must be a number" s)]))

(define (num+ (a : ValueC) (b : ValueC) (s : Store)) : AnswerC
  (type-case ValueC a
    [NumV (n-a)
          (type-case ValueC b
            [NumV (n-b) (ValueA (NumV (+ n-a n-b)) s)]
            [else (interp-error "The second operand to + must be a number" s)])]
    [else (interp-error "The first operand to + must be a number" s)]))

(define (string+ (a : ValueC) (b : ValueC) (s : Store)) : AnswerC
  (type-case ValueC a
    [StrV (n-a)
          (type-case ValueC b
            [StrV (n-b) (ValueA (StrV (string-append n-a n-b)) s)]
            [else (interp-error "The second operand to + must be a string" s)])]
    [else (interp-error "The first operand to + must be a string" s)]))

(define (tagof (v : ValueC)) : string
  (type-case ValueC v
    [UndefinedV () "undefined"]
    [ObjectV (fs) "ok"]
    [ClosureV (a b e) "function"]
    [NumV (n) "number"]
    [StrV (s) "string"]
    [TrueV () "boolean"]
    [FalseV () "boolean"]))

(define (pretty-object (fs : (listof FieldV))) : string
  (foldl string-append "" (map (lambda (n) (pretty (fieldV-value n))) fs)))

(define (pretty (v : ValueC)) : string
  (type-case ValueC v
    [UndefinedV () "undefined"]
    [ObjectV (fs) (pretty-object fs)]
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
                                  
(define (lookup-field (fs : (listof FieldV)) (s : string) (store : Store)) : AnswerC
  (cond
    ((empty? fs) (interp-error (string-append "Field not found: " s) store))
    ((string=? s (fieldV-name (first fs))) (ValueA (fieldV-value (first fs)) store))
    (else (lookup-field (rest fs) s store))))

(define-type Context 
  [GoodContext (env : Env) (store : Store)]
  [ExceptionContext (exception : AnswerC)])

(define-type FieldEvalContext 
  [ExceptionFEC (exception : AnswerC)]
  [GoodFEC (fields : (listof FieldV)) (store : Store)])
