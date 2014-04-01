#lang plai-typed

(require "../parseltongue-lang/typed-lang.rkt")
(require "interp-core-exn.rkt")

(define (desugar (exprP : ExprP)) : ExprC
  (type-case ExprP exprP

    [TryCatchP (b p c) (TryCatchC (desugar b) p (desugar c))]
    [RaiseP (r) (ErrorC (desugar r))]
    [VarP (i v) (ErrorC (StrC "Not supported"))]
    
    [NumP (n) (NumC n)]
    [StrP (s) (StrC s)]
    [TrueP () (TrueC)]
    [FalseP () (FalseC)]
    
    [ObjectP (fs) (ObjectC (map 
                            (lambda (x) 
                               (type-case FieldP x 
                                 (fieldP (n v)
                                   (fieldC n (desugar v))))) 
                            fs))]
    [DotP (o f) (GetFieldC (desugar o) (StrC (symbol->string f)))]
    [BracketP (o f) (GetFieldC (desugar o) (desugar f))]
    
    [DotMethodP (o f as) (LetC 'this-object (desugar o) 
                               (AppC (GetFieldC (IdC 'this-object) (StrC (symbol->string f))) (cons (IdC 'this-object) (map desugar as))))]
    
    [BrackMethodP (o f as) (LetC 'this-object (desugar o)
                                 (AppC (GetFieldC (IdC 'this-object) (desugar f)) (cons (IdC 'this-object) (map desugar as))))]
    
    [FuncP (a b) (FuncC a (desugar b))]
    [AppP (f a) (AppC (desugar f) (map desugar a))]
    
    [DefvarP (i bind bod) (LetC i (desugar bind) (desugar bod))]
    [DeffunP (n ids fbod bod) (LetC n (FuncC (list) (desugar-error "Dummy function"))
                                    (SeqC (Set!C n (FuncC ids (desugar fbod))) 
                                          (desugar bod)))] 
    
    [IdP (n) (IdC n)]

    [WhileP (test body)
          ;; dummy-fun will tell us it was called if we do so accidentally
          (local ([define dummy-fun (FuncC (list) (desugar-error "Dummy function"))])
          (IfC (desugar test)

               ;; while-var will hold the actual function once we tie
               ;; everything together
               (LetC 'while-var dummy-fun
                 (LetC 'while-func

                   ;; this function does the real work - it runs the body of
                   ;; the while loop, then re-runs it if the test is true, and
                   ;; stops if its false
                   (FuncC (list)
                     (LetC 'temp-var
                       (desugar body)
                       (IfC (desugar test)
                            (AppC (IdC 'while-var) (list))
                            (IdC 'temp-var))))

                   ;; The Set!C here makes sure that 'while-var will resolve
                   ;; to the right value later, and the AppC kicks things off
                   (SeqC (Set!C 'while-var (IdC 'while-func))
                         (AppC (IdC 'while-var) (list)))))

               (FalseC)))]
    
    [ForP (init test update body) 
          ;; Create initial dummy function
          (local ([define dummy-fun (FuncC (list) (desugar-error "Dummy function"))])
            ;; Initialize init as v, then evaluate test to v2
            (LetC 'v (desugar init)
                  (LetC 'v2 (desugar test)
                        ;; check the test value
                        (IfC (IdC 'v2)
                             ;; test is true
                             (LetC 'for-var dummy-fun
                                   (LetC 'for-fun 
                                         (FuncC (list)
                                                (LetC 'v3 (desugar body)
                                                      (LetC 'v4 (desugar update)
                                                            (LetC 'v5 (desugar test)
                                                                  (IfC (IdC 'v5)
                                                                       (AppC (IdC 'for-var) (list))
                                                                       (IdC 'v3))))))
                                                       
                                         (SeqC (Set!C 'for-var (IdC 'for-fun))
                                               (AppC (IdC 'for-var) (list)))))
                             ;; test is false, return v
                             (IdC 'v)))))]
    [AssignP (l r) (type-case LHS l
                     [BracketLHS (o f) (SetFieldC (desugar o) (desugar f) (desugar r))]
                     [DotLHS (o f) (SetFieldC (desugar o) (StrC (symbol->string f)) (desugar r))]
                     [IdLHS (i) (Set!C i (desugar r))])]
    
    [IfP (c t e) (IfC (desugar c) (desugar t) (desugar e))]
    [SeqP (e) (seqrec e)]
    
    [PrimP (op args)
        (case op
          ['- (cond
                [(= 0 (length args)) (desugar-error "Empty list for prim op")]
                [(< 0 (length args)) (desugar-subtract args)])]
          ['+ (cond
                [(= 0 (length args)) (desugar-error "Empty list for prim op")]
                [(< 0 (length args)) (desugar-add args)])]
          ['> (cond 
                ((= (length args) 2) (Prim2C '> (desugar (first args)) (desugar (second args))))
                (else (desugar-error "Bad primop")))]
          ['< (cond 
                ((= (length args) 2) (Prim2C '< (desugar (first args)) (desugar (second args))))
                (else (desugar-error "Bad primop")))]
          ['== (cond 
                ((= (length args) 2) (Prim2C '== (desugar (first args)) (desugar (second args))))
                (else (desugar-error "Bad arguments to ==")))]
          ['print (cond
                    [(= 1 (length args)) (Prim1C 'print (desugar (first args)))]
                    (else (desugar-error "Only single argument allowed for print")))]
          ['tagof (cond
                    [(= 1 (length args)) (Prim1C 'tagof (desugar (first args)))]
                    (else (desugar-error "Only single argument allowed for tagof")))]
          [else (ErrorC (StrC "Invalid operation"))])]
    
    [PrimAssignP (o l v) (type-case LHS l
                           [BracketLHS (ob f) 
                                       (LetC 'obj (desugar ob) 
                                             (LetC 'fv (desugar f) 
                                                   (SetFieldC (IdC 'obj) (IdC 'fv)
                                                              (prim-assign o (GetFieldC (IdC 'obj) (IdC 'fv)) (desugar v)))))]
                           [DotLHS (ob fs) 
                                   (LetC 'obj (desugar ob) 
                                         (SetFieldC (IdC 'obj) (StrC (symbol->string fs))
                                                    (prim-assign o (GetFieldC (IdC 'obj) (StrC (symbol->string fs))) (desugar v))))]
                           [IdLHS (i) (Set!C i (prim-assign o (IdC i) (desugar v)))])]

    [PreIncP (l) (SeqC (Set!C l (Prim2C 'num+ (NumC 1) (IdC l))) (IdC l))]
    [PostIncP (l) (LetC 'o (IdC l) (SeqC (Set!C l (Prim2C 'num+ (NumC 1) (IdC 'o))) (IdC 'o)))]
    [PreDecP (l) (SeqC (Set!C l (Prim2C 'num- (IdC l) (NumC 1))) (IdC l))]
    [PostDecP (l) (LetC 'o (IdC l) (SeqC (Set!C l (Prim2C 'num- (IdC 'o) (NumC 1))) (IdC 'o)))]))


(define (make-ids (n : number)) : (listof symbol)
  (build-list n (lambda (n) (string->symbol (string-append "var-" (to-string n))))))

;; cascade-lets will build up the nested lets, and use body as the
;; eventual body, preserving order of evaluation of the expressions
(define (cascade-lets (ids : (listof symbol))
                      (exprs : (listof ExprC))
                      (body : ExprC)) : ExprC
  (cond [(empty? ids) body]
        [(cons? ids)
         (LetC (first ids) (first exprs) (cascade-lets (rest ids) (rest exprs) body))]))

;; check-type builds an expression that checks the type of the expression
;; given as an argument
(define (check-type (expr : ExprC) (type : string)) : ExprC
  (Prim2C '== (Prim1C 'tagof expr) (StrC type)))

;; and builds up an and expression from its two pieces
(define (and (expr1 : ExprC) (expr2 : ExprC)) : ExprC
  (IfC expr1 expr2 (FalseC)))

(define (seqrec (exps : (listof ExprP))) : ExprC
  (cond 
    ((= (length exps) 1) (desugar (first exps)))
    ((= (length exps) 2) (SeqC (desugar (first exps)) (desugar (second exps))))
    (else (SeqC (desugar (first exps)) (seqrec (rest exps))))))

;; all builds up a series of ands over the expression arguments
(define (all (exprs : (listof ExprC))) : ExprC
  (foldl (lambda (exp result) (and exp result)) (TrueC) exprs))

;; map-subtract builds an expression that maps a Prim2C op over a collection of expressions
(define (map-op (op : symbol) (exprs : (listof ExprC))) : ExprC
  (foldl (lambda (expr result) (Prim2C op result expr)) (first exprs) (rest exprs)))

(define (desugar-subtract (args : (listof ExprP))) : ExprC
  (local ([define ids (make-ids (length args))]
          [define id-exps (map IdC ids)])
    (cascade-lets ids (map desugar args)
      (IfC (all (map (lambda (e) (check-type e "number")) id-exps))
           (map-op 'num- id-exps)
           (desugar-error "Bad arguments to -")))))

(define (desugar-add (args : (listof ExprP))) : ExprC
   (local ([define ids (make-ids (length args))]
          [define id-exps (map IdC ids)])
    (cascade-lets ids (map desugar args)
      ;; check if the first item is a number
      (IfC (check-type (first id-exps) "number")
           ;; if the first item is a number, then enforce that on all operands
           (IfC (all (map (lambda (e) (check-type e "number")) id-exps))
                (map-op 'num+ id-exps)
                (desugar-error "Bad arguments to +"))
           ;; otherwise, check if it's a string
           (IfC (check-type (first id-exps) "string")
                ;; if the first item is a string, then enforce that on all operands
                (IfC (all (map (lambda (e) (check-type e "string")) id-exps))
                     (map-op 'string+ id-exps)
                     (ErrorC (StrC "Bad arguments to +")))
                ;; if it's neither a string or a number, then fail
                (desugar-error "Bad arguments to +"))))))

(define (prim-assign (o : symbol) (lhs : ExprC) (rhs : ExprC)) : ExprC
  (case o
    ['- 
     (IfC (and (check-type lhs "number") (check-type rhs "number"))
          (Prim2C 'num- lhs rhs)
          (desugar-error "Bad arguments to -"))]
    ['+ 
     (IfC (and (check-type lhs "number") (check-type rhs "number"))
          (Prim2C 'num+ lhs rhs)
          (IfC (and (check-type lhs "string") (check-type rhs "string"))
               (Prim2C 'string+ lhs rhs)
               (desugar-error "Bad arguments to +")))]))