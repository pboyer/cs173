#lang plai-typed

(define-type Binding
  [binding (name : symbol) (named-expr : CFWAE)])

(define-type CFWAE
  [num (n : number)]
  [binop (op : (number number -> number)) (lhs : CFWAE) (rhs : CFWAE)]
  [with (lob : (listof Binding)) (body : CFWAE)]
  [id (name : symbol)]
  [if0 (c : CFWAE) (t : CFWAE) (e : CFWAE)]
  [fun (args : (listof symbol)) (body : CFWAE)]
  [app (f : CFWAE) (args : (listof CFWAE))])

(define-type Env
  [mtEnv]
  [anEnv (name : symbol) (value : CFWAE-Value) (env : Env)])

;; parse : expression -> CFWAE
;; This procedure parses an expression into a CFWAE
(define (parse (sexp : s-expression)) : CFWAE
  (cond
    [(s-exp-number? sexp) (num (s-exp->number sexp))]
    [(s-exp-symbol? sexp) (id (s-exp->symbol sexp))]
    [(s-exp-list? sexp)
     (if (s-exp-symbol? (first (s-exp->list sexp)))
         (let* ([sl (s-exp->list sexp)] [a (s-exp->symbol (first sl))])
           (cond 
             ((is-binop? a) (binop (lookup-binop a) (parse (second sl)) (parse (third sl))))
             ((symbol=? a 'if) (if0 (parse (second sl)) (parse (third sl)) (parse (fourth sl))))
             ((symbol=? a 'fun) (fun (parse-funargs (second sl)) (parse (third sl))))
             ((symbol=? a 'with) (with (parse-withargs (second sl)) (parse (third sl))))
             (else (error 'parse "Another failure"))))
         (let ([sl (s-exp->list sexp)])
           (app (parse (first sl)) (map parse (rest sl)))))]
    [else (error 'parse "Parsing failure")]))

(define (is-binop? (s : symbol)) : boolean
  (case s
    ((+ * / -) #t)
    (else #f)))

(define (parse-funargs (sexp : s-expression)) : (listof symbol)
  (let ([sl (s-exp->list sexp)])
    (map (lambda (x) (s-exp->symbol x)) sl)))

(define (parse-withargs (sexp : s-expression)) : (listof Binding)
  (parse-withargs-list (s-exp->list sexp)))

(define (parse-withargs-list (l : (listof s-expression))) : (listof Binding)
  (cond
    ((empty? l) (list))
    (else 
     (let ([r (rest (rest l))])
       (cons 
        (binding (s-exp->symbol (first l)) (parse (second l))) 
        (parse-withargs-list r))))))

(define (lookup-binop (s : symbol)) : (number number -> number)
  (case s
    ((+) +)
    ((*) *)
    ((/) /)
    ((-) -)
    (else (error 'lookup-binop "Couldn't find binary op"))))

(define-type CFWAE-Value
  [numV (n : number)]
  [closureV (params : (listof symbol))
            (body : CFWAE)
            (env : Env)])

;; interp : CFWAE -> CFWAE-Value
;; This procedure interprets the given CFWAE and produces a result 
;; in the form of a CFWAE-Value (either a closuerV or a numV)
(define (interp (expr : CFWAE)) : CFWAE-Value
  (interpC expr (mtEnv)))

;; interpC : CFWAE Env -> CFWAE-Value
;; This internal procedure is called with an Environment
(define (interpC (expr : CFWAE) (env : Env)) : CFWAE-Value
  (type-case CFWAE expr
    [num (n) (numV n)]
    [binop (o l r) (numV (o (numV-n (interpC l env)) (numV-n (interpC r env))))]
    [with (lob e) (interpC e (extend-env-with-bindings lob env))]
    [id (n) (lookup n env)]
    [if0 (c t e) (if (= 0 (numV-n (interpC c env)))
                  (interpC t env) (interpC e env))]
    [fun (a b) (closureV a b env)]
    [app (f a) (let ((c (interpC f env))) 
                 (interpC (closureV-body c) (extend-env-with-args (closureV-params c) a env (closureV-env c))))]))


(define (exists-in-env (what : symbol) (in : Env)) : boolean
  (type-case Env in
    (mtEnv () #f)
    (anEnv (n v e) (if (symbol=? n what) #t (exists-in-env what e)))))

;; Evaluate a list of bindings an place them in the given environment
(define (extend-env-with-bindings (what : (listof Binding)) (in : Env)) : Env
  (cond
    ((empty? what) in)
    (else (if (exists-in-env (binding-name (first what)) in)
              (error 'extend-env-with-bindings "Duplicate identifier in let")
              (let ((bn (binding-name (first what))) (be (binding-named-expr (first what))))
                (anEnv bn (interpC be in) (extend-env-with-bindings (rest what) in)))))))

;; Lookup a symbol in an environment
(define (lookup (what : symbol) (in : Env)) : CFWAE-Value
  (type-case Env in
    [mtEnv () (error 'lookup "Unbound identitier")]
    [anEnv (n v e)
           (cond 
             ((symbol=? n what) v)
             (else (lookup what e)))]))

;; Evaluate each argument and add it to env-clos
;; Each argument is evaluated in env-eval
(define (extend-env-with-args (params : (listof symbol)) (args : (listof CFWAE)) (env-eval : Env) (env-clos : Env)) : Env
  (cond 
    ((empty? params) env-clos)
    (else (extend-env-with-args 
           (rest params) 
           (rest args)
           env-eval
           (anEnv (first params) (interpC (first args) env-eval) env-clos)))))
    
;; Tests for simple numbers
(test (interp (num 5)) (numV 5))
(test (interp (num -1)) (numV -1))

;; Tests for parsing
(test (parse '5) (num 5))
(test (parse '(+ 1 2)) (binop + (num 1) (num 2)))
(test (parse '"o") (id 'o))
(test (parse '(if (+ 1 2) (+ 3 4) (- 1 2))) (if0 (binop + (num 1) (num 2))
                                                 (binop + (num 3) (num 4))
                                                 (binop - (num 1) (num 2))))
(test (parse '(fun (a b) (+ a b))) (fun (list 'a 'b) (binop + (id 'a) (id 'b))))
(test (parse '(with (a (+ 1 2) b (- 3 5)) (+ a b))) (with 
                                                     (list 
                                                      (binding 'a (binop + (num 1) (num 2))) 
                                                      (binding 'b (binop - (num 3) (num 5)))) 
                                                        (binop + (id 'a) (id 'b))))
(test (parse '((fun (a b) (+ a b)) 8 9)) (app (fun (list 'a 'b) (binop + (id 'a) (id 'b))) (list (num 8) (num 9))))

;; Tests for num interp
(test (interp (num 5)) (numV 5))

;; Tests for interp binop
(test (interp (binop + (num 1) (num 2))) (numV 3))
(test (interp (binop / (binop * (num 8) (num 9)) (num 2))) (numV 36))
(test (interp (fun (list 'a 'b) (binop + (id 'a) (id 'b)))) (closureV 
                                                             (list 'a 'b) 
                                                             (binop + (id 'a) (id 'b))
                                                             (mtEnv)))
(test (interp (app (fun (list 'a 'b) (binop + (id 'a) (id 'b))) (list (num 5) (num 1)))) (numV 6))
(test (interp (with (list (binding 'a (num 2))) (id 'a))) (numV 2))
(test (interp (with (list (binding 'a (num 2)) (binding 'b (num 5))) (binop + (id 'a) (id 'b)))) (numV 7))
(test (interp (with (list (binding 'f (fun (list 'a 'b) (binop * (id 'a) (id 'b))))) (app (id 'f) (list (num 2) (num 4))))) (numV 8))

;; Combined tests
(test (interp (parse '((fun (a b) (+ a b)) 8 9))) (numV 17))
