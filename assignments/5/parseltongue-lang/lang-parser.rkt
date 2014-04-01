#lang racket

(require "lang.rkt" racket/match)
(provide parse)

(define (parse syntax)
  (define (no-commas exprs) (filter (λ (e) (not (equal? e ","))) exprs))
  (define (no-semis exprs) (filter (λ (e) (not (equal? e ";"))) exprs))
  (define (parse-comma-list exprs) (map parse (no-commas exprs)))
  (define (parse-semi-list exprs) (map parse (no-semis exprs)))
  (define (parse-list syntax) (match syntax
      [(list 'expr-list e ...) (parse-comma-list e)]))
  (match syntax
    ;; NOTE(joe): These are all just shell expressions for
    ;; disambiguating in the lexer/parser
    [(or (list 'program e "")
         (list 'prog-expr e)
         (list 'expr e)
         (list 'sub-expr e)
         (list 'topexpr e)
         (list 'else-exp e)
         (list 'paren-exp "(" e ")")
         ;; NOTE(joe): this is a special case, where the braces were
         ;; essentially just parens in the seq-exp
         (list 'seq-exp "{" e "}"))
     (parse e)]
    [(list 'lhs e) (parse-lhs e)]
    [(list 'num-exp s) (Num (string->number s))]
    ;; NOTE(joe): parser includes the wrapping "" around the str,
    ;; so we strip them here
    [(list 'str-exp s)
     (define str (regexp-replace* (regexp-quote "\\n") s
                                (regexp-replace-quote "\n")))
     (Str (substring str 1 (- (string-length str) 1)))]
    [(list 'true-exp "true") (True)]
    [(list 'false-exp "false") (False)]
    [(list 'pre-op-exp "++" lhs)
     (PreInc (Id (string->symbol lhs)))]
    [(list 'pre-op-exp "--" lhs)
     (PreDec (Id (string->symbol lhs)))]
    [(list 'post-op-exp lhs "++")
     (PostInc (Id (string->symbol lhs)))]
    [(list 'post-op-exp lhs "--")
     (PostDec (Id (string->symbol lhs)))]

    [(list 'dot-method-exp obj "@" (? string? str) "(" args ")")
     (DotMethod (parse obj) (string->symbol str) (parse-list args))]
    [(list 'bracket-method-exp obj "@" "[" fld "]" "(" args ")")
     (BrackMethod (parse obj) (parse fld) (parse-list args))]
    [(list 'prim-exp (list 'op (? string? op)) "(" args ")")
     (Prim (string->symbol op) (parse-list args))]
    [(list 'app-exp fun "(" args ")")
     (App (parse fun) (parse-list args))]

    [(list 'func-exp "lambda" "(" (list 'namelist args ...) ")" body)
     (Func (map string->symbol (no-commas args)) (parse body))]
    [(list 'defvar-exp "defvar" ident "=" bind "in" body)
     (Defvar (string->symbol ident) (parse bind) (parse body))]
    [(list 'deffun-exp "deffun" ident
        "(" (list 'namelist args ...) ")" funbody "in" body)
     (Deffun (string->symbol ident) (map string->symbol (no-commas args))
             (parse funbody) (parse body))]

    [(list 'try-exp "try" body "catch" exn "in" catch)
     (TryCatch (parse body) (string->symbol exn) (parse catch))]
    [(list 'raise-exp "raise" exn) (Raise (parse exn))]

    ;; NOTE(joe): fields look like ('field-entry fld-expr ":" val-expr)
    ;; hence second and fourth below
    [(list 'obj-exp "{" (list 'field-list fields ...) "}")
     (define real-fields (no-commas fields))
     (Object (map cons (map second real-fields)
                       (map (compose parse fourth) real-fields)))]

    [(list 'seq-exp "{" (list 'expr-semi-list exprs ...) "}")
     (Seq (parse-semi-list exprs))]
    [(list 'expr-semi-list exprs ...)
     (Seq (parse-semi-list exprs))]
    [(list 'if-exp "if" test "then" e1 "else" e2)
     (If (parse test) (parse e1) (parse e2))]

    [(list 'var-exp "var" id "=" rhs)
     (Var (string->symbol id) (parse rhs))]
    [(list 'assign-exp lhs "=" rhs)
     (Assign (parse lhs) (parse rhs))]
    [(list 'prim-assign-exp lhs (? string? opeq) rhs)
     (let [(op (string->symbol (substring opeq 0 1)))]
       (PrimAssign op (parse lhs) (parse rhs)))]

    [(list 'for-exp "for" "(" init ";" check ";" update ")" body)
     (For (parse init) (parse check) (parse update) (parse body))]
    [(list 'while-exp "while" "(" test ")" body)
     (While (parse test) (parse body))]

))

(define (parse-lhs syntax)
  (match syntax
    ;; NOTE(joe): These are all just shell expressions for
    ;; disambiguating in the lexer/parser
    [(or (list 'program e "")
         (list 'expr e)
         (list 'sub-expr e)
         (list 'topexpr e)
         (list 'paren-exp e)
         (list 'lhs e))
     (parse-lhs e)]
    [(list 'id-exp x) (Id (string->symbol x))]
    [(list 'bracket-exp obj "[" fld "]")
     (Bracket (parse obj) (parse fld))]
    [(list 'dot-exp obj "." (? string? fld))
     (Dot (parse obj) (string->symbol fld))]
))

