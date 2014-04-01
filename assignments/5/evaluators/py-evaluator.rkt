#lang plai-typed

(require "../parseltongue-lang-py/typed-lang.rkt" "typed-desugar-py.rkt" "typed-interp-py.rkt")

(define (evaluate [exprP : ExprP]) : ValueC
  (interp (desugar exprP)))

(define pretty-printer pretty-value)
