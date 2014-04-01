#lang plai-typed

(require "../parseltongue-lang/typed-lang.rkt" "typed-desugar-js.rkt" "typed-interp-js.rkt")

(define (evaluate [exprP : ExprP]) : ValueC
  (interp (desugar exprP)))

(define pretty-printer pretty-value)

