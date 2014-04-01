#lang plai-typed

(require "../parseltongue-lang/typed-lang.rkt" "typed-desugar-exn.rkt" "typed-interp-exn.rkt")

(define (evaluate [exprP : ExprP]) : ValueC
  (interp (desugar exprP)))

(define pretty-printer pretty-value)
