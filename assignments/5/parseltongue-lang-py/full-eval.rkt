#lang racket

(require "lang.rkt"
         "lang-parser.rkt"
         "lang-get-syntax.rkt"
         "typed-untyped-adapter.rkt")

(require racket/runtime-path)

(provide mk-full-eval
         mk-proc-eval/full
         mk-cmdline-eval
         mk-typed-desugar
         mk-proc-desugar
         mk-typed-interp
         mk-untyped-interp
         mk-untyped-desugar
         mk-dynamic-evaluator)

(define (mk-full-eval interp desugar)
  (λ (name port)
    (interp (desugar (parse (get-syntax name port))))))

(define (mk-evaluator evaluate)
  (λ (name port)
    (evaluate (untyped-psl->typed-psl (parse (get-syntax name port))))))

(define (mk-dynamic-evaluator filename)
  (mk-evaluator (dynamic-require filename 'evaluate)))

(define (mk-typed-desugar desugar-file)
  (mk-proc-desugar (dynamic-require desugar-file 'desugar)))

(define (mk-proc-desugar desugar)
  (λ (syntax)
    (desugar (untyped-psl->typed-psl syntax))))

(define (mk-untyped-desugar desugar)
  (λ (syntax)
    (untyped-core->typed-core (desugar syntax))))

(define (mk-untyped-interp interp)
  (λ (e)
    (interp (typed-core->untyped-core e))))

(define (mk-typed-interp interp-file)
  (define interp (dynamic-require interp-file 'interp))
  (λ (e)
    (typed-value->untyped-value (interp e))))
  
(define (compose-eval desugar interp)
  (λ (name port)
    (interp (desugar (parse (get-syntax name port))))))

(define (mk-proc-eval/full name evaluator pretty exn-type?)
  (λ (name port)
    (define out (open-output-string "eval-out"))
    (define err (open-output-string "eval-err"))
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (with-handlers
        ([exn:interp-failure?
         (λ (exn)
           (display (format "~a\n" (exn-message exn))
                    (current-error-port)))]
         [exn:fail?
         (λ (exn)
           (display (format "~a\n" (exn-message exn))
                    (current-error-port)))])
        (define result (evaluator name port))
        (display (format "~a\n" (pretty result))))
      (define result (cons (get-output-string out) (get-output-string err)))
      (close-output-port out)
      (close-output-port err)
      result)))
  
(define (mk-cmdline-eval some-cmdline-path some-cmdline-args)
  (λ (name port)
    (define input-src (port->string port))
    (define proc (process* some-cmdline-path some-cmdline-args))
    (display input-src (second proc))
    (display eof (second proc))
    (flush-output (second proc))
    (close-output-port (second proc))
    ((fifth proc) 'wait)
    (define stdout (port->string (first proc)))
    (define stderr (port->string (fourth proc)))
    (close-input-port (fourth proc))
    (close-input-port (first proc))
    (cons stdout stderr)))

