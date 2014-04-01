#lang racket

(require racket/cmdline)
(require racket/runtime-path)
(require "full-eval.rkt" "run-tests.rkt")

(define the-stdin (current-input-port))
(define the-stdout (current-output-port))
(define the-stderr (current-error-port))
(define printer results-summary)

(define (do-with-ports f)
  (parameterize
    ([current-input-port the-stdin]
     [current-output-port the-stdout]
     [current-error-port the-stderr])
  (f)))

(define the-evaluator 'no-evaluator-set)
(define the-pretty-printer 'no-printer-set)

(define (mk-full-evaluator evaluate pretty-print)
  (mk-proc-eval/full "interp"
                     evaluate
                     pretty-print
                     exn:fail?))

(define (get-full-evaluator) (mk-full-evaluator the-evaluator the-pretty-printer))

(command-line
  #:once-each
  ("--stdin" filename "Provide a file to use as stdin"
    (set! the-stdin (open-input-file filename)))
  ("--stdout" filename "Provide a file to use as stdout"
    (set! the-stdout (open-output-file filename)))
  ("--stderr" filename "Provide a file to use as stderr"
    (set! the-stderr (open-output-file filename)))
  ("--brief" "Briefer test output"
    (set! printer simple-summary))

  ("--set-evaluator" filename "Set your evaluator"
    (set! the-evaluator (mk-dynamic-evaluator filename))
    (set! the-pretty-printer (dynamic-require filename 'pretty-printer)))

  ("--interp" "Run a program with the configured evaluator"
    (do-with-ports
      (λ ()
        (define output ((get-full-evaluator) "stdin" (current-input-port)))
        (display (car output))
        (display (cdr output) (current-error-port)))))

  ("--test" suite "Run a test suite with the configured evaluator"
    (printf "~a\n"
      (printer (run-tests (get-full-evaluator) suite)))))

