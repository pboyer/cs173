#lang racket

(require "lang-grammar.rkt" rackunit racket/match racket/generator
         parser-tools/lex "../lib/python-tokenizer/main.rkt")
(provide get-syntax)

(define (lex ip)
  (port-count-lines! ip)
  (lambda ()
    (default-lex/1 ip)))

(define (mk-syntax str)
  (syntax->datum (parse #f (lex (open-input-string str)))))

(define (adapt-python-tokenizer ip)
  (define tokens (sequence->generator (generate-tokens ip)))
  (lambda ()
    (let loop ()
      (define next-token (tokens))
      (match next-token
        [(list type text (list start-line start-col) (list end-line end-col) rest-string)
         ;; FIXME: improve the Python tokenizer to hold offsets too.
         (define start-pos (position #f start-line start-col))
         (define end-pos (position #f end-line end-col))
         (define (pt token) (position-token token start-pos end-pos))
         (case type
             [(NAME) 
              (cond [(hash-has-key? all-tokens-hash (string->symbol text))
                     (pt ((hash-ref all-tokens-hash (string->symbol text)) text))]
                    [else
                     (pt (token-NAME text))])]
             [(OP)
              (pt ((hash-ref all-tokens-hash (string->symbol text)) text))]
             [(NUMBER) 
              (pt (token-NUMBER text))]
             [(STRING) 
              (pt (token-STRING text))]
             [(COMMENT) (loop)]
             [(NL) (loop)]
             [(NEWLINE) (loop)]
             [(DEDENT) (loop)]
             [(INDENT) (loop)]
             [(ERRORTOKEN)
              (error 'uh-oh)]
             [(ENDMARKER) 
              (token-ENDMARKER text)])]
        [(? void)
         (token-EOF eof)]))))

(define (get-syntax name input-port)
  (define datum (syntax->datum (parse name (adapt-python-tokenizer input-port))))
  (close-input-port input-port)
  datum)

