#lang plai-typed

(define-type MisspelledAnimal
  [caml (humps : number)]
  [yacc (height : number)])

(define p : MisspelledAnimal (yacc 123))

(define (big? (a : MisspelledAnimal)) : boolean
  (type-case MisspelledAnimal a
    (caml (h) (>= h 0))
    (yacc (h) (>= h 0))))


(define b : MisspelledAnimal (caml -2))
                              
(test (big? p) #t)
(test (big? b) #f)
(test p (yacc 123))

(yacc 12)
