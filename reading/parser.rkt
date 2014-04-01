#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(plusC (numC 1) (numC 2))

(define (parse (s : s-expression)) : ArithC
  (cond 
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         (else (error 'parse "invalid list input"))))]
    [else (error 'parse "invalid input")]))

(parse '(+ (* 1 2) (+ 2 3)))

