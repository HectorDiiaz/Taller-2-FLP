#lang racket
;;fnc       ::= 'FNC <numero> (<and-exp>+)
;;<and-exp> ::= (<or-exp>+)
;;<or-exp>  ::= (<number>+) 

;;Constructores
(define fnc
  (lambda (n lista)
  (cond
    [(empty? lista) empty]
    [else (append (list 'FNC  n) (and-exp lista))]
    )
  ))

(define or-exp
  (
   lambda(lista)
    (cond
      [(empty? lista) empty]
      [(empty? (cdr lista)) ( list (car lista)) ]
      [else (append
             (list (car lista) 'OR) (or-exp (cdr lista)))]
     )
    ))

(define and-exp
  (
   lambda(lista)
    (cond
      [(empty? lista) empty]
      [(empty? (cdr lista)) ( list (or-exp (car lista)))]
      [else (append
             (list (or-exp (car lista)) 'AND)
             (and-exp (cdr lista)))]
     )
    ))



