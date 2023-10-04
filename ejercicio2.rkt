#lang racket
;;integrantes:
;: Nicolas Garces Larrahondo 2180066
;: Juan Pablo Ante 2140132
;: Hector Diaz 2310001
;:
;;export
(provide (all-defined-out))
;;imports
(require "ejercicio1.rkt")

;;PARSER
;;Encargado de recibir la representacion en gramatica concreta y retornar gramatica abstracta definida en el ejercicio1

(define PARSEBNF
  (lambda (exp)
    (cond
      [(empty? exp) empty]
      [(equal? (car exp) 'FNC) (fnc (cadr exp) (and-exp (filter-and-exp (caddr exp))))]
      )
    )
  )

;; se encarga de convertir la lista en gramatica concreta de expresiones and en gramatica abstracta
;; retorna una lista de listas de or-exp

(define filter-and-exp
  (lambda (exp)
  (cond
    [(empty? exp) empty]
    [(list? (car exp)) (append (list (or-exp (filter-or-exp (car exp)))) (filter-and-exp (cdr exp)))]
    [else (filter-and-exp (cdr exp))]
    )))

;;recibe expresiones or en gramatica abstracta y retorna una lista de los elementos para posteriormente construir el
;;or-exp en gramatica abstracta
(define filter-or-exp
  (lambda (exp)
  (cond
    [(empty? exp) empty]
    [(number? (car exp)) (append (list (car exp)) (filter-or-exp (cdr exp)))]
    [else (filter-or-exp (cdr exp))]
    )))
;;EJEMPLOS PARA EL PARSEBNF
(define ejemplobnf1
  (PARSEBNF '(FNC 4 ((1 or -2 or 3 or 4) and (-2 or 3) and
                                         (-1 or -2 or -3) and (3 or 4) and (2))))
  )

(define ejemplobnf2
  (PARSEBNF '(FNC 2 ((1 or -2) and (-2) and
                                         (-1 or -2) and (1 or -1) and (2))))
  )

(define ejemplobnf3
  (PARSEBNF '(FNC 2 ((2 or -2) and (-2 or 1) and
                                         )))
  )

;;UNPARSER
;; Se encarga de recibir una gramatica abstracta y la convierte en gramatica concreta

(define UNPARSEBNF
  (lambda (exp)
    (cond
      [(equal? (car exp) 'FNC) (append (list 'FNC (fnc->var exp)) (list (UNPARSECLAUSULAS (fnc->clausulas exp))))]
      )
    ))

;;retorna la sintaxis concreta para una entrada de and-exp en gramatica abstracta
(define UNPARSECLAUSULAS
  (lambda (exp)
    (cond
      [(empty? exp) empty]
      [(empty? (cdr exp)) (append (list (UNPARSEOR (or->varlist (car exp)))) (UNPARSECLAUSULAS (cdr exp)))]
      [(list? (car exp)) (append (list (UNPARSEOR (or->varlist (car exp))) 'and) (UNPARSECLAUSULAS (cdr exp)))]
      [else (UNPARSECLAUSULAS (cdr exp))]
      )
    ))

;;retorna la sintaxis concreta para una entrada en sintaxis abstracta de or-exp
(define UNPARSEOR
  (lambda (exp)
  (cond
    [(empty? exp) empty]
    [(empty? (cdr exp)) (append (list (car exp)) (UNPARSEOR (cdr exp)))]
    [(number? (car exp)) (append (list (car exp) 'or) (UNPARSEOR (cdr exp)))]
    ))
  )
;;ejemplos unparcebnf

(define ejemplounparcebnf1
  (UNPARSEBNF ejemplobnf1)
  )

(define ejemplounparcebnf2
  (UNPARSEBNF ejemplobnf2)
  )

(define ejemplounparcebnf3
  (UNPARSEBNF ejemplobnf3)
  )