#lang racket
;;imports
(require "ejercicio1.rkt")
(require "ejercicio2.rkt")

;;EVALUARSAT:
;; se encarga de extraer la la cantidad de variables, generar las combinaciones para la cantidad de variables,
;;extrae la and-exp para posteriormente invocar eval-combinaciones.

(define EVALUARSAT
 (lambda (exp)
   (eval-combinaciones (generar-combinaciones (fnc->var exp)) (fnc->clausulas exp))
     )
 )



;;EVAL-COMBINACIONES:
;;recorre esa lista de combinaciones y la evalua en la and expresion
;; si alguna de las combinaciones satisface la expresion retorna la combinacion
;; de lo contrario retorna una lista vacia


(define eval-combinaciones
  (lambda (combinaciones exp)
    (cond
      [(empty? combinaciones) (list 'insatisfactible '())]
      [(eval-and exp (car combinaciones)) (list 'satisfactible (car combinaciones))]
      [else (eval-combinaciones (cdr combinaciones) exp)]
      )
    ))


;;Recibe una lista de valores de un or-exp y evalua aplicando la funcion or, retornando #f o #v segun sea el caso, teniendo en cuenta
;;el conjunto de valores de verdad 
;;entradas:
;; exp = or-exp
;; values = lista de booleanos
;; retorna un valor booleano
(define eval-or
  (lambda (exp values)
  (cond
    [(empty? exp) #f]
    [(< (car exp) 0) (or (not (list-ref values (- (* -1 (car exp)) 1))) (eval-or (cdr exp) values))]
    [(> (car exp) 0) (or (list-ref values (- (car exp) 1)) (eval-or (cdr exp) values))]
    ))
  )
;;EJEMPLOS EVAL-OR

(define ejemploevalor1
  (eval-or '(1 2 3) '(#f #f #t))
  )

;;Se encarga de evaluar la expresion and con un conjunto de valores de verdad, a su vez haciendo el llamado para evaluar cada or-exp interna
;;recibe un and-exp y retorna un booleano segun sea el caso.
(define eval-and
  (lambda (exp values)
    (cond
      [(empty? exp) #t]
      [(list? (car exp)) (and (eval-or (or->varlist (car exp)) values) (eval-and (cdr exp) values))]
      [else (eval-and (cdr exp) values) ]
      )
    )
  )
;;EJEMPLOS EVAL-AND
(define ejemploevaland1
  (eval-and '(and-exp (or-exp 1 2) (or-exp -1 2) (or-exp 2)) '(#f #t)))
(define ejemploevaland2
  (eval-and '(and-exp (or-exp 1 2) (or-exp -1 2) (or-exp 2)) '(#f #t)))
(define ejemploevaland3
  (eval-and '(and-exp (or-exp 1 2 3) (or-exp -2 -2 1)) '(#f #t #t)))

;;La funcion generar combinaciones recibe un n entero y genera las posibles combinaciones de #t y #f para n variables.
(define generar-combinaciones 
  (lambda (n)
  (define (combinaciones-aux n)
    (if (= n 0)
        (list '())
        (append (map (lambda (c) (cons #f c)) (combinaciones-aux (- n 1)))
                (map (lambda (c) (cons #t c)) (combinaciones-aux (- n 1))))))

  (combinaciones-aux n)))
;;EJEMPLOS GENERAR-COMBINACIONES

(define ejemploc1
  (generar-combinaciones 1)
  )

(define ejemploc2
  (generar-combinaciones 3)
  )

(define ejemploc3
  (generar-combinaciones 5)
  )

;;EJEMPLOS EVALUARSAT EJEMPLOS
(EVALUARSAT (PARSEBNF '(FNC 4 ((1 or -2 or 3 or 4) and (-2 or 3) and
(-1 or -2 or -3) and (3 or 4) and (2)))))

(EVALUARSAT (PARSEBNF '(FNC 2 ((1 or 2) and (-1) and (-2)))))

(EVALUARSAT (PARSEBNF '(FNC 3 ((1 or 2 or 3) and (-2) and (1 or 3)))))
