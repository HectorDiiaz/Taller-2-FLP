#lang eopl
;;integrantes:
;: Nicolas Garces Larrahondo 2180066
;: Juan Pablo Ante 2140132
;: Hector Diaz 2310001
;:
;;exports
(provide (all-defined-out))

;;GRAMATICA:
;;fnc      ::= 'FNC <number> <and-exp> 
;;<and-exp> ::= 'and-exp <or-exp> {<or-exp>}*
;;<or-exp>  ::= 'or-exp <number> {<number>}* 

;;1.1) gramatica a partir de listas


;;Se encargan de generar la gramatica abstracta usando listas.
;;CONSTRUCTORES

(define fnc
  (lambda (n lista)
    (list 'FNC n lista)
    ))

(define or-exp
  (lambda (lista)
    (append (list 'or-exp) lista)
    ))

(define and-exp
  (lambda (lista)
  (append (list 'and-exp) lista)
  ))


;;EJEMPLOS DE APLICACION DE LOS CONSTRUCTORES

(define ejemplo1
 (fnc 2
       (and-exp (list
                 (or-exp (list 1 2))
                 (or-exp (list -1 2))
                 (or-exp (list 2))
                 ))))
(define ejemplo2
 (fnc 3
       (and-exp (list
                 (or-exp (list 1 2 -3))
                 (or-exp (list 1 -2 -3))
                 (or-exp (list -1 -2 3))
                 ))))
(define ejemplo3
 (fnc 2
       (and-exp (list
                 (or-exp (list 1 -2))
                 (or-exp (list -1 -2))
                 (or-exp (list 1 2))
                 ))))

;;Permiten extraer datos para ser operados y hacer las respectivas aplicaciones.
;;EXTRACTORES:


;;Obtiene la variable numerica que representa la cantidad de variables de la funcion FNC
(define fnc->var
  (lambda (list)
    (cadr list)
      ))

;;Dado una forma fnc retorna la lista de las clausulas
(define fnc->clausulas
  (lambda (lista)
    (caddr lista)
    )
  )

;;Dado un componente or-exp, retorna la lista de numeros
(define or->varlist
  (lambda (lista)
    (cdr lista)
    )
  )
;;EJEMPLOS
(define ejemploext1
  (fnc->var '(FNC 3 (and-exp (or-exp 1 2 3) (or-exp 1 2 3) (or-exp 1 2 3))))
  )

(define ejemploext2
  (fnc->clausulas '(FNC 3 (and-exp (or-exp 1 2 3) (or-exp 1 2 3) (or-exp 1 2 3))))
  )

(define ejemploext3
  (or->varlist '(or-exp 1 2 3))
  )

;;1.2) gramatica a partir de datatypes

(define-datatype sat sat?
  (dtfnc
   (n integer?)
   (and (list-of and-exp?))))

(define-datatype and-exps and-exp?
  (dtand-exp
   (or (list-of or-exp?))))

(define-datatype or-exps or-exp?
  (dtor-exp
   (num (list-of integer?))))


;;ejemplos
(define expr1
  (dtfnc 2
    (list
      (dtand-exp (list
                (dtor-exp (list 1 2))
                (dtor-exp (list -1 2))
                (dtor-exp (list 1 -2))
                )))))

(define expr2
  (dtfnc 3
    (list
      (dtand-exp (list
                (dtor-exp (list -1 2 3))
                (dtor-exp (list 1 2 3)))))))

(define expr3
  (dtfnc 2
    (list
      (dtand-exp (list
                (dtor-exp (list -1 -2))
                (dtor-exp (list -1 2)))))))