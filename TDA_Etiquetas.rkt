#lang racket

(require "tdalista.rkt")

; TDA = Etiquetas

;Representacion


; Tag1: un string etiqueta
; Tag2: un string etiqueta
; Tag3: un string etiqueta


; Las etiquetas son representadas con una lista de elementos, que representan los temas tratados en la pregunta, 
; para facilitar la busqueda, en esta representación las etiquetas serán 3 por pregunta.
; no se pueden agregar más etiquetas, solo modificar una menos representativa por una mejor.

; Constructor

(define (crearEtiquetas tag1 tag2 tag3)
    (if (and (and (esTag tag1) (esTag tag2)) (esTag tag3))
        (list tag1 tag2 tag3)
    )
)

; Pertenencia

(define (esEtiqueta etiqueta)
    (if (= (contadorLista etiqueta) 3)
        #t
        #f
    )
)

(define (esTag tag)
    (if  (and (not (null? tag))(string? tag))
    #t
    #f    
    )
)

; Selectores

(define (getTag1 N)
    (car N)
)

(define (getTag2 N)
    (car (cdr N))
)

(define (getTag3 N)
    (car (cdr (cdr N)))
)

; Modificadores

(define (modTag1 N etiqueta)
    (crearEtiquetas N tag2 tag3)
)

(define (modTag2 N etiqueta)
    (crearEtiquetas tag1 N tag3)
)

(define (modTag3 N etiqueta)
    (crearEtiquetas tag1 tag2 N)
)

; Otras funciones