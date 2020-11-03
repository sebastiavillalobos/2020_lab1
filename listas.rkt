#lang racket

(provide (all-defined-out))

; TDA Lista

; Representación lista: Una lista será representada por un conjunto de elementos dentro de () con n elementos, pudiendo ser una lista de elementos vacios.

;Constructor ###
;Entrada: un elemento, elementos, pueden ser listas, etc..
;Salida: Una lista

(define (createList lista)
    (list lista)
)

;Pertenencia ###
;Entrada: lista
;Salida: #t or #f
;Es una lista
(define (eslista lista)
  (if (list? lista)
      #t
      #f
      ))



;Selectores ###

; Selecciona un elemento ubicado en la posicion dada de la lista y lo retorna
(define (getElemento fila m)
  (getElementoAux fila m 1))

; Recursion de cola
(define (getElementoAux fila m contador)
  (if (= m contador)
      (car fila)
      (getElementoAux (cdr fila) m (+ 1 contador))
      )  
  )

; Modificadores ###

; #####
;Funcion que elimina un elemento de la lista en la posicion que uno quiera
;Entrada: lista, posicion
;Salida: Nueva lista sin el elemento ubicado en la posicion indicada
(define (eliminarElemento lista posicion)
  (append (Alista lista 1 posicion) (Blista lista 1 posicion))
  )

  ; Funcion que separa las listas 
; Crea una lista hasta el elemento n de la lista ej '(1 2 3 4 5 6) n=3 -> '(1 2)
(define (Alista lista cont n)
  (if (= cont n)
      null
      (cons (car lista) (Alista (cdr lista) (+ 1 cont) n))
      )

  )

;Funcion que separa las listas
; Crea una lista desde el elemento n de la lista ej '(1 2 3 4 5 6) n=3 -> '(4 5 6)
(define (Blista lista cont n)
  (if (null? lista)
      null
      (if (> cont n)
          (cons (car lista) (Blista (cdr lista) (+ 1 cont) n))
          (Blista (cdr lista) (+ 1 cont) n)
          )
      )
  )

; #####

;Funciones sobre TDA

; Funcion que recorre la lista y los compara con un elemento dado y si existe, retorna verdadero

(define (buscaElemento lista elementoAComparar N )
  (if (equal? (getElemento lista N) elementoAComparar)
      #t
      (if (> (contadorLista lista) 0)
      (buscaElemento (cdr lista) elementoAComparar (+ N 1) )  
      #f
      )
  )


;Contador de elementos de la lista
;Entrada: lista de elementos
;Salida: Numero de elementos de la lista
(define (contadorLista lista)
  (contadorListaAux lista 1)
  )
;Recursion de cola
(define (contadorListaAux lista contador)
  (if (null? (cdr lista))
      contador
      (contadorListaAux (cdr lista) (+ contador 1))
      )
  )

; Numero de elementos repetidos
;Funciona que retorna la cantidad de veces que se repite el mismo elemento en una lista
;Entrada: lista, elemento a comparar
;Salida: Numero

(define (elementosRepetidos lista elementoAComparar)
    (numeroElementosRepetidos lista elementoAComparar 0)
)

(define (numeroElementosRepetidos lista elementoAComparar contador)
    (if (> (contadorLista lista) 1)
        (if (equal? (car lista) elementoAComparar)
            (numeroElementosRepetidos (cdr lista) elementoAComparar (+ contador 1))
            (numeroElementosRepetidos (cdr lista) elementoAComparar contador)

        )
        (if (equal? (car lista) elementoAComparar)
            (+ contador 1)
            contador

        )   
    )

)
 
