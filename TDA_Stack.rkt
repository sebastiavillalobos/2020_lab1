#lang racket

(require "TDA_Lista.rkt" "TDA_Pregunta.rkt" "TDA_Usuario.rkt" "TDA_Respuesta.rkt" "TDA_Etiqueta.rkt" )

; TDA = Stack

;Representacion

;El stack está representado por una lista compuesta por una lista de preguntas, que representa todas las preguntas
;realizadas en la plataforma stack, con sus respectivas etiquetas y respuestas, los usuarios son reppresentados por una lista,
;con todos sus atributos y estado.

;Dominio: preguntas, usuarios
;Recorrido: stack

;preguntas: lista (TDA pregunta)
;usuarios: lista (TDA usuario)

#|
Ejemplo 
(crearStack (crearPregunta 12 202011101930 "yop" 123 2 20 1 10 500 "Quien mato a marilin?" (list "dfsf" "sdfsf" "sdfsdf") (list "sdfsdf" "sdfdsf"))
(crearUsuario 1 "bayron" "password" 50 1))

(crearStack (list (crearPregunta 1 202011101930 "jorge gonzalez" 123 2 20 1 10 500 "Quien mato a marilin?" (list "marilin" "los prisioneros" "asesinato") (list "la prensa" "la radio" "el raton mikey")) (crearPregunta 2 202011101930 "Seba" 123 2 20 1 10 500 "2+2 es 4??" (list "suma" "tonterias" "4") (list "sdfsdf" "sdfdsf")) (crearPregunta 3 202011101930 "Dios" 123 2 20 1 10 500 "Que le pasa a lupita??" (list "lupita" "que le pasa" "que") (list "nose" "bailar")))
(list (crearUsuario 1 "bayron" "password" 50 1)(crearUsuario 2 "yanira" "1234" 50 1)(crearUsuario 3 "berson" "notengo" 50 1)))

|#

; Constructor
(define (crearStack preguntas usuarios)
    (if (and (sonPreguntas preguntas)(sonUsuarios usuarios))
        (list preguntas usuarios)
        "No es un stack valido"
    )
)

; Pertenencia
(define (sonPreguntas preguntas)
    (list? preguntas)
)

(define (sonUsuarios usuarios)
    (list? usuarios)
)

; Selectores
(define (getPreguntas stack)
(car stack)
)

(define (getUsuarios stack)
(car(cdr stack))
)

(define (getTodosUsuarios stack)
 (listaIguales (getUsuarios stack) 2)
)

(define (getTodosIDP stack)
 (listaIguales (getPreguntas stack) 1)
)

(define (getTodasPreguntas stack)
 (listaIguales (getPreguntas stack) 10)
)

(define (getTodasRespuestas stack)
 (listaIguales (getPreguntas stack) 12)
)

; Funcion que retorna una lista de pares, que representa
; el ID y La pregunta
; Esta funcion sirve para listar todos los elementos de 2 listas que se encuentren 
;en la misma posicion 

(define (getId+Pregunta stack)
(id+preguntaAUX stack 1)
)

(define (id+preguntaAUX stack contador) 
    (if (= contador (contadorLista (getTodosIDP stack)))
    (list (cons (getElemento (getTodosIDP stack) contador)  (getElemento (getTodasPreguntas stack) contador)))
    (cons (cons (getElemento (getTodosIDP stack) contador)  (getElemento (getTodasPreguntas stack) contador)) (id+preguntaAUX stack (+ contador 1)) )
    )
)



; Otras funciones

;Funcion que retorna las respuestas asociadas a un ID de pregunta
;Segun ID busca en las respuestas y segun la posicion de ese id en la lista de preguntas retornas las respuetas asociadas, 
;que estan en la posicion 12

(define (lasRespuestas stack idpregunta)
    (if (number? (laPregunta (getPreguntas stack) idpregunta))
    (getElemento (getElemento (getPreguntas stack) (laPregunta (getPreguntas stack) idpregunta)) 12)
    "No existen respuestas asociadas a ese id de pregunta"
)
)

; Funcion Register

;Funcion que lista todos los id de usuarios creados
; OK (getTodosUsuarios stack)

;Funcion que compara si existe o no un usuario con ese ID
(define (existeUsuario stack username)
    (if (existeElemento (getTodosUsuarios stack) username)
        #t
        #f
    )
)

; Funcion que cuenta la cantidad de usuarios creados y asigna un id
; correlativo a la posicion del nuevo elemento
(define (nuevoIDusuario stack)
    (+ (contadorLista (getTodosUsuarios stack))1)
)

;Funcion que crea un nuevo usuario en la app.
;Funcion que crea un nuevo usuario en la app.
(define (register stack username password)
    (if (existeUsuario stack username)
    "El Usuario ya se encuentra registrado, elegir nuevo Usuario"
    (append (getUsuarios stack) (list(crearUsuario (nuevoIDusuario stack) username password 0 0)))
    )
)

; Funcion Login

