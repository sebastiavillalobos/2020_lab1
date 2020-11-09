#lang racket

(require "TDA_Lista.rkt" "TDA_Stack.rkt" "TDA_Pregunta.rkt" "TDA_Respuesta.rkt" "TDA_Etiqueta.rkt" )

; TDA = Usuario

;Representacion

;El usuario es representado por una lista de Int y Strings, que detallan el id del usuario, un numero unico, el nombre del usuario,
; la contraseña del usuario, y la reputacion.

;ID Usuario: Int, identificador unico para cada usuario.
;Nombre de Usuario: String, nombre del usuario en la app.
;Password: String, contraseña del usuario.
;Reputacion: Int, numero que representa la reputacion del usuario.

;Dominio: Int, String, String, Int
;Recorrido: Un usuario, representado por una lista, 
;con su id usuario, nombre usuario, contraseña y reputacion.


; Constructor

(define (crearUsuario id_usuario nombre password reputacion)
   (if (and (and (esIdUsuario id_usuario) (esNombre nombre))(and (esPassword password)(esReputacion reputacion)))
    (list id_usuario nombre password reputacion)
    "no es un usuario valido"
   )
)

; Pertenencia
(define (esIdUsuario id)
    (and (number? N) (> N 0))
    )

(define (esNombre nombre)
    (if  (and (not (null? nombre))(string? nombre))
    #t
    #f    
    )
)

(define (esPassword password)
    (if  (and (not (null? password))(string? password))
    #t
    #f    
    )
)

(define (esReputacion reputacion)
    (number? reputacion) 
)

; Selectores 

(define (getID N)
    (car N)
)

(define (getUsuario N)
    (car (cdr N))
)

(define (getPassword N)
    (car (cdr (cdr N)))
)

(define (getReputacion N)
    (car (cdr (cdr (cdr N))))
)

; Modificadores

(define (modID N usuario)
(crearUsuario N (getUsuario usuario) (getPassword usuario) (getReputacion usuario))
)
(define (modUsuario N usuario)
(crearUsuario (getID usuario) N (getPassword usuario) (getReputacion usuario))
)
(define (modPassword N usuario)
(crearUsuario (getID usuario) (getUsuario usuario) N (getReputacion usuario))
)
(define (modReputacion N usuario)
(crearUsuario (getID usuario) (getUsuario usuario) (getPassword usuario) N)
)

; Otras funciones 