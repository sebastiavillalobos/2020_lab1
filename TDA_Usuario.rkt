#lang racket
(provide (all-defined-out))
(require "TDA_Lista.rkt")

; TDA = Usuario

;Representacion

;El usuario es representado por una lista de Int y Strings, que detallan el id del usuario, un numero unico, el nombre del usuario,
; la contraseña del usuario, y la reputacion.

;ID Usuario: Int, identificador unico para cada usuario.
;Nombre de Usuario: String, nombre del usuario en la app.
;Password: String, contraseña del usuario.
;Reputacion: Int, numero que representa la reputacion del usuario, todo usuario nuevo inicia con 50 puntos de reputación.
;Estado: Int que representa si el usuario está activo o no, 1 si está activo, 0 si no lo está.

;Dominio: Int, String, String, Int, Int
;Recorrido: Un usuario, representado por una lista, 
;con su id usuario, nombre usuario, contraseña y reputacion.


; Constructor

(define (crearUsuario id_usuario nombre password reputacion estado)
   (if (and (and (and (esIdUsuario2 id_usuario) (esNombre2 nombre))(and (esPassword2 password)(esReputacion2 reputacion)))(esEstado2 estado))
    (list id_usuario nombre password reputacion estado)
    "No es un usuario valido"
   )
)

; Pertenencia
(define (esIdUsuario2 N)
    (and (number? N) (> N 0))
    )

(define (esNombre2 nombre)
    (if  (and (not (null? nombre))(string? nombre))
    #t
    #f    
    )
)

(define (esPassword2 password)
    (if  (and (not (null? password))(string? password))
    #t
    #f    
    )
)

(define (esReputacion2 reputacion)
    (number? reputacion) 
)

(define (esEstado2 N)
(and (number? N) (or (= N 0) (= N 1)))
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

(define (getEstado5 N)
    (car (cdr (cdr (cdr (cdr N)))))
)
; Modificadores

(define (modID2 N usuario)
(crearUsuario N (getUsuario usuario) (getPassword usuario) (getReputacion usuario)(getEstado5 usuario))
)
(define (modUsuario N usuario)
(crearUsuario (getID usuario) N (getPassword usuario) (getReputacion usuario)(getEstado5 usuario))
)
(define (modPassword N usuario)
(crearUsuario (getID usuario) (getUsuario usuario) N (getReputacion usuario)(getEstado5 usuario))
)
(define (modReputacion N usuario)
(crearUsuario (getID usuario) (getUsuario usuario) (getPassword usuario) N (getEstado5 usuario))
)

(define (modEstado5 N usuario)
(crearUsuario (getID usuario) (getUsuario usuario) (getPassword usuario) (getReputacion usuario) N)
)
; Otras funciones 