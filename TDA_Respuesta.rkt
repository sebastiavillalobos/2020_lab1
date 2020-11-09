#lang racket
(provide (all-defined-out))
(require "TDA_Lista.rkt" )
; TDA = Respuesta

;Representacion

; La respuesta está representada por una lista que contiene el id de respuesta, un numero que va a

; ID Respuesta: Int, identificador unico para cada respuesta
; Fecha de creación: Representado por un Int , con el formato AñoMesDiaHoraMinuto, ej 202011051221 -> 05 Noviembre del año 2020 hora: 12:21.; Autor: String que representa quien formula la pregunta.
; Votos Favor: Int que representa la cantidad de votos a favor.
; Votos Contra: Int que representa la cantidad de votos en contra.
; Estado  de Aceptacion: representa si la respuesta es aceptada o no, int, 1 aceptada 0 no aceptada.
; Reportes de Ofensa: int, que representa la cantidad de veces que ha sido reportada la respuesta.
; Respuesta: String que representa la respuesta dada a la respuesta.

; Constructor

(define (crearRespuesta id_respuesta fecha autor votosFavor votosContra estado reporte laRespuesta)
(if (and(and(and (esIdRespuesta3 id_respuesta)(esFecha3 fecha))(and(esAutor3 autor)(esVoto3 votosFavor)))(and(and(esVoto3 votosContra)(esEstado3 estado))(and (esReporte3 reporte)(esLaRespuesta3 laRespuesta))))
(list id_respuesta fecha autor votosFavor votosContra estado reporte laRespuesta)
"No es una respuesta valida"
)
)

; Pertenencia

(define (esIdRespuesta3 N)
    (and (number? N) (> N 0))
    )

(define (esFecha3 N)
    (and (number? N) (> N 202000000000)) 
    )

(define (esAutor3 N)
    (and (not (null? N))(string? N))
    )

(define (esVoto3 N)
    (and (number? N) (> N 0))
    )
(define (esEstado3 N)
(and (number? N) (or (= N 0) (= N 1)))
)

(define (esReporte3 N)
    (and (number? N) (> N 0))
    )

(define (esLaRespuesta3 N)
   (and (not (null? N))(string? N))
    )


; Selectores

(define (getID3 N)
    (car N)
)

(define (getFecha3 N)
    (car (cdr N))
)

(define (getAutor3 N)
    (car (cdr (cdr N)))
)

(define (getVotosFavor3 N)
    (car (cdr (cdr (cdr N))))
)

(define (getVotosContra3 N)
    (car (cdr (cdr (cdr (cdr N)))))
)

(define (getEstado3 N)
    (car (cdr (cdr (cdr (cdr (cdr N))))))
)
(define (getReporte3 N)
    (car (cdr (cdr (cdr (cdr (cdr (cdr N)))))))
)
(define (getLaRespuesta3 N)
    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr N))))))))
)


; Modificadores

(define (modId3 N respuesta)
(crearRespuesta N (getFecha3 respuesta) (getAutor3 respuesta) (getVotosFavor3 respuesta) (getVotosContra3 respuesta) (getEstado3 respuesta) (getReporte3 respuesta) (getLaRespuesta3 respuesta))
)

(define (modFecha3 N respuesta)
(crearRespuesta (getID3 respuesta) N (getAutor3 respuesta) (getVotosFavor3 respuesta) (getVotosContra3 respuesta) (getEstado3 respuesta) (getReporte3 respuesta) (getLaRespuesta3 respuesta))
)

(define (modAutor3 N respuesta)
(crearRespuesta (getID3 respuesta) (getFecha3 respuesta) N (getVotosFavor3 respuesta) (getVotosContra3 respuesta) (getEstado3 respuesta) (getReporte3 respuesta) (getLaRespuesta3 respuesta))
)

(define (modVotosFavor3 N respuesta)
(crearRespuesta (getID3 respuesta) (getFecha3 respuesta) (getAutor3 respuesta) N (getVotosContra3 respuesta) (getEstado3 respuesta) (getReporte3 respuesta) (getLaRespuesta3 respuesta))
)

(define (modVotosContra3 N respuesta)
(crearRespuesta (getID3 respuesta) (getFecha3 respuesta) (getAutor3 respuesta) (getVotosFavor3 respuesta) N (getEstado3 respuesta) (getReporte3 respuesta) (getLaRespuesta3 respuesta))
)

(define (modEstado3 N respuesta)
(crearRespuesta (getID3 respuesta) (getFecha3 respuesta) (getAutor3 respuesta) (getVotosFavor3 respuesta) (getVotosContra3 respuesta) N (getReporte3 respuesta) (getLaRespuesta3 respuesta))
)

(define (modReporte3 N respuesta)
(crearRespuesta (getID3 respuesta) (getFecha3 respuesta) (getAutor3 respuesta) (getVotosFavor3 respuesta) (getVotosContra3 respuesta) (getEstado3 respuesta) N (getLaRespuesta3 respuesta))
)

(define (modLaRespuesta3 N respuesta)
(crearRespuesta (getID3 respuesta) (getFecha3 respuesta) (getAutor3 respuesta) (getVotosFavor3 respuesta) (getVotosContra3 respuesta) (getEstado3 respuesta) (getReporte3 respuesta) N)
)

; Otras funciones