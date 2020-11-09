#lang racket

; TDA = Pregunta

;Representacion

; ID Pregunta: Int, identificador unico para cada pregunta.
; Fecha de creación: Representado por un Int , con el formato AñoMesDiaHoraMinuto, ej 202011051221 -> 05 Noviembre del año 2020 hora: 12:21.
; Autor: String que representa quien formula la pregunta.
; Votos Favor: Int que representa la cantidad de votos a favor. de 0 a infinito.
; Votos Contra: Int que representa la cantidad de votos en contra. de 0 a infinito.
; Recompensa: Int, representa cuantos puntos se dan por responder la pregunta.
; Estado: int que representa el estado de la pregunta, 1 para abierto (recibe respuestas) o 0 cerrado (no recibe respuestas).
; Reportes: Int que representa con un numero la cantidad de reportes negativos en la pregunta.
; Visualisaciones: Int, que representa con un numero la cantidad de visualisaciones de la pregunta.
; Etiquetas: Una lista representada en el TDA Etiquetas
; Respuestas: Una lista representada en el TDA Respuestas

; Ejemplo de una pregunta representada por una lista.
; (1 202011101930 "Sebastian Villalobos" 14 3 20 1 0 500 "dios existe?" (dios religion extraterrestres) (respuesta1 respuesta2 respuesta3))


;Constructor

(define (crearPregunta id_pregunta fecha autor votosFavor votosContra recompensa estado reportes visualizaciones laPregunta etiqueta respuesta)
  (if (and(and(and(and (esIdPregunta id_pregunta) (esFecha fecha)) (and (esAutor autor) (esVoto votosFavor)))(and(and (esVoto votosContra)(esRecompensa recompensa))
    (and(esEstado estado)(esReporte reportes))))(esVisualizacion visualizaciones))
        (list id_pregunta fecha autor votosFavor votosContra recompensa estado reportes visualizaciones laPregunta etiqueta respuesta)
        "No es una pregunta valida"
    )
  )
;Pertenencia

(define (esIdPregunta N)
    (and (number? N) (> N 0))
    )

(define (esFecha N)
    (and (number? N) (> N 202000000000)) 
    )

(define (esAutor N)
    (and (not (null? tag))(string? tag))
    )

(define (esVoto N)
    (and (number? N) (> N 0))
    )

(define (esRecompensa N)
    (and (number? N) (and (> N 0) (< N 50)))       
    )

(define (esEstado N)
(and (number? N) (or (= N 0) (= N 1)))
)

(define (esReporte N)
    (and (number? N) (> N 0))
    )

(define (esVisualizacion N)
    (and (number? N) (> N 0))
    )

(define (esLaPregunta N)
   (and (not (null? tag))(string? tag))
    )


;Selectores

(define (getIdpregunta N)
    (if (esIdPregunta N)
    (car N)
    null)
)

(define (getFecha N)
    (car (cdr N))
)

(define (getAutor N)
    (car (cdr (cdr N)))
)

(define (getVotosFavor N)
    (car (cdr (cdr (cdr N))))
)

(define (getVotosContra N)
    (car (cdr (cdr (cdr (cdr N)))))
)

(define (getRecompensa N)
    (car (cdr (cdr (cdr (cdr (cdr N))))))
)

(define (getEstado N)
    (car (cdr (cdr (cdr (cdr (cdr (cdr N)))))))
)

(define (getReportes N)
    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr N))))))))
)

(define (getVisualizaciones N)
    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr N)))))))))
)

(define (getLaPregunta N)
    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr N))))))))))
)

(define (getEtiqueta N)
    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr N)))))))))))
)

(define (getRespuesta N)
    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr N))))))))))))
)


;Modificadores

;Dominio: N= Int o String dependiendo de la funcion. / pregunta= Una lista que representa la pregunta.
;Recorrido: Una Pregunta

(define (modID N pregunta)
(crearPregunta N (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
(getReportes pregunta)(getVisualizaciones pregunta)(getLaPregunta pregunta)(getEtiqueta pregunta)(getRespuesta pregunta))
)

(define (modFecha N pregunta)
  (crearPregunta (getIdpregunta pregunta) N (getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
(getReportes pregunta)(getVisualizaciones pregunta)(getLaPregunta pregunta)(getEtiqueta pregunta)(getRespuesta pregunta)) 
)

(define (modAutor N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta) N (getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
    (getReportes pregunta)(getVisualizaciones pregunta)(getLaPregunta pregunta)(getEtiqueta pregunta)(getRespuesta pregunta)) 
)

(define (modVotosFavor N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta) N (getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
    (getReportes pregunta)(getVisualizaciones pregunta)(getLaPregunta pregunta)(getEtiqueta pregunta)(getRespuesta pregunta)) 
)

(define (modVotosContra N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta) N (getRecompensa pregunta)(getEstado pregunta)
    (getReportes pregunta)(getVisualizaciones pregunta)(getLaPregunta pregunta)(getEtiqueta pregunta)(getRespuesta pregunta)) 
)

(define (modRecompensa N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta) N (getEstado pregunta)
    (getReportes pregunta)(getVisualizaciones pregunta)(getLaPregunta pregunta)(getEtiqueta pregunta)(getRespuesta pregunta)) 
)

(define (modEstado N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta) N 
    (getReportes pregunta)(getVisualizaciones pregunta)(getLaPregunta pregunta)(getEtiqueta pregunta)(getRespuesta pregunta)) 
)

(define (modReportes N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
    N (getVisualizaciones pregunta)(getLaPregunta pregunta)(getEtiqueta pregunta)(getRespuesta pregunta)) 
)

(define (modVisualizaciones N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
    (getReportes pregunta) N (getLaPregunta pregunta)(getEtiqueta pregunta)(getRespuesta pregunta)) 
)

(define (modLaPregunta N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
    (getReportes pregunta) (getVisualizaciones pregunta) (getLaPregunta pregunta)(getEtiqueta pregunta)(getRespuesta pregunta))
)
(define (modEtiqueta N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
    (getReportes pregunta) (getVisualizaciones pregunta) (getLaPregunta pregunta) N (getRespuesta pregunta))
)

(define (modPregunta N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
    (getReportes pregunta) (getVisualizaciones pregunta) (getLaPregunta pregunta)(getEtiqueta pregunta) N)
)

;Otras funciones

