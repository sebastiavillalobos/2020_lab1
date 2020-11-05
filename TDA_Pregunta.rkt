#lang racket

; TDA = Pregunta

;Representacion

; ID Pregunta: Int, incremental
; Fecha: Representado por un Int dia mes año hora minuto, ej 051120201220 -> 05 Noviembre del año 2020 hora: 12:21
; Autor: String que representa quien formula la pregunta.
; Votos Favor: Int que representa la cantidad de votos a favor. de 0 a infinito (+10 cada voto)
; Votos Contra: Int que representa la cantidad de votos en contra. de 0 a infinito (-10 cada voto)
; Recompensas: Int a reconpensa por responder la pregunta (de 0 a 50)
; Estado: int, 1 abierta 0 cerrada.
; Reportes: Int, cantidad de reportes.
; Visualisaciones: Int, cada vez que se revisa la pregunta +1


;Constructor

(define (crearPregunta id_pregunta fecha autor votosFavor votosContra recompensa estado reportes visualizaciones)
  (if (and(and(and(and (esIdPregunta id_pregunta) (esFecha fecha)) (and (esAutor autor) (esVoto votosFavor)))(and(and (esVoto votosContra)(esRecompensa recompensa))
    (and(esEstado estado)(esReporte reportes))))(esVisualizacion visualizaciones))
        (list id_pregunta fecha autor votosFavor votosContra recompensa estado reportes visualizaciones)
        "No es una pregunta valida"
    )
  )
;Pertenencia

(define (esIdPregunta N)
    (and (number? N) (> N 0))
    )

(define (esFecha N)
    (and (number? N) (> N 1000000000)) 
    )

(define (esAutor N)
    (string? N)
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

;Modificadores

;Dominio: N= Int o String dependiendo de la funcion. / pregunta= Una lista que representa la pregunta.
;Recorrido: Una Pregunta

(define (modID N pregunta)
(crearPregunta N (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
(getReportes pregunta)(getVisualizaciones pregunta))
)

(define (modFecha N pregunta)
  (crearPregunta (getIdpregunta pregunta) N (getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
(getReportes pregunta)(getVisualizaciones pregunta)) 
)

(define (modAutor N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta) N (getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
    (getReportes pregunta)(getVisualizaciones pregunta)) 
)

(define (modVotosFavor N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta) N (getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
    (getReportes pregunta)(getVisualizaciones pregunta)) 
)

(define (modVotosContra N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta) N (getRecompensa pregunta)(getEstado pregunta)
    (getReportes pregunta)(getVisualizaciones pregunta)) 
)

(define (modRecompensa N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta) N (getEstado pregunta)
    (getReportes pregunta)(getVisualizaciones pregunta)) 
)

(define (modEstado N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta) N 
    (getReportes pregunta)(getVisualizaciones pregunta)) 
)

(define (modReportes N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
    N (getVisualizaciones pregunta)) 
)

(define (modVisualizaciones N pregunta)
    (crearPregunta (getIdpregunta pregunta) (getFecha pregunta)(getAutor pregunta)(getVotosFavor pregunta)(getVotosContra pregunta)(getRecompensa pregunta)(getEstado pregunta)
    (getReportes pregunta) N) 
)


;Otras funciones

