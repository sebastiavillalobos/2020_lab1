#lang racket

(require "listas.rkt")

; TDA “stack” que corresponde al contenedor de preguntas,  respuestas, usuarios y nombre del usuario con sesión activa.

;los usuarios: nombre de usuario, correo usuario, reputacion, numero preguntas realizadas, numero  respuestas realizadas

;las preguntas: además de información relativa a fecha de publicación, ID único incremental (se va generando un correlativo
;a partir de la última pregunta registrada), debe contar con información relativa a votos a favor, votos en contra, recompensas, 
;número de visualizaciones, estado (cerrada/abierta), listado de respuestas, reportes de ofensa 
;y otros datos que considere relevante para abordar el problema.

;las respuestas: se registra el autor de la respuesta, fecha de respuesta, ID único incremental en el contexto de la pregunta 
;(se va generando un correlativo a partir de la última respuesta registrada), votos a favor, votos en contra, 
;estado de aceptación (si/no), reportes de ofensa y otros datos que considere relevante para abordar problema.






; Stack Overflow