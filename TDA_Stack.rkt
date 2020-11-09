#lang racket

(require "TDA_Lista.rkt" "TDA_Pregunta.rkt" "TDA_Usuario.rkt" "TDA_Respuesta.rkt" "TDA_Etiqueta.rkt" "StakEjemplo.rkt" )

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
(list (crearUsuario 1 "bayron" "password" 50 0)(crearUsuario 2 "yanira" "1234" 50 0)(crearUsuario 3 "berson" "notengo" 50 0)))
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
(define (getTodosIDresp stack)
(listaIguales (getTodasRespuestas stack) 1)
)
(define (getTodasPreguntas stack)
 (listaIguales (getPreguntas stack) 10)
)

(define (getTodasRespuestas stack)
 (listaIguales (getPreguntas stack) 12)
)

; Modificadores

(define (modPreguntas stack preguntas)
(crearStack preguntas (getUsuarios stack))
)

(define (modUsuarios stack usuarios)
(crearStack (getPreguntas stack) usuarios)
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

; RF

;####################################################################################
; REGISTER OK
;####################################################################################
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

(define (register stack username password)
    (if (existeUsuario stack username)
    "El Usuario ya se encuentra registrado, elegir nuevo Usuario"
    (append (getUsuarios stack) (list(crearUsuario (nuevoIDusuario stack) username password 50 0)))
    )
)

; ####################################
; Funcion comando
; ####################################
(define comando (lambda (entrada)(comando entrada)))

;####################################################################################
; LOGIN OK
;####################################################################################

; Retorna un stack con el usuario con session activa, y depende 
;de comando para saber el retorno completo de la funcion

(define (login stack username password comando) 
    (if (existeUsuario stack username)
    (comando (modALLusers stack username) username )  
    "No existe usuario"
    )   
)
;#####################

; (buscaPosElem (getTodosUsuarios stack) username) posicion del usuario en la lista de usuarios

; --------------------------------
; Funcin que agrega los nuevos estados de usuario al stak
(define (modALLusers stack username)
    (if (existeUsuario stack username)
        (crearStack (getPreguntas stack) (modificaListaUsuarios stack username))
        "Usuario no existe"
    )
)
; Funcion que modifica un usuario en la lista de usuarios
; Retorna todos los usuarios con el estado modificado del usuario
(define (modificaListaUsuarios stack username)
    (if (existeUsuario stack username)
        (modElemento (getUsuarios stack) (buscaPosElem (getTodosUsuarios stack) username) (cambiarEstadoUsuario stack username))
        "No existe el usuario"
    )
)

; Funcion que modifica el estado de un usuario OK
(define (cambiarEstadoUsuario stack username)
    (modEstado5 1 (getElemento (getUsuarios stack) (buscaPosElem (getTodosUsuarios stack) username))) 
)
; --------------------------------

;####################################################################################
; ASK  OK
;####################################################################################
; retorna un stak con el usuario activo y la nueva pregunta formulada

(define (ask stack user) (lambda (fecha) (lambda (preguntaNueva etiquetasNueva)
  (crearStack (askAux stack user fecha preguntaNueva etiquetasNueva) (getUsuarios stack))    
)))
;#####################

(define (askAux stack user fecha preguntaNueva etiquetasNueva)
    (append (getPreguntas stack) (list (crearPregunta (nuevoIDpregunta stack) fecha user 0 0 0 1 0 0 preguntaNueva etiquetasNueva "Sin Respuestas")))

)

; Funcion que cuenta la cantidad de preguntas creadas y asigna un id
; correlativo a la posicion del nuevo elemento
(define (nuevoIDpregunta stack)
    (+ (contadorLista (getTodasPreguntas stack)) 1)
)

;####################################################################################
; REWARD  
;####################################################################################
;Funcion currificada que permite a un usuario con sesion iniciada en la plataforma ofrecer una recompensa para
;una determinada pregunta. La recompensa puede ir dirigida a cualquier pregunta.

;Dominio: Stack
;Recorrido: Stack

(define (reward stack username) (lambda (idpregunta)(lambda (recompensa)
    (if (and (existeUsuario stack username) (> (getRecompensaUsuario username stack) recompensa))
        (crearStack (modificaListaPreguntasR stack idpregunta recompensa) (modificaListaUsuariosR stack username recompensa))
        "No puedes poner esta recompensa"
        
    )
)))
;#####################

;**** Usuarios
; Funcion que modifica la lista de usuarios con la nueva reputacion del usuario OK!!
;****
(define (modificaListaUsuariosR stack username recompensa)
    (if (existeUsuario stack username)
        (modElemento (getUsuarios stack) (buscaPosElem (getTodosUsuarios stack) username) (cambiarReputacionUsuario stack username recompensa))
        "No existe el usuario"
    )
)

; Funcion que modifica la reputación de un usuario OK

(define (cambiarReputacionUsuario stack username recompensa)
(if (< (getRecompensaUsuario username stack) recompensa)
    "No puedes poner esta recompensa"
    (modReputacion (- (getReputacion (getElUsuario stack username) ) recompensa) (getElUsuario stack username))
)
)

; Funcion que retorna la reputacion de un usuario en un stack
(define (getRecompensaUsuario username stack)
(getReputacion  (getElUsuario stack username))
)

;Funcion que retorna el usuario segun su username
(define (getElUsuario stack username)
(getElemento (getUsuarios stack) (buscaPosElem (getTodosUsuarios stack) username))
)
;**** Preguntas
; Funcion que modifica la lista de preguntas con la nueva recompensa de pregunta OK!
;****
(define (modificaListaPreguntasR stack id recompensa)
        (modElemento (getPreguntas stack) (buscaPosElem (getTodosIDP stack) id) (cambiarRecompensaPregunta stack id recompensa))
)

; Funcion que modifica la recompensa de una Pregunta
(define (cambiarRecompensaPregunta stack id recompensa)
    (modRecompensa (+ (getRecompensaPregunta stack id )recompensa) (getLaPregun stack id))
 )


; Funcion que retorna la recompensa de una pregunta en el stak
(define (getRecompensaPregunta stack id)
(getReputacion  (getLaPregun stack id))
)


;Funcion que retorna la pregunta segun su id
(define (getLaPregun stack id)
(getElemento (getPreguntas stack) (buscaPosElem (getTodosIDP stack) id))
)

#|
(crearStack (list (crearPregunta 1 202011101930 "jorge gonzalez" 123 2 20 1 10 500 "Quien mato a marilin?" (list "marilin" "los prisioneros" "asesinato") (list "la prensa" "la radio" "el raton mikey")) (crearPregunta 2 202011101930 "Seba" 123 2 20 1 10 500 "2+2 es 4??" (list "suma" "tonterias" "4") (list "sdfsdf" "sdfdsf")) (crearPregunta 3 202011101930 "Dios" 123 2 20 1 10 500 "Que le pasa a lupita??" (list "lupita" "que le pasa" "que") (list "nose" "bailar")))
(list (crearUsuario 1 "bayron" "password" 50 0)(crearUsuario 2 "yanira" "1234" 50 0)(crearUsuario 3 "berson" "notengo" 50 0)))
|#

;####################################################################################
; ANSWER OK!
;####################################################################################

;Funcion currificada que permite a un usuario con sesion iniciada en la plataforma responder una pregunta
; Cada respuesta registra el autor, fecha de publicacion, la respuesta y 0 más etiquetas.
;Dominio: Stack
;Retorno: Stack

(define (answer stack user) (lambda (fecha)(lambda (idpregunta) (lambda (respuesta etiquetas)
(crearStack (modRespPregun stack fecha user idpregunta respuesta etiquetas) (getUsuarios stack))
))))
;#####################



;Funcion que retorna las respuestas de una pregunta segun su ID en el stack
(define (getRespuestas stack id)
    (getElemento (getLaPregun stack id) 12)
)

; Funcion que cuenta la cantidad de respuestas creados y asigna un id
; correlativo a la posicion del nuevo elemento
(define (nuevoIDrespuesta stack idpregunta)
    (+ (contadorLista (getRespuestas stack idpregunta))1)
)

;Funcion que crea una nueva respuesta para la pregunta y la agrega a la lista de respuestas
(define (agregaRespuesta stack fecha username idpregunta respuesta etiquetas)
    (append (getRespuestas stack idpregunta) (list (crearRespuesta (nuevoIDrespuesta stack idpregunta) fecha username 0 0 1 0 respuesta etiquetas)))
     )

;Funcion que modifica la pregunta agregando las respuestas a la pregunta segun ID
(define (modRespuestas stack fecha username idpregunta respuesta etiquetas)
    (modElemento (getLaPregun stack idpregunta) 12 (agregaRespuesta stack fecha username idpregunta respuesta etiquetas))
)

;Funcion que retorna la ubicacion de la pregunta segun su ID
(define (posPregID stack id)
(buscaPosElem (getTodosIDP stack) id)
)

;Funcion que modifica las preguntas agregando la pregunta con la nueva respuesta.
;Retorna todas las respuestas, con la nueva respuesta
(define (modRespPregun stack fecha username idpregunta respuesta etiquetas)
(modElemento (getPreguntas stack) (posPregID stack idpregunta) (modRespuestas stack fecha username idpregunta respuesta etiquetas))
)

;####################################################################################
; accept OK!
;####################################################################################

;Funcion currificada que permite a un usuario con sesion iniciada en lka plataforma aceptar una respuesta a una de sus preguntas.
;Dominio:Stack
;Recorridoi:Stack


;funcion que retorna la pregunta segun su id

;(buscaPosElem (getTodosIDresp stack) idrespuesta) -> Me indica la posicion del
; id respuesta en las respuestas
;estado pos 6 en respuesta

;(getRespuestas stack idpregunta)  -> obtiene las respuesta de la pregunta con ese idpregunta

;Funcion que retorna la posicion de la respuesta en una pregunta segun idpregunta y idrespuesta
(define (getLaRespuest stack idrespuesta idpregunta)
(getRespuestas stack idpregunta)

(getElemento (getRespuestas stack idpregunta) idrespuesta)
)

(define (modEstadoRes stack idrespuesta idpregunta)

)

;Funcion que retorna la respuesta segun id respuesta y idpregunta
(define (idPreg+idResp stack idpregunta idrespuesta)
(getElemento (getRespuestas stack idpregunta) idrespuesta)
)

;Funcion que compara si el usuario es el mismo que el autor de la pregunta
(define (esAutor username stack idpregunta idrespuesta)
    (if (equal? username ()))

)

(buscaPosElem lista elemento)
(getElemento fila m)
(modElemento lista posicion nuevoElemento)
