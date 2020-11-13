#lang racket

(require "TDA_Lista.rkt" "TDA_Stack.rkt" "TDA_Pregunta.rkt" "TDA_Respuesta.rkt" "TDA_Etiqueta.rkt" "TDA_Usuario.rkt")


;Requerimientos funcionales


;####################################################################################
; REGISTER OK
;####################################################################################
#|
Nombre funcion: Register
Funcion que permite registrar a un nuevo usuario en el stack.
Dominio: stack x string x string
Recorrido: stack
|#
(define (register stack username password)
    (if (existeUsuario stack username)
    "El Usuario ya se encuentra registrado, elegir nuevo Usuario"
    (crearStack (getPreguntas stack) (append (getUsuarios stack) (list(crearUsuario (nuevoIDusuario stack) username password 50 0))))
    )
)


;####################################################################################
; LOGIN OK
;####################################################################################
#|
Nombre funcion: Login
Funcion que permite autentificar a im usuario registrado, iniciar sesion y junto con ello
permite la ejecucion de comandos concretos dentro del stack
Dominio: stack x string x string x funcion
Recorrido: funcion
Recorrido Final: stack
Recursion de cola: OK
Funcion currificada: OK
|#
(define (login stack username password comando) 
    (if (existeUsuario stack username)
    (comando (modALLusers stack username) username )  
    "No existe usuario registradom compruebe usuario"
    )   
)

;####################################################################################
; ASK OK
;####################################################################################
#|
Nombre funcion: Ask
Funcion currificada que permite a un usuario con sesion iniciada en la plataforma realizar una nueva pregunta.
Dominio: stack
Recorrido: stack x intiger(Fecha) x string(Pregunta) x string list (Etiquetas)
Recorrido Final: stack
Funcion currificada: OK
|#
(define (ask stack user) (lambda (fecha) (lambda (preguntaNueva etiquetasNueva)
  (crearStack (askAux stack user fecha preguntaNueva etiquetasNueva) (getUsuarios stack))    
)))

;####################################################################################
; REWARD OK
;####################################################################################
#|
Nombre funcion: Reward
Funcion currificada que permite a un usuario con sesion iniciada en la plataforma ofrecer una recompensa para 
para una determinada pregunta.
Dominio: stack
Recorrido: stack x intiger(IDpregunta) x intiger(Recompensa) 
Recorrido Final: stack
Funcion currificada: OK
|#
(define (reward stack username) (lambda (idpregunta)(lambda (recompensa)
    (if (and (existeUsuario stack username) (> (getRecompensaUsuario username stack) recompensa))
        (crearStack (modificaListaPreguntasR stack idpregunta recompensa) (modificaListaUsuariosR stack username recompensa))
        "No puedes poner esta recompensa"
        
    )
)))


;####################################################################################
; ANSWER OK
;####################################################################################
#|
Nombre funcion: Answer
Funcion currificada que permite a un usuario con sesion iniciada en la plataforma responder una pregunta.
Dominio: stack
Recorrido: stack x intiger(IDpregunta) x string(Pregunta) x string list (Etiquetas)
Recorrido Final: stack
Funcion currificada: OK
|#
(define (answer stack user) (lambda (fecha)(lambda (idpregunta) (lambda (respuesta etiquetas)
(crearStack (modRespPregun stack fecha user idpregunta respuesta etiquetas) (getUsuarios stack))
))))

;####################################################################################
; ACCEPT OK
;####################################################################################
#|
Nombre funcion: Accept
Funcion currificada que permite a un usuario con sesion iniciada en la plataforma aceptar una respuesta a una de sus preguntas 
Dominio: stack
Recorrido: stack x intiger(IDpregunta) x intiger(IDrespuesta)
Recorrido Final: stack
Funcion currificada: OK
|#
(define (accept stack user) (lambda (idpregunta)(lambda (idrespuesta)
;user tiene que ser el mismo que el autor de la pregunta
(if (equal? (getAutor (getLaPregun stack idpregunta)) user)
    (crearStack (preguntasRespAceptada stack idpregunta idrespuesta) (modUserRep stack (getAutor3 (getLaRespuest stack idrespuesta idpregunta)) idpregunta))
    "Usuario no es el autor de la pregunta"
)
)))



#|

##################################### EJEMEPLO ##################################

##########################
#      STACK EJEMPLO     #
##########################
(crearStack (list (crearPregunta 1 202011101930 "jorge gonzalez" 123 2 20 1 10 500 "¿Quien mató a Marilin?" (list "marilin" "los prisioneros" "asesinato") (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "los comunistas" (list "red" "marx" "tiempos mejores"))) (crearPregunta 2 202011101930 "Seba" 0 12 20 1 10 35 "¿2+2 es 5?" (list "suma" "tonterias" "5") (list "Sin Respuesta")) (crearPregunta 3 202011101930 "yanira" 21 12 220 1 3 1243 "Que le pasa a lupita??" (list "lupita" "que le pasa" "que") (list (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "nose" "Lupitaaaa")(crearRespuesta 2 202011101930 "sebiuo" 0 0 0 0 "Ella quiere bailar" "baile")(crearRespuesta 3 202011101930 "jose" 0 0 0 0 "Le falta un amor" (list "amor" "dios es amor")))))
(list (crearUsuario 1 "bayron" "password" 50 0)(crearUsuario 2 "yanira" "1234" 50 0)(crearUsuario 3 "berson" "342423" 50 0)(crearUsuario 4 "sebiuo" "notengo" 50 0)(crearUsuario 5 "Seba" "sitengo" 50 0)(crearUsuario 6 "jorge gonzalez" "narea_te_amo" 50 0)))

Un stack que cuenta con 6 usuarios registrados, 3 preguntas, y respuestas a algunas preguntas.


#############
# Preguntas #
#############
(crearPregunta 1 202011101930 "jorge gonzalez" 123 2 20 1 10 500 "¿Quien mató a Marilin?" (list "marilin" "los prisioneros" "asesinato")
ID Pregunta: 1
Fecha de creación: 202011101930
Autor: "jorge gonzalez" 
Votos Favor: 123
Votos Contra: 2
Recompensa: 20
Estado: 1 abierta
Reportes: 10
Visualisaciones: 500
Pregunta: "¿Quien mató a Marilin?"
Etiquetas: "marilin" "los prisioneros" "asesinato"
Respuestas: 1 "los comunistas"

(crearPregunta 2 202011101930 "Seba" 0 12 20 1 10 35 "2+2 es 5??" (list "suma" "tonterias" "5") (list "Sin Respuesta"))
ID Pregunta: 2
Fecha de creación: 202011101930
Autor: "Seba"
Votos Favor: 0
Votos Contra: 12
Recompensa: 20
Estado: 1 abierta
Reportes: 10
Visualisaciones: 35
Pregunta: "¿2+2 es 5?"
Etiquetas: "suma" "tonterias" "5"
Respuestas: "Sin Respuesta"

(crearPregunta 3 202011101930 "yanira" 21 12 220 1 3 1243 "Que le pasa a lupita??" (list "lupita" "que le pasa" "que")
ID Pregunta: 3
Fecha de creación: 202011101930
Autor: "yanira" 
Votos Favor: 21
Votos Contra: 12
Recompensa: 220
Estado: 1 abierta
Reportes: 3
Visualisaciones: 1243
Pregunta: "Que le pasa a lupita??"
Etiquetas: "lupita" "que le pasa" "que"
Respuestas: 1 "nose" , 2 "Ella quiere bailar", 3 "Le falta un amor"

##############
# Respuestas #
##############
(crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "nose" "Lupitaaaa")
ID Respuesta: 1
Fecha de creación:202011101930
Autor: "bayron"
Votos Favor: 0
Votos Contra: 0
Estado de Aceptacion: 0
Reportes de Ofensa: 0
Respuesta: "nose"
Etiquetas: "Lupitaaaa"

(crearRespuesta 2 202011101930 "sebiuo" 0 0 0 0 "Ella quiere bailar" "baile")
ID Respuesta: 2
Fecha de creación: 202011101930
Autor: "sebiuo"
Votos Favor: 0
Votos Contra: 0
Estado de Aceptacion: 0
Reportes de Ofensa: 0
Respuesta: "Ella quiere bailar"
Etiquetas: "baile"

(crearRespuesta 3 202011101930 "jose" 0 0 0 0 "Le falta un amor" (list "amor" "dios es amor"))
ID Respuesta: 3
Fecha de creación:
Autor: "jose"
Votos Favor: 0
Votos Contra: 0
Estado de Aceptacion: 0
Reportes de Ofensa: 0
Respuesta: "Le falta un amor"
Etiquetas: "amor" "dios es amor"

#################################################### * FUNCIONES * ######################################################

#############
# Register  #
#############
(register stack username password)

1) Se registra un nuevo usaurio, Nombre: Rosa Password: Linda

(register (crearStack (list (crearPregunta 1 202011101930 "jorge gonzalez" 123 2 20 1 10 500 "¿Quien mató a Marilin?" (list "marilin" "los prisioneros" "asesinato") (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "los comunistas" (list "red" "marx" "tiempos mejores"))) (crearPregunta 2 202011101930 "Seba" 0 12 20 1 10 35 "¿2+2 es 5?" (list "suma" "tonterias" "5") (list "Sin Respuesta")) (crearPregunta 3 202011101930 "yanira" 21 12 220 1 3 1243 "Que le pasa a lupita??" (list "lupita" "que le pasa" "que") (list (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "nose" "Lupitaaaa")(crearRespuesta 2 202011101930 "sebiuo" 0 0 0 0 "Ella quiere bailar" "baile")(crearRespuesta 3 202011101930 "jose" 0 0 0 0 "Le falta un amor" (list "amor" "dios es amor")))))
(list (crearUsuario 1 "bayron" "password" 50 0)(crearUsuario 2 "yanira" "1234" 50 0)(crearUsuario 3 "berson" "342423" 50 0)(crearUsuario 4 "sebiuo" "notengo" 50 0)(crearUsuario 5 "Seba" "sitengo" 50 0)(crearUsuario 6 "jorge gonzalez" "narea_te_amo" 50 0)))
 "Rosa" "Linda")

Retorna un nuevo stack con el usuario creado
IdUsuario 7
Usuario: "Rosa"
Password: "Linda"
Reputacion: 50
Estado: 0

2) Se registra un nuevo usaurio, Nombre: Seba Password: 123654

(register (crearStack (list (crearPregunta 1 202011101930 "jorge gonzalez" 123 2 20 1 10 500 "¿Quien mató a Marilin?" (list "marilin" "los prisioneros" "asesinato") (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "los comunistas" (list "red" "marx" "tiempos mejores"))) (crearPregunta 2 202011101930 "Seba" 0 12 20 1 10 35 "¿2+2 es 5?" (list "suma" "tonterias" "5") (list "Sin Respuesta")) (crearPregunta 3 202011101930 "yanira" 21 12 220 1 3 1243 "Que le pasa a lupita??" (list "lupita" "que le pasa" "que") (list (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "nose" "Lupitaaaa")(crearRespuesta 2 202011101930 "sebiuo" 0 0 0 0 "Ella quiere bailar" "baile")(crearRespuesta 3 202011101930 "jose" 0 0 0 0 "Le falta un amor" (list "amor" "dios es amor")))))
(list (crearUsuario 1 "bayron" "password" 50 0)(crearUsuario 2 "yanira" "1234" 50 0)(crearUsuario 3 "berson" "342423" 50 0)(crearUsuario 4 "sebiuo" "notengo" 50 0)(crearUsuario 5 "Seba" "sitengo" 50 0)(crearUsuario 6 "jorge gonzalez" "narea_te_amo" 50 0)))"Seba" "123654")

Retorna: "El Usuario ya se encuentra registrado, elegir nuevo Usuario"


#############
#    ASK    #
#############
(((login stack usuario password ask)fecha)pregunta etiquetas)

1) Hace una pregunta el usuario Seba, con fecha 13-11-2020 16:30 , ¿Quien es el profesor del ramo?, 
con las etiquetas Ayuda, Paso el ramo, un 7, en el stack con los nuevos usuarios registrados.

(((login (register (crearStack (list (crearPregunta 1 202011101930 "jorge gonzalez" 123 2 20 1 10 500 "¿Quien mató a Marilin?" (list "marilin" "los prisioneros" "asesinato") (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "los comunistas" (list "red" "marx" "tiempos mejores"))) (crearPregunta 2 202011101930 "Seba" 0 12 20 1 10 35 "¿2+2 es 5?" (list "suma" "tonterias" "5") (list "Sin Respuesta")) (crearPregunta 3 202011101930 "yanira" 21 12 220 1 3 1243 "Que le pasa a lupita??" (list "lupita" "que le pasa" "que") (list (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "nose" "Lupitaaaa")(crearRespuesta 2 202011101930 "sebiuo" 0 0 0 0 "Ella quiere bailar" "baile")(crearRespuesta 3 202011101930 "jose" 0 0 0 0 "Le falta un amor" (list "amor" "dios es amor")))))
(list (crearUsuario 1 "bayron" "password" 50 0)(crearUsuario 2 "yanira" "1234" 50 0)(crearUsuario 3 "berson" "342423" 50 0)(crearUsuario 4 "sebiuo" "notengo" 50 0)(crearUsuario 5 "Seba" "sitengo" 50 0)(crearUsuario 6 "jorge gonzalez" "narea_te_amo" 50 0)))"Rosa" "Linda") "Seba" "sitengo" ask)202011131630)"¿Quien es el profesor del ramo?" (list  "Ayuda" "Paso el ramo" "un 7"))

El retorno es un nuevo stack con una nueva pregunta, con id 4, con estado abierto.

#################
#    REWARD     #
#################
 Se agregará una recompensa a la pregunta recien creada por el usuario Seba, 
al ser un nuevo usuario solo tiene 50 puntos maximo a ofrecer por pregunta, 
se revisan 3 escenarios posibles, en que el usuario quiera dar distintos montos.

1- Recompensa = 60
(((login (((login (register (crearStack (list (crearPregunta 1 202011101930 "jorge gonzalez" 123 2 20 1 10 500 "¿Quien mató a Marilin?" (list "marilin" "los prisioneros" "asesinato") (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "los comunistas" (list "red" "marx" "tiempos mejores"))) (crearPregunta 2 202011101930 "Seba" 0 12 20 1 10 35 "¿2+2 es 5?" (list "suma" "tonterias" "5") (list "Sin Respuesta")) (crearPregunta 3 202011101930 "yanira" 21 12 220 1 3 1243 "Que le pasa a lupita??" (list "lupita" "que le pasa" "que") (list (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "nose" "Lupitaaaa")(crearRespuesta 2 202011101930 "sebiuo" 0 0 0 0 "Ella quiere bailar" "baile")(crearRespuesta 3 202011101930 "jose" 0 0 0 0 "Le falta un amor" (list "amor" "dios es amor")))))
(list (crearUsuario 1 "bayron" "password" 50 0)(crearUsuario 2 "yanira" "1234" 50 0)(crearUsuario 3 "berson" "342423" 50 0)(crearUsuario 4 "sebiuo" "notengo" 50 0)(crearUsuario 5 "Seba" "sitengo" 50 0)(crearUsuario 6 "jorge gonzalez" "narea_te_amo" 50 0)))"Rosa" "Linda") "Seba" "sitengo" ask)202011131630)"¿Quien es el profesor del ramo?" (list  "Ayuda" "Paso el ramo" "un 7"))"Seba" "sitengo" reward)4)60)

Retorno: "No puedes poner esta recompensa"

2- Recompensa = 100
(((login (((login (register (crearStack (list (crearPregunta 1 202011101930 "jorge gonzalez" 123 2 20 1 10 500 "¿Quien mató a Marilin?" (list "marilin" "los prisioneros" "asesinato") (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "los comunistas" (list "red" "marx" "tiempos mejores"))) (crearPregunta 2 202011101930 "Seba" 0 12 20 1 10 35 "¿2+2 es 5?" (list "suma" "tonterias" "5") (list "Sin Respuesta")) (crearPregunta 3 202011101930 "yanira" 21 12 220 1 3 1243 "Que le pasa a lupita??" (list "lupita" "que le pasa" "que") (list (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "nose" "Lupitaaaa")(crearRespuesta 2 202011101930 "sebiuo" 0 0 0 0 "Ella quiere bailar" "baile")(crearRespuesta 3 202011101930 "jose" 0 0 0 0 "Le falta un amor" (list "amor" "dios es amor")))))
(list (crearUsuario 1 "bayron" "password" 50 0)(crearUsuario 2 "yanira" "1234" 50 0)(crearUsuario 3 "berson" "342423" 50 0)(crearUsuario 4 "sebiuo" "notengo" 50 0)(crearUsuario 5 "Seba" "sitengo" 50 0)(crearUsuario 6 "jorge gonzalez" "narea_te_amo" 50 0)))"Rosa" "Linda") "Seba" "sitengo" ask)202011131630)"¿Quien es el profesor del ramo?" (list  "Ayuda" "Paso el ramo" "un 7"))"Seba" "sitengo" reward)4)100)

Retorno: "No puedes poner esta recompensa"

3- Recompensa = 10

(((login (((login (register (crearStack (list (crearPregunta 1 202011101930 "jorge gonzalez" 123 2 20 1 10 500 "¿Quien mató a Marilin?" (list "marilin" "los prisioneros" "asesinato") (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "los comunistas" (list "red" "marx" "tiempos mejores"))) (crearPregunta 2 202011101930 "Seba" 0 12 20 1 10 35 "¿2+2 es 5?" (list "suma" "tonterias" "5") (list "Sin Respuesta")) (crearPregunta 3 202011101930 "yanira" 21 12 220 1 3 1243 "Que le pasa a lupita??" (list "lupita" "que le pasa" "que") (list (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "nose" "Lupitaaaa")(crearRespuesta 2 202011101930 "sebiuo" 0 0 0 0 "Ella quiere bailar" "baile")(crearRespuesta 3 202011101930 "jose" 0 0 0 0 "Le falta un amor" (list "amor" "dios es amor")))))
(list (crearUsuario 1 "bayron" "password" 50 0)(crearUsuario 2 "yanira" "1234" 50 0)(crearUsuario 3 "berson" "342423" 50 0)(crearUsuario 4 "sebiuo" "notengo" 50 0)(crearUsuario 5 "Seba" "sitengo" 50 0)(crearUsuario 6 "jorge gonzalez" "narea_te_amo" 50 0)))"Rosa" "Linda") "Seba" "sitengo" ask)202011131630)"¿Quien es el profesor del ramo?" (list  "Ayuda" "Paso el ramo" "un 7"))"Seba" "sitengo" reward)4)10)

Retorno: Un nuevo stack con una recompensa de 10 en la pregunta con id:4 y 10 puntos menos en los puntos del usuario.

#################
#    ANSWER     #
#################
Se agregará una respuesta a la pregunta con id:4 ,¿Quien es el profesor del ramo?, con la fecha 13-11-2020 16:30,
La respuesta es "Sir Roberto Gonzalez" y las etiquetas "Robert" "daProfe" "ayuda".

((((login (((login (((login (register (crearStack (list (crearPregunta 1 202011101930 "jorge gonzalez" 123 2 20 1 10 500 "¿Quien mató a Marilin?" (list "marilin" "los prisioneros" "asesinato") (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "los comunistas" (list "red" "marx" "tiempos mejores"))) (crearPregunta 2 202011101930 "Seba" 0 12 20 1 10 35 "¿2+2 es 5?" (list "suma" "tonterias" "5") (list "Sin Respuesta")) (crearPregunta 3 202011101930 "yanira" 21 12 220 1 3 1243 "Que le pasa a lupita??" (list "lupita" "que le pasa" "que") (list (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "nose" "Lupitaaaa")(crearRespuesta 2 202011101930 "sebiuo" 0 0 0 0 "Ella quiere bailar" "baile")(crearRespuesta 3 202011101930 "jose" 0 0 0 0 "Le falta un amor" (list "amor" "dios es amor")))))
(list (crearUsuario 1 "bayron" "password" 50 0)(crearUsuario 2 "yanira" "1234" 50 0)(crearUsuario 3 "berson" "342423" 50 0)(crearUsuario 4 "sebiuo" "notengo" 50 0)(crearUsuario 5 "Seba" "sitengo" 50 0)(crearUsuario 6 "jorge gonzalez" "narea_te_amo" 50 0)))"Rosa" "Linda") "Seba" "sitengo" ask)202011131630)"¿Quien es el profesor del ramo?" (list  "Ayuda" "Paso el ramo" "un 7"))"Seba" "sitengo" reward)4)10) "Seba" "sitengo" answer)202011131630)4)"Sir Roberto Gonzalez" (list "Robert" "daProfe" "ayuda"))

El retorno es un nuevo stack con la nueva respuesta a la pregunta realizada.

#################
#    ACCEPT     #
#################
(((login stack user pass accept)idpregunta)idrespuesta)

El usuario "yanira" aceptará la respuesta del usuario "bayron", de su pregunda con id:3 y la respuesta con id:1 con esto el estado de la respuesta cambiará a aceptado y
la recompensa se agregará al usuario.

(((login ((((login (((login (((login (register (crearStack (list (crearPregunta 1 202011101930 "jorge gonzalez" 123 2 20 1 10 500 "¿Quien mató a Marilin?" (list "marilin" "los prisioneros" "asesinato") (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "los comunistas" (list "red" "marx" "tiempos mejores"))) (crearPregunta 2 202011101930 "Seba" 0 12 20 1 10 35 "¿2+2 es 5?" (list "suma" "tonterias" "5") (list "Sin Respuesta")) (crearPregunta 3 202011101930 "yanira" 21 12 220 1 3 1243 "Que le pasa a lupita??" (list "lupita" "que le pasa" "que") (list (crearRespuesta 1 202011101930 "bayron" 0 0 0 0 "nose" "Lupitaaaa")(crearRespuesta 2 202011101930 "sebiuo" 0 0 0 0 "Ella quiere bailar" "baile")(crearRespuesta 3 202011101930 "jose" 0 0 0 0 "Le falta un amor" (list "amor" "dios es amor")))))
(list (crearUsuario 1 "bayron" "password" 50 0)(crearUsuario 2 "yanira" "1234" 50 0)(crearUsuario 3 "berson" "342423" 50 0)(crearUsuario 4 "sebiuo" "notengo" 50 0)(crearUsuario 5 "Seba" "sitengo" 50 0)(crearUsuario 6 "jorge gonzalez" "narea_te_amo" 50 0)))"Rosa" "Linda") "Seba" "sitengo" ask)202011131630)"¿Quien es el profesor del ramo?" (list  "Ayuda" "Paso el ramo" "un 7"))"Seba" "sitengo" reward)4)10) "Seba" "sitengo" answer)202011131630)4)"Sir Roberto Gonzalez" (list "Robert" "daProfe" "ayuda"))
 "yanira" "1234" accept)3)1)

El retorno es un nuevo stack con la reputación de bayron aumentada en 220, y la respuesta cambiada a estado acecptada
y un voto a favor.
|#