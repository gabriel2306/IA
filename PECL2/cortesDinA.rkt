#lang racket
; Resta en n las columnas, si no esta permitido se notifica.
; verticales identifica el maximo numero de cortes verticales.
(define (restaVertical actual n) (if(or (> n (car actual)) (= n 0)) (begin(printf "Movimiento vertical no posible\n") ; Si no es correcto notificamos y volvemos a pedir.
                                                                    (jugarVertical actual))
                                             (list (- (car actual) n) (cadr actual)))) ; Si es movimiento correcto devolvemos la lista.
; Resta en n las filas, si no esta permitido se notifica.
; horizontales identifica el maximo numero de cortes horizontales.
(define (restaHorizontal actual n) (if(or (> n (cadr actual)) (= n 0)) (begin(printf "Movimiento horizontal no posible\n") ; Si no es correcto notificamos y volvemos a pedir.
                                                                       (jugarHorizontal actual))
                                             (list (car actual) (- (cadr actual) n))))  ; Si es movimiento correcto devolvemos la lista.

; Si el jugador elige jugar Horizontalmente se le pide un numero de corte.
; actual es el nodo actual.
(define (jugarHorizontal actual) (if(> (cadr actual) 0) (begin(display "Maximo numero de cortes: ") ; Pregunta por jugada.
                                                              (display (cadr actual))
                                                              (display "\nElija un numero igual o inferior y superior a 0: ")
                                                              (let ([eleccion (read)]) (restaHorizontal actual eleccion))) ; Llamamos a resta Horizontal con la eleccion.
                                                         (begin(display "No ha seleccionado una opcion correcta\n") ; No era posible en esa direccion
                                                                                           (pedirJugada actual)))) 

; Si el jugador elige jugar Verticalmente se le pide un numero de corte.
; actual es el nodo actual.
(define (jugarVertical actual) (if(> (car actual) 0) (begin(display "Maximo numero de cortes: ") ; Pregunta por jugada.
                                                              (display (car actual))
                                                              (display "\nElija un numero igual o inferior y superior a 0: ")
                                                              (let ([eleccion (read)]) (restaVertical actual eleccion))) ; Llamamos a resta Horizontal con la eleccion.
                                                         (begin(display "No ha seleccionado una opcion correcta\n") ; No era posible en esa direccion
                                                                                           (pedirJugada actual)))) 

; Funcion que pide la jugada para el jugador.
; actual es el nodo actual.
(define (pedirJugada actual) (begin(display "\nSeleccione [0/1] Vertical/Horizontal? Valor actual: ") ; Pregunta por jugada.
                                   (display actual)
                                   (let ([eleccion (read)]) (if(= eleccion 0) (jugarVertical actual) ; Eligio verticalmente.
                                                                         (if(= eleccion 1) (jugarHorizontal actual) ; Eligió Horizontalmente
                                                                                           (begin(display "No ha seleccionado una opcion correcta\n") ; No puso ni 0 ni 1.
                                                                                           (pedirJugada actual)))))))

; Funciones maximo y minimo personalizadas.
(define (min n m) (if(>= m n) n m))
(define (max n m) (if(>= m n) m n))

; Funcion recursiva que recibe la longitud de la lista y devuelve el maximo de las jugadas.
; Anterior es el mayor anterior, jugadas es las jugadas restantes.
(define (llamarMax anterior jugadas) (if(> (length jugadas) 0) (max (max anterior (min-max (car jugadas) 1)) (llamarMax anterior (cdr jugadas))) ; Si es la ultima jugada, devuelvo el maximo entre el anterior y realizar esa.
                                                               0)) ; Si no quedan jugadas identificamos el valor maximo como 0.
; Funcion recursiva que recibe la longitud de la lista y devuelve el maximo de las jugadas.
; Anterior es el mayor anterior, jugadas es las jugadas restantes.
(define (llamarMin anterior jugadas) (if(> (length jugadas) 0) (min (min anterior (min-max (car jugadas) 0)) (llamarMin anterior (cdr jugadas))) ; Si es la ultima jugada, devuelvo el minimo entre el anterior y realizar esa.
                                                               1)) ; Si no quedan jugadas identificamos el valor minimo como 1.

; Genera las jugadas en vertical.
(define (genVerticales actual n) (if(<= n (car actual)) (cons (list (- (car actual) n) (cadr actual)) (genVerticales actual (+ n 1)))
                                                       '()))
; Genera las jugadas en horizontal.
(define (genHorizontales actual n) (if(<= n (cadr actual)) (cons (list (car actual) (- (cadr actual) n)) (genHorizontales actual (+ n 1)))
                                                       '()))

; Genera el total de jugadas permitidas de una rama.
; Actual identifica la rama. Devuelve una lista de lista ((2 1) (2 0) ...).
(define (genJugadas actual) (append (genVerticales actual 1) (genHorizontales actual 1)))

; Inicia la recursion Min-max
; Actual es lista de pares actual, jugador es el jugador actual. (0 Max, 1 Min).
(define (min-max actual jugador) (if(equal? actual '(0 0)) (if(= jugador 0) 0 1) ; Si se han acabado las jugadas si jugador igual max, min gana.
                                                      (if(= jugador 0) (llamarMax 0 (genJugadas actual)) ; En cualquier otro caso si el jugador es max, buscar el maximo
                                                                       (llamarMin 1 (genJugadas actual))))) ; Si no buscar el minimo.

; Determina que jugada permitirá a min ganar
(define (juegaMin jugadas) (if(> (length jugadas) 0) (if(= (min-max (car jugadas) 0) 0) (car jugadas) ; Si Max puede perder con esta jugada la devuelvo como buena.
                                                                             (juegaMin (cdr jugadas))) ; Si no busco una jugada mejor dentro de las posibles.
                                                                              -1)) ; Aqui ya perdería min, devolvemos un valor de control.
                                                             
(define (solitario actual jugador) (if(equal? actual '(0 0)) (if(= jugador 0) (begin(display "\n Ha perdido el jugador")
                                                                                    (pedirRepetir))
                                                                              (begin(display "\n Ha perdido la máquina")
                                                                                    (pedirRepetir))) ; Si se han acabado las jugadas si jugador igual max, min gana.
                                                     (if(= jugador 1) (if(list? (juegaMin (genJugadas actual))) (begin(display " Min decide jugar con: ")
                                                                                                                      (display (juegaMin (genJugadas actual)))
                                                                                                                      (solitario (juegaMin (genJugadas actual)) 0)) ; Min juega con la jugada mas apropiada.
                                                                                                                (begin(display " Min decide jugar con: ")
                                                                                                                      (display (car(genJugadas actual)))
                                                                                                                      (solitario (car(genJugadas actual)) 0))) ; Min ha visto que pierde pero juega con la primera jugada.
                                                                      (solitario (pedirJugada actual) 1)))) ; Si le toca a jugador solicitamos jugada y jugamos
                                                                                                            ; con el resultado.

; Procedimiento que lanza una moneda de 2 caras.
(define (moneda) (random 2))
; Realiza el sorteo para ver quien comienza.
(define (sortea lista) (if(= (moneda) 0) (begin(display "Le toca empezar al jugador 1\n")
                                               (solitario lista 0)) ; Comienzo yo.
                                         (begin(display "Le toca empezar a la maquina\n")
                                               (solitario lista 1)))) ; Comienza la maquina.

; Determina el juego dependiendo del tamaño elegido.
(define (tamanioFolio n) (cond((= n 1) (sortea (list 0 1))) ; 1 corte inicia el DinA1
                              ((= n 2) (sortea (list 1 1))) ; Otro horizontal el dinA2
                              ((= n 3) (sortea (list 1 3))) ; A partir de aqui se duplica y se suma la linea divisoria.
                              ((= n 4) (sortea (list 3 3)))
                              ((= n 5) (sortea (list 3 7)))
                              ((= n 6) (sortea (list 7 7)))
                              ((= n 7) (sortea (list 7 15)))
                              ((= n 8) (sortea (list 15 15)))
                              ((= n 9) (sortea (list 15 31)))
                              ((= n 10) (sortea (list 31 31)))
                              ((or (> n 10) (< n 1)) (begin(display "Algo a ido mal, seleccione un tamanio entre 1 y 10 :D\n") ; Eligio un dinA incorrecto.
                                                           (iniciaJuego)))))
; Pide al jugador si desea jugar de nuevo.
(define (pedirRepetir) (begin(display "\nDesea seguir jugando? [1/0]: ")
                        (let ([eleccion (read)]) (if(= eleccion 1) (iniciaJuego)
                                                    (if(= eleccion 0) (begin(display "\n\n GRACIAS POR UTILIZAR NUESTRO PROGRAMA")
                                                                            (display "\n CREADO POR: \n\t\tJOSE ANDRES GUERRA BERMEJO \n\t\tCRISTINA SAEZ ORTEGA \n\t\tJAVIER PALOMARES RUIZ")
                                                                            (read))
                                                     (begin(display "Elija una opcion correcta\n")
                                                           (pedirRepetir)))))))
; Inicia el juego.
(define (iniciaJuego) (begin(display "\nIntroduce el dato de 'n' para el folio » ")
                            (tamanioFolio (read))))                
  
(display "BIENVENIDO AL PROGRAMA\n")
(iniciaJuego)
                                                                      
