#lang racket

(define (prisma a b c) (list a b c))

(define (prisma_fin? x)
  (if (empty? x) #t (if (= (car x) 1) (prisma_fin? (cdr x)) #f)))

(define (elemento_l lista indice)
  (list-ref lista (- indice 1)))

(define (cortar_ancho x y) (cons (- (elemento_l x 1) y) (cdr x)))

(define (cortar_alto x y) (append (list (car x) (- (elemento_l x 2) y)) (cdr (cdr x))))

(define (cortar_largo x y) (append (list (car x) (car (cdr x))) (list (- (elemento_l x 3) y))))

;;Resta Vertical
(define (restaAncho prisma n) (if (or (>= n (elemento_l prisma 1)) (= n 0))
                                 (begin(display "Movimiento anchura no posible\n") ; Si no es correcto notificamos y volvemos a pedir.
                                                                    (jugarAncho prisma))
                                 (cortar_ancho prisma n))) ; Si es movimiento correcto devolvemos la lista.

(define (jugarAncho prisma) (if (> (elemento_l prisma 1) 1)
                               (begin
                                   (display "\nElija un numero para el corte: ")
                                   (restaAncho prisma (read))) ; Llamamos a resta Vertical con la eleccion.
                               (display "No se puede hacer el corte")))

;;Resta Horizontal
(define (restaAlto prisma n) (if (or (>= n (elemento_l prisma 2)) (= n 0))
                                 (begin(display "Movimiento altura no posible\n") ; Si no es correcto notificamos y volvemos a pedir.
                                                                    (jugarAlto prisma))
                                 (cortar_alto prisma n))) ; Si es movimiento correcto devolvemos la lista.

(define (jugarAlto prisma) (if (> (elemento_l prisma 2) 1)
                               (begin
                                   (display "\nElija un numero para el corte: ")
                                   (restaAlto prisma (read))) ; Llamamos a resta Vertical con la eleccion.
                               (display "No se puede hacer el corte")))

;;Resta Profundidad
(define (restaLargo prisma n) (if (or (>= n (elemento_l prisma 3)) (= n 0))
                                 (begin(display "Movimiento altura no posible\n") ; Si no es correcto notificamos y volvemos a pedir.
                                                                    (jugarLargo prisma))
                                 (cortar_largo prisma n))) ; Si es movimiento correcto devolvemos la lista.

(define (jugarLargo prisma) (if (> (elemento_l prisma 3) 1)
                               (begin
                                   (display "\nElija un numero para el corte: ")
                                   (restaLargo prisma (read))) ; Llamamos a resta Vertical con la eleccion.
                               (display "No se puede hacer el corte")))

;;Seleccionar jugada
(define (pedirJugada prisma)
  (begin
    (display "Prisma actual:")
    (display prisma)
    (display "\n")
        (display "\nSeleccione [0/1/2]-[Ancho/Alto/Largo]\n"); Pregunta por jugada.
        (let ([direccion (read)])
          (cond
            [(= direccion 0) (jugarAncho prisma)]
            [(= direccion 1) (jugarAlto prisma)]
            [(= direccion 2) (jugarLargo prisma)]
            [else (begin(display "\nNo ha seleccionado una opcion correcta\n")
                  (pedirJugada prisma))]))))

;;JUGAR
(define (jugar) (begin(display "\nIntroduce las dimensiones del prisma\n")
                      (let([prism (prisma (read)(read)(read))])
                      (if(= (turno) 0) (begin(display "Le toca empezar al jugador 1\n")
                                               (partida prism 0)) ; Comienzo yo.
                                         (begin(display "Le toca empezar a la maquina\n")
                                               (partida prism 1)))))) ; Comienza la maquina.))

(define (partida prisma jugador)
  (if (prisma_fin? prisma)
      (if (= jugador 0) (display "HAS PERDIDO\nMORAIS TE VA A ATROPELLAR") (display "HAS GANADO CRACK MORAIS"))
      (if (= jugador 0) (partida (pedirJugada prisma) 1)
          (partida (juegaMaquina prisma) 0))))

;Sorteamos que jugador juega primero
(define (turno) (random 2))

; Funciones maximo y minimo
(define (min n m) (if(>= m n) n m))
(define (max n m) (if(>= m n) m n))

; Genera las jugadas en Ancho.
(define (listaCortesAncho prisma) (listaCortesAncho_aux prisma 1))

(define (listaCortesAncho_aux prisma n) (if(< n (elemento_l prisma 1)) (cons (cortar_ancho prisma n) (listaCortesAncho_aux prisma (+ n 1))) '() ))

; Genera las jugadas en Alto.
(define (listaCortesAlto prisma) (listaCortesAlto_aux prisma 1))

(define (listaCortesAlto_aux prisma n) (if(< n (elemento_l prisma 2)) (cons (cortar_alto prisma n) (listaCortesAlto_aux prisma (+ n 1))) '() ))

; Genera las jugadas en Largo.
(define (listaCortesLargo prisma) (listaCortesLargo_aux prisma 1))

(define (listaCortesLargo_aux prisma n) (if(< n (elemento_l prisma 3)) (cons (cortar_largo prisma n) (listaCortesLargo_aux prisma (+ n 1))) '() ))

; Genera el total de jugadas permitidas de una rama.
(define (listaJugadas prisma) (append (append (listaCortesAncho prisma) (listaCortesAlto prisma)) (listaCortesLargo prisma)))

(define (juegaMaquina prisma)
  (let ([jugadasPosibles (listaJugadas prisma)])
    (let ([resultadoJuegaMaquina (juegaMaquina_aux jugadasPosibles)])
      (if (list? resultadoJuegaMaquina)
          (begin (display "Maquina juega con ")
                 (display resultadoJuegaMaquina)
                 resultadoJuegaMaquina)
          (begin (display "Maquina juega con ")
                 (display (car jugadasPosibles))
          (car jugadasPosibles))))))
      
(define (juegaMaquina_aux listaJugadas)
  (if (= (length listaJugadas) 0) -1
      (if (= (nodoMinMax (car listaJugadas) 1) 1) (car listaJugadas)
          (juegaMaquina_aux (cdr listaJugadas)))))

;;Nivel 0-> Max
(define (nodoMinMax nodo nivel)
  (if (prisma_fin? nodo)
      (if (= nivel 0) 0 1)
      (let ([listaJugadas (listaJugadas nodo)])
        (if (= nivel 0)
            (llamarMax listaJugadas)
            (llamarMin listaJugadas)))))

(define (llamarMax listaJugadas)
  (if (= (length listaJugadas) 0)
      0
      (max (max 0 (nodoMinMax (car listaJugadas) 1)) (llamarMax (cdr listaJugadas)))))

(define (llamarMin listaJugadas)
  (if (= (length listaJugadas) 0)
      1
      (min (min 1 (nodoMinMax (car listaJugadas) 0)) (llamarMin (cdr listaJugadas)))))