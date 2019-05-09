#lang racket

(define (prisma a b c) (list a b c))

(define (prisma_fin? x)
  (if (empty? x) #t (if (= (car x) 1) (prisma_fin? (cdr x)) #f)))

(define (prisma_positivo? x)
  (if (empty? x) #t (if (> (car x) 0) (prisma_positivo? (cdr x)) #f)))

(define (elemento_l lista indice)
  (list-ref lista (- indice 1)))

(define (cortar_ancho x y) (cons (- (elemento_l x 1) y) (cdr x)))

(define (cortar_alto x y) (append (list (car x) (- (elemento_l x 2) y)) (cdr (cdr x))))

(define (cortar_largo x y) (append (list (car x) (car (cdr x))) (list (- (elemento_l x 3) y))))

;;Resta Vertical
(define (corteAnchura prisma n)
  (if (or (>= n (elemento_l prisma 1)) (= n 0))
      (begin
        (display "Corte en anchura no posible\n")
        (jugarAncho prisma))
      (cortar_ancho prisma n)))

(define (jugarAncho prisma)
  (if (> (elemento_l prisma 1) 1)
      (begin
        (display "\nElija un numero de corte \n")
        (corteAnchura prisma (read)))
      (begin
        (display "No se puede hacer el corte")
        (pedirJugada prisma))))

;;Resta Horizontal
(define (corteAltura prisma n)
  (if (or (>= n (elemento_l prisma 2)) (= n 0))
      (begin
        (display "Corte en altura no posible\n")
        (jugarAlto prisma))
      (cortar_alto prisma n)))

(define (jugarAlto prisma)
  (if (> (elemento_l prisma 2) 1)
      (begin
        (display "\nElija un numero de corte \n")
        (corteAltura prisma (read)))
      (begin
        (display "No se puede hacer el corte")
        (pedirJugada prisma))))

;;Resta Profundidad
(define (corteLongitud prisma n)
  (if (or (>= n (elemento_l prisma 3)) (= n 0))
      (begin
        (display "Corte en profundidad no posible\n")
        (jugarLargo prisma))
      (cortar_largo prisma n)))

(define (jugarLargo prisma)
  (if (> (elemento_l prisma 3) 1)
      (begin
        (display "\nElija un numero de corte \n")
        (corteLongitud prisma (read)))
      (begin
        (display "No se puede hacer el corte")
        (pedirJugada prisma))))

;;Seleccionar jugada
(define (pedirJugada prisma)
  (begin
    (display "\nSeleccione [1/2/3]-[Ancho/Alto/Largo]\n");
    (let ([direccion (read)])
      (cond
        [(= direccion 1) (jugarAncho prisma)]
        [(= direccion 2) (jugarAlto prisma)]
        [(= direccion 3) (jugarLargo prisma)]
        [else (begin
                (display "\nNo ha seleccionado una opcion correcta\n")
                (pedirJugada prisma))]))))

(define (prisma_valido? prisma)
  (if (and (not (prisma_fin? prisma)) (prisma_positivo? prisma)) #t #f))

(define (pedirPrisma)
  (begin
    (display "\nIntroduce las dimensiones del prisma\n")
    (let ([prism (prisma (read)(read)(read))])
      (if (prisma_valido? prism)
          prism
          (begin
            (display "* Dimensiones del prisma inválidas *\n")
            (pedirPrisma))))))

;Sorteamos que jugador juega primero
(define (turno) (generarRandom 2))

(define (generarRandom limite) (random limite))

; Funciones maximo y minimo
(define (min n m) (if (>= m n) n m))
(define (max n m) (if (>= m n) m n))

; Genera las jugadas en Ancho.
(define (listaCortesAncho prisma) (listaCortesAncho_aux prisma 1))

(define (listaCortesAncho_aux prisma n)
  (if (< n (elemento_l prisma 1))
      (cons (cortar_ancho prisma n) (listaCortesAncho_aux prisma (+ n 1)))
      '()))

; Genera las jugadas en Alto.
(define (listaCortesAlto prisma) (listaCortesAlto_aux prisma 1))

(define (listaCortesAlto_aux prisma n)
  (if (< n (elemento_l prisma 2))
      (cons (cortar_alto prisma n) (listaCortesAlto_aux prisma (+ n 1)))
      '()))

; Genera las jugadas en Largo.
(define (listaCortesLargo prisma) (listaCortesLargo_aux prisma 1))

(define (listaCortesLargo_aux prisma n)
  (if (< n (elemento_l prisma 3))
      (cons (cortar_largo prisma n) (listaCortesLargo_aux prisma (+ n 1)))
      '()))

; Genera el total de jugadas permitidas desde un nodo
(define (listaJugadas prisma)
  (append (append (listaCortesAncho prisma) (listaCortesAlto prisma)) (listaCortesLargo prisma)))

;;JUGAR
(define (jugar)
  (begin
    (let([prism (pedirPrisma)])
      (if (= (turno) 0)
         (begin
           (display "\n*** COMIENZA EL JUGADOR ***\n")
           (partida prism 0))
         (begin
           (display "\n*** COMIENZA LA MAQUINA ***\n")
           (partida prism 1))))))

(define (partida prisma jugador)
  (begin
    (display "\n*** Prisma actual ")
    (display prisma)
    (display " ***\n")
    (if (prisma_fin? prisma)
      (if (= jugador 0) (display "\n*** HAS PERDIDO ***\n") (display "\n*** HAS GANADO ***\n"))
      (if (= jugador 0)
          (partida (pedirJugada prisma) 1)
          (partida (juegaMaquina prisma) 0)))))

;En caso de que no haya ninguna jugada para ganar, devolvemos una aleatoria de entre las posibles
(define (juegaMaquina prisma)
  (begin
    (display "\n*** Juega la maquina ***\n")
    (let ([jugadasPosibles (listaJugadas prisma)])
      (let ([resultadoJuegaMaquina (juegaMaquina_aux jugadasPosibles)])
        (if (number? resultadoJuegaMaquina)
            (elemento_l jugadasPosibles (+ (generarRandom (length jugadasPosibles)) 1))
            resultadoJuegaMaquina)))))
      
(define (juegaMaquina_aux listaJugadas)
  (if (= (length listaJugadas) 0) -1
      (if (= (nodoMinMax (car listaJugadas) 1) 1)
          (car listaJugadas)
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


(define (juego)
  (begin
    (display "\n*** BIENVENIDO AL JUEGO DE LA CANTERA ***")
    (display "\n  *** Gabriel - Sergio - Alvaro ***\n")
    (jugar)))

;Ejecucion automatica del juego
(juego)