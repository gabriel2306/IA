#lang racket
;; ************************
;;   JUEGO DE LA CANTERA
;;  López Cuenca, Gabriel
;;  Sanz Sacristán, Sergio
;; Zamorano Ortega, Álvaro
;; ************************

; Crear prisma con 3 numeros
(define (prisma a b c) (list a b c))

; Comprobar si es prisma final -> (1 1 1)
(define (prisma_fin? x)
  (if (empty? x) #t (if (= (car x) 1) (prisma_fin? (cdr x)) #f)))

; Comprobar si los valores del prisma son validos
(define (prisma_positivo? x)
  (if (empty? x) #t (if (> (car x) 0) (prisma_positivo? (cdr x)) #f)))

; Comprobar si el prisma es válido
(define (prisma_valido? prisma)
  (if (and (not (prisma_fin? prisma)) (prisma_positivo? prisma)) #t #f))

; Obtener elemento n de una lista
(define (elemento_l lista indice)
  (list-ref lista (- indice 1)))

; Generar random dado un limite
(define (generarRandom limite) (random limite))

; Minimo y maximo de dos numeros
(define (min n m) (if (>= m n) n m))
(define (max n m) (if (>= m n) m n))

;;********** CORTE VERTICAL **********
; Generar prisma con la primera coordenada disminuida
(define (cortar_ancho x y) (prisma (- (elemento_l x 1) y) (elemento_l x 2)(elemento_l x 3)))

; Comprobar si el corte está permitido
(define (corteAnchura prisma n)
  (if (or (>= n (elemento_l prisma 1)) (= n 0))
      (begin
        (display "Corte en anchura no posible\n")
        (jugarAncho prisma))
      (cortar_ancho prisma n)))

; Elegir cuanto se quiere cortar
(define (jugarAncho prisma)
  (if (> (elemento_l prisma 1) 1)
      (begin
        (display "\nElija un numero de corte \n")
        (corteAnchura prisma (read)))
      (begin
        (display "No se puede hacer el corte")
        (pedirJugada prisma))))

; Lista con todos los cortes en anchura posibles
(define (listaCortesAncho prisma) (listaCortesAncho_aux prisma 1))

; Funcion auxiliar para generar la lista de cortes posibles
(define (listaCortesAncho_aux prisma n)
  (if (< n (elemento_l prisma 1))
      (cons (cortar_ancho prisma n) (listaCortesAncho_aux prisma (+ n 1)))
      '()))

;;********** CORTE ALTURA **********
; Generar prisma con la segunda coordenada disminuida
(define (cortar_alto x y) (prisma (elemento_l x 1) (- (elemento_l x 2) y) (elemento_l x 3)))

; Comprobar si el corte está permitido
(define (corteAltura prisma n)
  (if (or (>= n (elemento_l prisma 2)) (= n 0))
      (begin
        (display "Corte en altura no posible\n")
        (jugarAlto prisma))
      (cortar_alto prisma n)))

; Elegir cuanto se quiere cortar
(define (jugarAlto prisma)
  (if (> (elemento_l prisma 2) 1)
      (begin
        (display "\nElija un numero de corte \n")
        (corteAltura prisma (read)))
      (begin
        (display "No se puede hacer el corte")
        (pedirJugada prisma))))

; Lista con todos los cortes en altura posibles
(define (listaCortesAlto prisma) (listaCortesAlto_aux prisma 1))

; Funcion auxiliar para generar la lista de cortes posibles
(define (listaCortesAlto_aux prisma n)
  (if (< n (elemento_l prisma 2))
      (cons (cortar_alto prisma n) (listaCortesAlto_aux prisma (+ n 1)))
      '()))

;;********** CORTE PROFUNDIDAD **********
; Generar prisma con la tercera coordenada disminuida
(define (cortar_largo x y) (prisma (elemento_l x 1) (elemento_l x 2) (- (elemento_l x 3) y)))

; Comprobar si el corte está permitido
(define (corteLongitud prisma n)
  (if (or (>= n (elemento_l prisma 3)) (= n 0))
      (begin
        (display "Corte en profundidad no posible\n")
        (jugarLargo prisma))
      (cortar_largo prisma n)))

; Elegir cuanto se quiere cortar
(define (jugarLargo prisma)
  (if (> (elemento_l prisma 3) 1)
      (begin
        (display "\nElija un numero de corte \n")
        (corteLongitud prisma (read)))
      (begin
        (display "No se puede hacer el corte")
        (pedirJugada prisma))))

; Lista con todos los cortes en profundidad posibles
(define (listaCortesLargo prisma) (listaCortesLargo_aux prisma 1))

; Funcion auxiliar para generar la lista de cortes posibles
(define (listaCortesLargo_aux prisma n)
  (if (< n (elemento_l prisma 3))
      (cons (cortar_largo prisma n) (listaCortesLargo_aux prisma (+ n 1)))
      '()))

;;********** CORTES POSIBLES **********
; Generar la lista con los cortes posibles en las 3 dimensiones
(define (listaJugadas prisma)
  (append (append (listaCortesAncho prisma) (listaCortesAlto prisma)) (listaCortesLargo prisma)))

;;********** FUNCIONES PARA MINIMAX **********
;Calcula la jugada ganadora del nodo o prisma raiz
(define (juegaMaquina prisma)
  (begin
    (display "\n*** Juega la maquina ***\n")
    (let ([jugadasPosibles (listaJugadas prisma)])
      (let ([resultadoJuegaMaquina (juegaMaquina_aux jugadasPosibles)])
        (if (number? resultadoJuegaMaquina)
            (elemento_l jugadasPosibles (+ (generarRandom (length jugadasPosibles)) 1)) ;Si no hay ganadora -> aleatoria entre posibles
            resultadoJuegaMaquina)))))

;Función auxiliar que calcula la jugada ganadora de entre uns lista de jugadas del nivel sucesorio al raíz
(define (juegaMaquina_aux listaJugadas)
  (if (= (length listaJugadas) 0) -1
      (if (= (nodoMinMax (car listaJugadas) 1) 1)
          (car listaJugadas)
          (juegaMaquina_aux (cdr listaJugadas)))))

;Devuelve un 1 o un 0 en función de si pasa un camino ganador de un nodo dado y su nivel (max-min) dentro del árbol
;;Nivel 0-> Max
;;Nivel 1-> Min
(define (nodoMinMax nodo nivel)
  (if (prisma_fin? nodo)
      (if (= nivel 0) 0 1);;Si el prisma fin se encuentra en un nodo max, devuelve un 0 (pierde)
      (let ([listaJugadas (listaJugadas nodo)])
        (if (= nivel 0)
            (calcNodoMax listaJugadas)
            (calcNodoMin listaJugadas)))))

;Devuelve el máximo de los valores de una lista de jugadas posibles de un nodo max
(define (calcNodoMax listaJugadas)
  (if (= (length listaJugadas) 0)
      0
      (max (max 0 (nodoMinMax (car listaJugadas) 1)) (calcNodoMax (cdr listaJugadas)))))

;Devuelve el mínimo de los valores de una lista de jugadas posibles de un nodo min
(define (calcNodoMin listaJugadas)
  (if (= (length listaJugadas) 0)
      1
      (min (min 1 (nodoMinMax (car listaJugadas) 0)) (calcNodoMin (cdr listaJugadas)))))

;;********** FUNCIONES PARA JUGAR **********
; Generamos aleatorio para saber quien comienza con el juego
(define (turno) (generarRandom 2))

; Recibir la coordenada en la que realizar el corte
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

; Recibir las dimensiones del prisma inicial
(define (pedirPrisma)
  (begin
    (display "\nIntroduce las dimensiones del prisma\n")
    (let ([prism (prisma (read)(read)(read))])
      (if (prisma_valido? prism)
          prism
          (begin
            (display "* Dimensiones del prisma inválidas *\n")
            (pedirPrisma))))))

; Comenzar la partida con el prisma dado y el jugador inicial
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

; Control de la partida
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

; Dar bienvenida e iniciar juego
(define (juego)
  (begin
    (display "\n*** BIENVENIDO AL JUEGO DE LA CANTERA ***")
    (display "\n  *** Gabriel - Sergio - Alvaro ***\n")
    (jugar)))

;Ejecucion del juego
(juego)