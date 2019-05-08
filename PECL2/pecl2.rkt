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