#lang racket
#|
 Enunciado:
  - Se tienen dos jarras, una de 4 litros y otra de 3.
  - Ninguna de ellas tiene marcas de medición.
  - Se tiene una bomba que permite llenar jarras de agua.
  - Averiguar cómo se puede lograr tener exactamente 2 litros de agua
    en la jarra de 4 litros de capacidad.

 Representación de estados: x y con x en {0,1,2,3,4} e y en {0,1,2,3}
 Numero de estados: 20
 Estado inicial: (0 0)
 Estado final: (2 y)
 Operadores:
  - Llenar la jarra de 4 litros con la bomba.
  - Llenar la jarra de 3 litros con la bomba.
  - Vaciar la jarra de 4 litros en el suelo.
  - Vaciar la jarra de 3 litros en el suelo.
  - Echar el contenido de la jarra de 4 litros en la jarra de 3 litros.
  - Echar el contenido de la jarra de 3 litros en la jarra de 4 litros.
|#

#| Representación de estados |#
(define (crea-estado x y)
  (list x y)
)
(define (contenido-jarra-4 estado)
  (car estado)
)
(define (contenido-jarra-3 estado)
  (cadr estado)
)

#| Estado inicial |#
(define *ESTADO-INICIAL*
  (crea-estado 0 0)
)

#| Estados finales |#
(define (es-estado-final estado)
  (= 2 (contenido-jarra-4 estado))
)

#| Operadores |#
(define (llenar-jarra-4 estado)
  (if (< (contenido-jarra-4 estado) 4)
    (crea-estado 4 (contenido-jarra-3 estado))
    estado
  )
)
(define (llenar-jarra-3 estado)
  (if (< (contenido-jarra-3 estado) 3)
    (crea-estado (contenido-jarra-4 estado) 3)
    estado
  )
)
(define (vaciar-jarra-4 estado)
  (if (> (contenido-jarra-4 estado) 0)
    (crea-estado 0 (contenido-jarra-3 estado))
    estado
  )
)
(define (vaciar-jarra-3 estado)
  (if (> (contenido-jarra-3 estado) 0)
    (crea-estado (contenido-jarra-4 estado) 0)
    estado
  )
)
(define (echar-jarra-4-en-jarra-3 estado)
  (let ([x (contenido-jarra-4 estado)]
        [y (contenido-jarra-3 estado)])
    (cond
      [(or (= x 0) (= y 3)) estado]
      [else (echar-jarra-4-en-jarra-3 (crea-estado (- x 1) (+ y 1)))]
    )
  )
)
(define (echar-jarra-3-en-jarra-4 estado)
  (let ([x (contenido-jarra-4 estado)]
        [y (contenido-jarra-3 estado)])
    (cond
      [(or (= x 4) (= y 0)) estado]
      [else (echar-jarra-3-en-jarra-4 (crea-estado (+ x 1) (- y 1)))]
    )
  )
)
(define *OPERADORES*
  (list
    llenar-jarra-4
    llenar-jarra-3
    vaciar-jarra-4
    vaciar-jarra-3
    echar-jarra-4-en-jarra-3
    echar-jarra-3-en-jarra-4)
)

#| Sucesores |#
(define (expandir operadores estado)
  (cond
    [(empty? operadores) empty]
    [(equal? ((car operadores) estado) estado) (expandir (cdr operadores) estado)]
    [else (cons ((car operadores) estado) (expandir (cdr operadores) estado))]
  )
)
(display "Sucesores '(4 1):\n")
(expandir *OPERADORES* '(4 1))

#| Busqueda en profundidad |#
(define (busqueda-en-profundidad abiertos cerrados)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        [(es-estado-final actual) actual]
        [(member actual cerrados) (busqueda-en-profundidad (cdr abiertos) cerrados)]
        [else (busqueda-en-profundidad
                   (append (expandir *OPERADORES* actual) (cdr abiertos))
                   (cons actual cerrados)
               )
         ]
      )
    )
  )
)
(display "Busqueda en profundidad:\n")
(busqueda-en-profundidad (list *ESTADO-INICIAL*) empty)

#| Busqueda en anchura |#
(define (busqueda-en-anchura abiertos cerrados)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        [(es-estado-final actual) actual]
        [(member actual cerrados) (busqueda-en-anchura (cdr abiertos) cerrados)]
        [else (busqueda-en-anchura
                   (append (cdr abiertos) (expandir *OPERADORES* actual))
                   (cons actual cerrados)
               )
         ]
      )
    )
  )
)
(display "Busqueda en anchura:\n")
(busqueda-en-anchura (list *ESTADO-INICIAL*) empty)

#|
(display "Abiertos: ")
(display abiertos)
(newline)
(display "Cerrados: ")
(display cerrados)
(newline)
(display "Actual: ")
(display actual)
(newline)
|#