;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ciudades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;Matriz de conexion entre ciudades
(define c1 '(0 1 1 0 0))
(define c2 '(1 0 0 1 1))
(define c3 '(1 0 0 1 0))
(define c4 '(0 1 1 0 1))
(define c5 '(0 1 0 1 0))

;Hace una lista con todas las ciudades
;(define matriz_ciudades (list c1 c2 c3 c4 c5))

;(define inicial 1)

;(define meta 5)

;Introduces ciudad como lista (cont te dice a que ciudades hay conexiones) como una lista
(define (sucesores_aux ciudad cont)
  (cond
    [(null? ciudad) '()]
    [(>= (car ciudad) 1) (cons cont (sucesores_aux (cdr ciudad) (+ cont 1)))]
    [else (sucesores_aux (cdr ciudad) (+ cont 1))]))

(define (to_string contador)
  (let ([str (format "~v" contador)])
    (string-append "c" str)
  ))

;te devuelve un elemento de posicion indice de una lista
(define (elemento_l lista indice)
  (list-ref lista (- indice 1)))

;devuelve el ultimo elemento de la lista
(define (ultimo-lista lista)
  (elemento_l lista (length lista)))

;te devuelve los sucesores dado un camino, lo completa. En una lista de listas
(define (sucesores lista matriz_ciudades)
  (crear_camino lista (sucesores_aux (elemento_l matriz_ciudades (ultimo-lista lista)) 1)))

;Te hace la lista con los distintos caminos para llegar a los sucesores
(define (crear_camino lista_original lista_sucesores)
  (cond
    [(null? lista_sucesores) '()]
    [else (cons (poner-final (car lista_sucesores) lista_original) (crear_camino lista_original (cdr lista_sucesores)))]))

;Introducir por el final
(define (poner-final x l)
  (reverse (cons x (reverse l))))

;Busqueda en PROFUNDIDAD
(define (busqueda_p abiertos cerrados meta matriz_ciudades)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        [(equal? (ultimo-lista actual) meta) actual]
        [(member (ultimo-lista actual) cerrados) (busqueda_p (cdr abiertos) cerrados meta matriz_ciudades)]
        [else (busqueda_p
                   (append (sucesores actual matriz_ciudades) (cdr abiertos))
                   (cons (ultimo-lista actual) cerrados) meta matriz_ciudades)]))))

;Busqueda en ANCHURA
(define (busqueda_a abiertos cerrados meta matriz_ciudades)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        [(equal? (ultimo-lista actual) meta) actual]
        [(member (ultimo-lista actual) cerrados) (busqueda_a (cdr abiertos) cerrados meta matriz_ciudades)]
        [else (busqueda_a
                   (append (cdr abiertos) (sucesores actual matriz_ciudades))
                   (cons (ultimo-lista actual) cerrados) meta matriz_ciudades)]))))

;Busqueda OPTIMAL
(define (busqueda_o abiertos cerrados meta matriz_ciudades)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        [(equal? (ultimo-lista actual) meta) actual]
        [(member (ultimo-lista actual) cerrados) (busqueda_o (ordenar (cdr abiertos) matriz_ciudades) cerrados meta matriz_ciudades)]
        [else (busqueda_o
                   (ordenar (append (cdr abiertos) (sucesores actual matriz_ciudades)) matriz_ciudades)
                   (cons (ultimo-lista actual) cerrados) meta matriz_ciudades)]))))

;Busqueda A*
(define (busqueda_ap abiertos cerrados meta matriz_ciudades)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        [(equal? (ultimo-lista actual) meta) actual]
        [(member (ultimo-lista actual) cerrados) (busqueda_ap (ordenar_h (cdr abiertos) matriz_ciudades meta) cerrados meta matriz_ciudades)]
        [else (busqueda_ap
                   (ordenar_h (append (cdr abiertos) (sucesores actual matriz_ciudades)) matriz_ciudades meta)
                   (cons (ultimo-lista actual) cerrados) meta matriz_ciudades)]))))

;Te da el coste del camino introducido.
(define (coste-camino camino matriz_ciudades)
  (cond
    [(= (length camino) 1) 0]
    [else (+ (elemento_l (elemento_l matriz_ciudades (elemento_l camino 1)) (elemento_l camino 2))(coste-camino (cdr camino) matriz_ciudades))]))

(define (menor lista matriz_ciudades)
  (cond
    [(empty? lista) empty]
    [(empty? (cdr lista)) (car lista)]
    [(< (coste-camino (car lista) matriz_ciudades) (coste-camino (car (cdr lista)) matriz_ciudades)) (menor (cons (car lista) (cddr lista)) matriz_ciudades)]
    [else (menor (cdr lista) matriz_ciudades)]))

(define (eliminar n lista)
  (cond
    [(empty? lista) empty]
    [(equal? n (car lista)) (cdr lista)]
    [else (cons (car lista) (eliminar n (cdr lista)))]))

(define (ordenar lista matriz_ciudades)
  (cond
    [(empty? lista) empty]
    [else (cons (menor lista matriz_ciudades) (ordenar (eliminar (menor lista matriz_ciudades) lista) matriz_ciudades))]))

(define (valor-heuristico camino meta)
  (cond
    [(equal? (ultimo-lista camino) meta) 0]
    [else (- meta (ultimo-lista camino))]))

(define (menor_h lista matriz_ciudades meta)
  (cond
    [(empty? lista) empty]
    [(empty? (cdr lista)) (car lista)]
    [(< (+ (coste-camino (car lista) matriz_ciudades) (valor-heuristico (car lista) meta))
        (+ (coste-camino (car (cdr lista)) matriz_ciudades) (valor-heuristico (car (cdr lista)) meta))) (menor_h (cons (car lista) (cddr lista)) matriz_ciudades meta)]
    [else (menor (cdr lista) matriz_ciudades)]))

(define (ordenar_h lista matriz_ciudades meta)
  (cond
    [(empty? lista) empty]
    [else (cons (menor_h lista matriz_ciudades meta) (ordenar_h (eliminar (menor_h lista matriz_ciudades meta) lista) matriz_ciudades meta))]))


(define (busqueda lista_ciudades tipo inicial meta)
  (cond
    [(= tipo 1) (busqueda_a (list(list inicial)) empty meta lista_ciudades)]
    [(= tipo 2) (busqueda_p (list(list inicial)) empty meta lista_ciudades)]
    [(= tipo 3) (busqueda_o (list(list inicial)) empty meta lista_ciudades)]
    [(= tipo 4) (busqueda_ap (list(list inicial)) empty meta lista_ciudades)]
    [else (display "Metodo de busqueda no valido\n")]))


(define (dar_bienvenida)
  (display "BIENVENIDO A LA BUSQUEDA EN GRAFOS\nIntroduzca lista de listas/Tipo de busqueda(Anchura -> 1, Profundidad -> 2, Optimal -> 3, A* -> 4)/Estado inicial/Estado meta\n"))

(dar_bienvenida)

;(busqueda (list '(0 1 1 0 0) '(1 0 0 1 1) '(1 0 0 1 0) '(0 1 1 0 1) '(0 1 0 1 0)) 1 1 5)