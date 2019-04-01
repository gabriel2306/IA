;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ciudades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;Introducir ciudad como lista de costes a las adyacentes, devuelve una lista con el numero de las adyacentes
(define (sucesores_aux ciudad cont)
  (cond
    [(null? ciudad) '()]
    [(>= (car ciudad) 1) (cons cont (sucesores_aux (cdr ciudad) (+ cont 1)))]
    [else (sucesores_aux (cdr ciudad) (+ cont 1))]))

;Devuelve el elemento de le lista en la posicion indice
(define (elemento_l lista indice)
  (list-ref lista (- indice 1)))

;Devuelve el ultimo elemento de la lista
(define (ultimo-lista lista)
  (elemento_l lista (length lista)))

;Dado un camino, te devuelve el/los camino para llegar a sus sucesores
(define (sucesores lista matriz_ciudades)
  (crear_camino lista (sucesores_aux (elemento_l matriz_ciudades (ultimo-lista lista)) 1)))

;Te hace la lista con los distintos caminos a los sucesores del elemento ultimo del camino original
(define (crear_camino lista_original lista_sucesores)
  (cond
    [(null? lista_sucesores) '()]
    [else (cons (poner-final (car lista_sucesores) lista_original) (crear_camino lista_original (cdr lista_sucesores)))]))

;Introducir elemento al final de la lista
(define (poner-final x l)
  (reverse (cons x (reverse l))))

;Eliminar el elemento n de la lista
(define (eliminar n lista)
  (cond
    [(empty? lista) empty]
    [(equal? n (car lista)) (cdr lista)]
    [else (cons (car lista) (eliminar n (cdr lista)))]))

;Busqueda en PROFUNDIDAD ---------------------------------
(define (busqueda_p abiertos cerrados meta matriz_ciudades)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        [(equal? (ultimo-lista actual) meta) actual]
        [(member (ultimo-lista actual) cerrados) (busqueda_p (cdr abiertos) cerrados meta matriz_ciudades)]
        [else (busqueda_p
                   (append (sucesores actual matriz_ciudades) (cdr abiertos))
                   (cons (ultimo-lista actual) cerrados) meta matriz_ciudades)]))))
;---------------------------------------------------------

;Busqueda en ANCHURA -------------------------------------
(define (busqueda_a abiertos cerrados meta matriz_ciudades)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        [(equal? (ultimo-lista actual) meta) actual]
        [(member (ultimo-lista actual) cerrados) (busqueda_a (cdr abiertos) cerrados meta matriz_ciudades)]
        [else (busqueda_a
                   (append (cdr abiertos) (sucesores actual matriz_ciudades))
                   (cons (ultimo-lista actual) cerrados) meta matriz_ciudades)]))))
;---------------------------------------------------------

;Busqueda OPTIMAL ----------------------------------------
(define (busqueda_o abiertos cerrados meta matriz_ciudades)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        [(equal? (ultimo-lista actual) meta) actual]
        [(member (ultimo-lista actual) cerrados) (busqueda_o (ordenar (cdr abiertos) matriz_ciudades) cerrados meta matriz_ciudades)]
        [else (busqueda_o
                   (ordenar (append (cdr abiertos) (sucesores actual matriz_ciudades)) matriz_ciudades)
                   (cons (ultimo-lista actual) cerrados) meta matriz_ciudades)]))))

;Devuelve el coste del camino introducido
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

(define (ordenar lista matriz_ciudades)
  (cond
    [(empty? lista) empty]
    [else (cons (menor lista matriz_ciudades) (ordenar (eliminar (menor lista matriz_ciudades) lista) matriz_ciudades))]))
;----------------------------------------------------------

;Busqueda A* ----------------------------------------------
(define (busqueda_ap abiertos cerrados meta matriz_ciudades)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        [(equal? (ultimo-lista actual) meta) actual]
        [(member (ultimo-lista actual) cerrados) (busqueda_ap (ordenar_h (cdr abiertos) matriz_ciudades meta) cerrados meta matriz_ciudades)]
        [else (busqueda_ap
                   (ordenar_h (append (cdr abiertos) (sucesores actual matriz_ciudades)) matriz_ciudades meta)
                   (cons (ultimo-lista actual) cerrados) meta matriz_ciudades)]))))

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
;-----------------------------------------------------------

;Funcion que inicia la ejecucion
(define (busqueda lista_ciudades tipo inicial meta)
  (cond
    [(= tipo 1) (busqueda_a (list(list inicial)) empty meta lista_ciudades)]
    [(= tipo 2) (busqueda_p (list(list inicial)) empty meta lista_ciudades)]
    [(= tipo 3) (busqueda_o (list(list inicial)) empty meta lista_ciudades)]
    [(= tipo 4) (busqueda_ap (list(list inicial)) empty meta lista_ciudades)]
    [else (display "Metodo de busqueda no valido\n")]))


;Funcion para mostrar instrucciones de ejecucion
(define (dar_bienvenida)
  (display "\nBIENVENIDO A LA BUSQUEDA DE RUTAS\n ->Introduzca:\n  (busqueda *matriz conexiones* *Tipo de busqueda(Anchura -> 1, Profundidad -> 2, Optimal -> 3, A* -> 4)* *Estado inicial* *Estado meta*)\n\n"))

;Ejecucion de dar_bienvenida al hacer Run
(dar_bienvenida)

;Ejemplo de ejecucion
;(busqueda (list '(0 1 1 0 0) '(1 0 0 1 1) '(1 0 0 1 0) '(0 1 1 0 1) '(0 1 0 1 0)) 1 1 5)

;Lista ciudades enunciado
;(busqueda '((0 171 356 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (171 0 455 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (356 455 0 0 280 0 193 0 0 0 0 0 0 0 0 0 0)(0 0 0 0 304 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 280 304 0 324 395 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 324 0 325 0 296 0 0 0 0 0 0 0 0)(0 0 193 0 395 325 0 0 0 0 251 0 0 403 335 0 0) (0 0 0 0 0 0 0 0 100 0 0 0 0 0 0 0 0) (0 0 0 0 0 296 0 100 0 349 0 0 0 0 0 0 0)(0 0 0 0 0 0 0 0 349 0 191 241 0 0 0 0 0) (0 0 0 0 0 0 251 0 0 191 0 150 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 241 150 0 278 0 0 0 0)(0 0 0 0 0 0 0 0 0 0 0 278 0 0 99 256 0) (0 0 0 0 0 0 403 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 335 0 0 0 0 0 99 0 0 242 0)(0 0 0 0 0 0 0 0 0 0 0 0 256 0 242 0 125) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 125 0)) 1 1 17)