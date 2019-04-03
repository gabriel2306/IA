;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ciudades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;BUSQUEDA EN GRAFO DE RUTAS
;Lopez Cuenca, Gabriel
;Sanz Sacristan, Sergio
;Zamorano Ortega, Alvaro

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

;Funcion para mostrar el camino final junto con su coste
(define (to-string camino)
  (cond
    [(= (length camino) 1) (string-append "-> " (string-append "Coste: " (number->string (car camino))))]
    [else (string-append (string-append (number->string (car camino)) " ") (to-string (cdr camino)))]))

;Funcion para añadir al final del camino su coste
(define (camino-final camino matriz_ciudades)
  (cond
    [(empty? camino) '()]
    [else (poner-final (coste-camino camino matriz_ciudades) camino)]))

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

;Funcion para obtener el coste del camino
(define (coste-camino camino matriz_ciudades)
  (cond
    [(= (length camino) 1) 0]
    [else (+ (elemento_l (elemento_l matriz_ciudades (elemento_l camino 1)) (elemento_l camino 2))(coste-camino (cdr camino) matriz_ciudades))]))

;Funcion para obtener el menor coste de la lista teniendo en cuenta los caminos
(define (menor lista matriz_ciudades)
  (cond
    [(empty? lista) empty]
    [(empty? (cdr lista)) (car lista)]
    [(< (coste-camino (car lista) matriz_ciudades) (coste-camino (car (cdr lista)) matriz_ciudades)) (menor (cons (car lista) (cddr lista)) matriz_ciudades)]
    [else (menor (cdr lista) matriz_ciudades)]))

;Funcion para ordenar la lista
(define (ordenar lista matriz_ciudades)
  (cond
    [(empty? lista) empty]
    [else (cons (menor lista matriz_ciudades) (ordenar (eliminar (menor lista matriz_ciudades) lista) matriz_ciudades))]))
;----------------------------------------------------------

;Funcion para iniciar la ejecucion
(define (busqueda lista_ciudades tipo inicial meta)
  (if (> meta (length lista_ciudades)) (display "  *Meta no valida*\n\n")
      (cond
        [(= tipo 1) (display (string-append "  ** Camino: "
                    (string-append (to-string (camino-final (busqueda_a (list(list inicial)) empty meta lista_ciudades) lista_ciudades)) " **\n\n")))]
        [(= tipo 2) (display (string-append "  ** Camino: "
                    (string-append (to-string (camino-final (busqueda_p (list(list inicial)) empty meta lista_ciudades) lista_ciudades)) " **\n\n")))]
        [(= tipo 3) (display (string-append "  ** Camino: "
                    (string-append (to-string (camino-final (busqueda_o (list(list inicial)) empty meta lista_ciudades) lista_ciudades)) " **\n\n")))]
        [else (display "Metodo de busqueda no valido\n")])))

;Funcion para mostrar instrucciones de ejecucion
(define (dar_bienvenida)
  (display "\nBIENVENIDO A LA BUSQUEDA DE RUTAS\n ->Introduzca:\n  (busqueda *Matriz de conexiones* *Tipo de busqueda(Anchura -> 1, Profundidad -> 2, Optimal -> 3)* *Estado inicial* *Estado meta*)\n\n"))

;Ejecucion de dar_bienvenida al hacer Run
(dar_bienvenida)

;Ejemplo de ejecucion
;(busqueda (list '(0 1 1 0 0) '(1 0 0 1 1) '(1 0 0 1 0) '(0 1 1 0 1) '(0 1 0 1 0)) 1 1 5)

;Lista ciudades enunciado
;(busqueda '((0 171 356 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (171 0 455 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (356 455 0 0 280 0 193 0 0 0 0 0 0 0 0 0 0)(0 0 0 0 304 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 280 304 0 324 395 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 324 0 325 0 296 0 0 0 0 0 0 0 0)(0 0 193 0 395 325 0 0 0 0 251 0 0 403 335 0 0) (0 0 0 0 0 0 0 0 100 0 0 0 0 0 0 0 0) (0 0 0 0 0 296 0 100 0 349 0 0 0 0 0 0 0)(0 0 0 0 0 0 0 0 349 0 191 241 0 0 0 0 0) (0 0 0 0 0 0 251 0 0 191 0 150 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 241 150 0 278 0 0 0 0)(0 0 0 0 0 0 0 0 0 0 0 278 0 0 99 256 0) (0 0 0 0 0 0 403 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 335 0 0 0 0 0 99 0 0 242 0)(0 0 0 0 0 0 0 0 0 0 0 0 256 0 242 0 125) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 125 0)) 1 1 17)