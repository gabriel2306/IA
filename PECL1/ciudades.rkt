;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ciudades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define c1 '(0 1 1 0 0))
(define c2 '(1 0 0 1 1))
(define c3 '(1 0 0 1 0))
(define c4 '(0 1 1 0 1))
(define c5 '(0 1 0 1 0))

(define matriz_ciudades (list c1 c2 c3 c4 c5))

(define inicial 1)

(define meta 5)

(define (sucesores_aux ciudad cont)
  (cond
    [(null? ciudad) '()]
    [(>= (car ciudad) 1) (cons cont (sucesores_aux (cdr ciudad) (+ cont 1)))]
    [else (sucesores_aux (cdr ciudad) (+ cont 1))]
  )
)

(define (to_string contador)
  (let ([str (format "~v" contador)])
    (string-append "c" str)
  )
)

(define (elemento_l lista indice)
  (list-ref lista (- indice 1))
)

(define (ultimo-lista lista)
  (elemento_l lista (length lista)))

(define (sucesores lista)
  (crear_camino lista (sucesores_aux (elemento_l matriz_ciudades (ultimo-lista lista)) 1))
)

(define (crear_camino lista_original lista_sucesores)
  (cond
    [(null? lista_sucesores) '()]
    [else (cons (poner-final (car lista_sucesores) lista_original) (crear_camino lista_original (cdr lista_sucesores)))])
 )

(define (poner-final x l)
  (reverse (cons x (reverse l))))

(define (busqueda_p abiertos cerrados)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        [(equal? (ultimo-lista actual) meta) actual]
        [(member (ultimo-lista actual) cerrados) (busqueda-en-profundidad (cdr abiertos) cerrados)]
        [else (busqueda-en-profundidad
                   (append (sucesores actual) (cdr abiertos))
                   (cons (ultimo-lista actual) cerrados)
               )
         ]
      )
    )
  )
)

(define (busqueda_a abiertos cerrados)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        [(equal? (ultimo-lista actual) meta) actual]
        [(member (ultimo-lista actual) cerrados) (busqueda-en-anchura (cdr abiertos) cerrados)]
        [else (busqueda-en-anchura
                   (append (cdr abiertos) (sucesores actual))
                   (cons (ultimo-lista actual) cerrados)
               )
         ]
      )
    )
  )
)

