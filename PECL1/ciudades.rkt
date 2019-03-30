;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ciudades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define c1 '(0 1 1 0 0))
(define c2 '(1 0 0 1 1))
(define c3 '(1 0 0 1 0))
(define c4 '(0 1 1 0 1))
(define c5 '(0 1 0 1 0))

(define matriz_ciudades (list c1 c2 c3 c4 c5))

(define inicial c1)

(define meta c5)

(define (sucesores_aux ciudad cont)
  (cond
    [(null? ciudad) '()]
    [(>= (car ciudad) 1) (cons cont (sucesores_aux (cdr ciudad) (+ cont 1)))]
    [else (sucesores_aux (cdr ciudad) (+ cont 1))]
  )
)

;(elemento_lista matriz_ciudades cont)

(define (to_string contador)
  (let ([str (format "~v" contador)])
    (string-append "c" str)
  )
)

(define (elemento_lista lista indice)
  (list-ref lista (- indice 1))
)

(define (sucesores ciudad)
  (sucesores_aux (elemento_lista matriz_ciudades ciudad) 1)
)

