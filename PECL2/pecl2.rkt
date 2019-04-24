#lang racket

(define (prisma a b c) (list a b c))

(define (prisma_fin x)
  (if (empty? x) #t (if (= (car x) 1) (prisma_fin (cdr x)) #f)))

(define (elemento_l lista indice)
  (list-ref lista (- indice 1)))

(define (cortar_ancho x y) (cons (- (elemento_l x 1) y) (cdr x)))

(define (cortar_alto x y) (append (list (car x) (- (elemento_l x 2) y)) (cdr (cdr x))))

(define (cortar_prof x y) (append (list (car x) (car (cdr x))) (list (- (elemento_l x 3) y))))