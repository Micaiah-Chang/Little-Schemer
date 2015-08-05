#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (lat? l)
  (cond [(null? l) #t]
        [(atom? (car l)) (lat? (cdr l))]
        [else #f]))

(define (member? a lat)
  (cond [(null? lat) #f]
        [else (or (eq? (car lat) a)
               (member? a (cdr lat)))]))

(define (rember a lat)
  (cond [(null? lat) '()]
        [(eq? a (car lat)) (cdr lat)]
        [else (cons (car lat)(rember a (cdr lat)))]))

(define (firsts l)
  (cond [(null? l) '()]
        [else (cons (car (car l)) (firsts (cdr l)))]))

(define (insertR new old lat)
  (cond [(null? lat) '()]
        [(eq? old (car lat)) (cons old (cons new (cdr lat)))]
        [else (cons (car lat) (insertR new old (cdr lat)))]))

(define (insertL new old lat)
  (cond [(null? lat) '()]
        [(eq? old (car lat)) (cons new (cons old (cdr lat)))]
        [else (cons (car lat) (insertR new old (cdr lat)))]))