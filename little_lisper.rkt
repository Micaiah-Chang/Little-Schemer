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

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

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
        [else (cons (car lat) (insertL new old (cdr lat)))]))

(define (subst new old lat)
  (cond [(null? lat) '()]
        [(eq? old (car lat)) (cons new (cdr lat))]
        [else (cons (car lat) (subst new old (cdr lat)))]))

(define (subst2 new o1 o2 lat)
  (cond [(null? lat) '()]
        [(eq? o1 (car lat)) (cons new (cdr lat))]
        [(eq? o2 (car lat)) (cons new (cdr lat))]
        [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))]))

(define (multirember a lat)
  (cond [(null? lat) '()]
        [(eq? a (car lat)) (multirember a (cdr lat))]
        [else (cons (car lat) (multirember a (cdr lat)))]))

(define (multiinsertR new old lat)
  (cond [(null? lat) '()]
        [(eq? old (car lat))
         (cons old (cons new (multiinsertR new old (cdr lat))))]
        [else (cons (car lat) (multiinsertR new old (cdr lat)))]))


(define (multiinsertL new old lat)
  (cond [(null? lat) '()]
        [(eq? old (car lat))
         (cons new (cons old (multiinsertL new old (cdr lat))))]
        [else (cons (car lat) (multiinsertL new old (cdr lat)))]))

(define (multisubst new old lat)
  (cond [(null? lat) '()]
        [(eq? old (car lat))
         (cons new (multisubst new old (cdr lat)))]
        [else (cons (car lat) (multisubst new old (cdr lat)))]))

(define (.+ n m)
  (cond [(zero? m) n]
        [else (.+ (add1 n) (sub1 m))]))

(define (.- n m)
  (cond [(zero? m) n]
        [else (.- (sub1 n) (sub1 m))]))

(define (addtup tup)
  (cond [(null? tup) 0]
        [else (.+ (car tup) (addtup (cdr tup)))]))

(define (.* n m)
  (cond [(zero? m) 0]
        [else (.+ n (.* n (sub1 m)))]))


(define (tup+ tup1 tup2)
  (cond [(and (null? tup1) (null? tup2)) '()]
        [(null? tup1) tup2]
        [(null? tup2) tup1]
        [else (cons (.+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))]))

(define (.> n m)
  (cond [(zero? n) #f]
        [(zero? m) #t]
        [else (.> (sub1 n) (sub1 m))]))