;Ian Polito
;ipolito
;CSCI-C311
;Assignment 1

#lang racket
;(require "a1studenttests.rkt")
;(test-file #:file-name "a1.rkt")

(define countdown
  (lambda (n)
    (cond
      [(zero? n) (list 0)]
      [else (cons n (countdown (sub1 n)))])))

(define insertR
  (lambda (s1 s2 l)
    (cond
      [(null? l) '()]
      [(eqv? (car l) s1) (cons (car l) (cons s2 (insertR s1 s2 (cdr l))))]
      [else (cons (car l) (insertR s1 s2 (cdr l)))])))

(define remv-1st
  (lambda (s l)
    (cond
      [(null? l) '()]
      [(eqv? (car l) s) (cdr l)]
      [else (cons (car l) (remv-1st s (cdr l)))])))

(define list-index-ofv?
  (lambda (s l)
    (cond
      [(eqv? (car l) s) 0]
      [else (add1 (list-index-ofv? s (cdr l)))])))

(define filter
  (lambda (func l)
    (cond
      [(null? l) '()]
      [(func (car l)) (cons (car l) (filter func (cdr l)))]
      [else (filter func (cdr l))])))

(define zip
  (lambda (a b)
    (cond
      [(null? a) '()]
      [(null? b) '()]
      [else (cons (cons (car a) (car b)) (zip (cdr a) (cdr b)))])))

(define map
  (lambda (func l)
    (cond
      [(null? l) '()]
      [else (cons (func (car l)) (map func (cdr l)))])))

(define append
  (lambda (a b)
    (cond
      [(null? a) (cond
                   [(null? b) '()]
                   [else (cons (car b) (append a (cdr b)))])]
      [else (cons (car a) (append (cdr a) b))])))

(define reverse
  (lambda (l)
    (cond
      [(null? l) '()]
      [else (append (reverse (cdr l)) `(,(car l)))])))

(define fact
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n (fact (sub1 n)))])))

(define memv
  (lambda (a l)
    (cond
      [(null? l) #f]
      [(eqv? a (car l)) l]
      [else (memv a (cdr l))])))

(define fib
  (lambda (n)
    (cond
      [(zero? n) 0]
      [(zero? (sub1 n)) 1]
      [else (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))])))

(equal? '((w x) y (z)) '((w . (x . ())) . (y . ((z . ()) . ()))))

(define binary->natural
  (lambda (l)
    (cond
      [(null? l) 0]
      [else (+ (car l) (* 2 (binary->natural (cdr l))))])))

(define minus
  (lambda (a b)
    (cond
      [(zero? b) a]
      [else (sub1  (minus a (sub1 b)))])))

(define div
  (lambda (a b)
    (cond
      [(zero? b) 0]
      [else (add1 (div (minus a b) (sub1 b)))])))

(define append-map
  (lambda (func l)
    (cond
      [(null? l) '()]
      [else (append (func (car l)) (append-map func (cdr l)))])))

(define contains?
  (lambda (n l)
    (cond
      [(null? l) #f]
      [(eqv? n (car l)) #t]
      [else (contains? n (cdr l))])))

(define set-difference
  (lambda (a b)
    (cond
      [(null? b) a]
      [(null? a) '()]
      [(contains? (car a) b) (set-difference (cdr a) b)]
      [else (cons (car a) (set-difference (cdr a) b))])))

;TEST CASES

(module+ test (require rackunit)
         (check-equal? (countdown 5) '(5 4 3 2 1 0))
         (check-equal? (insertR 'x 'y '(x z z x y x)) '(x y z z x y y x y))
         (check-equal? (remv-1st 'x '(x y z x)) '(y z x))
         (check-equal? (remv-1st 'y '(x y z y x)) '(x z y x))
         (check-equal? (list-index-ofv? 'x '(x y z x x)) 0)
         (check-equal? (list-index-ofv? 'x '(y z x x)) 2)
         (check-equal? (filter even? '(1 2 3 4 5 6)) '(2 4 6))
         (check-equal? (zip '(1 2 3) '(a b c)) '((1 . a) (2 . b) (3 . c))) 
         (check-equal? (zip '(1 2 3 4 5) '(a b c)) '((1 . a) (2 . b) (3 . c)))
         (check-equal? (zip '(1 2 3) '(a b c d e)) '((1 . a) (2 . b) (3 . c)))
         (check-equal? (fact 0) 1)
         (check-equal? (fact 5) 120)
         (check-equal? (map add1 '(1 2 3 4)) '(2 3 4 5))
         (check-equal? (append '(a b c) '(1 2 3)) '(a b c 1 2 3))
         (check-equal? (reverse '(a 3 x)) '(x 3 a))
         (check-equal? (memv 'a '(a b c)) '(a b c))
         (check-equal? (memv 'b '(a ? c)) #f)
         (check-equal? (memv 'b '(a b c b)) '(b c b))
         (check-equal? (fib 0) 0)
         (check-equal? (fib 1) 1)
         (check-equal? (fib 7) 13)
         (check-equal? (binary->natural '()) 0)
         (check-equal? (binary->natural '(0 0 1)) 4)
         (check-equal? (binary->natural '(0 0 1 1)) 12)
         (check-equal? (binary->natural '(1 1 1 1)) 15)
         (check-equal? (binary->natural '(1 0 1 0 1)) 21)
         (check-equal? (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191)
         (check-equal? (minus 5 3) 2)
         (check-equal? (minus 100 50) 50)
         (check-equal? (div 25 5) 5)
         (check-equal? (div 36 6) 6)
         (check-equal? (append-map countdown (countdown 5))
                       '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0))
         (check-equal? (set-difference '(1 2 3 4 5) '(2 4 6 8)) '(1 3 5))
         #;(check-equal?
          (letrec
              ((<-set
                (lambda (s1 s2)
                  (cond
                    ((null? s1) #f)
                    ((null? s2))
                    ((< (car s1) (car s2)) #f)
                    ((< (car s2) (car s1)))
                    (else (<-set (cdr s1) (cdr s2)))))))
            (sort (map (lambda (s) (sort s >)) (powerset '())) <-set))
          '(()))
         #;(check-equal? (letrec
                           ((<-set
                             (lambda (s1 s2)
                               (cond
                                 ((null? s1) #f)
                                 ((null? s2))
                                 ((< (car s1) (car s2)) #f)
                                 ((< (car s2) (car s1)))
                                 (else (<-set (cdr s1) (cdr s2)))))))
                         (sort (map (lambda (s) (sort s >)) (powerset '(2 0 1))) <-set))
                       '((2 1 0) (2 1) (2 0) (2) (1 0) (1) (0) ()))
         #;(check-equal?
          (letrec
          ((<-set
            (lambda (s1 s2)
              (cond
               ((null? s1) #f)
               ((null? s2))
               ((< (car s1) (car s2)) #f)
               ((< (car s2) (car s1)))
               (else (<-set (cdr s1) (cdr s2)))))))
          (sort (map (lambda (s) (sort s >)) (powerset '(1 3 4 5))) <-set))
        '((5 4 3 1)
          (5 4 3)
          (5 4 1)
          (5 4)
          (5 3 1)
          (5 3)
          (5 1)
          (5)
          (4 3 1)
          (4 3)
          (4 1)
          (4)
          (3 1)
          (3)
          (1)
          ()))
         #;(check-equal?
          (letrec
              ((>=-tuple
                (lambda (t1 t2)
                  (cond
                    ((null? t1))
                    ((> (car t1) (car t2)))
                    ((> (car t2) (car t1)) #f)
                    (else (>=-tuple (cdr t1) (cdr t2)))))))
            (sort (map (lambda (s) (sort s >)) (cartesian-product '((7 6 5) (3 2)))) >=-tuple))
          '((7 3) (7 2) (6 3) (6 2) (5 3) (5 2)))
         #;(check-equal? (insertR-fr 'x 'y '(x z z x y x))
                       '(x y z z x y y x y))
         #;(check-equal? (insertR-fr 'x 'y '(x x x))
                       '(x y x y x y))
         #;(check-equal? (filter-fr even? '(1 2 3 4 5 6)) '(2 4 6))
         #;(check-equal? (map-fr add1 '(1 2 3 4)) '(2 3 4 5))
         #;(check-equal? (append-fr '(a b c) '(1 2 3)) '(a b c 1 2 3))
         #;(check-equal? (reverse-fr '(a 3 x)) '(x 3 a))
         #;(check-equal? (append-map-fr countdown (countdown 5))
                       '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0))
         #;(check-equal? (set-difference-fr '(1 2 3 4 5) '(2 4 6 8))
                       '(1 3 5))
         #;(check-equal?
          (letrec
              ((<-set
                (lambda (s1 s2)
                  (cond
                    ((null? s1) #f)
                    ((null? s2))
                    ((< (car s1) (car s2)) #f)
                    ((< (car s2) (car s1)))
                    (else (<-set (cdr s1) (cdr s2)))))))
            (sort (map (lambda (s) (sort s >)) (powerset-fr '())) <-set))
          '(()))
         #;(check-equal?
          (letrec
              ((<-set
                (lambda (s1 s2)
                  (cond
                    ((null? s1) #f)
                    ((null? s2))
                    ((< (car s1) (car s2)) #f)
                    ((< (car s2) (car s1)))
                    (else (<-set (cdr s1) (cdr s2)))))))
            (sort (map (lambda (s) (sort s >)) (powerset-fr '(2 0 1))) <-set))
          '((2 1 0) (2 1) (2 0) (2) (1 0) (1) (0) ()))
         #;(check-equal?
          (letrec
              ((<-set
                (lambda (s1 s2)
                  (cond
                    ((null? s1) #f)
                    ((null? s2))
                    ((< (car s1) (car s2)) #f)
                    ((< (car s2) (car s1)))
                    (else (<-set (cdr s1) (cdr s2)))))))
            (sort (map (lambda (s) (sort s >)) (powerset-fr '(1 3 4 5))) <-set))
          '((5 4 3 1)
            (5 4 3)
            (5 4 1)
            (5 4)
            (5 3 1)
            (5 3)
            (5 1)
            (5)
            (4 3 1)
            (4 3)
            (4 1)
            (4)
            (3 1)
            (3)
            (1)
            ()))
         #;(check-equal?
          (letrec
              ((>=-tuple
                (lambda (t1 t2)
                  (cond
                    ((null? t1))
                    ((> (car t1) (car t2)))
                    ((> (car t2) (car t1)) #f)
                    (else (>=-tuple (cdr t1) (cdr t2)))))))
            (sort (map (lambda (s) (sort s >)) (cartesian-product-fr '((7 6 5) (3 2)))) >=-tuple))
          '((7 3) (7 2) (6 3) (6 2) (5 3) (5 2)))
         
         #;(check-equal? (binary->natural-fr '()) 0)
         #;(check-equal? (binary->natural-fr '(0 0 1)) 4)
         #;(check-equal? (binary->natural-fr '(0 0 1 1)) 12)
         #;(check-equal? (binary->natural-fr '(1 1 1 1)) 15)
         #;(check-equal? (binary->natural-fr '(1 0 1 0 1)) 21)
         #;(check-equal? (binary->natural-fr '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191)
         #;(check-equal? (collatz 12) 1)
         #;(check-equal? (collatz 120) 1)
         #;(check-equal? (collatz 9999) 1))