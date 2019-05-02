;Ian Polito
;ipolito
;CSCI-C311
;Assignment 6

#lang racket

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

;Problem 1 binary-to-decimal-cps
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n)
       (k 0)]
      [else
       (binary-to-decimal-cps (cdr n)
                              (lambda(x) (k (+ (* x 2) (car n)))))])))

;Tests
(binary-to-decimal-cps '() (empty-k))
(binary-to-decimal-cps '(1) (empty-k))
(binary-to-decimal-cps '(0 1) (empty-k))
(binary-to-decimal-cps '(1 1 0 1) (empty-k))

;Problem 2 times-cps
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls)
       (k 1)]
      [(zero? (car ls))
       (k 0)]
      [else
       (times-cps (cdr ls)
                  (lambda (v) (k (* v (car ls)))))])))

;Tests
(times-cps '(1 2 3 4 5) (empty-k))
(times-cps '(1 2 3 0 3) (empty-k))

;Problem 3 times-cps-shortcut
(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls)
       (k 1)]
      [(zero? (car ls))
       0]
      [else
       (times-cps-shortcut (cdr ls)
                           (lambda (v) (k (* v (car ls)))))])))

;Tests
(times-cps-shortcut '(1 2 3 4 5) (empty-k))
(times-cps-shortcut '(1 2 3 0 3) (empty-k))

;Problem 4 plus-cps
(define plus-cps
  (lambda (m k)
    (k (lambda (n k) (k (+ m n))))))

;Tests
((plus-cps 2 (empty-k)) 3 (empty-k))
((plus-cps ((plus-cps 2 (empty-k)) 3 (empty-k)) (empty-k)) 5 (empty-k))

;Problem 5 remv-first-9*-cps
(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
      (cond
        [(remv-first-9*-cps (car ls)
                            (lambda (x)(equal? x (car ls))))
         (remv-first-9*-cps (cdr ls)
                            (lambda (x) (k (cons (car ls) x))))]
        [else
         (remv-first-9*-cps (car ls)
                            (lambda (x) (k (cons x (cdr ls)))))]
        )]
      [(eqv? (car ls) '9)
       (cdr ls)]
      [else
       (remv-first-9*-cps (cdr ls)
                          (lambda (x) k (cons x (car ls))))])))

;Tests
(remv-first-9*-cps '((1 2 (3) 9)) (empty-k))
(remv-first-9*-cps '(9 (9 (9 (9)))) (empty-k))
(remv-first-9*-cps '(((((9) 9) 9) 9) 9) (empty-k))

;Problem 6 cons-cell-count-cps
(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (cdr ls)
                            (lambda (x) (cons-cell-count-cps (car ls)
                                                             (lambda (y) (k (add1 (+ x y)))))))]
      [else (k 0)])))

;Tests
(cons-cell-count-cps (cons (cons (cons (cons 3 '()) (cons 2 (cons 3 (cons 1 '())))) '()) '()) (empty-k))

;Problem 7 find-cps
(define find-cps
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr
          (find-cps (cdr pr) s k)
          (k u)))))

;Tests
(find-cps 5 '((5 . a) (6 . b) (7 . c)) (empty-k))
(find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (empty-k))
(find-cps 5 '((5 . 6) (9 . 6) (2 . 9)) (empty-k))

;Problem 8 ack-cps
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m)
       (k (add1 n))]
      [(zero? n)
       (ack-cps (sub1 m) 1 k)]
      [else
       (ack-cps m
                (sub1 n)
                (lambda (x) (ack-cps (sub1 m) x k)))])))

;Tests
(ack-cps 1 2 (empty-k))

;Problem 9 fib-cps
(define fib-cps
  (lambda (n k)
    ((lambda (fib-cps k)
       (fib-cps fib-cps n k))
     (lambda (fib-cps n k)
       (cond
         [(zero? n)
          (k n)]
         [(= 1 n)
          (k 1)]
         [else
          (fib-cps fib-cps
                   (sub1 (sub1 n))
                   (lambda(x) (fib-cps fib-cps
                                       (sub1 n)
                                       (lambda (y) (k (+ x y))))))])) k)))

;Tests
(fib-cps 1 (empty-k))
(fib-cps 2 (empty-k))
(fib-cps 3 (empty-k))
(fib-cps 20 (empty-k))

;Problem 10
;;null?-cps
(define null?-cps
  (lambda (ls k)
    (k (null? ls))))

;;car-cps
(define car-cps
  (lambda (pr k)
    (k (car pr))))

;;cdr-cps
(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))

;;unfold-cps
(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h-cps k)
       (h-cps h-cps
              (lambda (x) (x seed '() k))))
     (lambda (h-cps k)
       (k (lambda (seed ans k)
            (p seed (lambda (x)
                      (if x
                          (k ans)
                          (h-cps h-cps
                                 (lambda (y)
                                   (f seed (lambda (z)
                                             (g seed (lambda (a)
                                                       (y a (cons z ans) k))))))))))))) k)))

;Tests
(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k))

;Problem 11 unify-cps
;empty-s
(define empty-s
  (lambda ()
    '()))

;extend-s
(define extend-s
  (lambda (x v s)
`((,x . ,v) . ,s)))

(define unify-cps
  (lambda (v w s k)
    (find-cps v s 
              (lambda (v)
                (find-cps w s
                          (lambda (w)
                            (cond
                              [(eqv? v w) (k s)]
                              [(symbol? v) (k (extend-s v w s))]
                              [(symbol? w) (k (extend-s w v s))]
                              [(and (pair? v) (pair? w))
                               (unify-cps (car v) (car w) s
                                      (lambda (s^)
                                        (if s^
                                            (unify-cps (cdr v) (cdr w) s^ 
                                                       (lambda (s^^)
                                                         (k s^^)))#f)))]
                              [(equal? v w) (k s)]
                              [else (k #f)])))))))

;Tests
(unify-cps '(x y z) '(5 x y) (empty-s) (empty-k))

;Problem 12 M-cps
(define M-cps
  (lambda (f k)
    (k (lambda (ls k)
         (cond
           [(null? ls)
            (k '())]
           [else 
            (M-cps f
               (lambda (x)
                 (x (cdr ls) (lambda (y)
                            (f (car ls) (lambda (z)
                                          (k (cons z y))))))))])))))

;Tests
((M-cps (lambda (n k) (k (add1 n))) (empty-k)) '(1 2 3 4 5) (empty-k))

;Problem 13
(define use-of-M-cps
  ((M-cps (lambda (n k) (k (add1 n))) (empty-k)) '(1 2 3 4 5) (empty-k)))

;Tests
