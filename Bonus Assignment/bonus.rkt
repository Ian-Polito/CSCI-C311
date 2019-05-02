;Ian Polito
;ipolito
;CSCI-C311
;Bonus Assignment

#lang racket

;Part 1

;filter*-sps
(define filter*-sps
  (lambda (pre ls s)
    (cond
      [(empty? ls) (values ls s)]
      [(pre (car ls)) (let-values (((v ls2) (filter*-sps pre (cdr ls) s)))
                        (values (cons (car ls) v) ls2))]
      [else (let-values (((v ls2) (filter*-sps pre (cdr ls) s)))
              (values v (cons (car ls) ls2)))])))

;fib-sps
(define fib-sps
  (lambda (n s)
    (cond
      [(assv n s) => (lambda (pr) (values (cdr pr) s))]
      [(< n 2) (values n (cons (cons n n) s))]
      [else
       (let-values (((v s2) (fib-sps (- n 2) s)))
         (let-values (((v2 s3) (fib-sps (- n 1) s2)))
           (values (+ v v2) (cons (cons n (+ v v2)) s3))))])))

;Part 2

;and*
(define-syntax and*
  (syntax-rules ()
    [(and*) #t]
    [(and* n) n]
    [(and* start ... end) end]))

;list*
(define-syntax list*
  (syntax-rules ()
    [(list* ,n) ,n]
    [(list* start end ...) (cons start (list* end ...))]))

;macro-list
(define-syntax macro-list
  (syntax-rules ()
    [(macro-list) '()]
    [(macro-list start end ...) (cons start (macro-list end ...))]))

;mcond
(define-syntax mcond
  (syntax-rules ()
    [(mcond (else ,n)) ,n]
    [(mcond (#f n)) #f]
    [(mcond (#t n)) n]
    [(mcond n) n]
    [(mcond e1 e2 ...) (if (mcond e1) (mcond e1) (mcond e2 ...))]))

;macro-map
(define-syntax macro-map
  (syntax-rules ()
    [(macro-map ,pre (start end ...)) (cons (pre start) (macro-map pre (end ...)))]
    [(macro-map pre end) end]))