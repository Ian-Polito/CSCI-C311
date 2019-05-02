;Ian Polito
;ipolito
;CSCI-C311
;Assignment 10

#lang racket
(require "mk.rkt")
(require "numbers.rkt")

;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.

;; 1 What is the value of 

(run 2 (q)
  (== 5 q)
  (conde
   [(conde 
     [(== 5 q)
      (== 6 q)])
    (== 5 q)]
   [(== q 5)]))

; A: '(5)

;; 2 What is the value of
(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a)))

; A: '(((_.0 _.1) (=/= ((_.0 tag))) (sym _.0) (absento (tag _.1))))

;; 3 What do the following miniKanren constraints mean?
;; a ==
;must be equal
;; b =/=
;must be not equal
;; c absento
;must not be in the arguments
;; d numbero
;must be a number
;; e symbolo
;must be a symbol

;; Part II

(define assoc
  (lambda (x ls)
    (match-let* ((`(,a . ,d) ls)
                 (`(,aa . ,da) a))
      (cond
        ((equal? aa x) a)
        ((not (equal? aa x)) (assoc x d))))))

(define reverse
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else
       (match-let* ((`(,a . ,d) ls)
                    (res (reverse d)))
         (append res `(,a)))))))

(define stutter
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else 
        (match-let* ((`(,a . ,d) ls)
		     (res (stutter d)))
          `(,a ,a . ,res))))))

;assoco
(define assoco
  (lambda (x ls o)
    (fresh (a d)
           (== `(,a . ,d) ls)
           (fresh (aa da)
                  (== `(,aa . ,da) a)
                  (conde
                   [(== aa x)
                    (== a o)]
                   [(=/= aa x)
                    (fresh (res)
                           (assoco x d res))])))))

;reverseo
(define reverseo
  (lambda (ls o)
    (conde
     [(== '() ls)
      (== o '())]
     [(fresh (a d)
             (== `(,a . ,d) ls)
             (fresh (res)
                    (reverseo d res)
                    (appendo res `(,a) o)))])))

;stuttero
(define stuttero
  (lambda (ls o)
    (conde
     [(== ls '()) (== o '())]
     [(fresh (a d res)
             (== `(,a . ,d) ls)
             (== `(,a ,a . ,res) o)
             (stuttero d res))])))