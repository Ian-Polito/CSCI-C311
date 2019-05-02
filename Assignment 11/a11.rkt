;Ian Polito
;ipolito
;CSCI-C311
;Assignment 11

#lang racket
(require "mk.rkt")

;Part 2

(define apply-Go
  (lambda (G e t)
    (fresh (a G^)
      (== `(,a . ,G^) G)
      (fresh (aa da)
        (== `(,aa . ,da) a)
        (conde
          ((== aa e) (== da t))
((=/= aa e) (apply-Go G^ e t)))))))

(define !-
  (lambda (G e t)
    (conde
      ((numbero e) (== 'Nat t))
      ((== t 'Bool)
       (conde
         ((== #t e))
         ((== #f e))))
      ((fresh (ne1 ne2)
         (== `(+ ,ne1 ,ne2) e)
         (== 'Nat t)
         (!- G ne1 'Nat)
         (!- G ne2 'Nat)))
      ((fresh (teste anse elsee)
        (== `(if ,teste ,anse ,elsee) e)
        (!- G teste 'Bool)
        (!- G anse t)
        (!- G elsee t)))
      ((symbolo e) (apply-Go G e t))
      ((fresh (x b)
        (== `(lambda (,x) ,b) e)
        (symbolo x)
        (fresh (tx tb)          
          (== `(,tx -> ,tb) t)
          (!- `((,x . ,tx) . ,G) b tb))))
      ((fresh (e1 arg)
        (== `(,e1 ,arg) e)
        (fresh (targ)
          (!- G e1 `(,targ -> ,t))
          (!- G arg targ))))
      ;;zero?
      ((fresh (x)
              (== `(zero? ,x) e)
              (== 'Bool t)
              (!- G x 'Nat)))
      ;;sub1
      ((fresh (x)
              (== `(sub1 ,x) e)
              (== `Nat t)
              (!- G x 'Nat)))
      ;;not
      ((fresh (x)
              (== `(not ,x) e)
              (== 'Bool t)
              (!- G x 'Bool)))
      ;;*
      ((fresh (x1 x2)
              (== `(* ,x1 ,x2) e)
              (== 'Nat t)
              (!- G x1 'Nat)
              (!- G x2 'Nat)))
      ;;fix
      ((fresh (x)
              (== `(fix ,x) e)
              (!- G x `(,t -> ,t)))))))