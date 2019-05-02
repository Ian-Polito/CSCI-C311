;Ian Polito
;ipolito
;CSCI-C311
;Assignment 13

#lang racket
(require "mk.rkt")
(require "numbers.rkt")

;listo
(define listo
  (lambda (als)
    (conde
     [(== als empty)]
     [(fresh (x y)
             (== `(,x . ,y) als)
             (listo y))])))

;facto
(define facto
  (lambda (n1 n2) 
    (conde
      [(== n1 empty) (== n2 '(1))]
      [(fresh (n1_f n2_f)
              (facto n1_f n2_f)
              (minuso n1 '(1) n1_f)
              (*o n1 n2_f n2))])))

;fibso
(define fibso
  (lambda (n o1 o2)
    (conde
     [(== n empty) (== o1 '(1))
                   (== o2 '(1))]
     [(fresh (n_f o1_f o2_f)
             (minuso n '(1) n_f)
             (fibso n_f o1_f o2_f)
             (pluso o1_f o2_f o2)
             (== o1 o2_f))])))

;fo-lavo
(define (fo-lavo exp vars vals o)
  (conde
    ((symbolo exp) (lookup exp vars vals o))
    ((== exp `(,o etouq))
     (absento 'closure o)
     (absento  'etouq vars))
    ((fresh (x b)
       (== exp `(,b (,x) adbmal))
       (absento 'adbmal vars)
       (symbolo x)
       (== o `(closure ,x ,b ,vars ,vals))))
    ((fresh (exps re)
            (reverseo exp re)
            (== re `(tsil . ,exps))
            (absento 'tsil vars)
            (fo-lavo-help exps vars vals o)))
    ((fresh (rator rand)
       (== exp `(,rand ,rator))
      (fresh (x b vars^ vals^ a)
        (fo-lavo rand vars vals `(closure ,x ,b ,vars^ ,vals^))
        (fo-lavo rator vars vals a)
        (fo-lavo b `(,x . ,vars^) `(,a . ,vals^) o))))))

(define (fo-lavo-help exps vars vals o)
  (conde
    ((== `() exps) (== o `()))
    ((fresh (exp exps^)
       (== exps `(,exp . ,exps^))
       (fresh (v v^)
         (== o `(,v . ,v^))
         (fo-lavo exp vars vals v)
(fo-lavo-help exps^ vars vals v^))))))

(define (lookup x vars vals o)
  (fresh (y vars^ a vals^)
    (== `(,y . ,vars^) vars)
    (== `(,a . ,vals^) vals)
    (conde
      ((== x y) (== o a))
((=/= x y) (lookup x vars^ vals^ o)))))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define pairo
  (lambda (p)
    (fresh (a d)
(conso a d p))))

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

;color-middle-earth
(define middle-earth
  '((lindon eriador forodwaith)
    (forodwaith lindon rhovanion eriador)
    (eriador lindon forodwaith rhovanion enedwaith)
    (rhovanion forodwaith eriador enedwaith rohan rhun)
    (enedwaith eriador rhovanion rohan gondor)
    (rohan enedwaith rhovanion rhun gondor mordor)
    (gondor enedwaith rohan mordor)
    (rhun rohan rhovanion khand mordor)
    (mordor gondor rohan rhun khand harad)
    (khand mordor rhun harad)
(harad mordor khand)))