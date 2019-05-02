;Ian Polito
;ipolito
;CSCI-C311
;Assignment 9 Step 6

#lang racket

(require "parenthec.rkt")

;need to registerize the interpreter
;use define-registers
(define-registers
  exp
  env
  k
  rator
  rand
  v ;(apply-k)
  vari)

;;expr
(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))

;;clos
(define-union clos
  (closure body env-cps))

;;envr
(define-union envr
  (empty)
  (extend-env v env))

;;kt
(define-union kt
  (empty-k)
  (sub1-k k)
  (zero-k k)
  (mult-outer-k env-cps x k)
  (mult-inner-k v k)
  (if-k env-cps conseq alt k)
  (let-k env-cps body k)
  (throw-outer-k env-cps k-exp)
  (throw-inner-k v env-cps)
  (app-outer-k env-cps rand k)
  (app-inner-k v k))

;now all the previous let* calls need to be a begin block
;then use set!

;;value-of-cps
(define value-of-cps
  (lambda ()
  (union-case exp expr
              ;;const
              [(const cexp)
               ;example of begin block
               (begin
                 (set! v cexp)
                 (apply-k))]
              ;;mult
              [(mult nexp1 nexp2)
               ;example of begin block
               (begin
                 (set! exp nexp1)
                 (set! k (kt_mult-outer-k env nexp2 k))
                 (value-of-cps))]
              ;;sub1
              [(sub1 nexp)
               ;example of begin block
               (begin
                 (set! exp nexp)
                 (set! k (kt_sub1-k k))
                 (value-of-cps))]
              ;;zero
              [(zero nexp)
               ;example of begin block
               (begin
                 (set! exp nexp)
                 (set! k (kt_zero-k k))
                 (value-of-cps))]
              ;;if
              [(if test conseq alt)
               ;example of begin block
               (begin
                 (set! exp test)
                 (set! k (kt_if-k env conseq alt k))
                 (value-of-cps))]
              ;;letcc
              [(letcc body)
               ;example of begin block
               (begin
                 (set! exp body)
                 (set! env (envr_extend-env k env))
                 (set! k k)
                 (value-of-cps))]
              ;;throw
              [(throw kexp vexp)
               ;example of begin block
               (begin
                 (set! exp kexp)
                 (set! k (kt_throw-outer-k env vexp))
                 (value-of-cps))]
              ;;let
              [(let expr body)
               ;example of begin block
               (begin
                 (set! exp expr)
                 (set! k (kt_let-k env body k))
                 (value-of-cps))]
              ;;var
              [(var n)
               ;example of begin block
               (begin
                 (set! vari n)
                 (apply-env))]
              ;;lmabda
              [(lambda body)
               ;example of begin block
               (begin
                 (set! exp exp)
                 (set! v (clos_closure body env))
                 (apply-k))]
              ;;app
              [(app rator rand)
               ;example of begin block
               (begin
                 (set! exp rator)
                 (set! k (kt_app-outer-k env rand k))
                 (value-of-cps))]
              )))

;;apply-k
(define apply-k
  (lambda ()
  (union-case k kt
      ;;empty-k
      [(empty-k)
       v]
      ;;sub1-k
      [(sub1-k kx)
       ;example of begin block
       (begin
         (set! k kx)
         (set! v (sub1 v))
         (apply-k))]
      ;;zero-k
      [(zero-k kx)
       ;example of begin block
       (begin
         (set! k kx)
         (set! v (zero? v))
         (apply-k))]
      ;;mult-outer-k
      [(mult-outer-k env-cps x kx)
       ;example of begin block
       (begin
         (set! exp x)
         (set! env env-cps)
         (set! k (kt_mult-inner-k v kx))
         (value-of-cps))]
      ;;mult-inner-k
      [(mult-inner-k vx kx)
       ;example of begin block
       (begin
         (set! k kx)
         (set! v (* vx v))
         (apply-k))]
      ;;if-k
      [(if-k env-cps conseq alt kx)
       (if v
           ;example of begin block
           (begin
             (set! exp conseq)
             (set! env env-cps)
             (set! k kx)
             (value-of-cps))
           ;example of begin block
           (begin
             (set! exp alt)
             (set! env env-cps)
             (set! k kx)
             (value-of-cps)))]
      ;;throw-outer-k
      [(throw-outer-k env-cps k-exp)
       ;example of begin block
       (begin
         (set! exp k-exp)
         (set! env env-cps)
         (set! k (kt_throw-inner-k v env))
         (value-of-cps))]
      ;;throw-inner-k
      [(throw-inner-k v env-cps)
       ;example of begin block
       (begin
         (set! exp exp)
         (set! env env)
         (set! k v)
         (apply-k))]
      ;;let-k
      [(let-k env-cps body kx)
       ;example of begin block
       (begin
         (set! exp body)
         (set! env (envr_extend-env v env-cps))
         (set! k kx)
         (value-of-cps))]
      ;;app-outer-k
      [(app-outer-k env-cps rand kx)
       ;example of begin block
       (begin
         (set! exp rand)
         (set! env env-cps)
         (set! k (kt_app-inner-k v kx))
         (value-of-cps))]
      ;;app-inner-k
      [(app-inner-k vx kx)
       ;example of begin block
       (begin
         (set! exp exp)
         (set! env env)
         (set! rand v)
         (set! rator vx)
         (set! k kx)
         (apply-closure))]
      )))

;;apply-env
(define apply-env
  (lambda ()
    (union-case env envr
                [(empty) (error 'value-of-cps "unbound identifier")]
                [(extend-env re envx)
                 (if (zero? vari)
                     (begin
                       (set! k k)
                       (set! v re)
                       (apply-k))
                     (begin
                       (set! k k)
                       (set! env envx)
                       (set! vari (sub1 vari))
                       (apply-env)))]
                )))

;;apply-closure
(define apply-closure
  (lambda ()
    (union-case rator clos
                [(closure body env-cps)
                 (begin
                   (set! exp body)
                   (set! env (envr_extend-env rand env-cps))
                   (set! k k)
                   (value-of-cps))]
                )))

(define driver
  (lambda (expression)
    (begin (set! exp expression)
           (set! k (kt_empty-k))
           (set! env (envr_empty))
           (value-of-cps))))


(define main
  (lambda ()
    (driver (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0)
                    (expr_app (expr_app (expr_var 1) (expr_var 1))
                              (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_letcc
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0)
                     (expr_app (expr_app (expr_var 1)
                                         (expr_var 1)) (expr_const 4)))))(expr_const 5))))))