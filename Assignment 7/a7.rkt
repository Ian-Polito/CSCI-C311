;Ian Polito
;ipolito
;CSCI-C311
;Assignment 7

#lang racket
(require rackunit rackunit/text-ui racket/sandbox wxme)
(provide test-file)

(define test-file
  (lambda (#:file-name (file "./a7.rkt")
	   #:sec-of-eval (sec 5)
	   #:mb-of-eval (mb 5))
    (parameterize ((read-accept-reader #t)
                   (read-accept-lang #t))
      (let ((input-port (open-input-file file)))
        (if (is-wxme-stream? input-port)
            (error 'test-file "Your file contains non-text elements (e.g. pictures, comment boxes). Please remove them and retry")
            (let ((sandboxed-eval (make-module-evaluator (read input-port))))
              (set-eval-limits sandboxed-eval sec mb)
              (parameterize ((current-eval sandboxed-eval)
                             (error-print-context-length 0))
                (run-tests tests))))))))

;Part 1
;last-non-zero
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
          ((last-non-zero
            (lambda (ls)
              (cond
                [(null? ls) ls]
                [(eqv? (car ls) 0) (k (last-non-zero (cdr ls)))]
                [else (cons (car ls) (last-non-zero (cdr ls)))]))))
(last-non-zero ls)))))

;Part 2
;lex
(define lex
  (λ (lce ls)
    (match lce
      [`,y #:when (symbol? y)
           (if (memv y ls)
               `(var ,(length (takef ls (λ (e) (not (eqv? e y))))))
               '())]
      [`,y #:when (number? y) `(const ,y)]
      [`(zero? ,x) `(zero ,(lex x ls))]
      [`(sub1 ,x) `(sub1 ,(lex x ls))]
      [`(* ,x ,y) `(mult ,(lex x ls) ,(lex y ls))]
      [`(if ,test ,then ,alt) `(if ,(lex test ls)
                                   ,(lex then ls)
                                   ,(lex alt ls))]
      [`(let ((,x ,exp)) ,body) `(let ,(lex exp ls)
                                   ,(lex body (cons x ls)))]
      [`(let/cc ,x ,body) `(let/cc ,(lex body (cons x ls)))]
      [`(throw ,k-exp ,v-exp) `(throw ,(lex k-exp ls) ,(lex v-exp ls))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (cons x ls)))]
      [`(,rator ,rand) `(app ,(lex rator ls) ,(lex rand ls))])))

;Part 3
;Interpreter

;1. value-of-cps
(define value-of-cps
  (lambda (expr env-cps k)
    (match expr
      [`(const ,expr)
       (apply-k k expr)]
      [`(var ,expr)
       (apply-env env-cps expr k)]
      [`(mult ,x1 ,x2)
       (value-of-cps x1 env-cps (mult-outer-k x2 env-cps k))]
      [`(sub1 ,x)
       (value-of-cps x env-cps (sub1-k k))]
      [`(zero ,x)
       (value-of-cps x env-cps (zero-k k))]
      [`(if ,test ,conseq ,alt)
       (value-of-cps test env-cps (if-k conseq alt env-cps k))]
      [`(let/cc ,body)
       (value-of-cps body (extend-env k env-cps) k)]
      [`(throw ,k-exp ,v-exp)
       (value-of-cps k-exp env-cps (throw-k v-exp env-cps))]
      [`(let ,e ,body)
       (value-of-cps e env-cps (let-k body env-cps k))]
      [`(lambda ,body)
       (apply-k k (make-closure body env-cps))]
      [`(app ,rator ,rand)
(value-of-cps rator env-cps (app-outer-k rand env-cps k))])))

(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))

;2. apply-env, apply-closure, apply-k
(define apply-env
  (λ (env-cps y k)
    (match env-cps
      [`(empty-env) (error 'value-of-cps "unbound identifier")]
      [`(extend-env ,a^ ,env-cps^) (if (zero? y) 
                                       (apply-k k a^)
(apply-env env-cps^ (sub1 y) k))])))

(define apply-closure
  (λ (c-cps a k)
    (match c-cps
      [`(closure ,body ,env-cps)
(value-of-cps body (extend-env a env-cps) k)])))

(define apply-k
  (λ (k v)
    (match k
      [`(empty-k)
       v]
      [`(mult-inner-k ,v^ ,k^)
       (apply-k k^ (* v^ v))]
      [`(mult-outer-k ,x2^ , env-cps^ ,k^)
       (value-of-cps x2^ env-cps^ (mult-inner-k v k^))]
      [`(sub1-k ,k^)
       (apply-k k^ (sub1 v))]
      [`(zero-k ,k^)
       (apply-k k^ (zero? v))]
      [`(if-k ,conseq^ ,alt^ ,env-cps^ ,k^)
       (if v
           (value-of-cps conseq^ env-cps^ k^)
           (value-of-cps alt^ env-cps^ k^))]
      [`(throw-k ,v-exp^ ,env-cps^)
       (value-of-cps v-exp^ env-cps^ v)]
      [`(let-k ,body^ ,env-cps^ ,k^)
       (value-of-cps body^ (extend-env v env-cps^) k^)]
      [`(app-inner-k ,c-cps^ ,k^)
       (apply-closure c-cps^ v k^)]
      [`(app-outer-k ,rand^ ,env-cps^ ,k^)
(value-of-cps rand^ env-cps^ (app-inner-k v k^))])))

;3./4. extend-env
(define extend-env
  (λ (a^ env-cps^)
`(extend-env ,a^ ,env-cps^)))

;7. make-closure
(define make-closure
  (λ (body env-cps)
`(closure ,body ,env-cps)))

(define mult-outer-k
  (λ (x2^ env-cps^ k^)
`(mult-outer-k ,x2^ ,env-cps^ ,k^)))

(define mult-inner-k
  (λ (v^ k^)
`(mult-inner-k ,v^ ,k^)))

(define sub1-k
  (λ (k^)
`(sub1-k ,k^)))

(define zero-k
  (λ (k^)
`(zero-k ,k^)))

(define if-k
  (λ (conseq^ alt^ env-cps^ k^)
`(if-k ,conseq^ ,alt^ ,env-cps^ ,k^)))

(define throw-k
  (λ (v-exp^ env-cps^)
`(throw-k ,v-exp^ ,env-cps^)))

(define let-k
  (λ (body^ env-cps^ k^)
`(let-k ,body^ ,env-cps^ ,k^)))

(define app-outer-k
  (λ (rand^ env-cps^ k^)
`(app-outer-k ,rand^ ,env-cps^ ,k^)))

(define app-inner-k
  (λ (c-cps^ k^)
`(app-inner-k ,c-cps^ ,k^)))

;TEST CASES

(define tests
  (test-suite "a7"

    (test-suite "last-non-zero"
      (test-equal-if-defined last-non-zero
        ((last-non-zero '(0)) '())
        ((last-non-zero '(1 2 3 0 4 5)) '(4 5))
        ((last-non-zero '(1 0 2 3 0 4 5)) '(4 5))
        ((last-non-zero '(1 2 3 4 5)) '(1 2 3 4 5))))

    (test-suite "lex" 
      (test-equal-if-defined lex
        ((lex '(lambda (x) x) '())
          '(lambda (var 0)))
        ((lex '(lambda (x) 120) '())
          '(lambda (const 120)))
        ((lex '(lambda (y) (lambda (x) y)) '())
         '(lambda (lambda (var 1))))
        ((lex '((lambda (x) x) 5) '())
         '(app (lambda (var 0)) (const 5)))
        ((lex '((lambda (x) 0) 5) '())
         '(app (lambda (const 0)) (const 5)))
        ((lex '(lambda (y) (lambda (x) (x y))) '())
         '(lambda (lambda (app (var 0) (var 1)))))
        ((lex '(lambda (x) (lambda (x) (x x))) '())
         '(lambda (lambda (app (var 0) (var 0)))))
        ((lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 
         '(lambda (app (lambda (app (var 0) (var 1))) (lambda (lambda (app (var 2) (var 1)))))))
        ((lex '(let/cc k 5) '()) '(letcc (const 5)))
        ((lex '(let/cc k (throw k 5)) '()) '(letcc (throw (var 0) (const 5))))
        ((lex '(let/cc k (throw k (* 5 5))) '()) '(letcc (throw (var 0) (mult (const 5) (const 5)))))
        ((lex '(let/cc k (throw k (((lambda (x) x) k) (* 5 5)))) '()) 
         '(letcc
           (throw
            (var 0)
            (app (app (lambda (var 0)) (var 0)) (mult (const 5) (const 5))))))
        ((lex '(let/cc k (sub1 (throw k 5))) '()) '(letcc (sub1 (throw (var 0) (const 5)))))
        ((lex '(let/cc k (throw (throw k 5) 6)) '()) 
         '(letcc (throw (throw (var 0) (const 5)) (const 6))))
        ((lex '(let/cc k (throw 5 (throw k 5))) '())
         '(letcc (throw (const 5) (throw (var 0) (const 5)))))
        ((lex '(* 3 (let/cc k (throw 5 (throw k 5)))) '())
         '(mult (const 3) (letcc (throw (const 5) (throw (var 0) (const 5))))))
        ((lex '(lambda (a)
                 (lambda (b)
                   (lambda (c)
                     (lambda (a)
                       (lambda (b)
                         (lambda (d)
                           (lambda (a)
                             (lambda (e)
                               (((((a b) c) d) e) a)))))))))
              '())
         '(lambda
            (lambda
              (lambda
                (lambda
                  (lambda
                    (lambda
                      (lambda
                        (lambda
                          (app (app (app (app (app (var 1) (var 3)) (var 5)) (var 2)) (var 0)) (var 1)))))))))))
        ((lex '(lambda (a)
                 (lambda (b)
                   (lambda (c)
                     (lambda (w)
                       (lambda (x)
                         (lambda (y)
                           ((lambda (a)
                              (lambda (b)
                                (lambda (c)
                                  (((((a b) c) w) x) y))))
                            (lambda (w)
                              (lambda (x)
                                (lambda (y)
                                  (((((a b) c) w) x) y)))))))))))
              '())
         '(lambda 
            (lambda 
              (lambda 
                (lambda 
                  (lambda 
                    (lambda 
                      (app (lambda
                         (lambda
                           (lambda
                             (app (app (app (app (app (var 2) (var 1)) (var 0)) (var 5)) (var 4)) (var 3)))))
                       (lambda
                         (lambda
                           (lambda
                             (app (app (app (app (app (var 8) (var 7)) (var 6)) (var 2)) (var 1)) (var 0)))))))))))))
        ((lex '((lambda (x) x) 5) '())
         '(app (lambda (var 0)) (const 5)))
        ((lex '(lambda (!)
                 (lambda (n)
                   (if (zero? n) 1 (* n (! (sub1 n))))))
              '())
         '(lambda
            (lambda
              (if (zero (var 0))
                  (const 1)
                  (mult (var 0) (app (var 1) (sub1 (var 0))))))))
        ((lex
          '(let ((! (lambda (!)
                      (lambda (n)
                        (if (zero? n) 1 (* n ((! !) (sub1 n))))))))
             ((! !) 5))
          '())
         '(let (lambda
                 (lambda
                   (if (zero (var 0))
                       (const 1)
                       (mult (var 0) (app (app (var 1) (var 1)) (sub1 (var 0)))))))
            (app (app (var 0) (var 0)) (const 5))))))


    (test-suite "value-of-cps"
      (test-equal-if-defined value-of-cps
        ((value-of-cps '(const 5) (empty-env) (empty-k)) 5)
        ((value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) 25)
        ((value-of-cps '(zero (const 5)) (empty-env) (empty-k)) #f)
        ((value-of-cps '(sub1 (const 5)) (empty-env) (empty-k)) 4)
        ((value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) 3)
        ((value-of-cps '(zero (sub1 (const 6))) (empty-env) (empty-k)) #f)
        ((value-of-cps '(if (zero (const 5)) (const 3) (mult (const 2) (const 2))) (empty-env) (empty-k)) 4)
        ((value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k)) 4)
        ((value-of-cps '(app (lambda (const 5)) (const 6)) (empty-env) (empty-k)) 5) 
        ((value-of-cps '(app (lambda (var 0)) (const 5)) (empty-env) (empty-k)) 5)
        ((value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) 6)
        ((value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) 5)
        ((value-of-cps '(app (lambda (if (zero (var 0)) (const 4) (const 5))) (const 3)) (empty-env) (empty-k)) 5)
        ((value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k)) 4)
        ((value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k)) 5)
        ((value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) 25)
        ((value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 5)
        ((value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 3)
        ((value-of-cps '(letcc (const 5)) (empty-env) (empty-k)) 5)
        ((value-of-cps '(letcc (throw (var 0) (const 5))) (empty-env) (empty-k)) 5)
        ((value-of-cps '(letcc (throw (var 0) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
        ((value-of-cps '(letcc (throw (app (lambda (var 0)) (var 0)) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
        ((value-of-cps '(letcc (sub1 (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
        ((value-of-cps '(letcc (throw (throw (var 0) (const 5)) (const 6))) (empty-env) (empty-k)) 5)
        ((value-of-cps '(letcc (throw (const 5) (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
        ((value-of-cps '(mult (const 3) (letcc (throw (const 5) (throw (var 0) (const 5))))) (empty-env) (empty-k)) 15)
        ((value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
                       (empty-env)
                       (empty-k))
         4)
        ((value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
                       (empty-env)
                       (empty-k))
         4)
        ((value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                             (lambda
                               (lambda 
                                 (if (zero (var 0))  
                                     (const 1)
                                     (app (app (var 1) (var 1)) (sub1 (var 0)))))))
                       (empty-env)
                       (empty-k))
         1)))
    
    (test-suite "trib$"
      (test-equal-if-defined trib$
	((car$ trib$) 0)
	((car$ (cdr$ trib$)) 1)
	((take$ 7 trib$) '(0 1 1 2 4 7 13))))))

(define-syntax test-if-defined
  (syntax-rules ()
    ((_ sym tests ...)
     (test-case (format "~a undefined" 'sym)
                (check-not-false (lambda () (eval 'sym)))
                tests ...))))

(define-syntax test-equal-if-defined
  (syntax-rules ()
    ((_ ident (expr val) ...)
      (let ((n 1))
        (test-case (format "~a: undefined" 'ident)
                   (check-not-exn (lambda () (eval 'ident)))
                   (test-case (format "~a: ~a" 'ident n)
                              (with-check-info 
                               (('tested 'expr))
                               (set! n (add1 n))
                               (check equal? (eval 'expr) val))) ...)))))

(define-syntax ifdef-suite
  (syntax-rules ()
    ((_ ident (expr val) ...)
     (let ((n 1))
       (test-suite (~a 'ident)
        (test-case "undefined"
         (check-not-exn (lambda () (eval 'ident)))
         (test-case (~a n)
          (with-check-info (('tested 'expr))
           (set! n (add1 n))
           (check equal? (eval 'expr) val))) ...))))))