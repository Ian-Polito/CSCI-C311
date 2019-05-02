;Ian Polito
;ipolito
;CSCI-C311
;Assignment 4

#lang racket
(require rackunit rackunit/text-ui racket/sandbox wxme)
(provide test-file)

(define test-file
  (lambda (#:file-name (file "./a4.rkt")
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

;lex implementation
(define lex
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           `(var ,(if (memv y env)
                      (- (length env) (length (memv y env)))
                      0))]
      [`,y #:when (number? y)
           `(const ,y)]
      [`(zero? ,n)
       `(zero? ,(lex n env))]
      [`(sub1 ,n)
       `(sub1 ,(lex n env))]
      [`(* ,n1 ,n2)
       `(* ,(lex n1 env)
           ,(lex n2 env))]
      [`(if ,condi ,whentrue ,whenfalse)
       `(if ,(lex condi env)
           ,(lex whentrue env)
           ,(lex whenfalse env))]
      [`(let ((,U ,n)) ,body)
       `(let ,(lex n env)
          ,(lex body env))]
      [`(lambda (,n) ,body)
       `(lambda ,(lex body (cons n env)))]
      [`(,rator ,rand)
       (cons (lex rator env)
             (cons (lex rand env) empty))])))

;Part 2

;;value-of-fn
(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (apply-env env y)]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(zero? ,exp)
       (= (value-of-fn exp env) 0)]
      [`(sub1 ,exp)
       (- (value-of-fn exp env) 1)]
      [`(* ,exp1 ,exp2)
       (* (value-of-fn exp1 env)
          (value-of-fn exp2 env))]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (value-of-fn condi env)
           (value-of-fn whentrue env)
           (value-of-fn whenfalse env))]
      [`(let ((,x ,y)) ,body)
       (let ([z (value-of-fn y env)])
         (value-of-fn body (extend-env x z env)))]
      [`(lambda (,y) ,body)
       (closure-fn y body env)]
      [`(,rator ,rand)
       (apply-closure-fn (value-of-fn rator env)
                         (value-of-fn rand env))])))

;;empty-env
(define empty-env
  (lambda ()
    (lambda (y)
      (error 'fo-eulav "unbound variable ~s" y))))

;;extend-env
(define extend-env
  (lambda (x y env)
    `(extend-env ,x ,y ,env)))

;;apply-env
(define apply-env
  (lambda (env n)
    (match env
      (`(empty-env) `empty-env)
      (`(extend-env ,x ,y ,env) (if (eqv? x n)
                                    y
                                    (apply-env env n))))))

;;apply-closure-fn
(define apply-closure-fn
  (lambda (n1 n2)
    (n1 n2)))

;;closure-fn
(define closure-fn
  (lambda (y body env)
    (lambda (x)
      (value-of-fn body (extend-env y x env)))))

;Part 3

;;value-of-dynamic
(define value-of-dynamic
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (apply-env env y)]
      [`,y #:when (number? y)
           y]
      [`,y #:when(boolean? y)
           y]
      [`(quote ,exp)
       exp]
      [`(null? ,exp)
       (null? (value-of-dynamic exp env))]
      [`(zero? ,exp)
       (= (value-of-dynamic exp env) 0)]
      [`(sub1 ,exp)
       (- (value-of-dynamic exp env) 1)]
      [`(car ,exp)
       (car (value-of-dynamic exp env))]
      [`(cdr ,exp)
       (cdr (value-of-dynamic exp env))]
      [`(cons ,exp1 ,exp2)
       (cons (value-of-dynamic exp1 env)
             (value-of-dynamic exp2 env))]
      [`(* ,exp1 ,exp2)
       (* (value-of-dynamic exp1 env)
          (value-of-dynamic exp2 env))]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (value-of-dynamic condi env)
           (value-of-dynamic whentrue env)
           (value-of-dynamic whenfalse env))]
      [`(let ((,x ,y)) ,body)
       (let ([z (value-of-dynamic y env)])
         (value-of-dynamic body (extend-env x z env)))]
      [`(lambda (,y) ,body)
       `(lambda (,y) ,body)]
      [`(,rator ,rand)
       (match-let ((`(lambda (,y) ,body)
                    (value-of-dynamic rator env))
                   (`,x (value-of-dynamic rand env)))
         (value-of-dynamic body (extend-env y x env)))])))


;TEST CASES
    
(define tests
  (test-suite "a4"
    (test-suite "lex" 
      (test-equal-if-defined lex
        ((lex '(lambda (x) x) '())
          '(lambda (var 0)))
        ((lex '(lambda (y) (lambda (x) y)) '())
         '(lambda (lambda (var 1))))
        ((lex '(lambda (y) (lambda (x) (x y))) '())
         '(lambda (lambda ((var 0) (var 1)))))
        ((lex '(lambda (x) (lambda (x) (x x))) '())
         '(lambda (lambda ((var 0) (var 0)))))
        ((lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 
         '(lambda ((lambda ((var 0) (var 1))) (lambda (lambda ((var 2) (var 1)))))))
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
                          ((((((var 1) (var 3)) (var 5)) (var 2)) (var 0)) (var 1)))))))))))
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
                      ((lambda
                         (lambda
                           (lambda
                             ((((((var 2) (var 1)) (var 0)) (var 5)) (var 4)) (var 3)))))
                       (lambda
                         (lambda
                           (lambda
                             ((((((var 8) (var 7)) (var 6)) (var 2)) (var 1)) (var 0)))))))))))))
        ((lex '((lambda (x) x) 5) '())
         '((lambda (var 0)) (const 5)))
        ((lex '(lambda (!)
                 (lambda (n)
                   (if (zero? n) 1 (* n (! (sub1 n))))))
              '())
         '(lambda
            (lambda
              (if (zero? (var 0))
                  (const 1)
                  (* (var 0) ((var 1) (sub1 (var 0))))))))
        ((lex
          '(let ((! (lambda (!)
                      (lambda (n)
                        (if (zero? n) 1 (* n ((! !) (sub1 n))))))))
             ((! !) 5))
          '())
         '(let (lambda
                 (lambda
                   (if (zero? (var 0))
                       (const 1)
                       (* (var 0) (((var 1) (var 1)) (sub1 (var 0)))))))
            (((var 0) (var 0)) (const 5))))))          
    (test-suite "value-of/RI-closures/fn-reps"
      (test-equal-if-defined value-of-fn
        ((value-of-fn 
           '((lambda (x) (if (zero? x) 
                             12 
                             47)) 
              0) 
           (empty-env))
         12)    
        ((value-of-fn
          '(let ([y (* 3 4)])
             ((lambda (x) (* x y)) (sub1 6)))
          (empty-env))
         60)
        ((value-of-fn
          '(let ([x (* 2 3)])
             (let ([y (sub1 x)])
               (* x y)))
          (empty-env))
         30)
        ((value-of-fn
          '(let ([x (* 2 3)])
             (let ([x (sub1 x)])
               (* x x)))
          (empty-env))
         25)))
    (test-suite "value-of/RI-closures/ds-reps"
      (test-equal-if-defined value-of-ds
        ((value-of-ds
          '((lambda (x) (if (zero? x) 
        		    12 
        		    47)) 
            0) 
          (empty-env))
         12)    
        ((value-of-ds
          '(let ([y (* 3 4)])
             ((lambda (x) (* x y)) (sub1 6)))
          (empty-env))
         60)
        ((value-of-ds
          '(let ([x (* 2 3)])
             (let ([y (sub1 x)])
               (* x y)))
          (empty-env))
         30)
        ((value-of-ds
          '(let ([x (* 2 3)])
             (let ([x (sub1 x)])
               (* x x)))
          (empty-env))
         25)))
    (test-suite "value-of-dynamic"
      (test-equal-if-defined value-of-dynamic
        ((value-of-dynamic
          '(let ([x 2])
             (let ([f (lambda (e) x)])
               (let ([x 5])
                 (f 0))))
          (empty-env))
         '5)
        ((value-of-dynamic
          '(let ([! (lambda (n)
                      (if (zero? n) 
                          1
                          (* n (! (sub1 n)))))])
             (! 5))
          (empty-env))
         '120)
        ((value-of-dynamic
          '((lambda (!) (! 5))
            (lambda (n)
              (if (zero? n) 
                  1
                  (* n (! (sub1 n))))))
          (empty-env))
         120)
        ((value-of-dynamic
          '(let ([f (lambda (x) (cons x l))])
             (let ([cmap 
                    (lambda (f)
                      (lambda (l)               
                        (if (null? l) 
                            '()
                            (cons (f (car l)) ((cmap f) (cdr l))))))])
               ((cmap f) (cons 1 (cons 2 (cons 3 '())))))) 
          (empty-env))
         '((1 1 2 3) (2 2 3) (3 3)))))
    (test-suite "value-of-ri"
      (test-equal-if-defined "value-of-ri"
        (((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))
         5)
        (((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))
         5)
        (((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))
         5)
        (((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))
         5)))))


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