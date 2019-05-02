;Ian Polito
;ipolito
;CSCI-C311
;Assignment 3

#lang racket
(require rackunit rackunit/text-ui racket/sandbox wxme)
(provide test-file)

(define test-file
  (lambda (#:file-name (file "./a3.rkt")
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
(define value-of
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (env y)]
      [`,y #:when (number? y)
           y]
      [`,y #:when(boolean? y)
           y]
      [`(zero? ,exp)
       (= (value-of exp env) 0)]
      [`(sub1 ,exp)
       (- (value-of exp env) 1)]
      [`(* ,exp1 ,exp2)
       (* (value-of exp1 env)
          (value-of exp2 env))]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (value-of condi env)
           (value-of whentrue env)
           (value-of whenfalse env))]
      [`(let ((,x ,y)) ,body)
       (let ([z (value-of y env)])
         (value-of body
                   (lambda (i)
                     (if (eqv? x i)
                         z
                         (env i)))))]
      [`(lambda (,x) ,body)
       (lambda (i)
         (value-of body
                   (lambda (y)
                     (if (eqv? x y)
                         i
                         (env y)))))]
      [`(,rator ,rand)
       ((value-of rator env)
        (value-of rand env))]
      )))

;function
(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (apply-env-fn env y)]
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
         (value-of-fn body (extend-env-fn x z env)))]
      [`(lambda (,y) ,body)
       (lambda (x)
         (value-of-fn body (extend-env-fn y x env)))]
      [`(,rator ,rand)
       ((value-of-fn rator env)
        (value-of-fn rand env))]
      )))

(define empty-env-fn
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound variable ~s" y))))

(define extend-env-fn
  (lambda (x exp env)
    (lambda (y)
      (if (eqv? x y)
          exp
          (apply-env-fn env y)))))

(define apply-env-fn
  (lambda (env y)
    (env y)))

;data-structural
(define value-of-ds
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (apply-env-ds env y)]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(zero? ,exp)
       (= (value-of-ds exp env) 0)]
      [`(sub1 ,exp)
       (- (value-of-ds exp env) 1)]
      [`(* ,exp1 ,exp2)
       (* (value-of-ds exp1 env)
          (value-of-ds exp2 env))]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (value-of-ds condi env)
           (value-of-ds whentrue env)
           (value-of-ds whenfalse env))]
      [`(let ((,y ,x)) ,body)
       (let ([z (value-of-ds x env)])
         (value-of-ds body (extend-env-ds y z env)))]
      [`(lambda (,y) ,body)
       (lambda (x)
         (value-of-ds body (extend-env-ds y x env)))]
      [`(,rator ,rand)
       ((value-of-ds rator env)
        (value-of-ds rand env))]
      )))

(define empty-env-ds
  (lambda ()
    `(empty-env)))

(define extend-env-ds
  (lambda (x i env)
    `(extend-env-ds ,x ,i ,env)))

(define apply-env-ds
  (lambda (env y)
    (match env
      [`(empty-env)
       'error]
      [`(extend-env-ds ,x ,i ,env)
       (if (eqv? x y)
           i
           (apply-env-ds env y))]
      )))

;Part 2
(define fo-eulav
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (env y)]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(,exp ?orez)
       (= (fo-eulav exp env) 0)]
      [`(,exp 1bus)
       (- (fo-eulav exp env) 1)]
      [`(,exp1 ,exp2 *)
       (* (fo-eulav exp2 env)
          (fo-eulav exp1 env))]
      [`(,whenfalse ,whentrue ,condi fi)
       (if (fo-eulav condi env)
           (fo-eulav whentrue env)
           (fo-eulav whenfalse env))]
      [`(,body ((,x ,y)) tel)
       (let ([a (fo-eulav x env)])
         (fo-eulav body
                   (lambda (i)
                     (if (eqv? y i)
                         a
                         (env i)))))]
      [`(,body (,y) adbmal)
       (lambda (i)
         (fo-eulav body
                   (lambda (x)
                     (if (eqv? y x)
                         i
                         (env x)))))]
      
      [`(,rand ,rator)
       ((fo-eulav rator env)
        (fo-eulav rand env))]
      )))

(define empty-env
  (lambda ()
    (lambda (y)
      (error 'fo-eulav "unbound variable ~s" y))))

;TEST CASES

(define tests
  (test-suite "A3"

   (test-suite "value-of"
   (test-equal-if-defined value-of
     ;; 0 booleans                          
     [(value-of
      '((lambda (x) (if (zero? x)
                   #t
                   #f))
        0)
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     #t]
    ;; 1 if
    [(value-of
      '((lambda (x)
          (if (zero? x) 12 47))
                 0)
               (lambda (y) (error 'value-of "unbound variable ~s" y)))
     12]
    ;; 2 let
    [(value-of
      '(let ([y (* 3 4)])
         ((lambda (x) (* x y)) (sub1 6)))
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     60]
    ;; 3 let
    [(value-of
      '(let ([x (* 2 3)])
         (let ([y (sub1 x)])
           (* x y)))
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     30]
    ;; 4 let
    [(value-of
      '(let ([x (* 2 3)])
         (let ([x (sub1 x)])
           (* x x)))
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     25]
    ;; 5 Poor Man's Y using let - Fact
    [(value-of
      '(let ((! (lambda (x) (* x x))))
         (let ((! (lambda (n)
                    (if (zero? n)
                        1
                        (* n (! (sub1 n)))))))
           (! 5)))
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     80]
    ;; 6 Poor Man's Y using lambda - Fact
    [(value-of
      '(((lambda (f)
           (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
         (lambda (f)
           (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
        5)
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     120]))

   (test-suite "value-of-fn"
   (test-equal-if-defined value-of-fn
     ;; 0 booleans                          
     [(value-of-fn
      '((lambda (x) (if (zero? x)
                   #t
                   #f))
        0)
      (empty-env-fn))
     #t]
    ;; 1 if                      
    [(value-of-fn
      '((lambda (x) (if (zero? x)
                   12
                   47))
        0)
      (empty-env-fn))
     12]
    ;; 2 let
    [(value-of-fn
      '(let ([y (* 3 4)])
         ((lambda (x) (* x y)) (sub1 6)))
      (empty-env-fn))
     60]
    ;; 3 let
    [(value-of-fn
      '(let ([x (* 2 3)])
         (let ([y (sub1 x)])
           (* x y)))
      (empty-env-fn))
     30]
    ;; 4 let
    [(value-of-fn
      '(let ([x (* 2 3)])
         (let ([x (sub1 x)])
           (* x x)))
      (empty-env-fn))
     25]
    ;; 5 Poor Man's Y using lambda - Fact
    [(value-of-fn
      '(((lambda (f)
           (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
         (lambda (f)
           (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
        5)
      (empty-env-fn))
     120]))

   (test-suite "value-of-ds"
   (test-equal-if-defined value-of-ds
    ;; 0 booleans
    [(value-of-ds
      '((lambda (x) (if (zero? x)
                   #t
                   #f))
        0)
      (empty-env-ds))
     #t]
    ;; if
    [(value-of-ds
      '((lambda (x) (if (zero? x)
                   12
                   47))
        0)
      (empty-env-ds))
     12]
    ;; 2 let
    [(value-of-ds
      '(let ([y (* 3 4)])
         ((lambda (x) (* x y)) (sub1 6)))
      (empty-env-ds))
     60]
    ;; 3 let
    [(value-of-ds
      '(let ([x (* 2 3)])
         (let ([y (sub1 x)])
           (* x y)))
      (empty-env-ds))
     30]
    ;; 4 let
    [(value-of-ds
      '(let ([x (* 2 3)])
         (let ([x (sub1 x)])
           (* x x)))
      (empty-env-ds))
     25]
    ;; 6 Poor Man's Y using lamda - Fact
    [(value-of-ds
      '(((lambda (f)
           (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
         (lambda (f)
           (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
        5)
      (empty-env-ds))
     120]))

   (ifdef-suite fo-eulav
    ;; 1
    [(fo-eulav '(5 (x (x) adbmal)) (empty-env))  5]
    ;; Stnemugra sa Snoitcnuf
    [(fo-eulav '(((x 1bus) (x) adbmal)
                ((5 f) (f) adbmal))
              (empty-env))
    4]
    ;; Tcaf
    [(fo-eulav   '(5
                   (((((((n 1bus) (f f)) n *)
                       1
                       (n ?orez) fi)
                      (n) adbmal)
                     (f) adbmal)
                    ((((((n 1bus) (f f)) n *) 1 (n ?orez) fi)
                      (n) adbmal)
                     (f) adbmal))) (empty-env))
    120])

   (test-suite "Brain teasers"
     (test-suite "Set!"
      (test-equal-if-defined value-of
     [(value-of
       '(* (begin2 1 1) 3)
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
      3]
     [(value-of
       '((lambda (a)
           ((lambda (p)
              (begin2
               (p a)
               a)) (lambda (x) (set! x 4)))) 3)
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
      3]
     [(value-of
       '((lambda (f)
           ((lambda (g)
              ((lambda (z) (begin2
                       (g z)
                       z))
               55))
            (lambda (y) (f y)))) (lambda (x) (set! x 44)))
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
      55]
     [(value-of
      '((lambda (x)
          (begin2 (set! x 5) x))
        6)
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     5]
     [(value-of
       '(let ((a 3))
          (begin2 (begin2 a (set! a 4)) a))
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
      4]
     [(value-of
       '((lambda (x)
           (begin2
            ((lambda (y)
               (begin2
                (set! x 0)
                98))
             99)
            x))
         97)
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
     0]
     [(value-of
       '((lambda (y)
           (let ((x (begin2
                     (set! y 7)
                     8)))
             (begin2
              (set! y 3)
              ((lambda (z) y)
               x))))
         4)
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
      3]
     [(value-of 
       '(let ((a 5))
          (let ((y (begin2 (set! a (sub1 a)) 6)))
            (begin2
              (* y y)
              a)))
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
      4]))
     (test-suite "value-of-lex helpers"
      (test-equal-if-defined apply-env-lex
        ((value-of-lex '((lambda (var 0)) (const 5)) '())
         5)
        ((value-of-lex '(((lambda (lambda (var 1))) (const 5)) (const 6)) '())
         5)
        ((value-of-lex '((((lambda
                             (lambda
                               (lambda
                                 (if (zero (var 2))
                                     (var 1)
                                     (var 0)))))
                           (const 0))
                          (const 10))
                         (const 20))
                       '())
         10))))
   (test-suite "church numerals"
     (test-equal-if-defined csub1
       ((let ((c5 (lambda (f) (lambda (x) (f (f (f (f (f x)))))))))
          (((csub1 c5) add1) 0))
        4)
       ((let ((c0 (lambda (f) (lambda (x) x))))
          (((csub1 c0) add1) 0))
        0)))))

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