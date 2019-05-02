;Ian Polito
;ipolito
;CSCI-C311
;Assignment 12

#lang racket
(require "monads.rkt")
(require rackunit rackunit/text-ui racket/sandbox wxme)
(provide test-file)

(define test-file
  (lambda (#:file-name (file "./a12.rkt")
	   #:sec-of-eval (sec 5)
	   #:mb-of-eval (mb 5))
    (run-tests tests)
    #;
    (parameterize ((read-accept-reader #t)
                   (read-accept-lang #t))
      (let ((sandboxed-eval
             (make-module-evaluator (read (open-input-file file)))))
        (set-eval-limits sandboxed-eval sec mb)
        (parameterize ((current-eval sandboxed-eval)
	               (error-print-context-length 0))
          (run-tests tests))))))

;findf-maybe
(define (findf-maybe pred ls)
  (match ls
    ['() (fail)]
    [`(,a . ,d) (if (pred a) `(Just ,a) (findf-maybe pred d))]))

;partition-writer
(define (partition-writer fn als)
  (cond
    [(null? als) (inj-writer '())]
    [(not (fn (car als)))
     (bind-writer
      (tell (car als))
      (lambda (x)
        (partition-writer fn (cdr als))))]
    [else
     (bind-writer
      (partition-writer fn (cdr als))
      (lambda (x)
(inj-writer (cons (car als) x))))]))

;power (given)
(define power
  (lambda (x n)
    (cond
      [(zero? n) 1]
      [(zero? (sub1 n)) x]
      [(odd? n) (* x (power x (sub1 n)))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (let ((y (power x nhalf)))
                     (* y y)))])))

;powerXpartials
(define (powerXpartials x n)
  (cond
    [(zero? n)
     (inj-writer 0)]
    [(= n 1)
     (inj-writer x)]
    [(odd? n)
     (bind-writer
      (powerXpartials x (- n 1))
      (lambda (a)
        (bind-writer
         (tell a)
         (lambda (b)
           (inj-writer (* x a))))))]
    [(even? n)
     (bind-writer
      (powerXpartials x (/ n 2))
      (lambda (a)
        (bind-writer
         (tell a)
         (lambda (b)
           (inj-writer (* a a))))))]))

;replace-with-count
;does not compile, could not get this function to work
#;(define (replace-with-count x tr)
  (match tr
    ['() (inj-state '())]
    [`,y #:when (symbol? y) (if (equal? x y) get y)]
    [`(,a . ,d) (if (equal? x a)
                    (bind-state get
                                (lambda (s) (bind-state (put (add1 s))
                                                        (lambda (_) (replace-with-count x d)))))
                    (go-on ((s get))
                           (put (add1 s))
                           (replace-with-count x d)
                    (bind-state (replace-with-count x d)
                                (lambda (almost) (inj-state (cons s almost))))))]))

;traverse (given)
(define traverse
    (lambda (inj bind f)
      (letrec
        ((trav
           (lambda (tree)
             (cond
               [(pair? tree)
                (go-on ([a (trav (car tree))]
                        [d (trav (cdr tree))])
                  (inj (cons a d)))]
               [else (f tree)]))))
        trav)))

;reciprocal
(define (reciprocal x)
  (if (zero? x) (Nothing) `(Just ,(/ 1 x))))

;traverse-reciprocal (given)
(define traverse-reciprocal
  (traverse Just bind-maybe reciprocal))

;halve
(define (halve x)
  (if (zero? (modulo x 2)) (inj-writer (/ x 2)) (bind-writer (tell x) (lambda (y) (inj-writer x)))))

;traverse-halve (given)
(define traverse-halve
  (traverse inj-writer bind-writer halve))

;state/sum
(define (state/sum x)
  (lambda (a)
    ((inj-state a) (+ a x))))

;traverse-state/sum
(define traverse-state/sum
  (traverse inj-state bind-state state/sum))

;TEST CASES

(define tests
  (test-suite "a12"
    (test-suite "findf-maybe"
      (test-equal-if-defined findf-maybe
        ((findf-maybe symbol? '(1 2 c)) (Just 'c))
        ((findf-maybe boolean? '(#f 1 2 c)) (Just #f))
	((findf-maybe number? '(a b c)) (Nothing))))
    (test-suite "partition-writer"
      (test-equal-if-defined partition-writer
                             ((run-writer (partition-writer odd? '(1 2 3 4 5 6 7 8 9 10)))
	 '((1 3 5 7 9) . (2 4 6 8 10)))
                             ((run-writer (partition-writer even? '(1 2 3 4 5 6 7 8 9 10)))
	 '((2 4 6 8 10) . (1 3 5 7 9)))))
    (test-suite "powerXpartials"
      (test-equal-if-defined powerXpartials
                             ((run-writer (powerXpartials 2 6)) '((2 4 8) . 64 ))
                             ((run-writer (powerXpartials 3 5)) '((3 9 81) . 243))
                             ((run-writer (powerXpartials 5 7)) '((5 25 125 15625) . 78125))))
    (test-suite "replace-with-count"
      (test-equal-if-defined replace-with-count
                             (((run-state (replace-with-count 'o '(a o (t o (e o t ((n . m) . o) . f) . t) . r))) 0)
	'(4 . (a 0 (t 1 (e 2 t ((n . m) . 3) . f) . t) . r)))
                             (((run-state (replace-with-count 'o '(((h (i s . o) . a) o s o e . n) . m))) 0)
'(3 ((h (i s . 0) . a) 1 s 2 e . n) . m))
                             (((run-state (replace-with-count 'o '(o (h (o s . o) . o) . o))) 1)
	'(6 . (1 (h (2 s . 3) . 4) . 5)))))
    (test-suite "reciprocal"
      (test-equal-if-defined reciprocal			       
        ((reciprocal 0) (Nothing))
	((reciprocal 2) (Just 1/2))
	((traverse-reciprocal '((1 . 2) . (3 . (4 . 5)))) 
	 (Just '((1 . 1/2) . (1/3 . (1/4 . 1/5)))))
	((traverse-reciprocal '((1 . 2) . (0 . (4 . 5))))
	 (Nothing))))
    (test-suite "halve"
      (test-equal-if-defined halve
                             ((run-writer (halve 6)) '(() . 3))
                             ((run-writer (halve 5)) '((5) . 5))
	((run-writer (traverse-halve '((1 . 2) . (3 . (4 . 5)))))
	 '((1 3 5) . ((1 . 1) . (3 . (2 . 5)))))))
    (test-suite "state/sum"
      (test-equal-if-defined state/sum
                             (((run-state (state/sum 5)) 0) '(5 . 0))
                             (((run-state (state/sum 2)) 0) '(2 . 0))
                             (((run-state (state/sum 2)) 3) '(5 . 3))
	(((run-state (traverse-state/sum '((1 . 2) . (3 . (4 . 5))))) 0)
	 '(15 . ((0 . 1) 3 6 . 10)))))
    (test-suite "value-of-cps"
      (test-equal-if-defined value-of-cps
                             (((run-cont (value-of-cps 
                                            '((lambda (f)
                                                ((f f) 5))
                                              (lambda (f)
                                                (lambda (n)
                                                  (if (zero? n)
                                                      1
                                                      (* n ((f f) (sub1 n)))))))
                                            (empty-env)))
          (lambda (v) v))
         '120) 
                             (((run-cont (value-of-cps 
                                            '(* 3 (capture q (* 2 (return q 4)))) 
                                            (empty-env)))
          (lambda (v) v))
         '12)))
    #;
    (test-suite "same-fringe"
      (test-equal-if-defined yield-cont
	((driver '(("Time" . "flies") . ("like" . ("an" . "arrow")))
		 '("time" . ("FLIES" . (("like" . "an") . "aRrOw")))) 
	 '((("time" . "FLIES") . ("like" . ("an" . "aRrOw")))
	   ("Time" . ("flies" . (("like" . "an") . "arrow")))))
	((driver '(("Time" . "flies") . ("like" . ("arrow" . "an")))
		 '("time" . ("FLIES" . (("like" . "an") . "aRrOw")))) 
	 '#f)))))
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