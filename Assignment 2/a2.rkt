;Ian Polito
;ipolito
;CSCI-C311
;Assignment 2

#lang racket
(require rackunit rackunit/text-ui racket/sandbox wxme)
(provide test-file)

(sandbox-path-permissions
 (cons
  (list 'read (current-directory))
  (cons
   (list 'exists (current-directory))
   (sandbox-path-permissions))))

(define test-file
  (lambda (#:file-name (file "./a2.rkt")
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

;Problem 1
(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
	   (cond
             [(zero? n) ls]
             [else (cdr (nth-cdr (- n 1)))])
           )))
      (car (nth-cdr n)))))
;test

;Problem 2
(define union
  (lambda (ls1 ls2)
    (cond
      [(empty? ls2) ls1]
      [else (cond
              [(contains? ls1 (car ls2)) (union ls1 (cdr ls2))]
              [else (cons (car ls2) (union ls1 (cdr ls2)))])])))
(define contains?
  (lambda (l num)
    (cond
      [(empty? l) #f]
      [(eqv? (car l) num) #t]
      [else (contains? (cdr l) num)])))
;test

;Problem 3
(define extend
  (lambda (x pred)
    (lambda (e)
      (or (eqv? e x)
          (pred e)))))
;test

;Problem 4
(define walk-symbol
  (lambda (x ls)
    (let ([slot (assv x ls)])
      (cond
        [(not slot) x]
        [(symbol? (cdr slot))
         (walk-symbol (cdr slot) ls)]
        [else (cdr slot)]))))
;test


;Problem 5
(define lambda->lumbda
  (lambda (exp)
    (match exp
      [`(lambda (,m) ,y)
       `(lumbda (,m) ,(lambda->lumbda y))]
      [`(,rator ,rand)
       `(,(lambda->lumbda rator)
         ,(lambda->lumbda rand))]
      [`,m (not (pair? m)) m])))
;test


;Problem 6
(define var-occurs?
  (lambda (varnm exp)
    (match exp
      [`(lambda (,m) ,y)
       (var-occurs? varnm y)]
      [`(,rator ,rand)
       (or (var-occurs? varnm rator)
           (var-occurs? varnm rand))]
      [`,m (not (pair? m))
           (eq? varnm m)])))
;test


;Problem 7
(define vars
  (lambda (exp)
    (match exp
      [`(lambda (,m) ,y)
       (vars y)]
      [`(,rator ,rand)
       (append (vars rator)
               (vars rand))]
      [`,m (not (pair? m)) `(,m)])))
;test


;Problem 8
(define unique-vars
  (lambda (exp)
    (match exp
      [`(lambda (,m) ,y)
       (unique-vars y)]
      [`(,rator ,rand)
       (union (unique-vars rator)
              (unique-vars rand))]
      [`,m (not (pair? m)) `(,m)])))
;test


;Problem 9
(define var-occurs-free?
  (lambda (sym exp)
    (match exp
      [`(lambda (,m) ,y)
       (if (eq? m sym)
           #f
           (var-occurs-free? sym y))]
      [`(,rator ,rand)
       (or (var-occurs-free? sym rator)
           (var-occurs-free? sym rand))]
      [`,m (not (pair? m))
           (eq? m sym)])))
;test


;Problem 10
(define var-occurs-bound?
  (lambda (sym exp)
    (match exp
      [`(,rator ,rand)
       (or (var-occurs-bound? sym rator)
           (var-occurs-bound? sym rand))]
      [`(lambda (,m) ,y)
       (cond
         [(memv sym (vars y))
          (cond
            ((eq? sym m) #t)
            (else (var-occurs-bound? sym y)))]
         (else #f))]
      [`,m (symbol? m) #f])))
;test


;Problem 11
(define unique-free-vars
  (lambda (exp)
    (match exp
      [`(,rator ,rand)
       (union (unique-free-vars rator)
              (unique-free-vars rand))]
      [`(lambda (,m) ,y)
       (remv m (unique-free-vars y))]
      [`,m (symbol? m) `(,m)])))
;test


;Problem 12
(define unique-bound-vars
  (lambda (exp)
    (match exp
      [`(,rator ,rand)
       (union (unique-bound-vars rator)
              (unique-bound-vars rand))]
      [`(lambda (,m) ,y)
       (if (memv m (unique-vars y))
           (cons m (unique-bound-vars y))
           (unique-bound-vars y))]
      [`,m (symbol? m) '()])))
;test


;Problem 13
(define lex
  (lambda (exp accum)
    (match exp
      [`(,rator ,rand)
       (list (lex rator accum)
             (lex rand accum))]
      [`(lambda (,m) ,body)
       `(lambda ,(lex body (cons m accum)))]
      [`,m (symbol? m)
           `(var ,(- (length accum)
                     (length (memv m accum))))])))

;TEST CASES

(define tests
(test-suite "A2:"
  (test-suite "list-ref" 
    (test-equal-if-defined list-ref
      ((list-ref '(a b c) 2) 'c)
      ((list-ref '(a b c) 0) 'a)))

  (test-suite "union" 
    (test-equal-if-defined union
      ((sort (union '() '()) (lambda args (apply string<=? (map symbol->string args)))) '())
      ((sort (union '(x) '()) (lambda args (apply string<=? (map symbol->string args)))) '(x))
      ((sort (union '(x) '(x)) (lambda args (apply string<=? (map symbol->string args)))) '(x))
      ((sort (union '(x y) '(x z)) (lambda args (apply string<=? (map symbol->string args)))) '(x y z))))

  (test-suite "extend" 
    (test-equal-if-defined extend
      (((extend 1 even?) 0) '#t)
      (((extend 1 even?) 1) '#t)
      (((extend 1 even?) 2) '#t)
      (((extend 1 even?) 3) '#f)
      ((filter (extend 1 even?) '(0 1 2 3 4 5)) '(0 1 2 4))
      ((filter (extend 3 (extend 1 even?)) '(0 1 2 3 4 5)) '(0 1 2 3 4))
      ((filter (extend 7 (extend 3 (extend 1 even?))) '(0 1 2 3 4 5)) '(0 1 2 3 4))))

  (test-suite "walk-symbol" 
    (test-equal-if-defined walk-symbol
      ((walk-symbol 'a '((a . 5))) '5)
      ((walk-symbol 'a '((b . c) (a . b))) 'c)
      ((walk-symbol 'a '((a . 5) (b . 6) (c . a))) '5)
      ((walk-symbol 'c '((a . 5) (b . (a . c)) (c . a))) '5)
      ((walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a))) '((c . a)))
      ((walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e))) '5)
      ((walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e))) 'f)))

  (test-suite "lambda->lumbda'" 
    (test-equal-if-defined lambda->lumbda
      ((lambda->lumbda 'x) 'x)
      ((lambda->lumbda '(lambda (x) x)) '(lumbda (x) x))
      ((lambda->lumbda '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a)))))) '(lumbda (z) ((lumbda (y) (a z)) (h (lumbda (x) (h a))))))
      ((lambda->lumbda '(lambda (lambda) lambda)) '(lumbda (lambda) lambda))
      ((lambda->lumbda '((lambda (lambda) lambda) (lambda (y) y))) '((lumbda (lambda) lambda) (lumbda (y) y)))
      ((lambda->lumbda '((lambda (x) x) (lambda (x) x))) '((lumbda (x) x) (lumbda (x) x)))))

  (test-suite "var-occurs?" 
    (test-equal-if-defined var-occurs?
      ((var-occurs? 'x 'x) '#t)
      ((var-occurs? 'x 'y) '#f)
      ((var-occurs? 'x '(lambda (x) y)) '#f)
      ((var-occurs? 'x '(lambda (y) x)) '#t)
      ((var-occurs? 'x '((z y) x)) '#t)
      ((var-occurs? 'y '(lambda (x) (x y))) '#t)
      ((var-occurs? 'y '((lambda (y) (x y)) (lambda (x) (x y)))) '#t)
      ((var-occurs? 'x '((lambda (x) (x x)) (x x))) '#t)))  
  
  (test-suite "vars" 
    (test-equal-if-defined vars
      ((sort (vars 'x) (lambda args (apply string<=? (map symbol->string args)))) '(x))
      ((sort (vars '(lambda (x) x)) (lambda args (apply string<=? (map symbol->string args)))) '(x))
      ((sort (vars '((lambda (y) (x x)) (x y))) (lambda args (apply string<=? (map symbol->string args)))) '(x x x y))
      ((sort (vars '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a)))))) (lambda args (apply string<=? (map symbol->string args)))) '(a a h h z))))

  (test-suite "unique-vars" 
    (test-equal-if-defined unique-vars
      ((sort (unique-vars '((lambda (y) (x x)) (x y))) (lambda args (apply string<=? (map symbol->string args)))) '(x y))
      ((sort (unique-vars '((lambda (z) (lambda (y) (z y))) x)) (lambda args (apply string<=? (map symbol->string args)))) '(x y z))
      ((sort (unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a)))) (lambda args (apply string<=? (map symbol->string args)))) '(a b c))))

  (test-suite "var-occurs-free?" 
    (test-equal-if-defined var-occurs-free?
      ((var-occurs-free? 'x 'x) '#t)
      ((var-occurs-free? 'x '(lambda (y) y)) '#f)
      ((var-occurs-free? 'x '(lambda (x) (x y))) '#f)
      ((var-occurs-free? 'y '(lambda (x) (x y))) '#t)
      ((var-occurs-free? 'x '(lambda (x) (lambda (x) x))) '#f)
      ((var-occurs-free? 'y '((lambda (y) (x y)) (lambda (x) (x y)))) '#t)
      ((var-occurs-free? 'x '((lambda (x) (x x)) (x x))) '#t)))

  (test-suite "var-occurs-bound?" 
    (test-equal-if-defined var-occurs-bound?
      ((var-occurs-bound? 'x 'x) '#f)
      ((var-occurs-bound? 'x '(lambda (x) x)) '#t)
      ((var-occurs-bound? 'y '(lambda (x) x)) '#f)
      ((var-occurs-bound? 'x '((lambda (x) (x x)) (x x))) '#t)
      ((var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z)))) '#f)
      ((var-occurs-bound? 'z '(lambda (y) (lambda (z) (y z)))) '#t)
      ((var-occurs-bound? 'x '(lambda (x) y)) '#f)
      ((var-occurs-bound? 'x '(lambda (x) (lambda (x) x))) '#t)))

  (test-suite "unique-free-vars" 
    (test-equal-if-defined unique-free-vars
      ((sort (unique-free-vars 'x) (lambda args (apply string<=? (map symbol->string args)))) '(x))
      ((sort (unique-free-vars '(lambda (x) (x y))) (lambda args (apply string<=? (map symbol->string args)))) '(y))
      ((sort (unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c))))))) (lambda args (apply string<=? (map symbol->string args)))) '(e x y))))

  (test-suite "unique-bound-vars" 
    (test-equal-if-defined unique-bound-vars
      ((sort (unique-bound-vars 'x) (lambda args (apply string<=? (map symbol->string args)))) '())
      ((sort (unique-bound-vars '(lambda (x) y)) (lambda args (apply string<=? (map symbol->string args)))) '())
      ((sort (unique-bound-vars '(lambda (x) (x y))) (lambda args (apply string<=? (map symbol->string args)))) '(x))
      ((sort (unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c))))))) (lambda args (apply string<=? (map symbol->string args)))) '(c x))
      ((sort (unique-bound-vars '(lambda (x) y)) (lambda args (apply string<=? (map symbol->string args)))) '())
      ((sort (unique-bound-vars '(lambda (x) (y z))) (lambda args (apply string<=? (map symbol->string args)))) '())
      ((sort (unique-bound-vars '(lambda (x) (lambda (x) x))) (lambda args (apply string<=? (map symbol->string args)))) '(x))))
  
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
                             (((((a b) c) d) e) a))))))))) '())
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
                                (((((a b) c) w) x) y))))))))))) '())
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
                                             ((((((var 8) (var 7)) (var 6)) (var 2)) (var 1)) (var 0)))))))))))))))

  (test-suite "walk-symbol-update" 
    (test-equal-if-defined walk-symbol-update
      ((let ((a-list `((c . ,(box 15)) (e . ,(box 'f)) (b . ,(box 'c)) (a . ,(box 'b)))))
         (walk-symbol-update 'a a-list))
       '15)))       

  (test-suite "var-occurs-both?" 
    (test-equal-if-defined var-occurs-both?
      ((call-with-values (lambda () (var-occurs-both? 'x '(lambda (x) (x (lambda (x) x))))) cons) '(#f . #t)) 
      ((call-with-values (lambda () (var-occurs-both? 'x '(x (lambda (x) x)))) cons) '(#t . #t))
      ((call-with-values (lambda () (var-occurs-both? 'x '(lambda (y) (x (lambda (x) x))))) cons) '(#t . #t))
      ((call-with-values (lambda () (var-occurs-both? 'x '(lambda (x) (lambda (x) (x (lambda (x) x)))))) cons) '(#f . #t))
      ((call-with-values (lambda () (var-occurs-both? 'x '(lambda (x) (lambda (y) (lambda (x) (x (lambda (x) x))))))) cons) '(#f . #t))
      ((call-with-values (lambda () (var-occurs-both? 'x '(lambda (y) (lambda (x) (lambda (z) (lambda (x) (x (lambda (x) x)))))))) cons) '(#f . #t))))))
    
  

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
