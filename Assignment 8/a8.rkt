;Ian Polito
;ipolito
;CSCI-C311
;Assignment 8

#lang racket
(require rackunit rackunit/text-ui racket/sandbox wxme)
(provide test-file)

(define m* 0)
(define n* 0)
(define v* 0)
(define k* 0)
(define ls* 0)

;;empty-k
(define empty-k
  (lambda ()
    (lambda (v) v)))

;;empty-k-reg
(define empty-k-reg
  (lambda ()
`(empty-k-reg)))

(sandbox-path-permissions
 (cons
  (list 'read (current-directory))
  (cons
   (list 'exists (current-directory))
   (sandbox-path-permissions))))

(define test-file
  (lambda (#:file-name (file "./a8.rkt")
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

;ack
(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack (sub1 m) 1 k)]
      [else (ack m (sub1 n) (lambda (v) (ack (sub1 m) v k)))])))

(define ack-reg-driver
  (lambda (m n)
    (begin
     (set! m* m)
     (set! n* n)
     (set! k* (empty-k-reg))
     (ack-reg))))

(define ack-reg
  (lambda ()
    (cond
      [(zero? m*) (begin
                    (set! k* k*)
                    (set! v* (add1 n*))
                    (apply-k-ack))]
      [(zero? n*) (begin
                    (set! m* (sub1 m*))
                    (set! n* 1)
                    (ack-reg))]
      [else (begin
              (set! n* (sub1 n*))
              (set! k* (else-k-reg m* k*))
              (ack-reg))])))

(define apply-k-ack
  (lambda ()
    (match k*
      [`(else-k-reg ,m ,k)
       (begin
         (set! m* (sub1 m))
         (set! k* k)
         (set! n* v*)
         (ack-reg))]
      [`(empty-k-reg) v*]
      [else (k* v*)])))

(define else-k-reg
  (lambda (m k)
`(else-k-reg ,m ,k)))

;depth
(define depth
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(pair? (car ls))
       (depth (car ls)
	      (lambda (l)
		(depth (cdr ls)
		       (lambda (r)
			 (let ((l (add1 l)))
			   (if (< l r) (k r) (k l)))))))]
      [else (depth (cdr ls) k)])))

(define depth-reg-driver
  (lambda (ls)
    (begin
      (set! ls* ls)
      (set! k* (empty-k-reg))
      (depth-reg))))

(define depth-reg
  (lambda ()
    (cond
      [(null? ls*) (begin
                     (set! k* k*)
                     (set! v* 1)
                     (apply-k-depth-reg))]
      [(pair? (car ls*)) (begin
                           (set! ls* (car ls*))
                           (set! k* (depth-pair-1-k-reg ls* k*))
                           (depth-reg))]
      [else (begin
              (set! ls* (cdr ls*))
              (set! k* k*)
              (depth-reg))])))

(define apply-k-depth-reg
  (lambda ()
    (match k*
      [`(empty-k-reg)
       v*]
      [`(pair-1 ,ls ,k)
       (begin
         (set! ls* (cdr ls))
         (set! k* (depth-pair-2-k-reg v* k))
         (depth-reg))]
      [`(pair-2 ,l ,k)
       (begin
         (set! l (add1 l))
         (if (< l v*)
             (begin
               (set! k* k)
               (set! v* v*)
               (apply-k-depth-reg))
             (begin
               (set! k* k)
               (set! v* l)
               (apply-k-depth-reg))))]
      [else
       (k* v*)])))

(define depth-pair-1-k-reg
  (lambda (ls k)
`(pair-1 ,ls ,k)))

(define depth-pair-2-k-reg
  (lambda (l k)
`(pair-2 ,l ,k)))

;fact

(define fact
  (lambda (n k)
    ((lambda (fact k)
       (fact fact n k))
     (lambda (fact n k)
       (cond
         [(zero? n) (k 1)]
         [else (fact fact (sub1 n) (lambda (v) (k (* n v))))]))
     k)))

(define fact-reg-driver
  (lambda (n)
    (begin
      (set! n* n)
      (set! k* (empty-k-reg))
      (fact-reg))))

(define fact-reg
  (lambda ()
    ((lambda (fact-cps-ds)
       (fact-cps-ds fact-cps-ds))
     (lambda (fact-cps)
       (cond
         [(zero? n*)
          (begin
            (set! v* 1)
            (apply-k-fact))]
         [else
          (begin
            (set! k* (else-k-fact-reg n* k*))
            (set! n* (sub1 n*))
            (fact-reg))])))))

(define apply-k-fact
  (lambda ()
    (match k*
      [`(empty-k-reg)
       v*]
      [`(else-k-fact-reg ,n ,k)
       (begin
         (set! v* (* v* n))
         (set! k* k)
         (apply-k-fact))]
      [else (k* v*)])))

(define else-k-fact-reg
  (lambda (n k)
`(else-k-fact-reg ,n ,k)))

;pascal
(define pascal
  (lambda (n k)
    (let ((pascal
           (lambda (pascal k)
             (k (lambda (m a k)
		  (cond
		    [(> m n) (k '())]
		    [else (let ((a (+ a m)))
			    (pascal pascal (lambda (f) (f (add1 m) a (lambda (v) (k (cons a v)))))))]))))))
      (pascal pascal (lambda (f) (f 1 0 k))))))

(define pascal-reg-driver
  (lambda (n)
    (begin
      (set! n* 0)
      (set! k* (empty-k-reg))
(pascal-reg))))

(define pascal-reg
  (lambda ()
    (let ((pascal
           (lambda (pascal-cps k) 
              (k* (lambda (m* a* k*)
		  (cond
		    [(> m* n*) (begin
                                 (set! v* (apply-k-pascal k* '()))
                                 (pascal-reg))]
		    [else (begin
                           (set! a* (+ a* m*))
                           (set! k* (else-k-pascal a* k*))
                           (set! m* (+ 1 m*))
                           (pascal-reg))]))))))
      (pascal pascal (lambda (f) (f 1 0 k*))))))

(define apply-k-pascal
  (lambda ()
    (match k*
      [`(empty-k-reg)
       v*]
      [`(else-k-pascal ,x ,k)
       (begin (set! v* (cons x v*))
              (set! k* k)
              )]
      [else (k* v*)])))

(define else-k-pascal
  (lambda (x k)
`(else-k-pascal ,x ,k)))

;TEST CASES

(define tests
  (test-suite "a8"
    (test-suite "ack-reg-driver"
      (test-equal-if-defined ack-reg-driver
        ((ack-reg-driver 2 2) 7)))
    (test-suite "depth-reg-driver"
      (test-equal-if-defined depth-reg-driver
        ((depth-reg-driver '(1 (2 (3 (4))))) 4)))
    (test-suite "fact-reg-driver"
      (test-equal-if-defined fact-reg-driver
        ((fact-reg-driver 5) 120)))
    (test-suite "pascal-reg-driver"
      (test-equal-if-defined pascal-reg-driver
        ((pascal-reg-driver 10) '(1 3 6 10 15 21 28 36 45 55))))
    (test-suite "fib-ramp-driver"
      (test-equal-if-defined fib-ramp-driver
        ((fib-ramp-driver 6 -1 -1) 8)
        ((fib-ramp-driver -1 6 -1) 8)
        ((fib-ramp-driver -1 -1 6) 8)))
    (test-suite "bi-tramp-driver"
      (test-equal-if-defined bi-tramp-driver
        ((bi-tramp-driver 3 4) '(3 5))
	((bi-tramp-driver 4 3) '(3 5))
	((bi-tramp-driver 6 6) '(17 17))))))


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
