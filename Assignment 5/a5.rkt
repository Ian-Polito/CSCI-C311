;Ian Polito
;ipolito
;CSCI-C311
;Assignment 5

#lang racket
(require rackunit rackunit/text-ui racket/sandbox wxme)
(provide test-file)

(define test-file
  (lambda (#:file-name (file "./a5.rkt")
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
;given value-of
(define value-of
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (value-of n env))]
      [`(sub1 ,n) (sub1 (value-of n env))]
      [`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                  (value-of conseq env)
                                  (value-of alt env))]
      [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
      [`(random ,n) (random (value-of n env))]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(lambda (,x) ,body) (make-closure x body env)]
      [`(,rator ,rand) (apply-closure (value-of rator env)
                                      (value-of rand env))])))

;apply-clouse
(define apply-closure
  (lambda (n1 n2)
    (n1 n2)))
;make-closure
(define make-closure
  (lambda (y body env)
    (lambda (x)
      (value-of body (extend-env y x env)))))
;apply-env
(define apply-env
  (lambda (env n)
    (match env
      (`(empty-env) `empty-env)
      (`(extend-env ,x ,y ,env) (if (eqv? x n) y (apply-env env n))))))
;extend-env
(define extend-env
  (lambda (x y env)
    `(extend-env ,x ,y ,env)))

;val-of-cbv
(define val-of-cbv
  (lambda (exp env) 
    (match exp
      [`,y #:when (symbol? y)
           (unbox (apply-env env y))]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(zero? ,n)
       (= (val-of-cbv n env) 0)]
      [`(sub1 ,x)
       (- (val-of-cbv x env) 1)]
      [`(* ,x ,y)
       (* (val-of-cbv x env)
          (val-of-cbv y env))]
      [`(lambda (,x) ,body)
       (closure-cbv x body env)]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (val-of-cbv condi env)
           (val-of-cbv whentrue env)
           (val-of-cbv whenfalse env))]
      ;;random
      [`(random ,n)
       (random (val-of-cbv n env))]
      ;;begin
      [`(begin2 ,e1 ,e2)
       (begin (val-of-cbv e1 env)
              (val-of-cbv e2 env))]
      ;;set!
      [`(set! ,x ,rhs)
       (set-box! (apply-env env x)
                 (val-of-cbv rhs env))]
      ;;rand -> symbol
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbv rator env)
                                      (box (apply-env env rand)))]
      ;;normal rator, rand
      [`(,rator ,rand)
       (apply-closure (val-of-cbv rator env)
                      (box (val-of-cbv rand env)))])))

;closure for cbv
(define closure-cbv
  (lambda (y body env)
    (lambda (c)
      (val-of-cbv body (extend-env y c env)))))

;val-of-cbr
(define val-of-cbr
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (unbox (apply-env env y))]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(zero? ,n)
       (= (val-of-cbr n env) 0)]
      [`(sub1 ,x)
       (- (val-of-cbr x env) 1)]
      [`(* ,x ,y)
       (* (val-of-cbr x env)
          (val-of-cbr y env))]
      [`(lambda (,x) ,body)
       (closure-cbr x body env)]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (val-of-cbr condi env)
           (val-of-cbr whentrue env)
           (val-of-cbr whenfalse env))]
      [`(random ,n)
       (random (val-of-cbr n env))]
      [`(begin2 ,e1 ,e2)
       (begin (val-of-cbr e1 env)
              (val-of-cbr e2 env))]
      [`(set! ,x ,rhs)
       (set-box! (apply-env env x)
                 (val-of-cbr rhs env))]
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbr rator env)
                                      (apply-env env rand))]
      [`(,rator ,rand)
       (apply-closure (val-of-cbr rator env)
                      (box (val-of-cbr rand env)))])))
;closure for cbr
(define closure-cbr
  (lambda (y body env)
    (lambda (c)
      (val-of-cbr body (extend-env y c env)))))

;val-of-cbname
(define val-of-cbname
  (lambda (exp env)
    (match exp
      ;;Call-by-Name
      [`,y #:when (symbol? y)
           ((unbox (apply-env env y)))]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(zero? ,n)
       (= (val-of-cbname n env) 0)]
      [`(sub1 ,x)
       (- (val-of-cbname x env) 1)]
      [`(* ,x ,y)
       (* (val-of-cbname x env)
          (val-of-cbname y env))]
      [`(lambda (,x) ,body)
       (closure-cbname x body env)]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (val-of-cbname condi env)
           (val-of-cbname whentrue env)
           (val-of-cbname whenfalse env))]
      ;;random
      [`(random ,n)
       (random (val-of-cbname n env))]
      ;;begin
      [`(begin2 ,e1 ,e2)
       (begin (val-of-cbname e1 env)
              (val-of-cbname e2 env))]
      ;;set!
      [`(set! ,x ,rhs)
       (set-box! (val-of-cbname env x)
                 (val-of-cbname rhs env))]
      ;;rand -> symbol
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbname rator env)
                                      (apply-env env rand))]
      ;;normal rator, rand
      [`(,rator ,rand)
       (apply-closure (val-of-cbname rator env)
                      (box (lambda () (val-of-cbname rand env))))])))

;closure for cbname
(define closure-cbname
  (lambda (y body env)
    (lambda (c)
      (val-of-cbname body (extend-env y c env)))))

;val-of-cbneed
(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (let ([b (apply-env env y)])
             (let ([val ((unbox b))])
               (set-box! b (lambda () val))
               val))]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(zero? ,n)
       (= (val-of-cbneed n env) 0)]
      [`(sub1 ,x)
       (- (val-of-cbneed x env) 1)]
      [`(* ,x ,y)
       (* (val-of-cbneed x env)
          (val-of-cbneed y env))]
      [`(lambda (,x) ,body)
       (closure-cbneed x body env)]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (val-of-cbneed condi env)
           (val-of-cbneed whentrue env)
           (val-of-cbneed whenfalse env))]
      ;;random
      [`(random ,n)
       (random (val-of-cbneed n env))]
      ;;begin
      [`(begin2 ,e1 ,e2)
       (begin (val-of-cbneed e1 env)
              (val-of-cbneed e2 env))]
      ;;set!
      [`(set! ,x ,rhs)
       (set-box! (val-of-cbneed env x)
                 (val-of-cbneed rhs env))]
      ;;rand -> symbol
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbneed rator env)
                                      (apply-env env rand))]
      ;;normal rator, rand
      [`(,rator ,rand)
       (apply-closure (val-of-cbneed rator env)
                      (box (lambda () (val-of-cbneed rand env))))])))

;closure for cbneed
(define closure-cbneed
  (lambda (y body env)
    (lambda (c)
      (val-of-cbneed body (extend-env y c env)))))

;TEST CASES

(define tests
  (test-suite "a5"
    (test-suite "cbr"
      (test-equal-if-defined val-of-cbr	      
        ;; Making sure set! works
        ((val-of-cbr
	  '((lambda (x) (begin2 (set! x #t)
				(if x 3 5))) #f)
	  (empty-env))
	 3)
	
	;; Returns 4 under CBR...
	((val-of-cbr
	  '((lambda (a)
	      ((lambda (p)
		 (begin2
		  (p a)
		  a)) (lambda (x) (set! x 4)))) 3)
	  (empty-env))
	 4)
	
	;; returns 44 under CBR...
	((val-of-cbr
	  '((lambda (f)
	      ((lambda (g)
		 ((lambda (z) (begin2
			       (g z)
			       z))
		  55))
	       (lambda (y) (f y)))) (lambda (x) (set! x 44)))
	  (empty-env))
	 44)

	;; Returns 44 under CBR...
	((val-of-cbr
	  '((lambda (swap)
	      ((lambda (a)
		 ((lambda (b)
		    (begin2
		     ((swap a) b)
		     a)) 44)) 33))
	    (lambda (x)
	      (lambda (y)
		((lambda (temp)
		   (begin2
		    (set! x y)
		    (set! y temp))) x))))
	  (empty-env))
	 44)
	))
    (test-suite "cbv"
      (test-equal-if-defined val-of-cbv
        ;; ...but returns 3 under CBV.
        ((val-of-cbv
	  '((lambda (a)
	      ((lambda (p)
		 (begin2
		  (p a)
		  a)) (lambda (x) (set! x 4)))) 3)
	  (empty-env))
	 3)
	
	;; ...but returns 55 under CBV!  You can change the "begin2" to
	;; "begin" and evaluate this in the Scheme REPL as evidence that
	;; Scheme uses CBV.
	((val-of-cbv
	  '((lambda (f)
	      ((lambda (g)
		 ((lambda (z) (begin2
			       (g z)
			       z))
		  55))
	       (lambda (y) (f y)))) (lambda (x) (set! x 44)))
	  (empty-env))
	 55)
	
	;; ...but returns 33 under CBV.
	((val-of-cbv
	  '((lambda (swap)
	      ((lambda (a)
		 ((lambda (b)
		    (begin2
		     ((swap a) b)
		     a)) 44)) 33))
	    (lambda (x)
	      (lambda (y)
		((lambda (temp)
		   (begin2
		    (set! x y)
		    (set! y temp))) x))))
	  (empty-env))
	 33)))
    
    (test-suite "call-by-name"
      (test-equal-if-defined val-of-cbname
	;;P(false positive) <= .01                                            
	((let ((random-sieve 
		'((lambda (n)
		    (if (zero? n)
			(if (zero? n) 
			    (if (zero? n) 
				(if (zero? n) 
				    (if (zero? n) 
					(if (zero? n) 
					    (if (zero? n) #t 
						#f) 
					    #f) 
					#f) 
				    #f) 
				#f) 
			    #f)
			(if (zero? n) #f 
			    (if (zero? n) #f 
				(if (zero? n) #f 
				    (if (zero? n) #f 
					(if (zero? n) #f 
					    (if (zero? n) #f 
						#t))))))))
		  (random 2))))
	   (val-of-cbname random-sieve (empty-env)))
	 #f)

	;; Does not terminate with val-of-cbr or val-of-cbv -- try it!
	((val-of-cbname
	  '((lambda (z) 100)
	    ((lambda (x) (x x)) (lambda (x) (x x))))
	  (empty-env))
	 100)))

    (test-suite "cbneed"
      (test-equal-if-defined val-of-cbneed
        ;; call-by-need                 
        ((let ((random-sieve 
		'((lambda (n)
		    (if (zero? n)
			(if (zero? n) 
			    (if (zero? n) 
				(if (zero? n) 
				    (if (zero? n) 
					(if (zero? n) 
					    (if (zero? n) #t 
						#f) 
					    #f) 
					#f) 
				    #f) 
				#f) 
			    #f)
			(if (zero? n) #f 
			    (if (zero? n) #f 
				(if (zero? n) #f 
				    (if (zero? n) #f 
					(if (zero? n) #f 
					    (if (zero? n) #f 
						#t))))))))
		  (random 2))))
	   (val-of-cbneed random-sieve (empty-env)))
	 #t)))

      (test-suite "cons-should-not-evaluate"
	(test-equal-if-defined val-of-cbv
	  ((let ((cons-test
		  '(let ((fix 
			  (lambda (f)
			    ((lambda (x) (f (lambda (v) ((x x) v))))
			     (lambda (x) (f (lambda (v) ((x x) v))))))))
		     (let ((map 
			    (fix 
			     (lambda (map)
			       (lambda (f)
				 (lambda (l)
				   (if (null? l)
				       '()
				       (cons^ (f (car^ l))
					      ((map f) (cdr^ l))))))))))
		       (let ((take 
			      (fix 
			       (lambda (take)
				 (lambda (l)
				   (lambda (n)
				     (if (zero? n)
					 '()
					 (cons (car^ l) 
					       ((take (cdr^ l)) (sub1 n))))))))))
			 ((take 
			   ((fix 
			     (lambda (m)
			       (lambda (i)
				 (cons^ 1 
					((map (lambda (x) 
						(add1 x))) 
					 (m i)))))) 
			    0)) 
			  5))))))
	     (val-of-cbv cons-test (empty-env)))
	   '(1 2 3 4 5))))))


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