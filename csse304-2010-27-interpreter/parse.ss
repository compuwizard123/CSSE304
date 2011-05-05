; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp
  (lambda (datum)
    (cond
     [(or (symbol? datum)(procedure? datum))  (var-exp datum)]
     [(or (string? datum)(null? datum) (boolean? datum) (vector? datum) (number? datum) (eqv? (car datum) 'quote))
      (lit-exp datum)]
     [(improper? datum) ; perhaps means that improper-list-exp is not needed. 
      (eopl:error 'parse-exp
		  "expression is not a proper list.~s" datum)]
     [(pair? datum)
      (cond [(eqv? (car datum) 'lambda) ; lambda case
	     (cond [(< (length datum) 3) 
		    (eopl:error 'parse-exp
				"lambda expression ~s is missing some part" datum)]
		   [(not (or (symbol? (cadr datum)) ((my-list-of (lambda (x) (or (symbol? x) (and (pair? x) (eqv? (car x) 'ref))))) (cadr datum))))
		    (eopl:error 'parse-exp
				"lambda bindings must be symbols lists of ref assignments. ~s" datum)]
		   [else (lambda-exp (parse-lambda-vars (cadr datum))
				     (map parse-exp (cddr datum)))])]
	    
					; deals with if case
	    [(eqv? (car datum) 'if)
	     (cond [(or (> (length datum) 4) (null? (cdr datum)) (null? (cddr datum)))
		    (eopl:error 'parse-exp
				"Too many (or too few) arguments in if-exp ~s" datum)]
		   [(= (length datum) 3) (if-then-exp (parse-exp (cadr datum))
						      (parse-exp (caddr datum)))]
		   [else (if-exp (parse-exp (cadr datum))
				 (parse-exp (caddr datum))
				 (parse-exp (cadddr datum)))])]
			 	    
	    ; deals with the let, let*, and letrec cases
	    [(or (eqv? (car datum) 'let) (eqv? (car datum) 'let*) (eqv? (car datum) 'letrec))
	     (let* ([len (length datum)] [proc (if (< len 3) "don't care" (if  (symbol? (cadr datum)) caddr cadr))])
	       (cond [(< len 3) 
		      (eopl:error 'parse-exp 
				  "let statement ~s is missing some important part. Too few elements." datum)]
		     [(not (list? (proc datum))) 
		      (eopl:error 'parse-exp 
				  "let statement definition must be a list. ~s" datum)]
		     [(improper? (proc datum))
		      (eopl:error 'parse-exp
				  "assignment cannot be an improper list. ~s" datum)]		    
		     [(not (andmap list? (proc datum)))
		      (eopl:error 'parse-exp
				  "every element in the assignment list must be a proper list. ~s" datum)]
		     [(and (> (length (proc datum)) 0) (andmap improper? (proc datum)))
		      (eopl:error 'parse-exp
				  "assignments cannot contain improper lists. ~s" datum)]
		     [(not (valid-assignments? (proc datum))) 
		      (eopl:error 'parse-exp
				  "let statement cannot assign values to non-symbols. Error in: ~s" datum)]
		     [(not (andmap pair? (proc datum))) 
		      (eopl:error 'parse-exp 
				  "let statement definition must have lists inside. ~s" datum)]
		     [(not (andmap (lambda (l) (= (length l) 2)) (proc datum)))
		      (eopl:error 'parse-exp 
				  "let statement ~s has incorrect number of arguments in assignment section" datum)]
		     [(eqv? (car datum) 'let) ; finally we're ready to parse. Is it let?
		      (cond [(symbol? (cadr datum))  (let-named-exp (cadr datum) ;name
								    (map car (caddr datum)) ;syms
								    (map parse-exp (map cadr (caddr datum))) ;exps
								    (map parse-exp (cdddr datum)))] ; bodies
			    [else (let-exp (map car (cadr datum)) ;syms
					   (map parse-exp (map cadr (cadr datum))) ; exps
					   (map parse-exp (cddr datum)))])] ; bodies 
		     [(eqv? (car datum) 'let*) ; or let*?
		      (let*-exp (map car (cadr datum)) ;syms
				(map parse-exp (map cadr (cadr datum))) ; exps
				(map parse-exp (cddr datum)))]
		     [(eqv? (car datum) 'letrec) ; or letrec? 
		      (letrec-exp (map car (cadr datum)) ;proc-names
				  (map cadadr (cadr datum)) ;idss
				  (map (lambda (x) (map parse-exp x)) (map cddadr (cadr datum))) ;bodiess
				  (map parse-exp (cddr datum)))] ; letrec bodies
		     [else (eopl:error 'parse-exp 
				       "Something has gone wrong with the let case.")]
		     ))] 

    	    ; deals with set!, set-car! and set-cdr!
	    [(or (eqv? (car datum) 'set!) (eqv? (car datum) 'set-car!) (eqv? (car datum) 'set-cdr!))
	     (cond [(not (equal? (length datum) 3)) 
		    (eopl:error 'parse-exp
				"Incorrect number of arguments to set! procedure: ~s." datum)]
		   [(and (eqv? (car datum) 'set!) (not (symbol? (cadr datum)))) 
		    (eopl:error 'parse-exp
				"Variable to be set must be a symbol. ~s" datum)]
		   [(or (eqv? (car datum) 'set-car!) (eqv? (car datum) 'set-cdr!))
		    (let ([var (cadr datum)])
		      (if (or (pair? var) (null? var) (symbol? var))
			  (set!-list-exp (parse-exp (car datum)) (parse-exp var) (parse-exp (caddr datum)))
			  (eopl:error 'parse-exp
				      "Wrong variable type for ~s. ~s" (car datum) datum)))]
		   [else (set!-exp (cadr datum) (parse-exp (caddr datum)))])]
	    [(eqv? (car datum) 'and)
	     (and-exp (map parse-exp (cdr datum)))]
	    [(eqv? (car datum) 'or)
	     (or-exp (map parse-exp (cdr datum)))]
	    [(eqv? (car datum) 'while)
	     (while-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum)))]
	    [(eqv? (car datum) 'begin)
	     (begin-exp (map parse-exp (cdr datum)))]
	    [(eqv? (car datum) 'cond)
	     (cond-exp (map parse-exp (map car (cdr datum))) (map parse-exp (map (lambda (x) (if (null? (cdr x)) '() (cadr x))) (cdr datum))))]
	    [(eqv? (car datum) 'case)
	     (case-exp (parse-exp (cadr datum)) (map (lambda (x) (if (eqv? 'else (car x)) (list (parse-exp (car x))) (map parse-exp (car x)))) (cddr datum)) (map parse-exp (map cadr (cddr datum))))]
	    [(eqv? (car datum) 'load)
	     (cond
	      [(or (null? (cdr datum)) (not (null? (cddr datum))))
	       (eopl:error 'parse-exp
			   "Wrong number of arguments for ~s. ~s" (car datum) datum)]
	      [(string? (cadr datum))
	       (load-exp (cadr datum))]
	      [else
	       (eopl:error 'parse-exp
			   "Invalid ~s." datum)])]
	    [(eqv? (car datum) 'define)
	     (cond
	      [(or (null? (cdr datum)) (null? (cddr datum)) (not (null? (cdddr datum))))
	       (eopl:error 'parse-exp
			   "Wrong number of arguments for ~s. ~s" (car datum) datum)]
	      [(symbol? (cadr datum))
	       (define-exp (cadr datum) (parse-exp (caddr datum)))]
	      [else
	       (eopl:error 'parse-exp
			   "Invalid ~s." datum)])]
	    ; improper check. Might be a problem
	    [(improper? datum)
	     (improper-list-exp (map-improper parse-exp datum))]
	    
	    ;it must be an application procedure
	    [else  
	     (app-exp (parse-exp (car datum))
		      (map parse-exp (cdr datum)))])]
     [else (eopl:error 'parse-exp
		       "Invalid concrete syntax ~s" datum)])))

(define parse-lambda-vars
	(lambda (ls)
		(cond
			[(null? ls)
				'()]
			[(list? ls)
				(map (lambda (x) (if (and (pair? x) (eqv? 'ref (car x))) (ref-var (cadr x)) (var x))) ls)]
			[else
				ls])))
		
(define unparse-exp ; an inverse for parse-exp
  (lambda (exp)
    (cases expression exp
	   [var-exp (id) id]
	   [lambda-exp (id bodies) 
		       (append (cons 'lambda (list id)) (map unparse-exp bodies))]
	   [if-exp (comp first second)
		   (list 'if (unparse-exp comp) 
			 (unparse-exp first)
			 (unparse-exp second))]
	   [if-then-exp (comp first)
			(list 'if (unparse-exp comp)
			      (unparse-exp first))]
	   [lit-exp (exp) exp]
	   [set!-exp (var val) (list 'set! var (unparse-exp val))]
	   [set!-list-exp (type var val) (list (unparse-exp type) (unparse-exp var) (unparse-exp val))]
	   [let-exp (syms exps bodies)
		    (append (list 'let (pair-merge syms (map unparse-exp exps)))  (map unparse-exp bodies))]
	   [let-named-exp (name syms exps bodies)
		    (append (list 'let name (pair-merge syms (map unparse-exp exps))) (map unparse-exp bodies))]
	   [let*-exp (syms exps bodies)
		    (append (list 'let* (pair-merge syms (map unparse-exp exps))) (map unparse-exp bodies))]
	   [letrec-exp (syms exps bodies)
		    (append (list 'letrec (pair-merge syms (map unparse-exp exps))) (map unparse-exp bodies))]
	   [empty (null)
		  '()]
	   [app-exp (rator rand)
		    (cons (unparse-exp rator) (map unparse-exp rand))]
	   [improper-list-exp (body)
			      (improperize (map unparse-exp body))]
	   ;[list-exp ]
	   [and-exp (tests)
		    (cons 'and tests)]
	   [or-exp (tests)
		   (cons 'or tests)]
	   [while-exp (test bodies)
		      (append (list 'while test) bodies)]
	   [begin-exp (bodies)
		      (cons 'begin bodies)]
	   [cond-exp (tests results)    ; INCORRECT!
		     (list tests results)]
	   [case-exp (item tests results)
		     "NOT YET IMPLEMENTED"]
	    
	    )))
	  

; takes two lists and merges them into pairs
; ex: (a b c) (1 2 3) --> ((a 1) (b 2) (c 3))
(define pair-merge
  (lambda (a b)
    (if (null? a)
	'()
	(cons (list (car a) (car b)) (pair-merge (cdr a) (cdr b))))))
    


; same as list-of, but allows improper lists.
(define my-list-of
  (lambda (pred?)
    (lambda (l)
      (or ((list-of pred?) l)
	  ((list-of pred?) (properize l))))))

; used in my-list-of
(define properize
  (lambda (l)
    (if (not (pair? l))
	(list l)
	(cons (car l) (properize (cdr l))))))

; used in parse-exp
(define map-improper
  (lambda (proc l)
    (if (not (pair? l))
	(proc l)
	(cons (proc (car l)) (map-improper proc (cdr l))))))

; used in parse-exp
(define improper?
  (lambda (l)
    (cond [(null? l) #f]
	  [(null? (cdr l)) #f]
	  [(not (pair? (cdr l))) #t]
	  [else (improper? (cdr l))])))

; improperize
(define improperize
  (lambda (l)
    (if (null? (cdr l))
	(car l)
	(cons (car l) (improperize (cdr l))))))

; takes a list of pairs and returns true if the car of each pair is a symbol
(define valid-assignments?
  (lambda (p)
    (andmap (lambda (a) (symbol? (car a)))  p)))