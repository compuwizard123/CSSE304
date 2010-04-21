; Kevin Risde
; Assignment 10
;
; #1
;(diff-tree? obj) Is this object a diff-tree?
(define diff-tree?
	(lambda (diff)
		(cond
			[(or (null? diff) (not (pair? diff)))
				#f]
			[(eqv? (car diff) 'one)
				#t]
			[(eq? (length diff) 3)
				(andmap (lambda (x) (diff-tree? x)) (cdr diff))]
			[else
				#f])))
;(diff-tree-negate dt) Produces a diff-tree that represents the negative of the integer represented by dt.
(define diff-tree-negate
	(lambda (dt)
		(list 'diff '(diff (one) (one)) dt)))
;(diff-tree-plus dt1 dt2) Produces a diff-tree that represents the sum of the integers represented by dt1 and dt2.
(define diff-tree-plus
	(lambda (dt1 dt2)
		(list 'diff dt1 (list 'diff '(diff (one) (one)) dt2))))
;(diff-tree-minus dt1 dt2)
(define diff-tree-minus
	(lambda (dt1 dt2)
		(list 'diff dt1 dt2)))
;(diff-tree-equal? dt1 dt2)
(define diff-tree->integer
	(lambda (dt)
		(if (eqv? (car dt) 'one)
			1
			(- (diff-tree->integer (cadr dt)) (diff-tree->integer (caddr dt))))))
(define diff-tree-equal?
	(lambda (dt1 dt2)
		(eq? (diff-tree->integer dt1) (diff-tree->integer dt2))))
; #2
(load "chez-init.ss") ; remove this isf using Dr. Scheme EoPL language
(define-datatype assignment-expression assignment-expression?
	[assignment-exp
		(var symbol?)
		(val expression?)])
(define-datatype expression expression?
	[var-exp
		(id symbol?)]
	[lit-exp
		(literal (lambda (x) #t))]
	[lambda-exp
		(id (lambda (x) (or (symbol? x) (list-of var-exp))))
		(body (list-of expression?))]
	[if-exp
		(condition expression?)
		(result (list-of expression?))]
	[set!-exp
		(id symbol?)
		(val expression?)]
	[let-exp
		(vals (list-of assignment-expression?))
		(body (list-of expression?))]
	[named-let-exp
		(name symbol?)
		(vals (list-of assignment-expression?))
		(body (list-of expression?))]
	[let*-exp
		(vals (list-of assignment-expression?))
		(body (list-of expression?))]
	[letrec-exp
		(vals (list-of assignment-expression?))
		(body (list-of expression?))]
	[app-exp
		(rator expression?)
		(rand (list-of expression?))])

(define parse-exp
	(lambda (datum)
		(cond
			[(symbol? datum)
				(var-exp datum)]
			[(or (number? datum) (string? datum) (boolean? datum) (vector? datum) (and (pair? datum) (or (not (list? datum)) (number? (car datum)) (vector? (car datum)) (string? (car datum)) (boolean? (car datum)))) (null? datum))
				(lit-exp datum)]
			[(pair? datum)
				(let ([first (car datum)])
					(cond
						[(eqv? first 'lambda)
							(cond
								[(null? (cdr datum))
									(eopl:error 'parse-exp "Invalid lambda syntax ~s\n~s" datum "No parameter list or body/bodies")]
								[(null? (cddr datum))
									(eopl:error 'parse-exp "Invalid lambda syntax ~s\n~s" datum "No body/bodies")]
								[(and (list? (cadr datum)) (not (andmap symbol? (cadr datum))))
									(eopl:error 'parse-exp "Invalid lambda syntax ~s\n~s" datum "Invalid parameter list")]
								[(not (or (symbol? (cadr datum)) (pair? (cadr datum)) (null? (cadr datum))))
									(eopl:error 'parse-exp "Invalid lambda syntax ~s\n~s" datum "Invalid parameter list")]
								[else
									(lambda-exp (cadr datum) (map parse-exp (cddr datum)))])]
						[(eqv? first 'if)
							(cond
								[(null? (cdr datum))
									(eopl:error 'parse-exp "Invalid if syntax ~s\n~s" datum "No condition or results")]
								[(null? (cddr datum))
									(eopl:error 'parse-exp "Invalid if syntax ~s\n~s" datum "No result")]
								[(and (not (null? (cdddr datum))) (not (null? (cddddr datum))))
									(eopl:error 'parse-exp "Invalid if syntax ~s\n~s" datum "Too many arguments")]
								[else
									(if-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum)))])]
						[(eqv? first 'set!)
							(cond
								[(null? (cdr datum))
									(eopl:error 'parse-exp "Invalid set! syntax ~s\n~s" datum "No variable or value")]
								[(not (symbol? (cadr datum)))
									(eopl:error 'parse-exp "Invalid set! syntax ~s\n~s" datum "Variable not a symbol")]
								[(null? (cddr datum))
									(eopl:error 'parse-exp "Invalid set! syntax ~s\n~s" datum "No value specified")]
								[(not (null? (cdddr datum)))
									(eopl:error 'parse-exp "Invalid set! syntax ~s\n~s" datum "Too many arguments")]
								[else
									(set!-exp (cadr datum) (parse-exp (caddr datum)))])]
						[(eqv? first 'let)
							(cond
								[(null? (cdr datum))
									(eopl:error 'parse-exp "Invalid let syntax ~s\n~s" datum "No variable/value list or body/bodies")]
								[(null? (cddr datum))
									(eopl:error 'parse-exp "Invalid let syntax ~s\n~s" datum "Let: No body/bodies list; Named-let: No variable/value list")]
								[(not (or (symbol? (cadr datum)) (list? (cadr datum))))
									(eopl:error 'parse-exp "Invalid let syntax ~s\n~s" datum "Invalid variable/value list")]
								[(symbol? (cadr datum))
									(cond
										[(not (list? (caddr datum)))
											(eopl:error 'parse-exp "Invalid named let syntax ~s\n~s" datum "Invalid variable/value list")]
										[(null? (cdddr datum))
											(eopl:error 'parse-exp "Invalid named let syntax ~s\n~s" datum "No body/bodies")]
										[(not (andmap (lambda (x) (and (pair? x) (not (null? (cdr x))) (null? (cddr x)) (symbol? (car x)))) (caddr datum)))
											(eopl:error 'parse-exp "Invalid named let syntax ~s\n~s" datum "Invalid variable/value list")]
										[else
											(named-let-exp (cadr datum) (map (lambda (x) (assignment-exp (car x) (parse-exp (cadr x)))) (caddr datum)) (map parse-exp (cdddr datum)))])]
								[else
									(cond
										[(not (andmap (lambda (x) (and (pair? x) (not (null? (cdr x))) (null? (cddr x)) (symbol? (car x)))) (cadr datum)))											(eopl:error 'parse-exp "Invalid let syntax ~s\n~s" datum "Invalid variable/value list")]
										[else
											(let-exp (map (lambda (x) (assignment-exp (car x) (parse-exp (cadr x)))) (cadr datum)) (map parse-exp (cddr datum)))])])]
						[(eqv? first 'let*)
							(cond
								[(null? (cdr datum))
									(eopl:error 'parse-exp "Invalid ~s syntax ~s\n~s" first datum "Invalid variable/value list")]
								[(null? (cddr datum))
									(eopl:error 'parse-exp "Invalid ~s syntax ~s\n~s" first datum "No body/bodies")]
								[(not (list? (cadr datum)))
									(eopl:error 'parse-exp "Invalid ~s syntax ~s\n~s" first datum "Invalid variable/value list")]
								[(not (andmap (lambda (x) (and (pair? x) (not (null? (cdr x))) (null? (cddr x)) (symbol? (car x)))) (cadr datum)))
									(eopl:error 'parse-exp "Invalid ~s syntax ~s\n~s" first datum "Invalid variable/value list")]
								[else
									(let*-exp (map (lambda (x) (assignment-exp (car x) (parse-exp (cadr x)))) (cadr datum)) (map parse-exp (cddr datum)))])]
						[(eqv? first 'letrec)
							(cond
								[(null? (cdr datum))
									(eopl:error 'parse-exp "Invalid ~s syntax ~s\n~s" first datum "Invalid variable/value list")]
								[(null? (cddr datum))
									(eopl:error 'parse-exp "Invalid ~s syntax ~s\n~s" first datum "No body/bodies")]
								[(not (list? (cadr datum)))
									(eopl:error 'parse-exp "Invalid ~s syntax ~s\n~s" first datum "Invalid variable/value list")]
								[(not (andmap (lambda (x) (and (pair? x) (not (null? (cdr x))) (null? (cddr x)) (symbol? (car x)))) (cadr datum)))
									(eopl:error 'parse-exp "Invalid ~s syntax ~s\n~s" first datum "Invalid variable/value list")]
								[else
									(letrec-exp (map (lambda (x) (assignment-exp (car x) (parse-exp (cadr x)))) (cadr datum)) (map parse-exp (cddr datum)))])]
						[else
							(app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))]))]
			[else
				(eopl:error 'parse-exp "Invalid concrete syntax ~s" datum)])))

(define unparse-exp
	(lambda (exp)
		(letrec ([unparse-assignment-exp
			(lambda (exp)
				(cases assignment-expression exp
					[assignment-exp (var val)
						(list var (unparse-exp val))]))])
			(cases expression exp
				[var-exp (id)
					id]
				[lit-exp (literal)
					literal]
				[lambda-exp (id body)
					(cons 'lambda (cons id (map unparse-exp body)))]
				[if-exp (condition result)
					(cons 'if (cons (unparse-exp condition) (map unparse-exp result)))]
				[set!-exp (id val)
					(list 'set! id (unparse-exp val))]
				[let-exp (vals body)
					(cons 'let (cons (map unparse-assignment-exp vals) (map unparse-exp body)))]
				[named-let-exp (name vals body)
					(cons 'let (cons name (cons (map unparse-assignment-exp vals) (map unparse-exp body))))]
				[let*-exp (vals body)
					(cons 'let* (cons (map unparse-assignment-exp vals) (map unparse-exp body)))]
				[letrec-exp (vals body)
					(cons 'letrec (cons (map unparse-assignment-exp vals) (map unparse-exp body)))]
				[app-exp (rator rand)
					(cons (unparse-exp rator) (map unparse-exp rand))]))))