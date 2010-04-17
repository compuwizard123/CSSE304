; Kevin Risden
; Assignment 8
;
; #1
(define free-vars
	(lambda (LcExp)
		(letrec ([helper (lambda (LcExp bound vars)
			(cond
				[(not (pair? LcExp)) ; Identifier
					(if (member LcExp bound)
						'()
						(list LcExp))]
				[(eq? 'lambda (car LcExp)) ; (lambda (Identifier) LcExp)
					(append (helper (caddr LcExp) (append (cadr LcExp) bound) vars) vars)]
				[else ; (LcExp LcExp)
					(append (helper (car LcExp) bound vars) (append (helper (cadr LcExp) bound vars) vars))]))]
			[remove-dup (lambda (vars ls)
				(if (null? vars)
					ls
					(let ((item (car vars)))
						(if (member item ls)
							(remove-dup (cdr vars) ls)
							(remove-dup (cdr vars) (cons item ls))))))])
		(remove-dup (helper LcExp '() '()) '()))))
(define bound-vars
	(lambda (LcExp)
		(letrec ((helper (lambda (LcExp bound vars)
			(cond
				[(not (pair? LcExp)) ; Identifier
					(if (member LcExp bound)
						(list LcExp)
						'())]
				[(eq? 'lambda (car LcExp)) ; (lambda (Identifier) LcExp)
					(append (helper (caddr LcExp) (append (cadr LcExp) bound) vars) vars)]
				[else ; (LcExp LcExp)
					(append (helper (car LcExp) bound vars) (append (helper (cadr LcExp) bound vars) vars))]))))
		(helper LcExp '() '()))))

; #2
(define occurs-free?
	(lambda (var exp)
		(cond
			[(symbol? exp)
				(eqv? var exp)]
			[(eqv? (car exp) 'lambda)
				(and (andmap (lambda (x) (not (eqv? var x))) (cadr exp)) (if (symbol? (caddr exp)) #t (occurs-free? var (caddr exp))))]
			[(eqv? (car exp) 'let)
				(and (ormap (lambda (x) (if (symbol? x) (eqv? var x) (occurs-free? var x))) (map cadr (cadr exp))) (occurs-free? var (caddr exp)))]
			[(eqv? (car exp) 'let*)
				(and (andmap (lambda (x) (if (eqv? var (car x)) (occurs-free? var (cadr x)) #t)) (cadr exp)) (occurs-free? var (caddr exp)))]
			[(eqv? (car exp) 'set!)
				#f]
			[(eqv? (car exp) 'if)
				(let ((chk1 (occurs-free? var (caddr exp))) (chk2 (occurs-free? var (cadddr exp))))
					(and
						(if (symbol? (caddr exp))
							(not chk1)
							chk1)
						(if (symbol? (cadddr exp))
							(not chk2)
							chk2)))]
			[else
				(ormap (lambda (x)
					(let ((temp (occurs-free? var x)))
						(if (and (symbol? x) temp)
							#t
							(not temp)))) exp)])))

(define occurs-bound?
	(lambda (var exp)
		(cond
			[(symbol? exp)
				#f]
			[(eqv? (car exp) 'lambda)
				(or (occurs-bound? var (caddr exp)) (and (ormap (lambda (x) (eqv? x var)) (cadr exp)) (occurs-free? var (caddr exp))))]
			[(eqv? (car exp) 'let)
				(or (ormap (lambda (x) (or (eqv? var (car x)) (occurs-bound? var (cadr x)))) (cadr exp)) (occurs-bound? var (caddr exp)))]
			[(eqv? (car exp) 'let*)
				(or (ormap (lambda (x)
						(and
							(eqv? var (car x))
							(ormap (lambda (y)
								(if (symbol? y)
									(eqv? var y)
									(occurs-bound? var y))) (map cadr (cadr exp)))))
						(cadr exp))
					(occurs-bound? var (caddr exp)))]
			[(eqv? (car exp) 'set!)
				#f]
			[(eqv? (car exp) 'if)
				(or (occurs-bound? var (caddr exp)) (occurs-bound? var (cadddr exp)))]
			[else
				(ormap (lambda (x) (occurs-bound? var x)) exp)])))
				
; #3
(define lexical-address
	(lambda (LcExp)
		(letrec (
			[list-index
				(lambda (item ls)
					(letrec ((checkList (lambda (item ls num)
						(if (null? ls)
							#f
							(if (eqv? item (car ls))
								num
								(checkList item (cdr ls) (+ num 1)))))))
					(checkList item ls 0)))]
			[find-bound
				(lambda (item bound-vars depth)
					(if (null? bound-vars)
						(list ': 'free item)
						(let ((index (list-index item (car bound-vars))))
							(if index
								(list ': depth index)
								(find-bound item (cdr bound-vars) (+ depth 1))))))]
			[helper
				(lambda (LcExp bound-vars)
					(cond
						[(symbol? LcExp)
							(find-bound LcExp bound-vars 0)]
						[(eqv? 'lambda (car LcExp))
							(list 'lambda (cadr LcExp) (helper (caddr LcExp) (cons (cadr LcExp) bound-vars)))]
						[(eqv? 'if (car LcExp))
							(list 'if (if (pair? (cadr LcExp)) (map (lambda (x) (helper x bound-vars)) (cadr LcExp)) (helper (cadr LcExp) bound-vars)) (helper (caddr LcExp) bound-vars) (helper (cadddr LcExp) bound-vars))]
						[else
							(map (lambda (x) (helper x bound-vars)) LcExp)]))])
		(helper LcExp '()))))
		
; #4
(define un-lexical-address
	(lambda (exp)
		(letrec (
			[unbind
				(lambda (bound-vars depth n)
					(if (zero? depth)
						(car (list-ref bound-vars n))
						(list-ref (list-ref bound-vars depth) n)))]
			[convert-to-LcExp
				(lambda (exp bound-vars)
					(if (eqv? (cadr exp) 'free)
								(caddr exp)
								(unbind bound-vars (cadr exp) (caddr exp))))]
			[helper
				(lambda (exp bound-vars)
					(cond
						[(symbol? exp)
							exp]
						[(and (eqv? ': (car exp)) (eqv? 'free (cadr exp)))
							(caddr exp)]
						[(and (eqv? ': (car exp)) (number? (cadr exp)))
							(convert-to-LcExp exp bound-vars)]
						[(eqv? 'lambda (car exp))
							(list 'lambda (cadr exp) (helper (caddr exp) (cons (cadr exp) bound-vars)))]
						[(eqv? 'if (car exp))
							(list 'if (if (eqv? ': (car (cadr exp))) (convert-to-LcExp (cadr exp) bound-vars) (map (lambda (x) (helper x bound-vars)) (cadr exp))) (helper (caddr exp) bound-vars) (helper (cadddr exp) bound-vars))]
						[else
							(map (lambda (x) (helper x bound-vars)) exp)]))])
		(helper exp '()))))