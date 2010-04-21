(define testDataGood '(
	()
	#t
	#f
	1
	2
	#()
	#(1 2 3)
	""
	"abc"
	(1 2)
	(set! a b)
	(if a x)
	(if a x y)
	(lambda () x)
	(lambda () x y)
	(lambda (x) x)
	(lambda (x) x y)
	(lambda x y)
	(lambda x y z)
	(lambda (a b c) d)
	(lambda (a b c) d e)
	(lambda (a b c . d) e)
	(lambda (a b c . d) e f)
	(let () x)
	(let ((a #f)) a)
	(let () x y)
	(let ((a b)) c)
	(let ((a b)) c d)
	(let* () x)
	(let* ((a #f)) a)
	(let* () x y)
	(let* ((a b)) c)
	(let* ((a b)) c d)
	(letrec () x)
	(letrec ((a #f)) a)
	(letrec () x y)
	(letrec ((a b)) c)
	(letrec ((a b)) c d)
	(let name () x y)
	(let name ((a #f)) a)
	(let name ((x y)) x y)
	(let name ((a c) (b d)) e f)
	((lambda (x)
		(if x 3 4))
		5)
	(lambda x (if (< x (* x 2)) #t "abc"))
	(lambda (temperature)
			(* (/ 5 9) (- temperature 32)))
    (lambda (interval number)
		(let ((lower (list-ref interval 0)) (upper (list-ref interval 1)))
		(if (or (< number lower) (> number upper)) #f #t)))
    (lambda (i1 i2)
		(let ((l1 (list-ref i1 0)) (u1 (list-ref i1 1)) (l2 (list-ref i2 0)) (u2 (list-ref i2 1)))
		(if (> l2 u1) #f (if (< u2 l1) #f #t))))
	(lambda (i1 i2)
		(if (interval-intersects? i1 i2)
			(let ((l1 (list-ref i1 0))
				(u1 (list-ref i1 1))
				(l2 (list-ref i2 0))
				(u2 (list-ref i2 1)))
				(if (< l1 l2) 
					(if (> u1 u2) (list (list l1 u1)) (list (list l1 u2)))
					(if (> u1 u2) (list (list l2 u1)) (list (list l2 u2)))))
			(list i1 i2)))
	(lambda (number)
		(if (= 0 (modulo number 7)) #t #f))
	(lambda (number)
		(if (= 0 (modulo (- number 7) 10)) #t #f))
	(lambda (n)
      (if (eq? n 0)
		1
		(* n (fact (- n 1)))))
	(lambda (n k)
		(/ (fact n) (* (fact k) (fact (- n k)))))
	(lambda (l h)
		(if (>= l h)
			'()
			(cons l (make-range (+ 1 l) h))))
	(lambda (item ls)
		(if (null? ls)
			#t
			(if (equal? item (car ls))
				#f
				(check-unique? item (cdr ls)))))
	(lambda (ls)
		(if (and (list? ls) (null? ls))
			#t
			(and (check-unique? (car ls) (cdr ls)) (set? (cdr ls)))))
	(lambda (set1 set2)
		(if (null? set1)
			'()
			(if (check-unique? (car set1) set2)
				(intersection (cdr set1) set2)
				(cons (car set1) (intersection (cdr set1) set2)))))
	(lambda (set1 set2)
		(andmap (lambda (x) (not (check-unique? x set2))) set1))
	(lambda (item)
		(if (and (list? item) (set? item))
			(andmap (lambda (ls) (and (list? ls) (eq? (length ls) 2))) item)
			#f))
	(lambda (ls)
		(if (null? ls)
			'()
			(if (check-unique? (car ls) (cdr ls))
				(cons (car ls) (remove-dup (cdr ls)))
				(remove-dup (cdr ls)))))
	(lambda (rel)
		(if (null? rel)
			'()
			(remove-dup (map car rel))))
	(lambda (f)
		(lambda (n)
			(lambda (x) 
				(f n x))))
	(lambda (f)
		(lambda (g)
			(lambda (x)
				(f (g x)))))
	(lambda list-of-functions
		(if (null? (cdr list-of-functions))
			(car list-of-functions)
			(lambda (x) ((car list-of-functions) ((apply compose (cdr list-of-functions)) x)))))
	(lambda (n x)
		(if (= n 0)
			'()
			(cons x (make-list (- n 1) x))))
	(lambda (n)
		(lambda (x)
			(make-list n x)))
	(lambda (m row col)
		(list-ref (list-ref m row) col))
	(lambda (n)
		(/ (* (- n 1) n) 2))
	(lambda (x ls)
		(if (null? ls)
			#f
			(or (equal? x (car ls)) (checkEdges? x (cdr ls)))))
	(lambda (x ls)
		(if (null? ls)
			#t
			(if (equal? x (caar ls))
				(checkList? x (cdr ls))
				(and (checkEdges? x (cadar ls)) (checkList? x (cdr ls))))))
	(lambda (points ls)
		(if (null? points)
			#t
			(and (checkList? (car points) ls) (checkPoints? (cdr points) ls))))
	(lambda (ls)
		(let ([points (map car ls)])
			(if (<= (length points) 1)
				#t
				(checkPoints? points ls))))
	(lambda (x ls)
		(if (null? ls)
			'()
			(if (equal? x (car ls))
				(getCon x (cdr ls))
				(cons (car ls) (getCon x (cdr ls))))))
	(lambda (points ls)
		(if (null? points)
			'()
			(cons (list (car points) (getCon (car points) ls)) (makeComplete (cdr points) ls))))
	(lambda (ls)
		(if (null? ls)
			'()
			(makeComplete ls ls)))
    (lambda (n)
      (if (eq? n 0)
		1
		(* n (fact (- n 1)))))
	(lambda (n k)
		(/ (fact n) (* (fact k) (fact (- n k)))))
	(lambda (n k)
		(if (eq? n k)
			'(1)
			(cons (binomial n k) (create-row n (+ k 1)))))
	(lambda (n)
		(if (<= n -1)
			'()
			(cons (create-row n 0) (pascal-triangle (- n 1)))))
	(lambda (ls)
		(letrec ((filterList (lambda (f ls)
			(if (null? ls)
				'()
				(if (f (car ls))
					(cons (car ls) (filterList f (cdr ls)))
					(filterList f (cdr ls)))))))
		(let ((args (filterList (lambda (x) (eq? (length x) 2)) (list-ref ls 1))) (expression (list-ref ls 2)))
			(let ((vars (map car args)) (vals (map cadr args)))
				(append (list (list 'lambda vars expression)) vals)))))
	(lambda (ls)
		(letrec ((makeLet (lambda (ls expression)
			(if (null? ls)
				expression
				(list 'let (list (car ls)) (makeLet (cdr ls) expression))))))
			(let ((args (list-ref ls 1)) (expression (list-ref ls 2)))
				(makeLet args expression))))
	(lambda (pred? ls)
		(if (null? ls)
			'()
			(if (pred? (car ls))
				(cons (car ls) (filter-in pred? (cdr ls)))
				(filter-in pred? (cdr ls)))))
	(lambda (pred? ls)
		(if (null? ls)
			'()
			(if (pred? (car ls))
				(filter-out pred? (cdr ls))
				(cons (car ls) (filter-out pred? (cdr ls))))))
	(lambda (ls)
		(/ (apply + (map length (map cadr ls))) 2))
	(lambda (ls vertex)
		(if (null? ls)
			'()
			(let ((point (car ls)))
				(if (equal? (car point) vertex)
					(remove-vertex (cdr ls) vertex)
					(letrec ((removeEdge (lambda (ls vertex)
						(if (null? ls)
							'()
							(if (equal? (car ls) vertex)
								(removeEdge (cdr ls) vertex)
								(cons (car ls) (removeEdge (cdr ls) vertex)))))))
						(cons (list (car point) (removeEdge (cadr point) vertex)) (remove-vertex (cdr ls) vertex)))))))
	(lambda (ls)
		(letrec ((check-points (lambda (ls)
			(letrec ((count-occurrences  (lambda (x ls)
				(cond
				  ((member x ls) => (lambda (ls) (+ (count-occurrences x (cdr ls)) 1)))
				(else 0)))))
			(andmap (lambda (x) (< (count-occurrences x ls) 2)) ls)))))
			(letrec ((check-list (lambda (points edges)
				(if (null? edges)
					#t
					(and (andmap (lambda (x) (member x points)) (car edges)) (check-list points (cdr edges)))))))
				(if (list? ls)
					(let ((points (map car ls)))
						(if (andmap symbol? points)
							(and (check-list points (map cadr ls)) (check-points points))
							#f))
					#f))))
    (lambda (i1 i2)
      (let ((l1 (list-ref i1 0)) (u1 (list-ref i1 1)) (l2 (list-ref i2 0)) (u2 (list-ref i2 1)))
      (if (> l2 u1) #f (if (< u2 l1) #f #t))))
   (lambda (i1 i2)
     (if (interval-intersects? i1 i2)
         (let ((l1 (list-ref i1 0))
             (u1 (list-ref i1 1))
             (l2 (list-ref i2 0))
             (u2 (list-ref i2 1)))
             (if (< l1 l2) 
                 (if (> u1 u2) (list (list l1 u1)) (list (list l1 u2)))
                 (if (> u1 u2) (list (list l2 u1)) (list (list l2 u2)))))
         (list i1 i2)))
	(lambda (ls)
		(letrec ((check-list (lambda (ls interval)
			(letrec ((check-item (lambda (item interval)
				(letrec ((expand-interval (lambda (item interval)
					(if (null? interval)
						'()
						(let ((interval-item (car interval)))
							(if (interval-intersects? item interval-item)
								(cons (car (interval-union item interval-item)) (expand-interval item (cdr interval)))
								(cons interval-item (expand-interval item (cdr interval)))))))))
				(if (ormap (lambda (x) (interval-intersects? item x)) interval)
					(expand-interval item interval)
					(append (list item) interval))))))
			(if (null? ls)
				interval
				(check-list (cdr ls) (check-item (car ls) interval)))))))
		(check-list (cdr ls) (list (car ls)))))
	(lambda (f ls)
		(ormap f ls))
	(lambda (f ls)
		(letrec ((checkList (lambda (f ls num)
			(if (null? ls)
				#f
				(if (f (car ls))
					num
					(checkList f (cdr ls) (+ num 1)))))))
		(checkList f ls 0)))
	(lambda (ls)
		(map (lambda (x) (list (cadr x) (car x))) ls))
	(lambda (f vec)
		(let ((ls (vector->list vec)))
			(letrec ((checkList (lambda (f ls num)
				(if (null? ls)
					#f
					(if (f (car ls))
						num
						(checkList f (cdr ls) (+ num 1)))))))
			(checkList f ls 0))))
	(lambda (ls1 ls2)
		(if (null? ls1)
			'()
			(append (map (lambda (x) (list (car ls1) x)) ls2) (product (cdr ls1) ls2))))
	(lambda (v ls)
		(letrec (
			[append-vector (lambda (nv v i vlen)
				(if (eq? i vlen)
					nv
					(begin
						(vector-set! nv i (vector-ref v i))
						(append-vector nv v (+ i 1) vlen))))]
			[append-list (lambda (nv ls i)
				(if (null? ls)
					nv
					(begin 
						(vector-set! nv i (car ls))
						(append-list nv (cdr ls) (+ i 1)))))])
		(let ((vlen (vector-length v)) (lslen (length ls)))
			(let ((total-length (+ vlen lslen)))
				(let ((nv (make-vector total-length)))
					(append-list (append-vector nv v 0 vlen) ls vlen))))))
	(lambda (ls)
		(letrec ((check-k (lambda (L K ls)
			(letrec (
				[check-list (lambda (x ls)
					(if (null? ls)
						#f
						(let ((vertex (car ls)))
							(if (equal? (car vertex) x)
								vertex
								(check-list x (cdr ls))))))]
				[add-to-K (lambda (L K edges)
				(if (null? edges)
					K
					(let ((vertex (car edges)))
						(if (member vertex L)
							(add-to-K L K (cdr edges))
							(add-to-K L (append K (list vertex)) (cdr edges))))))]
				[add-to-L (lambda (L vertex)
					(if (member vertex L)
						L
						(append L (list vertex))))])
			(if (null? K)
				L
				(let ((vertex (check-list (car K) ls)))
					(if vertex
						(let ((K (add-to-K L K (cadr vertex))) (L (add-to-L L (car vertex))))
							(check-k L (cdr K) ls))
						(check-k L (cdr K) ls))))))))
		(let ((x (car ls)))
			(eq? (length (map car ls)) (length (check-k (list (car x)) (cadr x) ls))))))
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
		(remove-dup (helper LcExp '() '()) '())))
	(lambda (tree)
		(letrec (
			[helper
				(lambda (tree)
					(cases bintree tree
						[leaf-node (datum) datum]
						[interior-node (key left right)
							(let ([left-ans (helper left)] [right-ans (helper right)])
								(cond 
									[(and (integer? left-ans) (integer? right-ans))
										(let ([node-sum (+ left-ans right-ans)])
											(list node-sum (list key node-sum)))]
									[(integer? left-ans)
										(let ([node-sum (+ (car right-ans) left-ans)] [right-max (cadr right-ans)])
											(if (> node-sum (cadr right-max))
												(list node-sum (list key node-sum))
												(list node-sum right-max)))]
									[(integer? right-ans)
										(let ([node-sum (+ (car left-ans) right-ans)] [left-max (cadr left-ans)])
											(if (> node-sum (cadr left-max))
												(list node-sum (list key node-sum))
												(list node-sum left-max)))]
									[else
										(let ([node-sum (+ (car left-ans) (car right-ans))] [left-max (cadr left-ans)] [right-max (cadr right-ans)])
											(if (>= (cadr left-max) (cadr right-max))
												(if (> node-sum (cadr left-max))
													(list node-sum (list key node-sum))
													(list node-sum left-max))
												(if (> node-sum (cadr right-max))
													(list node-sum (list key node-sum))
													(list node-sum right-max))))]))]))])
			(caadr (helper tree))))
	(define compose2
	(lambda (f g)
		(lambda (x)
			(f (g x)))))
			
(define nth-cdr
	(lambda (n)
		(letrec ((helper (lambda (n f)
			(if (eq? n 1)
				f
				(helper (- n 1) (compose2 f cdr))))))
			(if (zero? n)
				(lambda (x) x)
				(helper n cdr)))))

; 10
(define up
	(lambda (lst)
		(if (null? lst)
			'()
			(let ((x (car lst)))
				(if (list? (car lst))
					(append x (up (cdr lst)))
					(append (list x) (up (cdr lst))))))))
					
; 11
(define pair-up
	(lambda (ls)
		(if (null? ls)
			'()
			(let ((x (car ls)))
				(cond
					[(null? (cdr ls))
						(list (list x x))]
					[(null? (cddr ls))
						(list (list x (cadr ls)))]
					[else
						(cons (list x (cadr ls)) (pair-up (cddr ls)))])))))

; 12
(define max-contiguous-nonempty-subsequence-sum
	(lambda (ls)
		(letrec ((helper (lambda (m maxList ls)
			(if (null? ls)
				m
				(let* ((item (car ls)) (tempMaxList (append (map (lambda (x) (+ item x)) maxList) (list item))) (t (apply max tempMaxList)))
					(if (> t m)
						(helper t tempMaxList (cdr ls))
						(helper m tempMaxList (cdr ls))))))))
			(if (null? ls)
				ls
				(helper (car ls) '() ls)))))
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
(define-datatype expression expression?
	[var-exp
		(id symbol?)]
	[lit-exp
		(literal (lambda (x) (or (number? x) (string? x) (boolean? x) (vector? x) (and (pair? x) (not (list? x))) (null? x))))]
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
		(type symbol?)
		(vals (list-of (lambda (x) (and (symbol? (car x)) (expression? (cadr x))))))
		(body (list-of expression?))]
	[named-let-exp
		(name symbol?)
		(vals (list-of (lambda (x) (and (symbol? (car x)) (expression? (cadr x))))))
		(body (list-of expression?))]
	[app-exp
		(rator expression?)
		(rand (list-of expression?))])

(define parse-exp
	(lambda (datum)
		(cond
			[(symbol? datum)
				(var-exp datum)]
			[(or (number? datum) (string? datum) (boolean? datum) (vector? datum) (and (pair? datum) (not (list? datum))) (null? datum))
				(lit-exp datum)]
			[(pair? datum)
				(let ([first (car datum)])
					(cond 
						[(eqv? first 'lambda)
							(if (or (null? (cdr datum)) (null? (cddr datum)) (and (not (symbol? (cadr datum))) (not (pair? (cadr datum))) (not (null? (cadr datum)))) (and (list? (cadr datum)) (ormap list? (cadr datum))))
								(eopl:error 'parse-exp "Invalid lambda syntax ~s" datum)
								(lambda-exp (cadr datum) (map parse-exp (cddr datum))))]
						[(eqv? first 'if)
							(if (or (null? (cdr datum)) (null? (cddr datum)) (and (not (null? (cdddr datum))) (not (null? (cddddr datum)))))
								(eopl:error 'parse-exp "Invalid if syntax ~s" datum)
								(if-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum))))]
						[(eqv? first 'set!)
							(if (or (null? (cdr datum)) (not (symbol? (cadr datum))) (null? (cddr datum)) (not (null? (cdddr datum))))
								(eopl:error 'parse-exp "Invalid set! syntax ~s" datum)
								(set!-exp (cadr datum) (parse-exp (caddr datum))))]
						[(eqv? first 'let)
							(if (or (null? (cdr datum)) (null? (cddr datum)) (if (symbol? (cadr datum)) (or (not (list? (caddr datum))) (null? (cdddr datum)) (not (andmap (lambda (x) (and (pair? x) (eq? (length x) 2) (symbol? (car x)) (or (symbol? (cadr x)) (list? (cadr x))))) (caddr datum)))) (not (andmap (lambda (x) (and (pair? x) (eq? (length x) 2) (symbol? (car x)) (or (symbol? (cadr x)) (list? (cadr x))))) (cadr datum)))))
								(eopl:error 'parse-exp "Invalid ~s syntax ~s" first datum)
								(if (symbol? (cadr datum))
									(named-let-exp (cadr datum) (map (lambda (x) (list (car x) (parse-exp (cadr x)))) (caddr datum)) (map parse-exp (cdddr datum)))
									(let-exp first (map (lambda (x) (list (car x) (parse-exp (cadr x)))) (cadr datum)) (map parse-exp (cddr datum)))))]
						[(or (eqv? first 'let*) (eqv? first 'letrec))
							(if (or (null? (cdr datum)) (null? (cddr datum)) (not (list? (cadr datum))) (not (andmap (lambda (x) (and (pair? x) (eq? (length x) 2) (symbol? (car x)) (or (symbol? (cadr x)) (list? (cadr x))))) (cadr datum))))
								(eopl:error 'parse-exp "Invalid let syntax ~s" datum)
								(let-exp first (map (lambda (x) (list (car x) (parse-exp (cadr x)))) (cadr datum)) (map parse-exp (cddr datum))))]
						[else
							(app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))]))]
			[else
				(eopl:error 'parse-exp "Invalid concrete syntax ~s" datum)])))

(define unparse-exp
	(lambda (exp)
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
			[let-exp (type vals body)
				(cons type (cons (map (lambda (x) (list (car x) (unparse-exp (cadr x)))) vals) (map unparse-exp body)))]
			[named-let-exp (name vals body)
				(cons 'let (cons name (cons (map (lambda (x) (list (car x) (unparse-exp (cadr x)))) vals) (map unparse-exp body))))]
			[app-exp (rator rand)
				(cons (unparse-exp rator) (map unparse-exp rand))])))
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
	; Stephen Mayhew/Kevin Risden
; Assignment 7
;
; #1
;(define t1 '(a (b 1 4) (c (d 2 5) 3)))
;(define t2 '(e 6 (a (b 1 4) (c (d 2 5) 3))))
(define bt-recur
	(lambda (base-value num-proc tree-proc)
		(letrec ([helper (lambda (ls)
			(cond
				[(null? ls)base-value]
			    [(number? ls)
					(num-proc ls)]
			    [else
					(cond
						[(null? (cdr ls))
							(tree-proc (car ls) '() '())]
						[(null? (cddr ls))
							(tree-proc (car ls) (helper (cadr ls)) '())]
						[else
							(tree-proc (car ls) (helper (cadr ls)) (helper (caddr ls)))])]))])
		helper)))

; a
(define bt-sum
	(bt-recur 0 (lambda (x) x) (lambda (x y z) (+ y z))))
; b
(define bt-inorder
	(bt-recur '()
		(lambda (x) '())
		(lambda (x y z)
			(append y (list x) z))))
			
; #2
(define make-stack
 (lambda ()
  (let ([stk '()])
   (lambda (msg  . args ) 
    (case msg
      [(empty?) (null? stk)]
      [(push)   (set! stk (cons (car args) stk))]
      [(pop)    (let ([top (car stk)])
                   (set! stk (cdr stk))
                   top)]
      [else (error 'stack "illegal message to stack object: ~a" msg)])))))

(define make-slist-leaf-iterator
	(lambda (ls)
		(let ((stack (make-stack)))
			(letrec ((next-item (lambda ()
				(if (stack 'empty?)
					#f
					(let ((item (stack 'pop)))
						(cond
							[(symbol? item)
								item]
							[(null? item)
								(next-item)]
							[(null? (cdr item))
								(begin
									(stack 'push (car item))
									(next-item))]
							[else
								(begin
									(stack 'push (cdr item))
									(stack 'push (car item))
									(next-item))]))))))
				(begin
					(stack 'push ls)
					(lambda ()
						(next-item)))))))

; #3
; (subst-leftmost 'k 'b '((c d a (e () f b (c b)) (a b)) (b)) eq?)
; (subst-leftmost 2 1 '(3 (-1 5) 1 4) (lambda (x y) (= (abs x) (abs y))))
(define subst-leftmost
  (lambda (new old snlist eq-pred?)
	(if (null? snlist)
		'()
		 (cdr (subst-leftmost2 new old snlist eq-pred?)))))

(define subst-leftmost2
 (lambda (new old snlist eq-pred?)
    (cond [(null? snlist) (cons #f '())]
	  [(null? (car snlist))
	   (let ([sl (subst-leftmost2 new old (cdr snlist) eq-pred?)])
	     (cons (car sl) (cons '() (cdr sl))))]
	  [(pair? (car snlist)) ; faster list checking 
	   (let ([sl (subst-leftmost2 new old (car snlist) eq-pred?)])
	   (cons (car sl) (cons (cdr sl) (if (car sl)
			(cdr snlist)
			(cdr (subst-leftmost2 new old (cdr snlist) eq-pred?))))))]
	  [(eq-pred? (car snlist) old) (cons #t (cons new (cdr snlist)))]
	  [else (let ([sl (subst-leftmost2 new old (cdr snlist) eq-pred?)])
		  (cons (car sl) (cons (car snlist) (cdr sl))))])))
	; Kevin Risden
; Assignment 6
;
; #1
(define snlist-recur
  (lambda (base-value list-proc comb-proc)
    (letrec ((helper (lambda (ls)
                       (cond
						[(null? ls)
                           base-value]
						[(list? (car ls))
							(list-proc (helper (car ls)) (helper (cdr ls)))]
						[else
                           (comb-proc (car ls) (helper (cdr ls)))]))))
      helper)))
; a
(define sn-list-sum
	(snlist-recur 0 + +))
; b
(define sn-list-map
	(lambda (f snlist)
		((snlist-recur '()
			(lambda (x y) (cons x y))
			(lambda (x y) (cons (f x) y))) snlist)))
; c
(define paren-count
	(snlist-recur 2 + (lambda (x y) y)))
; d
(define sn-list-reverse
	(snlist-recur '() (lambda (x y) (append y (list x))) (lambda (x y) (append y (list x)))))
; e
(define sn-list-occur
	(lambda (s snlist)
		((snlist-recur 0 +
			(lambda (x y)
				(if (equal? s x)
					(+ y 1)
					y))) snlist)))
; f
(define depth
	(snlist-recur 1 (lambda (x y) (+ (max x (- y 1)) 1)) (lambda (x y) y)))
	; Kevin Risden
; Assignment 5
;
; #1
(define invert
	(lambda (ls)
		(map (lambda (x) (list (cadr x) (car x))) ls)))
; #2
(define vector-index
	(lambda (f vec)
		(let ((ls (vector->list vec)))
			(letrec ((checkList (lambda (f ls num)
				(if (null? ls)
					#f
					(if (f (car ls))
						num
						(checkList f (cdr ls) (+ num 1)))))))
			(checkList f ls 0)))))
; #3
(define product
	(lambda (ls1 ls2)
		(if (null? ls1)
			'()
			(append (map (lambda (x) (list (car ls1) x)) ls2) (product (cdr ls1) ls2)))))
; #4
(define vector-append-list
	(lambda (v ls)
		(letrec (
			[append-vector (lambda (nv v i vlen)
				(if (eq? i vlen)
					nv
					(begin
						(vector-set! nv i (vector-ref v i))
						(append-vector nv v (+ i 1) vlen))))]
			[append-list (lambda (nv ls i)
				(if (null? ls)
					nv
					(begin 
						(vector-set! nv i (car ls))
						(append-list nv (cdr ls) (+ i 1)))))])
		(let ((vlen (vector-length v)) (lslen (length ls)))
			(let ((total-length (+ vlen lslen)))
				(let ((nv (make-vector total-length)))
					(append-list (append-vector nv v 0 vlen) ls vlen)))))))
; #5
(define rotate
	(lambda (ls)
		(letrec ((do-rotation (lambda (nls ls)
			(cond
				((null? ls)
					ls)
				((null? (cdr ls))
					(cons (car ls) nls))
				(else (do-rotation (append nls (list (car ls))) (cdr ls)))))))
		(do-rotation '() ls))))
		
; #6
(define flatten
	(lambda (ls)
		(letrec ((flatten-list (lambda (ls total)
		(if (null? ls)
			total
			(let ((item (car ls)))
				(if (list? item)
					(flatten-list (cdr ls) (flatten-list item total))
						(flatten-list (cdr ls) (append total (list item)))))))))
		(flatten-list ls '()))))
; #7
(define merge
	(lambda (ls1 ls2)
		(letrec ((merge (lambda (ls ls1 ls2)
			(cond
				((and (null? ls1) (null? ls2))
					ls)
				((null? ls1)
					(append ls ls2))
				((null? ls2)
					(append ls ls1))
				(else (let ((item1 (car ls1)) (item2 (car ls2)))
						(if (<= item1 item2)
							(merge (append ls (list item1)) (cdr ls1) ls2)
							(merge (append ls (list item2)) ls1 (cdr ls2)))))))))
		(merge '() ls1 ls2))))
; #8
(define path
	(lambda (n ls)
		(letrec ((get-path (lambda (n ls path)
			(if (null? ls)
				#f
				(let ((x (car ls)))
					(cond
						((< n x)
							(get-path n (cadr ls) (append path '(left))))
						((> n x)
							(get-path n (caddr ls) (append path '(right))))
						(else
							path)))))))
		(get-path n ls '()))))
; #9
(define qsort
	(lambda (f ls)
		(letrec ((qsort (lambda (f ls p l g)
			(if (null? ls)
				(let ((p (list p)))
					(cond
						[(and (null? l) (null? g))
							p]
						[(null? l)
							(append p (qsort f (cdr g) (car g) '() '()))]
						[(null? g)
							(append (qsort f (cdr l) (car l) '() '()) p)]
						[(null? (cdr l))
							(append (qsort f '() (car l) '() '()) p (qsort f (cdr g) (car g) '() '()))]
						[(null? (cdr g))
							(append (qsort f (cdr l) (car l) '() '()) p (qsort f '() (car g) '() '()))]
						[else
							(append (qsort f (cdr l) (car l) '() '()) p (qsort f (cdr g) (car g) '() '()))]))
				(let ((x (car ls)))
					(if (f x p)
						(qsort f (cdr ls) p (append l (list x)) g)
						(qsort f (cdr ls) p l (append g (list x)))))))))
		(if (or (null? ls) (null? (cdr ls)))
			ls
			(qsort f (cdr ls) (car ls) '() '())))))
; #10
(define connected?
	(lambda (ls)
		(letrec ((check-k (lambda (L K ls)
			(letrec (
				[check-list (lambda (x ls)
					(if (null? ls)
						#f
						(let ((vertex (car ls)))
							(if (equal? (car vertex) x)
								vertex
								(check-list x (cdr ls))))))]
				[add-to-K (lambda (L K edges)
				(if (null? edges)
					K
					(let ((vertex (car edges)))
						(if (member vertex L)
							(add-to-K L K (cdr edges))
							(add-to-K L (append K (list vertex)) (cdr edges))))))]
				[add-to-L (lambda (L vertex)
					(if (member vertex L)
						L
						(append L (list vertex))))])
			(if (null? K)
				L
				(let ((vertex (check-list (car K) ls)))
					(if vertex
						(let ((K (add-to-K L K (cadr vertex))) (L (add-to-L L (car vertex))))
							(check-k L (cdr K) ls))
						(check-k L (cdr K) ls))))))))
		(let ((x (car ls)))
			(eq? (length (map car ls)) (length (check-k (list (car x)) (cadr x) ls)))))))
	))

(define testDataBad '(
	(set!)
	(set! a)
	(set! a b c)
	(if)
	(if a b c d)
	(lambda)
	(lambda ())
	(lambda (()))
	(lambda ((x)))
	(let)
	(let x)
	(let x y)
	(let (x))
	(let (x) y)
	(let (x y) z)
	(let (((x) y)) z)
	(let (((x y))) z)
	(let*)
	(let* x)
	(let* x y)
	(let* (x))
	(let* (x) y)
	(let* (x y) z)
	(let* (((x) y)) z)
	(let* (((x y))) z)
	(letrec)
	(letrec x)
	(letrec x y)
	(letrec (x))
	(letrec (x) y)
	(letrec (x y) z)
	(letrec (((x) y)) z)
	(letrec (((x y))) z)
	(let name)
	(let name x)
	(let name x y)
	(let name (x))
	(let name (x) y)
	(let name (x y) z)
	(let name (((x) y)) z)
	(let name (((x y))) z)
	(lambda (a b #f) x)
	))

(define testGood
	(lambda ()
		(let ((tempData (map parse-exp testDataGood)))
			(display (format "~s\n" tempData))
			(display (format "~s\n" testDataGood))
			(display (map unparse-exp tempData)))))
(define testBad
	(lambda ()
		(let ((tempData (map parse-exp testDataBad))) 
			(display (format "~s\n" tempData)))))
(define testGood2
	(lambda ()
		(andmap (lambda (x) (equal? (unparse-exp (parse-exp x)) x)) testDataGood)))
(define testBad2
	(lambda()
		(andmap null? (map parse-exp testDataBad))))
	