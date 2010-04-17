; Kevin Risden
; Assignment 4
;
; #1
(define let->application
	(lambda (ls)
		(letrec ((filterList (lambda (f ls)
			(if (null? ls)
				'()
				(if (f (car ls))
					(cons (car ls) (filterList f (cdr ls)))
					(filterList f (cdr ls)))))))
		(let ((args (filterList (lambda (x) (eq? (length x) 2)) (list-ref ls 1))) (expression (list-ref ls 2)))
			(let ((vars (map car args)) (vals (map cadr args)))
				(append (list (list 'lambda vars expression)) vals))))))
; #2
(define let*->let
	(lambda (ls)
		(letrec ((makeLet (lambda (ls expression)
			(if (null? ls)
				expression
				(list 'let (list (car ls)) (makeLet (cdr ls) expression))))))
			(let ((args (list-ref ls 1)) (expression (list-ref ls 2)))
				(makeLet args expression)))))
; #3
(define filter-in
	(lambda (pred? ls)
		(if (null? ls)
			'()
			(if (pred? (car ls))
				(cons (car ls) (filter-in pred? (cdr ls)))
				(filter-in pred? (cdr ls))))))
; #4
(define filter-out
	(lambda (pred? ls)
		(if (null? ls)
			'()
			(if (pred? (car ls))
				(filter-out pred? (cdr ls))
				(cons (car ls) (filter-out pred? (cdr ls)))))))
; #5
(define edge-count
	(lambda (ls)
		(/ (apply + (map length (map cadr ls))) 2)))
; #6
(define remove-vertex
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
						(cons (list (car point) (removeEdge (cadr point) vertex)) (remove-vertex (cdr ls) vertex))))))))
; #7
(define graph?
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
					#f)))))
; #8
(define interval-intersects?
    (lambda (i1 i2)
      (let ((l1 (list-ref i1 0)) (u1 (list-ref i1 1)) (l2 (list-ref i2 0)) (u2 (list-ref i2 1)))
      (if (> l2 u1) #f (if (< u2 l1) #f #t)))))
(define interval-union
   (lambda (i1 i2)
     (if (interval-intersects? i1 i2)
         (let ((l1 (list-ref i1 0))
             (u1 (list-ref i1 1))
             (l2 (list-ref i2 0))
             (u2 (list-ref i2 1)))
             (if (< l1 l2) 
                 (if (> u1 u2) (list (list l1 u1)) (list (list l1 u2)))
                 (if (> u1 u2) (list (list l2 u1)) (list (list l2 u2)))))
         (list i1 i2))))
(define minimize-interval-list
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
		(check-list (cdr ls) (list (car ls))))))
; #9
(define exists?
	(lambda (f ls)
		(ormap f ls)))
; #10
(define list-index
	(lambda (f ls)
		(letrec ((checkList (lambda (f ls num)
			(if (null? ls)
				#f
				(if (f (car ls))
					num
					(checkList f (cdr ls) (+ num 1)))))))
		(checkList f ls 0))))
