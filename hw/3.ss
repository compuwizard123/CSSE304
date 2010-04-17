; Kevin Risden
; Assignment 3
;
; #1
(define curry2
	(lambda (f)
		(lambda (n)
			(lambda (x) 
				(f n x)))))
; #2
(define curried-compose
	(lambda (f)
		(lambda (g)
			(lambda (x)
				(f (g x))))))
; #3
(define compose
	(lambda list-of-functions
		(if (null? (cdr list-of-functions))
			(car list-of-functions)
			(lambda (x) ((car list-of-functions) ((apply compose (cdr list-of-functions)) x))))))
; #4
(define make-list
  (lambda (n x)
    (if (= n 0)
        '()
        (cons x (make-list (- n 1) x)))))
(define make-list-c
	(lambda (n)
		(lambda (x)
			(make-list n x))))
; #5
(define matrix-ref
	(lambda (m row col)
		(list-ref (list-ref m row) col)))
; #6
(define max-edges
	(lambda (n)
		(/ (* (- n 1) n) 2)))
; #7
(define checkEdges?
	(lambda (x ls)
		(if (null? ls)
			#f
			(or (equal? x (car ls)) (checkEdges? x (cdr ls))))))
(define checkList?
	(lambda (x ls)
		(if (null? ls)
			#t
			(if (equal? x (caar ls))
				(checkList? x (cdr ls))
				(and (checkEdges? x (cadar ls)) (checkList? x (cdr ls)))))))
(define checkPoints?
	(lambda (points ls)
		(if (null? points)
			#t
			(and (checkList? (car points) ls) (checkPoints? (cdr points) ls)))))
(define complete?
	(lambda (ls)
		(let ([points (map car ls)])
			(if (<= (length points) 1)
				#t
				(checkPoints? points ls)))))
; #8
(define getCon
	(lambda (x ls)
		(if (null? ls)
			'()
			(if (equal? x (car ls))
				(getCon x (cdr ls))
				(cons (car ls) (getCon x (cdr ls)))))))
(define makeComplete
	(lambda (points ls)
		(if (null? points)
			'()
			(cons (list (car points) (getCon (car points) ls)) (makeComplete (cdr points) ls)))))
(define complete
	(lambda (ls)
		(if (null? ls)
			'()
			(makeComplete ls ls))))
; #9
(define fact
    (lambda (n)
      (if (eq? n 0)
		1
		(* n (fact (- n 1))))))
(define binomial
	(lambda (n k)
		(/ (fact n) (* (fact k) (fact (- n k))))))
(define create-row
	(lambda (n k)
		(if (eq? n k)
			'(1)
			(cons (binomial n k) (create-row n (+ k 1))))))
(define pascal-triangle
	(lambda (n)
		(if (<= n -1)
			'()
			(cons (create-row n 0) (pascal-triangle (- n 1))))))