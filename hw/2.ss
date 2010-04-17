; Kevin Risden
; Assignment 2
;
; #1a
(define fact
    (lambda (n)
      (if (eq? n 0)
		1
		(* n (fact (- n 1))))))
; #1b
(define choose
	(lambda (n k)
		(/ (fact n) (* (fact k) (fact (- n k))))))
; #2
(define make-range
	(lambda (l h)
		(if (>= l h)
			'()
			(cons l (make-range (+ 1 l) h)))))
; #3
(define check-unique?
	(lambda (item ls)
		(if (null? ls)
			#t
			(if (equal? item (car ls))
				#f
				(check-unique? item (cdr ls))))))
(define set?
	(lambda (ls)
		(if (and (list? ls) (null? ls))
			#t
			(and (check-unique? (car ls) (cdr ls)) (set? (cdr ls))))))
; #4
(define intersection
	(lambda (set1 set2)
		(if (null? set1)
			'()
			(if (check-unique? (car set1) set2)
				(intersection (cdr set1) set2)
				(cons (car set1) (intersection (cdr set1) set2))))))
; #5
(define subset?
	(lambda (set1 set2)
		(andmap (lambda (x) (not (check-unique? x set2))) set1)))
; #6
(define relation?
	(lambda (item)
		(if (and (list? item) (set? item))
			(andmap (lambda (ls) (and (list? ls) (eq? (length ls) 2))) item)
			#f)))
; #7
(define remove-dup
	(lambda (ls)
		(if (null? ls)
			'()
			(if (check-unique? (car ls) (cdr ls))
				(cons (car ls) (remove-dup (cdr ls)))
				(remove-dup (cdr ls))))))
(define domain
	(lambda (rel)
		(if (null? rel)
			'()
			(remove-dup (map car rel)))))