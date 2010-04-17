; Kevin Risden
; Exam 1
; 
; 9
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
						