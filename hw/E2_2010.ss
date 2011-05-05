; Kevin Risden
; Exam 1
; 
; 7

(define counter-maker
	(lambda (proc)
		(let ([count 0])
		(lambda args
				(if (and (eq? (length args) 1) (eqv? (car args) 'count))
					count
					(begin
						(set! count (+ count 1))
						(apply proc args)))))))