; Kevin Risde
; Assignment 11
; 
; #2 Written Answer
; The in class memoized version included a max count which enabled us to go
; directly to the place in the list for the result instead of needing to search
; the entire result list before knowing the result isn't available. Additionally
; the in class version is able to add together the results of (fib-memo (- n 1))
; and (fib-memo (- n 2)) without doing an extra lookup for each value. These two
; things when put together save time for the in class version of fib-memo compared
; to the generic memoize generated fib.

; #1
; #a
(define member?-cps
	(lambda (sym ls k)
		(if (null? ls)
			(k #f)
			(member?-cps sym (cdr ls) (lambda (v)
				(if v
					(k #t)
					(k (equal? sym (car ls)))))))))

; #b
(define set?-cps
	(lambda (ls k)
		(cond
			[(null? ls) (k #t)]
			[(not (pair? ls)) (k #f)]
			[else
				(set?-cps (cdr ls) (lambda (v)
					(member?-cps (car ls) (cdr ls) (lambda (v2)
						(if v2
							(k #f)
							(k v))))))])))

; #c
(define intersection-cps
	(lambda (ls1 ls2 k)
		(if (null? ls1)
			(k '())
			(intersection-cps (cdr ls1) ls2
				(lambda (v)
					(member?-cps (car ls1) ls2
						(lambda (v2)
							(if v2
								(k (cons (car ls1) v))
								(k v)))))))))

; #d
(define make-cps
	(lambda (proc)
		(lambda (data k)
			(k (proc data)))))

; #e
(define andmap-cps
	(lambda (pred-cps ls k)
		(if (null? ls)
			(k #t)
			(pred-cps (car ls) (lambda (v)
				(if v
					(andmap-cps pred-cps (cdr ls) k)
					(k #f)))))))
					
; #f
(define list?-cps
	(lambda (ls k)
		(if (null? ls)
			(k #t)
			(if (pair? ls)
				(list?-cps (cdr ls) k)
				(k #f)))))		
		
(define length-cps
	(lambda (ls k)
		(let count_ls ([ls ls] [count 0])
			(if (null? ls)
				(k count)
				(count_ls (cdr ls) (+ count 1))))))

(define matrix?-cps
	(lambda (m k)
		(list?-cps m (lambda (list?-v)
			(if list?-v
				(if (not (null? m))
					(if (not (null? (car m)))
						(andmap-cps list?-cps m (lambda (andmap-v)
							(if andmap-v
								(andmap-cps (make-cps (lambda (L) (length-cps L (lambda (v1)
									(length-cps (car m) (lambda (v2) (= v1 v2))))))) (cdr m) k)
								(k #f))))
						(k #f))
					(k #f))
				(k #f))))))

; #2
;;class memoized-fib
;(define fib-memo
;  (let ([max 1]
;        [sofar '((1 . 1) (0 . 1))])
;    (lambda (n)
;      (if (<= n max)
;          (cdr (assq n sofar))
;          (let* ([v1 (fib-memo (- n 1))]
;                 [v2 (fib-memo (- n 2))]
;                 [v3 (+ v2 v1)])
;            (set! max n)
;            (set! sofar (cons (cons n v3) sofar))
;            v3)))))

(define memoize
	(lambda (func)
		(let ([res '()]) ;;no previous results
			(lambda arg ;;list of args
				(let ([res-pair (assoc arg res)]) ;; find first argument in result
					(if res-pair ;;if there is previous results
						(cdr res-pair) ;;get answer from previous result
						(let ([save (apply func arg)]) ;;get the result to save
							(set! res (cons (cons arg save) res)) ;;save the id and result to results
							save))))))) ;;display answer
