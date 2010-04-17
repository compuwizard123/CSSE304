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