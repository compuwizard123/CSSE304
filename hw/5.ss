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