; Kevin Risde
; Assignment 9
;
; Binary trees using define-datatype
;(load "chez-init.ss") ; If you use Chez scheme, uncomment this line
;; from EOPL2, page 50
(define-datatype bintree bintree?
	(leaf-node
		(num integer?))
	(interior-node
		(key symbol?)
		(left-tree bintree?)
		(right-tree bintree?)))

; #1
; a
(define-syntax my-let
 (syntax-rules ()
   [(_ ((x v) ...) e1 e2 ...)
    ((lambda (x ...) e1 e2 ...) 
     v ...)]
   [(_ name ((x v) ...) e1 e2 ...)
    (letrec ((name (lambda (x ...) e1 e2 ...))) (name v ...))]))
; b
(define-syntax my-or
  (syntax-rules ()
    ((_) #f)
    ((_ exp) exp)
   ((_ exp exp2 ...)
		(let ((temp exp))
			(if temp
				temp
				(my-or exp2 ...))))))

; c
(define-syntax +=
  (syntax-rules ()
    [(_ e1 e2)
     (begin (set! e1 (+ e1 e2)) e1)]))
; d
(define-syntax return-first
  (syntax-rules ()
    [(_ e1 e2 ...)
     (let ((temp e1))
       (begin e2 ... temp))]))

; #2
(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      [leaf-node (datum) (list 'leaf-node datum)]
      [interior-node (key left right)
        (append '(interior-node) (list key) (list (bintree-to-list left)) (list (bintree-to-list right)))])))

; #3
;(define tree-1
;  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
;(define tree-2
;  (interior-node 'bar (leaf-node -1) tree-1))
;(define tree-3
;  (interior-node 'baz tree-2 (leaf-node 1)))
;(define tree-a (interior-node 'a (leaf-node 2) (leaf-node 3)))
;(define tree-b (interior-node 'b (leaf-node -1) tree-a))
;(define tree-c (interior-node 'c tree-b (leaf-node 1)))
;(define tree-d (interior-node 'd tree-c tree-a))
;(define tree-f (interior-node 'f tree-c (interior-node 'e tree-d tree-a)))
;(define tree-r 
;  (interior-node 
;    'root 
;    (interior-node 'l tree-f tree-d)
;    (interior-node 'r (interior-node 
;                                'rl 
;                                (leaf-node -50) 
;                                (interior-node 'rlr tree-f tree-f))
;                      tree-b)))
(define max-interior
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
		(caadr (helper tree)))))