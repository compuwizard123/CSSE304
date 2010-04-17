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