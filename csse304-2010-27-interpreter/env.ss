; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

; vals is a vector
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector (map (lambda (x) (if (box? x) x (box x))) (vector->list vals))) env)))

(define extend-env-recursively
  (lambda (proc-names idss bodiess old-env k)
    (let ([len (length proc-names)])
      (let ([vec (make-vector len)])
	(let ([env (extended-env-record proc-names vec old-env)])
	  (for-each
	   (lambda (pos ids bodies)
	     (vector-set! vec pos (box (closure ids bodies env))))
	   (iota len)
	   idss
	   bodiess)
	  (apply-k k env))))))

(define iota
  (lambda (n)
    (let loop ([n (- n 1)] [ls '()])
      (if (zero? n)
	  (cons 0 ls)
	  (loop (- n 1) (cons n ls))))))

					; succeed and fail are procedures applied if the var is or isn't found, respectively
(define apply-env-ref
  (lambda (env sym k fail) ; k (succeed) and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
	   [empty-env-record ()
			     (fail)]
	   [extended-env-record (syms vals env)
			(let ((pos (list-find-position sym syms)))
				(if (number? pos)
					(apply-k k (vector-ref vals pos))
					(apply-env-ref env sym k fail)))])))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     [(null? ls) #f]
     [(pred (car ls)) 0]
     [else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))])))



; takes a variable and an associated value
; a list of symbols, and a vector of values
; returns a list whose members are:
;         syms with var added to the front
;         vals with val added the front
(define add-to-list
  (lambda (var val syms vals)
    (let ([pos (list-find-position var syms)])
      (if (number? pos)
	  (begin
	    (vector-set! vals pos val)
	    (list syms vals))
	  (list (cons var syms) (list->vector (cons val (vector->list vals))))))))

(define extend-global-env
  (lambda (var val)
    (cases environment global-env
	   [empty-env-record ()
			     (eopl:error 'extend-global-env "Bad global environment")]
	   [extended-env-record (syms vals env)
				(let ([data (add-to-list var (box val) syms vals)])
				  (extended-env-record (car data) (cadr data) env))])))