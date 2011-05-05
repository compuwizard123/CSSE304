(define-syntax my-if
  (syntax-rules (then else)
    [(_ e1 then e2 else e3)
     (if e1 e2 e3)]
    [(_ e1 then e2)
     (if e1 e2)]))

(define-syntax ++
  (syntax-rules ()
    [(_ x) (begin (set! x (+ x 1)) x)]))

(define-syntax for
  (syntax-rules (:)
    [(_ (init : test : update ...) body ...)
     (begin 
       init
       (let loop ()
	 (if test
	     (begin
	       body ...
	       update ...
	       (loop)))))]))

(define l '())

(for ((set! i 0) : (< i 10) : (++ i))
       (display i)
       (newline))