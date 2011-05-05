; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (cases expression form
	   [begin-exp (bodies)
		      (for-each top-level-eval (map syntax-expand bodies))]
	   [define-exp (var val)
			 (eval-exp (syntax-expand val) global-env (define-exp-k var))]	
	   [else
	    (eval-exp form (empty-env) (ident-k))]))) ;just add empty environment

; eval-exp is the main component of the interpreter
(define eval-exp
  (lambda (exp env k)
    (cases expression exp
	   [define-exp (var val)
			(eval-exp (syntax-expand val) global-env (define-exp-k var))]
		 [lit-exp (datum) (if (and (pair? datum) (not (null? (cdr datum))) (equal? (car datum) 'quote)) (apply-k k (cadr datum)) (apply-k k datum))]
	   [var-exp (id)
				(apply-env-ref env id  ; look up its value.
			    (apply-env-k k) ; procedure to call if id is in the environment 
			    (lambda ()
						(apply-env-ref global-env id
						(apply-env-k k)
					  (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
								   "variable not found in environment: ~s"
								   id)))))]
	   [list-exp (items)
		     (eval-rands items env k)]
	   [lambda-exp (ids bodies)
		    (apply-k k (closure ids bodies env))]
	   [if-exp (comp first second)
			(eval-exp (syntax-expand comp) env (if-exp-k first second env k))]
	   [if-then-exp (comp first)
			(eval-exp (syntax-expand comp) env (if-then-exp-k first env k))]
	   [begin-exp (bodies) ;NOT IN CPS
		      (for-each (lambda (x) (eval-exp x env k)) (map syntax-expand bodies))]
	   [app-exp (rator rands)
				(eval-exp rator env (app-exp-k rands env k))]
	   [letrec-exp (proc-names idss bodiess letrec-bodies)
			(extend-env-recursively proc-names idss bodiess env (extend-env-recur-k letrec-bodies k))]
	   [set!-exp (var val) ;NOT IN CPS
		     (set-box!
						(apply-env-ref env var ; look up its value.
							(apply-env-ref-k k) ; procedure to call if id is in the environment 
							(lambda () 
								(apply-env-ref global-env var
									(apply-env-ref-k k)
									(lambda () (eopl:error 'set!-apply-env ; procedure to call if id not in env
								   "variable not found in environment: ~s"
								   var)))))
			       (eval-exp (syntax-expand val) env k))]
	   [set!-list-exp (type var val)
			  (eval-exp type env (rator-k (list var val) env k))]
	   [while-exp (test bodies)
				(eval-exp test env (while-exp-k test bodies env k))]
	   [load-exp (path) ;;FIXME
		     (eval-file path env k)]
	   [else
	    (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list
(define eval-rands ;NOT IN CPS
  (lambda (rands env k)
		(let loop ([rands rands] [ls '()])
			(if (null? rands)
				(apply-k k (reverse ls))
				(loop (cdr rands) (cons (eval-exp (syntax-expand (car rands)) env (ident-k)) ls))))))
    ;(map (lambda (r) (eval-exp (syntax-expand r) env (ident-k))) rands)))
 

(define syntax-expand
  (lambda (exp)
    (cases expression exp
	   [and-exp (tests)
		    (if (null? tests)
			(lit-exp #t)
			(let and-expand ([tests tests])
			  (let ([temp (syntax-expand (car tests))])
			    (if (null? (cdr tests))
				temp
				(syntax-expand
				 (let-exp '(test) (list temp) (list 
							       (if-exp (var-exp 'test) (and-expand (cdr tests)) (var-exp 'test)))))))))]
	   [or-exp (tests)
		   (if (null? tests)
		       (lit-exp #f)
		       (let or-expand ([tests tests])
			 (if (null? (cdr tests))
			     (syntax-expand (car tests))
			     (syntax-expand
			      (let-exp
			       '(temp)
			       (list (syntax-expand (car tests)))
			       (list (if-exp (var-exp 'temp) (var-exp 'temp) (or-expand (cdr tests)))))))))]
	   [let-exp (syms exps bodies)
		    (app-exp (lambda-exp (parse-lambda-vars syms) (map syntax-expand bodies)) (map syntax-expand exps))]
	   [let*-exp (syms exps bodies)
		     (let let*-expand ([syms syms] [exps exps])
		       (if (null? (cdr syms))
			   (app-exp (lambda-exp (parse-lambda-vars (list (car syms))) (map syntax-expand bodies)) (list (syntax-expand (car exps))))
			   (app-exp (lambda-exp (parse-lambda-vars (list (car syms))) (list (let*-expand (cdr syms) (cdr exps)))) (list (syntax-expand (car exps))))))]
	   [let-named-exp (name syms exps bodies)
			  (app-exp
			   (letrec-exp 
			    (list name) 
			    (list syms) 
			    (list bodies) 
			    (list (var-exp name))) 
			   exps)]
	   [cond-exp (tests results)
		     (let cond-expand ([tests tests] [results results])
		       (if (null? (cdr tests))
			   (syntax-expand (car results))
			   (if-exp (syntax-expand (car tests)) (syntax-expand (car results)) (cond-expand (cdr tests) (cdr results)))))]
	   [case-exp (item tests results)
		     (let case-expand ([tests tests] [results results])
		       (if (null? (cdr tests))
			   (syntax-expand (car results))
			   (if-exp (app-exp (var-exp 'memv) (list item (list-exp (car tests)))) (syntax-expand (car results)) (case-expand (cdr tests) (cdr results)))))]
	   [else exp])))
		 
(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
	   [prim-proc (op) (apply-prim-proc op args k)]
	   [closure (ids bodies env)
		    (let ([bodies (map syntax-expand bodies)])
		      (cond
		       [(symbol? ids)
						(let ([new-env (extend-env (list ids) (vector args) env)])
							(let loop ([bodies bodies])
								(if (null? (cdr bodies))
							(eval-exp (car bodies) new-env)
							(begin
								(eval-exp (car bodies) new-env)
								(loop (cdr bodies))))))]
		       [(improper? ids)  
			(let* (
			       [pids (properize ids)]
			       [improp-cutoff (- (length pids) 1)]
			       [new-env (extend-env pids (list->vector (collect-args args improp-cutoff)) env)])
			  (let loop ([bodies bodies])
			    (if (null? (cdr bodies))
				(eval-exp (car bodies) new-env)
				(begin
				  (eval-exp (car bodies) new-env)
				  (loop (cdr bodies))))))]
		       [else    ; ids must be the normal list type expression.
			(let ([new-env (extend-env (eval-lambda-ids ids) (list->vector args) env)])
			  (let loop ([bodies bodies])
			    (if (null? (cdr bodies))
				(eval-exp (car bodies) new-env k)
				(begin
				  (eval-exp (car bodies) new-env k)
				  (loop (cdr bodies))))))]))]
		 [continuation-proc (k)
			(apply-k k (car args))]
  [else (error 'apply-proc
	       "Attempt to apply bad procedure: ~s" 
	       proc-value)])))

(define apply-k
  (lambda (k val)
    (cases continuation k
	   [ident-k ()
			val]
		 [define-exp-k (var)
			(set! global-env (extend-global-env var val))]
	   [if-exp-k (then-exp else-exp env k)
		   (if val 
		       (eval-exp (syntax-expand then-exp) env k)
		       (eval-exp (syntax-expand else-exp) env k))]
		 [if-then-exp-k (then-exp env k)
		   (if val 
		       (eval-exp (syntax-expand then-exp) env k))]
	   [rator-k (rands env k)
		    (eval-rands rands env (rands-k val k))]
	   [rands-k (proc-value k)
				(apply-proc proc-value val k)]
		 [extend-env-recur-k (letrec-bodies k)
			(eval-rands letrec-bodies val (letrec-exp-k k))]
		 [letrec-exp-k (k)
				(apply-k k (car val))]
	   [apply-env-k (k)
			(if (box? val)
			    (apply-k k (unbox val))
			    (apply-k k val))]
		 [apply-env-ref-k (k)
		    (apply-k k val)]
		 [eval-lambda-vars-k (proc-value k)
			(apply-proc proc-value val k)]
		 [app-exp-k (rands env k)
			(cases proc-val val
			  [closure (ids bodies closure-env)
				  (if (list? ids)
						(eval-lambda-vars ids rands env (eval-lambda-vars-k val k))
						(eval-rands rands env (rands-k val k)))]
			  [else
					(eval-rands rands env (rands-k val k))])]
		 [while-exp-k (test bodies env k)
			(if val
				(eval-rands bodies env (while-loop-k test bodies env k))
				(apply-k k val))]
		 [while-loop-k (test bodies env k)
			(eval-exp test env (while-exp-k test bodies env k))]
    [else (eopl:error "There is no continuation of that type.")])))
			
					; takes a list of args - of which the final n of which must put in a list, and rewrites the list
					; used for improper lambda bindings
					; (collect-args (1 2 3 4 5 6 7) 3) -> (1 2 3 4 (5 6 7))
(define collect-args
  (lambda (l n)
    (if (zero? n)
	(list l)
	(cons (car l) (collect-args (cdr l) (- n 1))))))

(define eval-lambda-ids
	(lambda (ids)
		(map
			(lambda (x)
				(if (var-type? x)
					(cases var-type x
						[var (id)
							id]
						[ref-var (id)
							id])
					x))
			ids)))

(define eval-lambda-vars
  (lambda (ids args env k)
		(if (null? ids)
			(apply-k k (list '() (vector)))
			(let ([new-args (list->vector args)])
				(begin
					(for-each
						(lambda (pos var-id arg)
							(if (var-type? var-id)
								(cases var-type var-id
									[var (id)
										(vector-set! new-args pos (eval-exp (syntax-expand arg) env (ident-k)))]
									[ref-var (id)
										(cases expression arg
											[var-exp (arg-id)
												(vector-set! new-args pos
													(apply-env-ref
														env
														arg-id
														(ident-k) ; procedure to call if id is in the environment 
														(lambda () 
															(apply-env-ref
																global-env
																arg-id
																(ident-k)
																(lambda () (eopl:error 'eval-lambda-vars ; procedure to call if id not in env
																	"variable not found in environment: ~s"
																	arg-id))))))]
											[else
												(eopl:error 'eval-lambda-vars "invalid args")])])
								(vector-set! new-args pos (eval-exp (syntax-expand arg) env (ident-k)))))
						(iota (length ids))
						ids
						args)
					(apply-k k (vector->list new-args)))))))

(define *prim-proc-names* '(+ - * < > <= >= add1 sub1 / car cdr cadr cddr caar caddr caadr caaar cdaar cddar cdddr cadar cdadr  < list cons = procedure? null? vector vector? eq? equal? eqv? not assq atom? length list->vector list? pair? vector->list make-vector vector-ref number? symbol? set-car! set-cdr! vector-set! apply map member memq memv max zero? printf eopl:pretty-print pretty-print error list-ref display newline call/cc))

(define init-env         ; for now, our initial environment 
  (extend-env            ; only contains procedure names.
   *prim-proc-names*   ; Recall that an environment associates a
   (list->vector (map prim-proc      ; value (not an expression) with a variable.
	*prim-proc-names*))
   (empty-env)))

(define global-env init-env)

(define reset-global-env
	(lambda ()
		(set! global-env
			(extend-env
				*prim-proc-names*
				(list->vector (map prim-proc *prim-proc-names*))
				(empty-env)))))

(define apply-prim-proc
  (lambda (prim-proc args k)
    (case prim-proc
      [(+) (apply-k k (+ (1st args) (apply + (cdr args))))]
      [(-) (apply-k k (- (1st args) (2nd args)))]
      [(*) (apply-k k (* (1st args) (2nd args)))]
      [(<) (apply-k k (< (1st args) (2nd args)))]
      [(>) (apply-k k (> (1st args) (2nd args)))]
      [(<=) (apply-k k (<= (1st args) (2nd args)))]
      [(>=) (apply-k k (>= (1st args) (2nd args)))]
      [(add1) (apply-k k (+ (1st args) 1))]
      [(sub1) (apply-k k (- (1st args) 1))]
      [(/) (apply-k k (apply / args))]
      [(car) (apply-k k (car (1st args)))]
      [(cdr) (apply-k k (cdr (1st args)))]
      [(cadr) (apply-k k (cadr (1st args)))]
      [(cddr) (apply-k k (cddr (1st args)))]
      [(caar) (apply-k k (caar (1st args)))]
      [(caddr) (apply-k k (caddr (1st args)))]
      [(caadr) (apply-k k (caadr (1st args)))]
      [(caaar) (apply-k k (caaar (1st args)))]
      [(cdaar) (apply-k k (cdaar (1st args)))]
      [(cddar) (apply-k k (cddar (1st args)))]
      [(cdddr) (apply-k k (cdddr (1st args)))]
      [(cadar) (apply-k k (cadar (1st args)))]
      [(cdadr) (apply-k k (cdadr (1st args)))]
      [(<) (apply-k k (< (1st args) (2nd args)))]
      [(list) (apply-k k args)]
      [(cons) (apply-k k (cons (1st args) (2nd args)))]
      [(=) (apply-k k (= (1st args) (2nd args)))]
      [(procedure?) (apply-k k (proc-val? (1st args)))]
      [(null?) (apply-k k (null? (1st args)))]
      [(vector) (apply-k k (apply vector args))]
      [(vector?) (apply-k k (vector? (1st args)))]
      [(eq?) (apply-k k (eq? (1st args) (2nd args)))]
      [(equal?) (apply-k k (equal? (1st args) (2nd args)))]
      [(eqv?) (apply-k k (eqv? (1st args)(2nd args)))]
      [(not) (apply-k k (not (1st args)))]
      [(assq) (apply-k k (assq (1st args) (2nd args)))]
      [(atom?) (apply-k k (atom? (1st args)))]
      [(length) (apply-k k (length (1st args)))]
      [(list->vector) (apply-k k (list->vector (1st args)))]
      [(list?) (apply-k k (list? (1st args)))]
      [(pair?) (apply-k k (pair? (1st args)))]
      [(vector->list) (apply-k k (vector->list (1st args)))]
      [(make-vector) (apply-k k (make-vector (1st args) (2nd args)))]
      [(vector-ref) (apply-k k (vector-ref (1st args) (2nd args)))]
      [(number?) (apply-k k (number? (1st args)))]
      [(symbol?) (apply-k k (symbol? (1st args)))]
      [(set-car!) (apply-k k (set-car! (1st args) (2nd args)))]
      [(set-cdr!) (apply-k k (set-cdr! (1st args) (2nd args)))]
      [(vector-set!) (apply-k k (vector-set! (1st args) (2nd args)))]
      [(apply) (apply-k k (apply-proc (1st args) (2nd args) k))] 
      [(map) (apply-k k (map-proc (1st args) (2nd args) k))]
      [(member) (apply-k k (member (1st args) (2nd args)))]
      [(memq) (apply-k k (memq (1st args) (2nd args)))]
      [(memv) (apply-k k (memv (1st args) (2nd args)))]
      [(max) (apply-k k (apply max args))]
      [(zero?) (apply-k k (zero? (1st args)))]
      [(printf) (apply-k k (printf (1st args) args))]
      [(eopl:pretty-print) (apply-k k (eopl:pretty-print (1st args)))]
      [(pretty-print) (apply-k k (pretty-print args))]
      [(error) (apply-k k (error args))]
      [(list-ref) (apply-k k (list-ref (1st args) (2nd args)))]
      [(display) (apply-k k (display (1st args)))]
      [(newline) (apply-k k (newline))]
			[(call/cc) (apply-proc (car args) (list (continuation-proc k) (lambda (v) v)) k)]
      [else (error 'apply-prim-proc 
		   "Bad primitive procedure name: ~s" 
		   prim-proc)])))

(define map-proc
  (lambda (proc lst k)
		(if (null? lst)
			'()
			(cons (apply-proc proc (list (car lst)) k) (map-proc proc (cdr lst))))))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([read (read)])
      (if (equal? read '(exit))
	  (display "Exited PLC Scheme interpreter\n")
	  (let ([answer (top-level-eval (syntax-expand (parse-exp read)))])
	    ;; TODO: are there answers that should display differently?
			(if (eqv? (void) answer)
				(rep)
				(begin
					(eopl:pretty-print answer)
					;(newline)
					(rep))))))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))








