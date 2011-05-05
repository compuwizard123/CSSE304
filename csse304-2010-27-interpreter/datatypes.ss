; Datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (ids (lambda (x) (or (var-type? x) ((my-list-of var-type?) x) ((my-list-of symbol?) x))))
   (bodies (list-of expression?))
   (env environment?)]
	[continuation-proc
	 (k continuation?)])
   
(define-datatype var-type var-type?
	[var
		(id (lambda (x) (or (symbol? x) (procedure? x))))]
	[ref-var
		(id symbol?)])

;; Parsed expression datatypes
(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
  (var-exp
   (id (lambda (x) (or (symbol? x) (procedure? x)))))
  (list-exp
   (items (list-of expression?)))
  (lambda-exp
   (ids (lambda (x) (or (var-type? x) ((my-list-of var-type?) x) ((my-list-of symbol?) x))))
   (bodies (list-of expression?)))
  (if-exp
   (comp expression?) 
   (first expression?)
   (second expression?))
  (if-then-exp
   (comp expression?)
   (first expression?))
  (app-exp
   (rator expression?)
   (rand (list-of expression?)))
  (lit-exp
   (datum
   (lambda (x)
     (ormap 
      (lambda (pred) (pred x))
      (list number? vector? boolean? symbol? string? pair? null?)))))
  (set!-exp
   (var symbol?)
   (val expression?))
  (set!-list-exp
   (type expression?)
   (var expression?)
   (val expression?))
  (let-exp
   (syms (list-of symbol?))
   (exps (list-of expression?))
   (bodies (list-of expression?)))
  (let-named-exp
   (name symbol?)
   (syms (list-of symbol?))
   (exps (list-of expression?))
   (bodies (list-of expression?)))
  (let*-exp
   (syms (list-of symbol?))
   (exps (list-of expression?))         
   (bodies (list-of expression?)))
  (letrec-exp
   (proc-names (list-of symbol?))
   (idss (list-of (list-of symbol?)))
   (bodiess (list-of (list-of expression?)))
   (letrec-bodies (list-of expression?)))
  (improper-list-exp
   (body (my-list-of expression?)))
  (and-exp
   (tests (list-of expression?)))
  (or-exp
   (tests (list-of expression?)))
  (while-exp
   (test expression?)
   (bodies (list-of expression?)))
  (begin-exp
   (bodies (list-of expression?)))
  (cond-exp
   (tests (list-of expression?))
   (results (list-of expression?)))
  (case-exp
   (item expression?)
   (tests (list-of (list-of expression?)))
   (results (list-of expression?)))
	(load-exp
	 (path string?))
	(define-exp
		(var symbol?)
		(val expression?))
	(exit-exp
		(bodies (list-of expression?)))
  (empty
   (null null?)))

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (lambda (id) (or ((my-list-of symbol?) id) (symbol? id))))
   (vals (lambda (vals) (vector? vals)))
   (env environment?)))

(define-datatype continuation continuation?
  [ident-k]
	[define-exp-k
		(var symbol?)]
  [if-exp-k
   (then-exp expression?)
   (else-exp expression?)
   (env environment?)
   (k continuation?)]
	[if-then-exp-k
   (then-exp expression?)
   (env environment?)
   (k continuation?)]
  [rator-k
   (rands (list-of expression?))
   (env environment?)
   (k continuation?)]
  [rands-k 
   (proc-value scheme-value?)
   (k continuation?)]
	[extend-env-recur-k
		(letrec-bodies (list-of expression?))
		(k continuation?)]
	[letrec-exp-k
	 (k continuation?)]
  [apply-env-k
		(k continuation?)]
	[apply-env-ref-k
		(k continuation?)]
	[eval-lambda-vars-k
	 (proc-val proc-val?)
	 (k continuation?)]
	[app-exp-k
	 (rands (list-of expression?))
	 (env environment?)
	 (k continuation?)]
	[while-exp-k
	 (test expression?)
	 (bodies (list-of expression?))
	 (env environment?)
	 (k continuation?)]
	[while-loop-k
	 (test expression?)
	 (bodies (list-of expression?))
	 (env environment?)
	 (k continuation?)]
)