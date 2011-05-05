; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2009



(define load-all ; make it easy to reload during debugging
  (lambda ()
    (load "chez-init.ss")
    (load "datatypes.ss")
    (load "parse.ss")
    (load "env.ss")
		(load "file.ss")
    (load "interpreter.ss")))

(load-all)