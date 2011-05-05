;; save the nicer chez behavior
(define chez-printf printf)
(define chez-pretty-print pretty-print)


;; use the nicer chez behavior for these
(define sllgen:pretty-print chez-pretty-print)
(define eopl:pretty-print chez-pretty-print)
(define define-datatype:pretty-print chez-pretty-print)


;;I do not want to get into the debugger:
(define eopl:error-stop (lambda () '()))

(define eopl:error error)

(load "define-datatype_2.ss")
