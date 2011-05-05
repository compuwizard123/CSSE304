;; define-datatype.scm 

;; this line must be within 8 lines of the top of the file
'(let ((time-stamp "Time-stamp: <2001-06-08 10:36:53 dfried>"))
  (display (string-append
             "define-datatype.scm version J3 "
             (substring time-stamp 13 29)
             (string #\newline))))

;;; This is an r5rs-compliant datatype system.

;;; exports define-datatype, isa, cases, list-of?, always?
;;; test with (define-datatype:test-all)



;; new error reporting system added by mw Mon Apr 24 14:49:03 2000.
(define define-datatype:report-error eopl:error)
;   (lambda (symbol format . data)
;     ;; print the message
;     (eopl:printf "Error in ~s: " symbol)
;     (apply eopl:printf (cons format data))
;     (newline)
;     (eopl:error-stop)))  

(define define-datatype:reset-registries 'ignored)
(define define-datatype:is-a-type? 'ignored)
(define define-datatype:datatype-checker&registry-updater 'ignored)
(define define-datatype:case-checker 'ignored)

(let ((define-datatype:type-registry '())
      (define-datatype:variant-registry '()))  

  (set! define-datatype:reset-registries
    (lambda ()
      (set! define-datatype:type-registry '())
      (set! define-datatype:variant-registry '())
      #t))

  (set! define-datatype:is-a-type?
    (lambda (type-name)
      (memq type-name define-datatype:type-registry)))

  (set! define-datatype:datatype-checker&registry-updater
    (letrec ((set?
               (lambda (s)
                 (if (null? s) #t
                   (and (not (memq (car s) (cdr s))) (set? (cdr s)))))))
      (lambda (Type-name Variants)
        (if (not (symbol? Type-name))
          (define-datatype:report-error 'define-datatype
            " The data type name ~s is not an identifier."
            Type-name))
        (for-each
          (lambda (variant)
            (if (not (symbol? (car variant)))
              (define-datatype:report-error 'define-datatype
                (string-append
                  "(While defining the ~a datatype) "
                  "  The variant-name ~s is not an identifier.")
                Type-name (car variant))))
          Variants)
        (let ((variant-names (map car Variants)))
          (if (not (set? variant-names))
            (define-datatype:report-error 'define-datatype
              (string-append
                "(While defining the ~a datatype) "
                "  Some of the variant-names are repeated: ~s.")
              Type-name variant-names))
          (for-each
            (lambda (v)
              (cond  ;;; This assq cannot be changed.
                ((assq v define-datatype:variant-registry) =>
                 (lambda (pair)
                   (if (not (eq? (cdr pair) Type-name))
                     (define-datatype:report-error 'define-datatype
                       (string-append
                         "(While defining the ~a data type) "
                         "  The variant-name ~s has already been "
                         "  used as a variant name in ~s.")
                       Type-name v (cdr pair)))))))
            variant-names)
          (cond ;;; This assq could be a memq over variant names, only.
                ;;; but would reqire a third local registry.
            ((assq Type-name define-datatype:variant-registry) =>
             (lambda (pair)
               (define-datatype:report-error 'define-datatype
                 (string-append
                   "(While defining the ~a data type) "
                   "  The type name ~s has already been "
                   "  used as a variant name ~s in the "
                   "  data type ~s.")
                 Type-name Type-name (car pair) (cdr pair))))
            ((memq Type-name variant-names)
             (define-datatype:report-error 'define-datatype
               (string-append
                 "(While defining the ~a data type) "
                 "  Variant name is the same as the data type name.")
               Type-name)))
          (for-each
            (lambda (variant-name)
              (cond
                ((memq variant-name define-datatype:type-registry)
                 (define-datatype:report-error 'define-datatype
                   (string-append
                     "(While defining the ~a data type) "
                     "  The variant name ~s has already been "
                     "  used as a type name.")
                   Type-name variant-name))))
            variant-names)
          (set! define-datatype:variant-registry
            (append
              (map (lambda (v) (cons v Type-name)) variant-names)
              define-datatype:variant-registry))
          (cond 
            ((memq Type-name define-datatype:type-registry) =>
             (lambda (pair)
               (set-car! pair Type-name)))
            (else
              (set! define-datatype:type-registry
                (cons Type-name define-datatype:type-registry))))))))
  
  (set! define-datatype:case-checker
    (let ((remq-or-false
            (lambda (sym ls)
              (call-with-current-continuation
                (lambda (k)
                  (let f ((ls ls))
                    (cond ((null? ls) (k #f))
                      ((eq? (car ls) sym) (cdr ls))
                      (else (cons (car ls) (f (cdr ls)))))))))))
      (lambda (Type-value Type-name Expression clauses)
        (if (eq? Type-name Expression)
          (begin
            (define-datatype:report-error 'cases
              (string-append
                "The data type ~s should not be the same "
                "  as a lexical variable.")
              Type-name))
          (let ((variant-table (cdr Type-value)))
            (let f ((clauses* clauses)
                    (unused-variants (map car variant-table)))
              (if (null? clauses*)
                (if (not (null? unused-variants))
                  (begin
                    (define-datatype:report-error 'cases "Missing variant clauses for ~s."
                      unused-variants)))
                (let* ((head-clause (car clauses*))
                       (tail-clauses (cdr clauses*))
                       (purported-variant (car head-clause)))
                  (if (eq? purported-variant Expression)
                    (begin
                      (define-datatype:report-error 'cases
                        (string-append
                          "The variant name ~s should not be the same "
                          "  as a lexical variable.")
                        Expression))
                    (cond
                      ((and (null? tail-clauses) (eq? purported-variant 'else))
                 ; do nothing, we're fine
                       )                        
                      ((assq purported-variant variant-table)
                       =>
                       (lambda (p)
                         (let ((fields (cdr p))
                               (purported-fields (cadr head-clause))
                               (new-unused-variants-or-false
                                 (remq-or-false
                                   purported-variant
                                   unused-variants)))
                           (if (not (=
                                      (length fields)
                                      (length purported-fields)))
                             (begin
                               (define-datatype:report-error 'cases "Bad fields in ~s." head-clause)))
                           (if (not new-unused-variants-or-false)
                             (begin
                               (define-datatype:report-error 'cases "Duplicate variant clause: ~s."
                                 head-clause)))
                           (f tail-clauses new-unused-variants-or-false))))
                      (else
                       (define-datatype:report-error 'cases
                          "Bad clause: ~s."
                          head-clause)))))))))))))

;;; ------------------------------
;;; general helpers

(define always?
  (lambda (x) #t))

(define list-of
  (lambda (pred . l)
    (let ((all-preds (cons pred l)))
      (lambda (obj)
        (let loop ((obj obj) (preds '()))
          (or 
            ;; if list is empty, preds should be, too
            (and (null? obj) (null? preds))
            (if (null? preds)
                ;; if preds is empty, but list isn't, then recycle
                (loop obj all-preds)
                ;; otherwise check and element and recur.
                (and (pair? obj)
                     ((car preds) (car obj))
                     (loop (cdr obj) (cdr preds))))))))))