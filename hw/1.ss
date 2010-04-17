; Kevin Risden
; Assignment 1
;
; #1
(define Fahrenheit->Celsius
    (lambda (temperature)
      (* (/ 5 9) (- temperature 32))))
; #2
(define interval-contains?
    (lambda (interval number)
      (let ((lower (list-ref interval 0)) (upper (list-ref interval 1)))
      (if (or (< number lower) (> number upper)) #f #t))))
; #3
(define interval-intersects?
    (lambda (i1 i2)
      (let ((l1 (list-ref i1 0)) (u1 (list-ref i1 1)) (l2 (list-ref i2 0)) (u2 (list-ref i2 1)))
      (if (> l2 u1) #f (if (< u2 l1) #f #t)))))
; #4
(define interval-union
   (lambda (i1 i2)
     (if (interval-intersects? i1 i2)
         (let ((l1 (list-ref i1 0))
             (u1 (list-ref i1 1))
             (l2 (list-ref i2 0))
             (u2 (list-ref i2 1)))
             (if (< l1 l2) 
                 (if (> u1 u2) (list (list l1 u1)) (list (list l1 u2)))
                 (if (> u1 u2) (list (list l2 u1)) (list (list l2 u2)))))
         (list i1 i2))))
; #5
(define divisible-by-7?
   (lambda (number)
	(if (= 0 (modulo number 7)) #t #f)))
; #6
(define ends-with-7?
   (lambda (number)
   (if (= 0 (modulo (- number 7) 10)) #t #f)))