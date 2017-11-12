

;; Exercise 2.5. Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a
;; and b as the integer that is the product 2a3b. Give the corresponding definitions of the procedures cons, car, and cdr.


(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car x)
  (num-times-divides x 2))

(define (cdr x)
  (num-times-divides x 3))


(define (num-times-divides n d)
  (define (iter quotient result)
    (if (= 0 (remainder quotient d))
	(iter (/ quotient d) (+ result 1))
	result))
  (iter n 0))
