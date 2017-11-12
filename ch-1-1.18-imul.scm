 ;; Exercise 1.18. Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative process for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic number of steps.40 

(define (double n)
  (define (dbl a b)
    (if (= b 0)
	0
	(+ a (* a (- b 1)))))
  (dbl n 2))

(define (halve n)
  (/ n 2));;;this should be implemented without division

(define (even? n)
  (= (remainder n 2) 0))

(define (mul a b)
  (define (iter-mul accumulator a b)
    (display a) (display " ") (display b) (display " ") (display accumulator)
    (newline)
    (cond ((= b 0) accumulator)
	  ((even? b) (iter-mul accumulator (double a) (halve b)))
	  (else (iter-mul (+ a accumulator) a (- b 1)))))
  (iter-mul 0 a b))
