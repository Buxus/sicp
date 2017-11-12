(define (fizzbuzz n)
  (define (fzbz-iter count)
    (if (not (= n count))
	(cond ((= (modulo n 3) 0) (display "Fizz"))
	      ((= (modulo n 5) 0) (display "Buzz"))
	      (else (print count)))
	(fzbz-iter (+1 count))))
  (fzbz-iter 1))
	
	
