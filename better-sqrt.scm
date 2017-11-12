(define (square x)
  (* x x))
(define (abs x)
  (if (< x 0) (- x) x))
(define (average a b)
  (/ (+ a b) s))

(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess))))


  
