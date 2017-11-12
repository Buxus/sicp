;;; Newton's method for square roots

(define (my-sqrt x)
  (sqrt-iter 1.0 0.0 x))

(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess x)
      guess
      (sqrt-iter (improve guess x)
		 guess
		 x)))

(define (good-enough? guess prev-guess x)
  (< (abs (/ (- prev-guess guess) guess)) 0.0000005))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (< x 0) (- x) x))

(define (square x)
  (* x x))

;;; Exercise 1.8: Newton's method for cube roots

(define (my-cubic-root x)
  (cbrt-iter 1.0 0.0 x))

(define (cbrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess x)
      guess
      (cbrt-iter (improve-cbrt guess x)
		 guess
		 x)))

(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

