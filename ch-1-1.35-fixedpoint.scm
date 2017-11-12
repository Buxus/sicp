;; Exercise 1.35. Show that the golden ratio (section 1.2.2) is a fixed point of the transformation
;; x↦1+1x, and use this fact to compute ϕ by means of the fixed-point procedure.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;;  Exercise 1.37. An infinite continued fraction is an expression of the form
;; f=N1D1+N2D2+N3D3+⋯

;; As an example, one can show that the infinite continued fraction expansion with the Ni
;; and the Di all equal to 1 produces 1ϕ, where ϕ is the golden ratio (described in section 1.2.2). One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation — a so-called k-term finite continued fraction — has the form
;; f=N1D1+N2⋱+NkDk

;; Suppose that n and d are procedures of one argument (the term index i
;; that return the Ni and Di of the terms of the continued fraction. Define a procedure cont-frac such that evaluating (cont-frac n d k) computes the value of the k-term finite continued fraction. 

(define (cont-frac n d k)
  (define (helper i)
    (cond ((= i k) (/ (n i)
		      (d i)))
	  (else (/ (n i)
		   (+ (d i) (helper (+ i 1)))))))
  (helper 1))



(define (cont-frac-iterative n d k)
  (define (iter i result)
    (if (= i 0) result
	(iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))


 ;; Exercise 1.38. In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis, which included a continued fraction expansion for e−2, where e is the base of the natural logarithms. In this fraction, the Ni are all 1, and the Di are successively 1,2,1,1,4,1,1,6,1,1,8,… Write a program that uses your cont-frac procedure from exercise 1.37 to approximate e, based on Euler's expansion. 


(define (d i)
  (cond ((= (remainder (+ i 1) 3) 0)
	 (/ (* 2 (+ i 1)) 3))
	(else 1)))	  

(define e
  (+ 2 (cont-frac-iterative (lambda (x) 1.0)
			    d
			    10)))

(define (square x)
  (* x x))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
	x
	(- (square x))))
  (define (d i)
    (- (* 2 i) 1))
  
  (cont-frac n
	     d
	     k))
