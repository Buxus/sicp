;;  Exercise 1.31.

;; The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures.51 Write an analogous procedure called product that returns the product of the values of a function at points over a given range. Try both a recursive and an iterative approach. Show how to define factorial in terms of product. Also use product to compute approximations to using the formula52
;; π4=2⋅4⋅4⋅6⋅6⋅8⋯3⋅3⋅5⋅5⋅7⋅7⋯


(define (recursive-product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (recurvsive-product term (next a) next b))))

(define (iter-product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result
			  (term a)))))
  (iter a 1))

(define (approx-pi n)
  (define (term n) (* (/ (* 2 n)
			 (- (* 2 n)
			    1))
		      (/ (* 2 n)
			 (+ (* 2 n)
			    1))))
  (define (next x) (+ x 1))
  (* 2.0 (iter-product term 1 next n)))
  
