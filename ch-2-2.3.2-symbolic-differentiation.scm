(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product (make-product (exponent exp)
				     (make-exponentiation (base exp)
							  (- (exponent exp) 1)))
		       (deriv (base exp) var)))
	(else
	 (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; (define (make-sum a1 a2) (list '+ a1 a2))

;; (define (make-product m1 m2) (list '* m1 m2)) 

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)
(+ 1 0)

(deriv '(* x y) 'x)
(+ (* x 0) (* 1 y))

(deriv '(* (* x y) (+ x 3)) 'x)
(+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

(deriv '(* x 2) 'x)
(+ (* x 0) (* 1 2))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(deriv '(+ x 3) 'x)
1

(deriv '(* x y) 'x)
y

(deriv '(* (* x y) (+ x 3)) 'x)
(+ (* x y) (* y (+ x 3)))

(deriv '(* x 3) 'x)
3

;; *Exercise 2.56:* Show how to extend the basic differentiator to
;; handle more kinds of expressions.  For instance, implement the
;; differentiation rule

;;      n_1   n_2
;;      --- = ---  if and only if n_1 d_2 = n_2 d_1
;;      d_1   d_2

;; by adding a new clause to the `deriv' program and defining
;; appropriate procedures `exponentiation?', `base', `exponent', and
;; `make-exponentiation'.  (You may use the symbol `**' to denote
;; exponentiation.)  Build in the rules that anything raised to the
;; power 0 is 1 and anything raised to the power 1 is the thing
;; itself.

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	((and (number? base) (number? exponent)) (expt base exponent))
	(else (list '** base exponent))))

(deriv '(** x 3) 'x)
(* 3 (** x 2))

(deriv '(** x 2) 'x)
(* 2 x)

(deriv '(* (** x 2) (+ x 1)) 'x)
(deriv '(+ (** x 2) (* (* 2 x) (+ x 1))) 'x)
(deriv '(+ (* 2 x) (+ (* 2 x) (* 2 (+ x 1)))) 'x)
6

(deriv '(** x 1) 'x)
1

;; *Exercise 2.57:* Extend the differentiation program to handle sums
;; and products of arbitrary numbers of (two or more) terms.  Then
;; the last example above could be expressed as

;; (deriv '(* x y (+ x 3)) 'x)

;; Try to do this by changing only the representation for sums and
;; products, without changing the `deriv' procedure at all.  For
;; example, the `addend' of a sum would be the first term, and the
;; `augend' would be the sum of the rest of the terms.

