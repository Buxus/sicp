(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (identity x) x)

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

;; (define (sum? x)
;;   (and (pair? x) (eq? (car x) '+)))

;; (define (addend s) (cadr s))

;; (define (augend s)
;;   (accumulate make-sum 0 (cddr s)))

;; (define (product? x)
;;   (and (pair? x) (eq? (car x) '*)))

;; (define (multiplier p) (cadr p))

;; (define (multiplicand p) (accumulate make-product 1 (cddr p)))

;; (deriv '(+ x 3) 'x)
;; (+ 1 0)

;; (deriv '(* x y) 'x)
;; (+ (* x 0) (* 1 y))

;; (deriv '(* (* x y) (+ x 3)) 'x)
;; (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

;; (deriv '(* x 2) 'x)
;; (+ (* x 0) (* 1 2))

;; (define (make-sum a1 a2)
;;   (cond ((=number? a1 0) a2)
;; 	((=number? a2 0) a1)
;; 	((and (number? a1) (number? a2)) (+ a1 a2))
;; 	(else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; (define (make-product m1 m2)
;;   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;; 	((=number? m1 1) m2)
;; 	((=number? m2 1) m1)
;; 	((and (number? m1) (number? m2)) (* m1 m2))
;; 	(else (list '* m1 m2))))

;; (deriv '(+ x 3) 'x)
;; 1

;; (deriv '(* x y) 'x)
;; y

;; (deriv '(* (* x y) (+ x 3)) 'x)
;; (+ (* x y) (* y (+ x 3)))

;; (deriv '(* x 3) 'x)
;; 3

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

;; (define (base e) (cadr e))

;; (define (exponent e) (caddr e))

;; (define (exponentiation? x)
;;   (and (pair? x) (eq? (car x) '**)))

;; (define (make-exponentiation base exponent)
;;   (cond ((=number? exponent 0) 1)
;; 	((=number? exponent 1) base)
;; 	((and (number? base) (number? exponent)) (expt base exponent))
;; 	(else (list '** base exponent))))

;; (deriv '(** x 3) 'x)
;; (* 3 (** x 2))

;; (deriv '(** x 2) 'x)
;; (* 2 x)

;; (deriv '(* (** x 2) (+ x 1)) 'x)
;; (deriv '(+ (** x 2) (* (* 2 x) (+ x 1))) 'x)
;; (deriv '(+ (* 2 x) (+ (* 2 x) (* 2 (+ x 1)))) 'x)
;; 6

;; (deriv '(** x 1) 'x)
;; 1

;; *Exercise 2.57:* Extend the differentiation program to handle sums
;; and products of arbitrary numbers of (two or more) terms.  Then
;; the last example above could be expressed as

;; (deriv '(* x y (+ x 3)) 'x)

;; Try to do this by changing only the representation for sums and
;; products, without changing the `deriv' procedure at all.  For
;; example, the `addend' of a sum would be the first term, and the
;; `augend' would be the sum of the rest of the terms.


;; *Exercise 2.58:* Suppose we want to modify the differentiation
;; program so that it works with ordinary mathematical notation, in
;; which `+' and `*' are infix rather than prefix operators.  Since
;; the differentiation program is defined in terms of abstract data,
;; we can modify it to work with different representations of
;; expressions solely by changing the predicates, selectors, and
;; constructors that define the representation of the algebraic
;; expressions on which the differentiator is to operate.

;;   a. Show how to do this in order to differentiate algebraic
;;      expressions presented in infix form, such as `(x + (3 * (x +
;;      (y + 2))))'.  To simplify the task, assume that `+' and `*'
;;      always take two arguments and that expressions are fully
;;      parenthesized.

(define (addend s) (car s))

(define (augend s) (simplify (cddr s)))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (simplify (cddr p)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

(define (base e) (car e))

(define (exponent e) (caddr e))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	((and (number? base) (number? exponent)) (expt base exponent))
	(else (list base '** exponent))))

;; (deriv '(x + (3 * (x + (y + 2)))) 'x)
;; 4


;;   b. The problem becomes substantially harder if we allow standard
;;      algebraic notation, such as `(x + 3 * (x + y + 2))', which
;;      drops unnecessary parentheses and assumes that multiplication
;;      is done before addition.  Can you design appropriate
;;      predicates, selectors, and constructors for this notation
;;      such that our derivative program still works?


(define (simplify exp)
  (if (null? (cdr exp))
      (car exp)
      exp))


;; (deriv '(x + 3 * (x + y + 2)) 'x)
;; 4
;; This doesn't actually work right
