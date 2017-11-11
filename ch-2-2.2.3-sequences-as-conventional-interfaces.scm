(define (filter predicate sequence)
  (cond ((null? sequence) '())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
	      '()
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))

(define (list-fibs n)
  (accumulate cons
	      '()
	      (map fib
		   (enumerate-interval 0 n))))

(map square (list-fibs 10))
(1 1 4 9 25 64 169 441 1156 3025 7921)


(define (fib n)
  (cond ((= n 0) 1)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define (list-fib-squares n)
  (accumulate cons
	      '()
	      (map square
		   (map fib
			(enumerate-interval 0 n)))))

(list-fib-squares 10)
(1 1 4 9 25 64 169 441 1156 3025 7921)


(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
	      1
	      (map square
		   (filter odd? sequence))))

(product-of-squares-of-odd-elements (enumerate-interval 1 5))
225

;; *Exercise 2.33:* Fill in the missing expressions to complete the
;; following definitions of some basic list-manipulation operations
;; as accumulations:

(define test-sequence (enumerate-interval 1 5))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(map square test-sequence)
 (1 4 9 16 25)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append test-sequence (enumerate-interval 6 10))
(1 2 3 4 5 6 7 8 9 10)

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length test-sequence)
5

(length (filter odd? (enumerate-interval 1 1000)))
500

;; *Exercise 2.34:* Evaluating a polynomial in x at a given value of
;; x can be formulated as an accumulation.  We evaluate the polynomial

;;      a_n r^n | a_(n-1) r^(n-1) + ... + a_1 r + a_0

;; using a well-known algorithm called "Horner's rule", which
;; structures the computation as

;;      (... (a_n r + a_(n-1)) r + ... + a_1) r + a_0

;; In other words, we start with a_n, multiply by x, add a_(n-1),
;; multiply by x, and so on, until we reach a_0.(3)

;; Fill in the following template to produce a procedure that
;; evaluates a polynomial using Horner's rule.  Assume that the
;; coefficients of the polynomial are arranged in a sequence, from
;; a_0 through a_n.

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ (* higher-terms x)
		   this-coeff))
	      0
	      coefficient-sequence))

;; For example, to compute 1 + 3x + 5x^3 + x^(5) at x = 2 you would
;; evaluate

(horner-eval 2 (list 1 3 0 5 0 1))
79


;; *Exercise 2.35:* Redefine `count-leaves' from section *Note
;; 2-2-2:: as an accumulation:

(define (count-leaves t)
  (accumulate +
	      0
	      (map (lambda (x) 1)
		   (enumerate-tree t))))

;; or

(define (count-leaves t)
  (accumulate (lambda (x y) (+ 1 y))
	      0
	      (enumerate-tree t)))

(define x (cons (list 1 2) (list 3 4)))

(define x (list (list 1 2) (list 1 2 3) 1))
(enumerate-tree x)
 (1 2 1 2 3 1)

(count-leaves x)
6


;; *exercise 2.36:* The procedure `accumulate-n' is similar to
;; `accumulate' except that it takes as its third argument a sequence
;; of sequences, which are all assumed to have the same number of
;; elements.  It applies the designated accumulation procedure to
;; combine all the first elements of the sequences, all the second
;; elements of the sequences, and so on, and returns a sequence of
;; the results.  For instance, if `s' is a sequence containing four
;; sequences, `((1 2 3) (4 5 6) (7 8 9) (10 11 12)),' then the value
;; of `(accumulate-n + 0 s)' should be the sequence `(22 26 30)'.
;; Fill in the missing expressions in the following definition of
;; `accumulate-n':

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 s)
(22 26 30)

(map cdr s)
((2 3) (5 6) (8 9) (11 12))
(map cddr s)
((3) (6) (9) (12))
(map car s)
(1 4 7 10)

