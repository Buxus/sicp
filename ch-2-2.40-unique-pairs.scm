;; *Exercise 2.40:* Define a procedure `unique-pairs' that, given an
;; integer n, generates the sequence of pairs (i,j) with 1 <= j< i <=
;; n.  Use `unique-pairs' to simplify the definition of
;; `prime-sum-pairs' given above.

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

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

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))




(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;; (unique-pairs 5)
;; ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4)) 

;; (prime-sum-pairs 5)
;; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7)) 

;; *Exercise 2.41:* Write a procedure to find all ordered triples of
;; distinct positive integers i, j, and k less than or equal to a
;; given integer n that sum to a given integer s.


(define (ordered-triples n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
			(map (lambda (k)
			       (list i j k))
			     (enumerate-interval 1 (- j 1))))
		      (enumerate-interval 1 (- i 1))))	   
	   (enumerate-interval 1 n)))

;; (ordered-triples 5)
;; ((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3))

(define (ordered-quadruples n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
			(flatmap (lambda (k)
				   (map (lambda (l)
					  (list i j k l))
					(enumerate-interval 1 (- k 1))))
				 (enumerate-interval 1 (- j 1))))
		      (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

;; (ordered-quadruples 5)
;; ((4 3 2 1) (5 3 2 1) (5 4 2 1) (5 4 3 1) (5 4 3 2))


					      

;; (define (seq-sums-to-s? seq s)
;;   (= (accumulate + 0 seq)
;;      s))

(define (make-triple-sum triple)
  (append triple
	  (list (accumulate + 0 triple))))

;; (make-triple-sum '(1 2 3))
;; (1 2 3 6)

;; (seq-sums-to-s? '(1 2 3) 6)
;; #t


;; (define (triples-sum-to-s n s)
;;   (flatmap (lambda (i)
;; 	     (cond ((seq-sums-to-s? i s) i)))
;; 	   (ordered-triples n)))

(define (triples-sum-to-s n s)
  (define (triple-sum triple)
    (= s (accumulate + 0 triple)))
  (map make-triple-sum
       (filter triple-sum
	       (ordered-triples n))))


;; (define (triples-sum-to-s n s)
;;   (flatmap (lambda (seq)
;; 	     (filter seq-sums-to-s? seq))
;; 	   (ordered-triples n)))

(define (fizzbuzz n)
  (map (lambda (i)
	 (cond ((= 0 (remainder i 15)) (display "fizzbuzz"))
	       ((= 0 (remainder i 3))  (display "fizz"))
	       ((= 0 (remainder i 5))  (display "buzz"))
	       (else (display i)))
	 (display "\n"))
       (enumerate-interval 1 n)))
