;; *Exercise 2.38:* The `accumulate' procedure is also known as
;; `fold-right', because it combines the first element of the
;; sequence with the result of combining all the elements to the
;; right.  There is also a `fold-left', which is similar to
;; `fold-right', except that it combines elements working in the
;; opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))
;; What are the values of

(fold-right / 1 (list 1 2 3))
3/2

(fold-left / 1 (list 1 2 3))
1/6

(fold-right list '() (list 1 2 3))
(1 (2 (3 ())))

(fold-left list '() (list 1 2 3))
(((() 1) 2) 3)

;; Give a property that `op' should satisfy to guarantee that
;; `fold-right' and `fold-left' will produce the same values for any
;; sequence.

;;op needs to satisfy the commutative property

(fold-right + 1 (list 1 2 3))
7

(fold-left + 1 (list 1 2 3))
7

(fold-right - 1 (list 1 2 3))
1

(fold-left - 1 (list 1 2 3))
-5

(fold-right * 1 (list 1 2 3))
6

(fold-left * 1 (list 1 2 3))
6

;; *Exercise 2.39:* Complete the following definitions of `reverse'
;; (*Note Exercise 2-18::) in terms of `fold-right' and `fold-left'
;; from *Note Exercise 2-38:::

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(reverse (list 1 2 3))
(3 2 1)


(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(reverse (list 1 2 3))
(3 2 1)



