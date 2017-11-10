(define (scale-tree tree factor)
  (cond ((null? tree) '())
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
	    10)
(10 (20 (30 40) 50) (60 70))


(define (scale-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (scale-tree sub-tree factor)
	     (* sub-tree factor)))
       tree))


(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (square sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
(1 (4 (9 16) 25) (36 49))



;; *Exercise 2.31:* Abstract your answer to *Note Exercise 2-30:: to
;;  produce a procedure `tree-map' with the property that
;;  `square-tree' could be defined as

(define (square-tree tree) (tree-map square tree))

(define (tree-map factor tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (factor tree))
	(else (cons (tree-map factor (car tree))
		    (tree-map factor (cdr tree))))))

(define (tree-map factor tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map factor sub-tree)
	     (factor sub-tree)))
       tree))


;; *Exercise 2.32:* We can represent a set as a list of distinct
;; elements, and we can represent the set of all subsets of the set as
;; a list of lists.  For example, if the set is `(1 2 3)', then the
;; set of all subsets is `(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2
;; 3))'.  Complete the following definition of a procedure that
;; generates the set of subsets of a set and give a clear explanation
;; of why it works:

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define test (list 1 2 3))

(subsets test)
(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))



