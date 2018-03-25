;; (define (element-of-set? x set)
;;        (cond ((null? set) false)
;;              ((equal? x (car set)) true)
;;              (else (element-of-set? x (cdr set)))))


;; (define (adjoin-set x set)
;;   (if (element-of-set? x set)
;;       set
;;       (cons x set)))


;; (define (intersection-set set1 set2)
;;   (cond ((or (null? set1) (null? set2)) '())
;; 	((element-of-set? (car set1) set2)
;; 	 (cons (car set1)
;; 	       (intersection-set (cdr set1) set2)))
;; 	(else (intersection-set (cdr set1) set2))))

;; *Exercise 2.59:* Implement the `union-set' operation for the
;; unordered-list representation of sets.

;; (define (union-set set1 set2)
;;   (cond ((null? set1) set2)
;; 	((null? set2) set1)
;; 	((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
;; 	(else (cons (car set1) (union-set (cdr set1) set2)))))


;; (define test-set '(1 2 3))
;; (define test-set2 '(4 5 6))

;; (union-set test-set test-set2)
;; (3 2 1 4 5 6)


;; *Exercise 2.60:* We specified that a set would be represented as a
;; list with no duplicates.  Now suppose we allow duplicates.  For
;; instance, the set {1,2,3} could be represented as the list `(2 3 2
;; 1 3 2 2)'.  Design procedures `element-of-set?', `adjoin-set',
;; `union-set', and `intersection-set' that operate on this
;; representation.  How does the efficiency of each compare with the
;; corresponding procedure for the non-duplicate representation?  Are
;; there applications for which you would use this representation in
;; preference to the non-duplicate one?

(define test-set '(2 3 2 1 3 2 2))

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))
;; This function works as-is
;; (element-of-set? '2 test-set)
;; #t

;; (element-of-set? '1 test-set)
;; #t

;; (element-of-set? '4 test-set)
;; #f

(define (adjoin-set x set)
  (cons x set))

;; adjoin-set needed to be modded to just cons x and set
;; O(n) -> O(1)


;; (adjoin-set '2 test-set)
;; (2 2 3 2 1 3 2 2)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

;; (define test-set2 '(1 3 1 2 3))

;; (intersection-set test-set2 test-set)
;; (1 3 1 2 3)


(define (union-set set1 set2)
  (append set1 set2))

;; (union-set test-set test-set2)
;; (2 3 2 1 3 2 2 1 3 1 2 3)
;; O(n^2) -> O(n)

;; Sets as ordered lists


(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1
		     (intersection-set (cdr set1)
				       (cdr set2))))
	      ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set set1 (cdr set2)))))))

;; *Exercise 2.61:* Give an implementation of `adjoin-set' using the
;; ordered representation.  By analogy with `element-of-set?' show how
;; to take advantage of the ordering to produce a procedure that
;; requires on the average about half as many steps as with the
;; unordered representation.

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

;; (define odds '(1 3 5 7))
;; (adjoin-set '0 odds)
;; (0 1 3 5 7)
;; (adjoin-set '1 odds)
;; (1 3 5 7)
;; (adjoin-set '4 odds)
;; (1 3 4 5 7)
;; (adjoin-set '8 odds)
;; (1 3 5 7 8)

;; *Exercise 2.62:* Give a [theta](n) implementation of `union-set'
;; for sets represented as ordered lists.

;; (define (union-set set1 set2)
;;   (cond ((null? set1) set2)
;; 	((null? set2) set1)
;; 	(else (let ((x1 (car set1)) (x2 (car set2)))
;; 		(cond ((= x1 x2)
;; 		       (cons x1
;; 			     (union-set (cdr set1)
;; 					(cdr set2))))
;; 		      ((< x1 x2)
;; 		       (append (list x1)
;; 			       (list x2)
;; 			       (union-set (cdr set1)
;; 					  (cdr set2))))
;; 		      ((< x2 x1)
;; 		       (append (list x2)
;; 			       (list x1)
;; 			       (union-set (cdr set1)
;; 					  (cdr set2)))))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((= (car set1) (car set2))
	 (cons (car set1) (union-set (cdr set1) (cdr set2))))
	((< (car set1) (car set2))
	 (cons (car set1) (union-set (cdr set1) set2)))
	(else (cons (car set2) (union-set set1 (cdr set2))))))

;; (define (union-set set1 set2)
;;   (cond ((null? set1) set2)
;; 	((null? set2) set1)
;; 	((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
;; 	(else (cons (car set1) (union-set (cdr set1) set2)))))

;; (define evens '(2 4 6 8))
;; (union-set odds evens)
;;  (1 2 3 4 5 6 7 8)
;; (union-set evens odds)
;;  (1 2 3 4 5 6 7 8)

;; (union-set evens test-set)
;;  (2 3 2 1 3 2 2 4 6 8)
;; (union-set test-set evens)
;; (2 3 2 1 3 2 2 4 6 8)


(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (entry set)) true)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

;; *Exercise 2.63:* Each of the following two procedures converts a
;; binary tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

;; a. Do the two procedures produce the same result for every tree?
;; If not, how do the results differ?  What lists do the two
;; procedures produce for the trees in *Note Figure 2-16::?

;; figure 2.16 trees
;; {1,3,5,7,9,11}
;; (define tree1 (adjoin-set 11 (adjoin-set 9 (adjoin-set 5 (adjoin-set 1 (adjoin-set 3 (adjoin-set 7 '())))))))

;; (define tree2 (adjoin-set 11 (adjoin-set 9 (adjoin-set 5 (adjoin-set 7 (adjoin-set 1 (adjoin-set 3 '())))))))

;; (define tree3 (adjoin-set 1 (adjoin-set 7 (adjoin-set 11 (adjoin-set 3 (adjoin-set 9 (adjoin-set 5 '())))))))

;; tree1
;; (7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))
;; tree2
;; (3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))
;; tree3
;; (5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))

;; (tree->list-1 tree1)
;; (1 3 5 7 9 11)
;; (tree->list-2 tree1)
;; (1 3 5 7 9 11)

;; (tree->list-1 tree2)
;; (1 3 5 7 9 11)
;; (tree->list-2 tree2)
;; (1 3 5 7 9 11)

;; (tree->list-1 tree3)
;; (1 3 5 7 9 11)
;; (tree->list-2 tree3)
;; (1 3 5 7 9 11)

;; The two procedures return the same in-order traversal of each tree.  

;; b. Do the two procedures have the same order of growth in the
;; number of steps required to convert a balanced tree with n
;; elements to a list?  If not, which one grows more slowly?

;; tree->list-2 calls cons at each step which is O(1), therefore the algorithm has a growth of O(n).
;; tree->list-1 calls append at each step:

;; (define (append list1 list2)
;;   (if (null? list1)
;;       list2
;;       (cons (car list1) (append (cdr list1) list2))))

;; The order of growth of append is proportional to the size of the first argument passed to it,
;; which is the left-branch of the tree.  Therefore, at each step the complexity is reduced by half.
;; tree->list-1 therefore grows at O(nlogn) for a balanced tree.


;; *Exercise 2.64:* The following procedure `list->tree' converts an
;; ordered list to a balanced binary tree.  The helper procedure
;; `partial-tree' takes as arguments an integer n and list of at least
;; n elements and constructs a balanced tree containing the first n
;; elements of the list.  The result returned by `partial-tree' is a
;; pair (formed with `cons') whose `car' is the constructed tree and
;; whose `cdr' is the list of elements not included in the tree.

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

;; a. Write a short paragraph explaining as clearly as you can how
;; `partial-tree' works.  Draw the tree produced by `list->tree' for
;; the list `(1 3 5 7 9 11)'.

(define test-list '(1 3 5 7 9 11))
;; (list->tree test-list)
;; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;            5
;           / \
;          1   9
;         / \ / \  
;           3 7 11

;; (partial-tree test-list 0)
;; (() 1 3 5 7 9 11)

;; (partial-tree test-list 1)
;; ((1 () ()) 3 5 7 9 11)

;; (partial-tree test-list 2)
;; ((1 () (3 () ())) 5 7 9 11)

;; (partial-tree test-list 10)
;; ERRORError: geiser-debugger

;; partial-tree takes a list of elements and makes a tree out of the first n elements in the list.

;; b. What is the order of growth in the number of steps required by
;; `list->tree' to convert a list of n elements?

;; This procedure calls partial-tree which visits each element of the tree once, and applies cons
;; for each element.  This results in O(n) growth.

;; *Exercise 2.65:* Use the results of *Note Exercise 2-63:: and
;; *Note Exercise 2-64:: to give [theta](n) implementations of
;; `union-set' and `intersection-set' for sets implemented as
;; (balanced) binary trees.(5)

(define (union-set tree1 tree2)
  (define (union-list set1 set2)
    (cond ((null? set1) set2)
	  ((null? set2) set1)
	  ((= (car set1) (car set2))
	   (cons (car set1) (union-list (cdr set1) (cdr set2))))
	  ((< (car set1) (car set2))
	   (cons (car set1) (union-list (cdr set1) set2)))
	  (else (cons (car set2) (union-list set1 (cdr set2))))))
  (list->tree (union-list (tree->list-2 tree1)
			  (tree->list-2 tree2))))


(define (intersection-set tree1 tree2)
  (define (intersection-list set1 set2)
    (if (or (null? set1) (null? set2))
	'()
	(let ((x1 (car set1)) (x2 (car set2)))
	  (cond ((= x1 x2)
		 (cons x1
		       (intersection-list (cdr set1)
					  (cdr set2))))
		((< x1 x2)
		 (intersection-list (cdr set1) set2))
		((< x2 x1)
		 (intersection-list set1 (cdr set2)))))))
  (list->tree (intersection-list (tree->list-2 tree1)
				 (tree->list-2 tree2))))


(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((equal? given-key (key (car set-of-records)))
	 (car set-of-records))
	(else (lookup given-key (cdr set-of-records)))))

;; *Exercise 2.66:* Implement the `lookup' procedure for the case
;; where the set of records is structured as a binary tree, ordered
;; by the numerical values of the keys.

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
	((equal? given-key (car set-of-records))
	 (car set-of-records))
	((< given-key (car set-of-records))
	 (lookup given-key (cdr set-of-records)))
	((> given-key (car set-of-records))
	 (lookup given-key (cddr set-of-records)))))
