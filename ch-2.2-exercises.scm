(cons 1
      (cons 2
	    (cons 3
		  (cons 4 nil))))

(list 1 2 3 4)

(car (cddr (list 1 2 3 4)))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define test
  (list 0 1 2 3 4 5 6 7))

;; test
;; (0 1 2 3 4 5 6 7)

;; (list-ref test 0)
;; 0

;; (list-ref test 3)
;; 3

;; (list-ref test 7)
;; 7

;; (list-ref test 12)
;; ERRORError: retort-syntax


(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;; (length test)
;; 8

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))


(count-leaves test)
(define x (cons (list 1 2) (list 3 4)))
(length x)
3

(count-leaves x)
4

(length (list x x))
2

(count-leaves (list x x))
8


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; (append test test)
;; (0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7)

;; *Exercise 2.17:* Define a procedure `last-pair' that returns the
;; list that contains only the last element of a given (nonempty)
;; list:

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

;; (last-pair (list 23 72 149 34))
;; (34)

;; (last-pair test)
;; (7)

;; *Exercise 2.18:* Define a procedure `reverse' that takes a list as
;; argument and returns a list of the same elements in reverse order:

(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

;; (reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)



;; Exercise 2.19: change-counting

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))


(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

;; (cc 100 us-coins)
;; 292
;; (cc 500 us-coins)


;; (cc 100 uk-coins)

;; (cc 27 uk-coins)
;; 728

;; ;;Order does not matter.  See:
;; (cc 100 us-coins)
;; 292
;; (cc 100 (reverse us-coins))
;; 292


(define (same-parity x . y)
  (define (iter l y)
    (if (null? y)
	l
	(if (equal? (remainder (car l) 2)
		    (remainder (car y) 2))
	    (iter (append l
			  (list (car y)))
		  (cdr y))
	    (iter l
		  (cdr y)))))
  (iter (list x) y))

;; (same-parity 1 2 3 4 5 6 7)
;; (1 3 5 7)

;; (same-parity 2 3 4 5 6 7 8)
;; (2 4 6 8)

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
	    (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
	    (map proc (cdr items)))))

(map (lambda (x) (* x x))
     test)
(1 4 9 16 25 36 49)

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(scale-list test 5)
(5 10 15 20 25 30 35)


;; *Exercise 2.21:* The procedure `square-list' takes a list of
;; numbers as argument and returns a list of the squares of those
;; numbers.

;; (square-list (list 1 2 3 4))
;; (1 4 9 16)

;; Here are two different definitions of `square-list'.  Complete
;; both of them by filling in the missing expressions:

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (square-list (cdr items)))))
(square-list (list 1 2 3 4))
(1 4 9 16)


(define (square-list items)
  (map square items))

(square-list (list 1 2 3 4))
(1 4 9 16)


(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(square-list (list 1 2 3 4))
(16 9 4 1)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items '()))

(square-list (list 1 2 3 4))
((((() . 1) . 4) . 9) . 16)

;; First  function above returns list in reverse of desired order because
;; in the (cons call (square (Car things)) comes before answer. It can
;; be fixed using append.  

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
		      (list (square (car things)))))))
  (iter items '()))

(square-list (list 1 2 3 4))
(1 4 9 16)

;; *Exercise 2.23:* The procedure `for-each' is similar to `map'.  It
;; takes as arguments a procedure and a list of elements.  However,
;; rather than forming a list of the results, `for-each' just applies
;; the procedure to each of the elements in turn, from left to right.
;; The values returned by applying the procedure to the elements are
;; not used at all--`for-each' is used with procedures that perform
;; an action, such as printing.  For example,

;; (for-each (lambda (x) (newline) (display x))
;; 	  (list 57 321 88))
;; 57
;; 321
;; 88

;; The value returned by the call to `for-each' (not illustrated
;;  above) can be something arbitrary, such as true.  Give an
;; implementation of `for-each'.


(define (for-each proc items)
  (cond ((null? items) '())
	(else (proc (car items))
	      (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
	  (list 57 321 88))

 
;; *Exercise 2.24:* Suppose we evaluate the expression `(list 1 (list
;;    2 (list 3 4)))'.  Give the result printed by the interpreter, the
;;    corresponding box-and-pointer structure, and the interpretation of
;;    this as a tree (as in *Note Figure 2-6::).

(list 1 (list 2 (list 3 4)))
(1 (2 (3 4)))

;;                              ((2 (3 4)))
;; (1 (2 (3 4))) --> [*][*]---> [*][/] 
;;                    |          |          
;;                    1          (2 (3 4))   ((3 4))
;;                               [*][*]----> [*][/]
;;                                |           |         
;;                                2    (3 4) [*][*]--->[*][/]
;;                                            |         |
;;                                            3         4

;; (1 (2 (3 4)))
;;     |   \
;;     1    (2 (3 4))
;;            /  \
;;           2  (3 4)
;;               /\
;;              3  4

;; *Exercise 2.25:* Give combinations of `car's and `cdr's that will
;; pick 7 from each of the following lists:

(car (cdr (car (cddr (list 1 3 (list 5 7) 9)))))

(caar (list (list 7)))

(1 (2 (3 (4 (5 (6 7))))))
(define x (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))

;; *Exercise 2.26:* Suppose we define `x' and `y' to be two lists:

;;        (define x (list 1 2 3))

;;        (define y (list 4 5 6))

;;   What result is printed by the interpreter in response to
;;   evaluating each of the following expressions:

;;        (append x y)

;;        (cons x y)

;;        (list x y)

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(1 2 3 4 5 6)

(cons x y)
((1 2 3) 4 5 6) 

(list x y)
((1 2 3) (4 5 6))

;; *Exercise 2.27:* Modify your `reverse' procedure of *Note Exercise
;; 2-18:: to produce a `deep-reverse' procedure that takes a list as
;; argument and returns as its value the list with its elements
;; reversed and with all sublists deep-reversed as well.  For example,

(define x (list (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

(define (deep-reverse items)
    (if (null? items)
	items
	(append (deep-reverse (cdr items))
		(if (not (pair? (car items)))
		    (list (car items))
		    (list (deep-reverse (car items)))))))
	
	 
x
((1 2) (3 4))

(reverse x)
((3 4) (1 2))

(deep-reverse x)
((4 3) (2 1))


;; *Exercise 2.28:* Write a procedure `fringe' that takes as argument
;; a tree (represented as a list) and returns a list whose elements
;; are all the leaves of the tree arranged in left-to-right order.
;; For example,

(define x (list (list 1 2) (list 3 4)))
x
((1 2) (3 4))

;;  ((1 2) (3 4))
;;       / \
;;    (1 2) (3 4)
;;    /  \   / \
;;   1   2  3   4
;;

(define (fringe items)
  (cond ((null? items) '())
	((not (pair? items)) (list items))
	(else (append (fringe (car items))
		      (fringe (cdr items))))))

(car x) (car (cdr x))

(cdr x)
((3 4))


(fringe x)
(1 2 3 4)

(fringe (list x x))
(1 2 3 4 1 2 3 4)

;; *Exercise 2.29:* A binary mobile consists of two branches, a left
;; branch and a right branch.  Each branch is a rod of a certain
;; length, from which hangs either a weight or another binary mobile.
;; We can represent a binary mobile using compound data by
;; constructing it from two branches (for example, using `list'):

(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a `length' (which must be a number)
;; together with a `structure', which may be either a number
;; (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

;; a. Write the corresponding selectors `left-branch' and
;;    `right-branch', which return the branches of a mobile, and
;;    `branch-length' and `branch-structure', which return the
;;    components of a branch.

;; b. Using your selectors, define a procedure `total-weight' that
;;    returns the total weight of a mobile.

;; c. A mobile is said to be "balanced" if the torque applied by
;;    its top-left branch is equal to that applied by its top-right
;;    branch (that is, if the length of the left rod multiplied by
;;    the weight hanging from that rod is equal to the
;;    corresponding product for the right side) and if each of the
;;    submobiles hanging off its branches is balanced. Design a
;;    predicate that tests whether a binary mobile is balanced.

;; d. Suppose we change the representation of mobiles so that the
;;    constructors are

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;; How much do you need to change your programs to convert to
;; the new representation?

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))
   
(define x (make-mobile 3 4))
(right-branch x)

(define (is-balanced? mobile)
  (= (length 
