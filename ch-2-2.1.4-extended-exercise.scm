;; Alyssa postulates the existence of an abstract object called an
;; "interval" that has two endpoints: a lower bound and an upper bound.
;; She also presumes that, given the endpoints of an interval, she can
;; construct the interval using the data constructor `make-interval'.
;; Alyssa first writes a procedure for adding two intervals.  She reasons
;; that the minimum value the sum could be is the sum of the two lower
;; bounds and the maximum value it could be is the sum of the two upper
;; bounds:

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

;; Alyssa also works out the product of two intervals by finding the
;; minimum and the maximum of the products of the bounds and using them as
;; the bounds of the resulting interval.
;; (`Min' and `max' are primitives that find the minimum or maximum of any number of arguments.)

			;; Exercise 2.11

;;this version really sucks
;; (define (mul-interval x y)
;;    (let ((xlo (lower-bound x))
;;          (xhi (upper-bound x))
;;          (ylo (lower-bound y))
;;          (yhi (upper-bound y)))
;;    (cond ((and (>= xlo 0)
;;                (>= xhi 0)
;;                (>= ylo 0)
;;                (>= yhi 0))
;;           ; [+, +] * [+, +]
;;           (make-interval (* xlo ylo) (* xhi yhi)))
;;          ((and (>= xlo 0)
;;                (>= xhi 0)
;;                (<= ylo 0)
;;                (>= yhi 0))
;;           ; [+, +] * [-, +]
;;           (make-interval (* xhi ylo) (* xhi yhi)))
;;          ((and (>= xlo 0)
;;                (>= xhi 0)
;;                (<= ylo 0)
;;                (<= yhi 0))
;;           ; [+, +] * [-, -]
;;           (make-interval (* xhi ylo) (* xlo yhi)))
;;          ((and (<= xlo 0)
;;                (>= xhi 0)
;;                (>= ylo 0)
;;                (>= yhi 0))
;;           ; [-, +] * [+, +]
;;           (make-interval (* xlo yhi) (* xhi yhi)))
;;          ((and (<= xlo 0)
;;                (>= xhi 0)
;;                (<= ylo 0)
;;                (>= yhi 0))
;;           ; [-, +] * [-, +]
;;           (make-interval (min (* xhi ylo) (* xlo yhi))
;;                          (max (* xlo ylo) (* xhi yhi))))
;;          ((and (<= xlo 0)
;;                (>= xhi 0)
;;                (<= ylo 0)
;;                (<= yhi 0))
;;           ; [-, +] * [-, -]
;;           (make-interval (* xhi ylo) (* xlo ylo)))
;;          ((and (<= xlo 0)
;;                (<= xhi 0)
;;                (>= ylo 0)
;;                (>= yhi 0))
;;           ; [-, -] * [+, +]
;;           (make-interval (* xlo yhi) (* xhi ylo)))
;;          ((and (<= xlo 0)
;;                (<= xhi 0)
;;                (<= ylo 0)
;;                (>= yhi 0))
;;           ; [-, -] * [-, +]
;;           (make-interval (* xlo yhi) (* xlo ylo)))
;;          ((and (<= xlo 0)
;;                (<= xhi 0)
;;                (<= ylo 0)
;;                (<= yhi 0))
;;           ; [-, -] * [-, -]
;;           (make-interval (* xhi yhi) (* xlo ylo))))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))	
;; To divide two intervals, Alyssa multiplies the first by the
;; reciprocal of the second.  Note that the bounds of the reciprocal
;; interval are the reciprocal of the upper bound and the reciprocal of
;; the lower bound, in that order.

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))


;; *Exercise 2.7:* Alyssa's program is incomplete because she has not
;; specified the implementation of the interval abstraction.  Here is
;; a definition of the interval constructor:

(define (make-interval a b) (cons a b))

;; Define selectors `upper-bound' and `lower-bound' to complete the
;; implementation.

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))


;; *Exercise 2.8:* Using reasoning analogous to Alyssa's, describe
;; how the difference of two intervals may be computed.  Define a
;; corresponding subtraction procedure, called `sub-interval'.

;; The minimum value of the difference could be is the difference of the two lower bounds
;; and the maximum value it could be is the difference of the two upper bounds

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))

;; *Exercise 2.9:* The "width" of an interval is half of the
;; difference between its upper and lower bounds.  The width is a
;; measure of the uncertainty of the number specified by the
;; interval.  For some arithmetic operations the width of the result
;; of combining two intervals is a function only the widths of the
;; argument intervals, whereas for others the width of the
;; combination is not a function of the widths of the argument
;; intervals.  Show that the width of the sum (or difference) of two
;; intervals is a function only of the widths of the intervals being
;; added (or subtracted).  Give examples to show that this is not
;; true for multiplication or division.

(define i (make-interval 3 5))
(define j (make-interval 2 6))

(define (width-interval x)
  (* .5
     (- (upper-bound x) (lower-bound x))))

(width-interval i) ;;1
(width-interval j) ;;
2
(width-interval (add-interval i j))
;;3

(width-interval (sub-interval i j))
;;-1

(width-interval (div-interval i j))
;;.875

(width-interval (mul-interval i j))
;;7

;;width of mul/div did not change from j (2 . 4) to (2 . 6)
;;add and sub interval are same as directly adding widths of intervals


;; *Exercise 2.10:* Ben Bitdiddle, an expert systems programmer,
;; looks over Alyssa's shoulder and comments that it is not clear what
;; it means to divide by an interval that spans zero.  Modify
;; Alyssa's code to check for this condition and to signal an error
;; if it occurs.


(define (check-zero? interval)
  (cond ((= 0 (upper-bound interval)) #f)
	((= 0 (lower-bound interval)) #f)
	((and (> (upper-bound interval) 0) (< (lower-bound interval) 0)) #f)
	(else #t)))

(define (print-error interval interval2)
  (display "Error: operation on interval containing zero: ")
  (newline)
  (display interval)
  (newline)
  (display interval2)
  (newline)
  #f)

(define test (make-interval -2 5))
(define test2 (make-interval 2 5))
(define test3 (make-interval 0 4))

(check-zero? test)

(define (div-interval x y)
  (if (and (check-zero? x)
	   (check-zero? y))
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))
      (print-error x y)))

;; After debugging her program, Alyssa shows it to a potential user,
;; who complains that her program solves the wrong problem.  He wants
;; a program that can deal with numbers represented as a center value
;; and an additive tolerance; for example, he wants to work with
;; intervals such as 3.5 +/- 0.15 rather than [3.35, 3.65].  Alyssa
;; returns to her desk and fixes this problem by supplying an
;; alternate constructor and alternate selectors:

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c tolerance)
  (make-center-width c
		     (* tolerance c)))

(define (percent i)
  (/ (width i)
     (center i)))

(define test (make-center-percent 3 .05))
(percent test)
;; 0.049999999...

;; *Exercise 2.13:* Show that under the assumption of small
;; percentage tolerances there is a simple formula for the approximate
;; percentage tolerance of the product of two intervals in terms of
;; the tolerances of the factors.  You may simplify the problem by
;; assuming that all numbers are positive.

(define test2 (make-center-percent 4 .10))
(percent (mul-interval test test2))
;; 0.149

(define test3 (make-center-percent 4 .15))
(percent (mul-interval test test3))
;; .198

(define (tolerance-of-product x y)
  (+ (percent x)
     (percent y)))

(tolerance-of-product test test)
;;.09

(tolerance-of-product test test3)
;;.199


;; After considerable work, Alyssa P. Hacker delivers her finished
;; system.  Several years later, after she has forgotten all about
;; it, she gets a frenzied call from an irate user, Lem E. Tweakit.
;; It seems that Lem has noticed that the formula for parallel
;; resistors can be written in two algebraically equivalent ways:

;;       R_1 R_2
;;      ---------
;;      R_1 + R_2

;; and

;;            1
;;      -------------
;;      1/R_1 + 1/R_2

;; He has written the following two programs, each of which computes
;; the parallel-resistors formula differently:

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

;; *Exercise 2.14:* Demonstrate that Lem is right.  Investigate the
;; behavior of the system on a variety of arithmetic expressions.
;; Make some intervals A and B, and use them in computing the
;; expressions A/A and A/B.  You will get the most insight by using
;; intervals whose width is a small percentage of the center value.
;; Examine the results of the computation in center-percent form (see
;; *Note Exercise 2-12::).

(define a (make-interval 2 5))
(define b (make-interval 7 15))

;;
                                                                                                                                              ;; a a
;;
;;By hand:
;;par1 : (.8 . 12.5)
;;par2 : (1 . 2.5)

(par1 a a)
;; (.4 . 6.25)
(par2 a a)
;; (1. . 2.5)  

(percent (par1 a a))
;;  .8796992481203006
(center (par1 a a))
;; 3.325
;; [3.325 +/- 88%]

(center (par2 a a))
;; 1.75
(percent (par2 a a))
;; .4285714285714285
;; [1.75 +/- 43%]

;;
;; a b
;;

(par1 a b)
;; (.7000000000000001 . 8.333333333333332)
(par2 a b)
;; (1.5555555555555558 . 3.75)

(percent (par1 a b))
;; .8450184501845018
(center (par1 a b))
;; 4.516666666666666
;; [4.5 +/- 85%]


(percent (par2 a b))
;; .41361256544502617
(center (par2 a b))
;; 2.6527777777777777
;; [2.65 +/- 41%]




;; *Exercise 2.15:* Eva Lu Ator, another user, has also noticed the
;; different intervals computed by different but algebraically
;; equivalent expressions. She says that a formula to compute with
;; intervals using Alyssa's system will produce tighter error bounds
;; if it can be written in such a form that no variable that
;; represents an uncertain number is repeated.  Thus, she says,
;; `par2' is a "better" program for parallel resistances than `par1'.
;; Is she right?  Why?

;; Yes, she is right.  The errors get carried through multiple operations in the algorithm described by par1.  In par2, there are no repeated variables representing uncertain numbers.


;; *Exercise 2.16:* Explain, in general, why equivalent algebraic
;; expressions may lead to different answers.  Can you devise an
;; interval-arithmetic package that does not have this shortcoming,
;; or is this task impossible?  (Warning: This problem is very
;; difficult.)

;; The issue comes from the fact that in algebraically equivalent forms of an expression,
;; variables with uncertainties may be repeated.  In order to minimize this, when possible,
;; algorithms should be designed which do not repeat variables with uncertainties.
