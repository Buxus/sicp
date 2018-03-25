;; Exercise 2.2. Consider the problem of representing line segments in a plane. Each segment is represented as a pair of points: a starting point and an ending point. Define a constructor make-segment and selectors start-segment and end-segment that define the representation of segments in terms of points. Furthermore, a point can be represented as a pair of numbers: the x coordinate and the y coordinate. Accordingly, specify a constructor make-point and selectors x-point and y-point that define this representation. Finally, using your selectors and constructors, define a procedure midpoint-segment that takes a line segment as argument and returns its midpoint (the point whose coordinates are the average of the coordinates of the endpoints). To try your procedures, you'll need a way to print points:


(define (make-point x-point y-point)
  (cons x-point y-point))

(define (make-segment start-segment end-segment)
  (cons start-segment end-segment))

(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (point-a segment)
  (car segment))
(define (point-b segment)
  (cdr segment))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (midpoint-segment segment)
  (let ((midpoint-x (average (x-point (point-a segment))
			    (x-point (point-b segment))))
	(midpoint-y (average (y-point (point-a segment))
			    (x-point (point-b segment)))))
    (make-point midpoint-x midpoint-y)))

(define (average x y)
  (/ (+ x y) 2))

;; Exercise 2.3. Implement a representation for rectangles in a plane. (Hint: You may want to make use of Exercise 2-2.) In terms of your constructors and selectors, create procedures that compute the perimeter and the area of a given rectangle. Now implement a different representation for rectangles. Can you design your system with suitable abstraction barriers, so that the same perimeter and area procedures will work using either representation? 


;; ;;;Rectangle
;;     l
;;  A----B
;;  |    | w
;;  C----D
;;
;;
;; (((Ax . Ay) (Bx . By)) . ((Cx . Cy) . (Dx . Dy)))


(define (point-1 rectangle)
  (car (car rectangle)))
(define (point-2 rectangle)
  (cdr (car rectangle)))
(define (point-3 rectangle)
  (car (cdr rectangle)))
(define (point-4 rectangle)
  (cdr (cdr rectangle)))

(define (side-l rectangle)
  (make-segment (point-1 rectangle)
		(point-2 rectangle)))
(define (side-w rectangle)
  (make-segment (point-3 rectangle)
		(point-4 rectangle)))

(define (make-rectangle pointa pointb pointc pointd)
  (cons (cons pointa pointb) (cons pointc pointd)))

(define (side-length side)
  (sqrt (+ (square (- (x-point (point-a side))
		      (x-point (point-b side))))
	   (square (- (y-point (point-a side))
		      (y-point (point-b side)))))))

(define (area rectangle)
  (* (side-length (side-l rectangle))
     (side-length (side-w rectangle))))

(define (perimeter rectangle)
  (+ (* 2 (side-length (side-l rectangle)))
     (* 2 (side-length (side-w rectangle)))))

