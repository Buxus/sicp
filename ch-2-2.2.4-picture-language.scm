(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;; *Exercise 2.44:* Define the procedure `up-split' used by
;; `corner-split'.  It is similar to `right-split', except that it
;; switches the roles of `below' and `beside'.

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))


;; *Exercise 2.45:* `Right-split' and `up-split' can be expressed as
;;  instances of a general splitting operation. Define a procedure
;;  `split' with the property that evaluating produces procedures
;;  `right-split' and `up-split' with the behaviors as the ones already defined.

(define (split transform1 transform2)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split transform1 transform2) painter (- n 1))))
	  (transform1 painter (transform2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(paint (right-split einstein 2))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

;; *Exercise 2.46:* A two-dimensional vector v running from the
;; origin to a point can be represented as a pair consisting of an
;; x-coordinate and a y-coordinate.  Implement a data abstraction for
;; vectors by giving a constructor `make-vect' and corresponding
;; selectors `xcor-vect' and `ycor-vect'.  In terms of your selectors
;; and constructor, implement procedures `add-vect', `sub-vect', and
;; `scale-vect' that perform the operations vector addition, vector
;; subtraction, and multiplying a vector by a scalar:

;; (x_1, y_1) + (x_2, y_2) = (x_1 + x_2, y_1 + y_2)
;; (x_1, y_1) - (x_2, y_2) = (x_1 - x_2, y_1 - y_2)
;; s * (x, y) = (sx, sy)

(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

;; (define test-vect (make-vect 1 3))
;; test-vect
;; (1 . 3)
;; (xcor-vect test-vect)
;; 1
;; (ycor-vect test-vect)
;; 3

(define (add-vect vect1 vect2)
  (cons (+ (xcor-vect vect1) (xcor-vect vect2))
	(+ (ycor-vect vect1) (ycor-vect vect2))))

;; (define test-vect2 (make-vect 2 4))
;; (add-vect test-vect test-vect2)
;; (3 . 7)


(define (sub-vect vect1 vect2)
  (cons (- (xcor-vect vect1) (xcor-vect vect2))
	(- (ycor-vect vect1) (ycor-vect vect2))))

;; (sub-vect test-vect test-vect2)
;; (-1 . -1)


(define (scale-vect s vect)
  (cons (* s (xcor-vect vect))
	(* s (ycor-vect vect))))

;; (scale-vect 5 test-vect)
;; (5 . 15)


;; *Exercise 2.47:* Here are two possible constructors for frames:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-origin frame)
  (car frame))

(define (frame-edge1 frame)
  (cadr frame))

(define (frame-edge2 frame)
  (caddr frame))

(define test-frame (make-frame (make-vect 1 2) (make-vect 3 4) (make-vect 2 3)))

;; (frame-origin test-frame)   (1 . 2)
;; (frame-edge1 test-frame)   (3 . 4)
;; (frame-edge2 test-frame)   (2 . 3)


(define (make-frame origin edge1 edge2)
   (cons origin (cons edge1 edge2)))

(define (frame-origin frame)
  (car frame))

(define (frame-edge1 frame)
  (cadr frame))

(define (frame-edge2 frame)
  (cddr frame))

;; (frame-origin test-frame)    (1 . 2)
;; (frame-edge1 test-frame)    (3 . 4)
;; (frame-edge2 test-frame)     (2 . 3)

;; For each constructor supply the appropriate selectors to produce an
;; implementation for frames.



;; *Exercise 2.48:* A directed line segment in the plane can be
;; represented as a pair of vectors--the vector running from the
;; origin to the start-point of the segment, and the vector running
;; from the origin to the end-point of the segment.  Use your vector
;; representation from *Note Exercise 2-46:: to define a
;; representation for segments with a constructor `make-segment' and
;; selectors `start-segment' and `end-segment'.

(define (make-segment start-vector end-vector) (cons start-vector end-vector))

(define (start-segment segment) (car segment))

(define (end-segment segment) (cdr segment))

;; *Exercise 2.49:* Use `segments->painter' to define the following
;; primitive painters:

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;;   a. The painter that draws the outline of the designated frame.
(define paint-frame-outline
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0 1))
			   (make-segment (make-vect 0 1) (make-vect 1 1))
			   (make-segment (make-vect 1 1) (make-vect 1 0))
			   (make-segment (make-vect 1 0) (make-vect 0 0)))))

;;   b. The painter that draws an "X" by connecting opposite corners
;;      of the frame.
(define paint-x
  (segments->painter (list (make-segment (make-vect 1 0) (make-vect 0 1))
			   (make-segment (make-vect 0 0) (make-vect 1 1)))))

;;   c. The painter that draws a diamond shape by connecting the
;;      midpoints of the sides of the frame.
(define paint-diamond
  (segments->painter (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
			   (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
			   (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
			   (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))

;;   d. The `wave' painter. 


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)   ; new `origin'
		     (make-vect 1.0 1.0)   ; new end of `edge1'
		     (make-vect 0.0 0.0))) ; new end of `edge2'

(define (shrink-to-upper-right painter)
  (transform-painter painter
		     (make-vect 0.5 0.5)
		     (make-vect 1.0 0.5)
		     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))


(define (tilt painter)
  (transform-painter painter
		     (make-vect 0.5 0.0)
		     (make-vect 1.0 0.5)
		     (make-vect -0.5 1.0)))

(define (zoom painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 2.0 0.0)
		     (make-vect 0.0 2.0)))

(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

;; *Exercise 2.50:* Define the transformation `flip-horiz', which
;; flips painters horizontally, and transformations that rotate
;; painters counterclockwise by 180 degrees and 270 degrees.

(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(define (rotate-ccw-180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate-ccw-270 painter)
  (rotate90 (rotate-ccw-180 painter)))

(define (rotate-ccw-360 painter)
  (rotate90 (rotate-ccw-270 painter)))

;; *Exercise 2.51:* Define the `below' operation for painters.
;; `Below' takes two painters as arguments.  The resulting painter,
;; given a frame, draws with the first painter in the bottom of the
;; frame and with the second painter in the top.  Define `below' in
;; two different ways--first by writing a procedure that is analogous
;; to the `beside' procedure given above, and again in terms of
;; `beside' and suitable rotation operations (from *Note Exercise
;; 						2-50::).

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-above
	   (transform-painter painter1
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0)))
	  (paint-below
	   (transform-painter painter2
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point)))
      (lambda (frame)
	(paint-above frame)
	(paint-below frame)))))

(define (below painter1 painter2)
  (rotate-ccw-270 (beside (rotate90 painter1)
			  (rotate90 painter2))))

(define (four-square painter1 painter2 painter3 painter4)
  (below (beside painter1 painter2)
	 (beside painter3 painter4)))

(define four-pepes
  (four-square pepe
	       (flip-horiz pepe)
	       (flip-vert pepe)
	       (flip-vert (flip-horiz pepe))))
