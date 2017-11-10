;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ (* .5 a (square t))
       (* v t)
       u)))

;; you need to complete this procedure, then show some test cases

; (position 0 0 0 0) 0
; (position 0 0 20 0) 20
; (position 0 5 10 10) 60
; (position 2 2 2 2) 10
; (position 5 5 5 5) 92.5


;; Problem 2

(define discriminant
  (lambda (a b c)
    (- (square b)
       (* 4 a c))))

(define root1
  (lambda (a b c)
    (/ (+ (- b)
	  (sqrt (discriminant a b c)))
       (* 2 a))))

(define root2
  (lambda (a b c)
    (/ (- (- b)
	  (sqrt (discriminant a b c)))
       (* 2 a))))

;; complete these procedures and show some test cases

;; (root1 1 0 -16) -4
;; (root2 1 0 -16) 4
;; (root1 1 0 16) -4i
;; (root2 1 0 16) 4i


;; Problem 3
;; (define yt
;;   (lambda (v alpha t gravity h)
;;     (- (+ (* v
;; 	     (sin alpha)
;; 	     t)
;; 	  h)
;;        (* 0.5
;; 	  gravity
;; 	  (square t)))))


;; d = vit + 1/2 at2 <- Initial vertical velocity is zero so...
;; d = 1/2 at2
;; t = sqr root (2d/a) = sqr root (2 x 15.0 / 9.81)
;; t = 1.75 s

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root2 (* -0.5 gravity) vertical-velocity elevation)))

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (root2 (* -0.5 gravity) vertical-velocity (- elevation target-elevation))))

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (let ((rangle (degree2radian angle)))
      (let ((vy (* velocity
		   (sin rangle)))
	    (vx (* velocity
		   (cos rangle))))
	(let ((t0 (time-to-impact vy elevation)))
	  (let ((x0 (position 0 vx 0 t0)))
	    x0))))))

;; (travel-distance-simple 1 45 0)  Value: 20.32892781536815

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)

;;.4517539514526256 seconds, 20.329 m

;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)

;; (time-to-impact (* 45 (sin (degree2radian 90))) 1)
;; 9.205842177978788 seconds, 7.292620665419303e-10 m

;; at an angle of (/ pi 4) radians or 45 degrees

;; 6.52510983052591 seconds , 207.6278611514906 m



;; what is the distance traveled in each case?
;; record both in meters and in feet


;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 0.01)

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))

(define (iterative-improve good-enough? improve)
  (define (helper guess)
    (if (good-enough? guess)
	guess
	(helper (improve guess))))
  helper)

(define (find-best-angle velocity elevation)
  (define (good-enough? guess)
    (> (travel-distance-simple elevation velocity guess)
       (travel-distance-simple elevation velocity (improve guess))))
  (define (improve guess)
    ;;(+ guess alpha-increment))
  ((iterative-improve good-enough? improve) 1))

;; find best angle
;; ~45 degrees for all angles/heights
;; try for other velocities
;; try for other heights

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define dt 0.01)

(define integrate
  (lambda (x y u v dt g m beta)
    (if (< y 0)
	x
	(let ((speed (sqrt (+ (square u) (square v)))))
	  (let ((drag (* beta (square speed))))
	    (let ((dx (* u dt))
		  (dy (* v dt))
		  (du (* (/ -1 m) beta speed u dt))
		  (dv (* (* -1 (+ (* (/ 1 m) speed v beta)
				  gravity))
			 dt)))
	      (integrate
	       (+ x dx)
	       (+ y dy)
	       (+ u du)
	       (+ v dv)
	       dt g m beta)))))))

(define travel-distance
  (lambda (elevation initial-velocity angle)
    (let ((rangle (degree2radian angle)))
      (let ((u0 (* initial-velocity (cos rangle)))
	    (v0 (* initial-velocity (sin rangle))))
	(integrate 0
		   elevation
		   u0
		   v0
		   dt
		   gravity
		   mass
		   beta)))))

;; RUN SOME TEST CASES

;; (travel-distance 1 45 45)
;; 75.410079

;; (travel-distance 1 45 90)
;; 2.2548266671799244e-4

;; (travel-distance 1 45 0)
;; 11.936509337563422

;; (travel-distance 1 35 45)
;; 59.09136533132533

;; what about Denver?

;; For what range of angles will the ball land over the fence?

(define homerun?
  (lambda (elevation initial-velocity angle)
    (let ((distance-travelled (travel-distance elevation initial-velocity angle)))
      (if (> distance-travelled (feet-to-meters 300))
	  distance-travelled
	  #f))))

(define (test-angles elevation initial-velocity)
  (define (iter-angles test-angle)
    (if (> test-angle 90)
	(display "Done.")
	(let ((distance-travelled (travel-distance elevation initial-velocity test-angle)))
	  (cond ((> distance-travelled (feet-to-meters 300))
		 (display test-angle) (display "° ") (display distance-travelled) (display " meters") (newline)))
	  (iter-angles (+ test-angle 1)))))
  (iter-angles 0))

	    
	   

;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance


;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s

;; x(t) = v cos alpha t

(define integrate-time
  (lambda (x y u v dt g m beta time-total)
    (if (< y 0)
	time-total
	(let ((speed (sqrt (+ (square u) (square v)))))
	  (let ((drag (* beta (square speed))))
	    (let ((dx (* u dt))
		  (dy (* v dt))
		  (du (* (/ -1 m) beta speed u dt))
		  (dv (* (* -1 (+ (* (/ 1 m) speed v beta)
				  gravity))
			 dt)))
	      (integrate-time
	       (+ x dx)
	       (+ y dy)
	       (+ u du)
	       (+ v dv)
	       dt g m beta
	       (+ time-total dt))))))))

(define reached?
  (lambda (elevation initial-velocity angle distance-to-receiver)
    (let ((distance-travelled (travel-distance elevation initial-velocity angle)))
      (if (< (abs (- distance-travelled distance-to-receiver)) 0.25) 
	  distance-travelled
	  #f))))

(define (throw-angle distance-to-receiver throw-velocity receiver-elevation)
  (define (iter angle)
    (if (reached? 1 throw-velocity angle distance-to-receiver)
	angle
	(if (< angle -90)
	    #f
	    (iter (- angle 0.25)))))
  (iter 90))

(define travel-time
  (lambda (elevation initial-velocity angle)
    (let ((rangle (degree2radian angle)))
      (let ((u0 (* initial-velocity (cos rangle)))
	    (v0 (* initial-velocity (sin rangle))))
	(integrate-time 0
			elevation
			u0
			v0
			dt
			gravity
			mass
			beta
			0)))))

(define (throw-travel-time distance-to-receiver throw-velocity receiver-elevation)
  (travel-time 1
	       throw-velocity
	       (throw-angle distance-to-receiver throw-velocity receiver-elevation)))
      
    ;(let ((alpha (acos (/ distance-to-receiver (* throw-velocity time)))))

;; Problem 8

;; Problem 9
