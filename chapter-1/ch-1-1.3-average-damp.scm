(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

(define (cuberoot x)
  (fixed-point-of-transform (lambda (y) (/ x (square y)))
			    average-damp		 
			    1.0))

;; In order to implement Newton's method as a procedure, we must first express the idea of derivative. Note that “derivative”, like average damping, is something that transforms a function into another function. For instance, the derivative of the function x↦x3 is the function x↦3x2. In general, if g is a function and dx is a small number, then the derivative Dg of g is the function whose value at any number x is given (in the limit of small dx) by
;; Dg(x)=g(x+dx)−g(x)dx

;; Thus, we can express the idea of derivative (taking dx
;; 						    to be, say, 0.00001) as the procedure

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.0001)

(define (cube x)
  (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; Exercise 1.40. Define a procedure cubic that can be used together with the newtons-method procedure in expressions of the form

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (quadratic a b c)
  (lambda (x) (+ (* a (square x)) (* b x) c)))

(newtons-method (cubic a b c) 1)


;; Exercise 1.41. Define a procedure double that takes a procedure of one argument as argument and returns a procedure that applies the original procedure twice. For example, if inc is a procedure that adds 1 to its argument, then (double inc) should be a procedure that adds 2.

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (smooth f dx)
  (lambda (x)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx)))
       3)))

(define (nth-smoothed f dx n)
  (repeated (smooth f dx) n))

(define pi
  (fixed-point-of-transform (lambda (x) (sin x))
			    newton-transform
			    3.0))

;; Exercise 1.45. We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed point of y↦xy does not converge, and that this can be fixed by average damping. The same method works for finding cube roots as fixed points of the average-damped y↦xy2. Unfortunately, the process does not work for fourth roots — a single average damp is not enough to make a fixed-point search for y↦xy3 converge. On the other hand, if we average damp twice (i.e., use the average damp of the average damp of y↦xy3) the fixed-point search does converge. Do some experiments to determine how many average damps are required to compute nth roots as a fixed-point search based upon repeated average damping of y↦xyn−1. Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp, and the repeated procedure of exercise 1.43. Assume that any arithmetic operations you need are available as primitives.

(define (nth-power x n)
  (cond ((= n 0) 1)
	((= n 1) x)
	(else ((repeated square (- n 1)) x))))


(define (nth-root x n)
  (fixed-point
   ((repeated average-damp (floor (log2 n)))
    (lambda (y) (/ x (expt y (- n 1)))))
   1.0))


(define (log2 x)
  (/ (log x) (log 2)))


;; Exercise 1.46. Several of the numerical methods described in this chapter are instances of an extremely general computational strategy known as iterative improvement. Iterative improvement says that, to compute something, we start with an initial guess for the answer, test if the guess is good enough, and otherwise improve the guess and continue the process using the improved guess as the new guess. Write a procedure iterative-improve that takes two procedures as arguments: a method for telling whether a guess is good enough and a method for improving a guess. Iterative-improve should return as its value a procedure that takes a guess as argument and keeps improving the guess until it is good enough. Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of section 1.3.3 in terms of iterative-improve.

(define (iterative-improve good-enough? improve)
  (define (helper guess)
    (if (good-enough? guess)
	guess
	(helper (improve guess))))
  helper)

(define (sqrt x)
  ((iterative-improve
    (lambda (guess) (< (abs (- (square guess) x))
		       0.001))
    (lambda (guess) (average guess (/ x guess))))
   1.0))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess) (< (abs (- (f guess) guess))
		       .001))
    (lambda (guess) (f guess)))
   first-guess))
