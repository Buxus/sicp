;; (define (add-complex z1 z2)
;;   (make-from-real-imag (+ (real-part z1) (real-part z2))
;; 		       (+ (imag-part z1) (imag-part z2))))

;; (define (sub-complex z1 z2)
;;   (make-from-real-imag (- (real-part z1) (real-part z2))
;; 		       (- (imag-part z1) (imag-part z2))))

;; (define (mul-complex z1 z2)
;;   (make-from-mag-ang (* (magnitude z1) (magnitude z2))
;; 		     (+ (angle z1) (angle z2))))

;; (define (div-complex z1 z2)
;;   (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
;; 		     (- (angle z1) (angle z2))))

;; (define (attach-tag type-tag contents)
;;   (cons type-tag contents))

;; (define (type-tag datum)
;;   (if (pair? datum)
;;       (car datum)
;;       (error "Bad tagged datum -- TYPE-TAG" datum)))

;; (define (contents datum)
;;   (if (pair? datum)
;;       (cdr datum)
;;       (error "Bad tagged datum -- CONTENTS" datum)))

;; (define (rectangular? z)
;;   (eq? (type-tag z) 'rectangular))

;; (define (polar? z)
;;   (eq? (type-tag z) 'polar))

;; (define (real-part-rectangular z) (car z))

;; (define (imag-part-rectangular z) (cdr z))

;; (define (magnitude-rectangular z)
;;   (sqrt (+ (square (real-part-rectangular z))
;; 	   (square (imag-part-rectangular z)))))

;; (define (angle-rectangular z)
;;   (atan (imag-part-rectangular z)
;; 	(real-part-rectangular z)))

;; (define (make-from-real-imag-rectangular x y)
;;   (attach-tag 'rectangular (cons x y)))

;; (define (make-from-mag-ang-rectangular r a)
;;   (attach-tag 'rectangular
;; 	      (cons (* r (cos a)) (* r (sin a)))))

;; (define (real-part-polar z)
;;   (* (magnitude-polar z) (cos (angle-polar z))))

;; (define (imag-part-polar z)
;;   (* (magnitude-polar z) (sin (angle-polar z))))

;; (define (magnitude-polar z) (car z))

;; (define (angle-polar z) (cdr z))

;; (define (make-from-real-imag-polar x y)
;;   (attach-tag 'polar
;; 	      (cons (sqrt (+ (square x) (square y)))
;; 		    (atan y x))))

;; (define (make-from-mag-ang-polar r a)
;;   (attach-tag 'polar (cons r a)))

;; (define (real-part z)
;;   (cond ((rectangular? z)
;; 	 (real-part-rectangular (contents z)))
;; 	((polar? z)
;; 	 (real-part-polar (contents z)))
;; 	(else (error "Unknown type -- REAL-PART" z))))

;; (define (imag-part z)
;;   (cond ((rectangular? z)
;; 	 (imag-part-rectangular (contents z)))
;; 	((polar? z)
;; 	 (imag-part-polar (contents z)))
;; 	(else (error "Unknown type -- IMAG-PART" z))))

;; (define (magnitude z)
;;   (cond ((rectangular? z)
;; 	 (magnitude-rectangular (contents z)))
;; 	((polar? z)
;; 	 (magnitude-polar (contents z)))
;; 	(else (error "Unknown type -- MAGNITUDE" z))))

;; (define (angle z)
;;   (cond ((rectangular? z)
;; 	 (angle-rectangular (contents z)))
;; 	((polar? z)
;; 	 (angle-polar (contents z)))
;; 	(else (error "Unknown type -- ANGLE" z))))

;; (define (make-from-real-imag x y)
;;   (make-from-real-imag-rectangular x y))

;; (define (make-from-mag-ang r a)
;;   (make-from-mag-ang-polar r a))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types -- APPLY-GENERIC"
	   (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))



;; *Exercise 2.73:* Section *Note 2-3-2:: described a program that
;; performs symbolic differentiation:

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;            (make-product (multiplier exp)
;;                          (deriv (multiplicand exp) var))
;;            (make-product (deriv (multiplier exp) var)
;;                          (multiplicand exp))))
;;         <MORE RULES CAN BE ADDED HERE>
;;         (else (error "unknown expression type -- DERIV" exp))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
	       var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;; a. Explain what was done above.  Why can't we assimilate the
;;    predicates `number?' and `same-variable?' into the
;;    data-directed dispatch?

;; The sum? and product? predicates were generalized into an expression
;; handling generic derivative operations.  number? and same-variable?
;; take a different number of arguments so they couldn't be generalized
;; the same way

;; b. Write the procedures for derivatives of sums and products,
;;    and the auxiliary code required to install them in the table
;;    used by the program above.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (install-sum-package)
  ;; internal procedures
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))
  (define (addend s) (cadr s))
  (define (augend s)
    (accumulate make-sum 0 (cddr s)))

  ;; interface to the rest of system
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '(sum) deriv-sum)
  (put 'make-sum '+
       (lambda (x y) (tag (make-sum x y))))
  'done)

(define (make-sum x y)
  ((get 'make-sum '+) x y))
   
(define (install-product-package)
  ;; internal procedures
  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
		   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
		   (multiplicand exp))))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (accumulate make-product 1 (cddr p)))

  ;; interface to the rest of system
  (define (tag x) (attach-tag '* x))
  (put 'deriv '(*) deriv-product)
  (put 'make-sum '*
       (lambda (x y) (tag (make-product x y))))
  (put 'make-product '*
       (lambda (x y) (tag (make-product x y))))
  'done)

(define (make-product x y)
  ((get 'make-product '*) x y))

(define (deriv x) (apply-generic 'deriv x))
		   
  
;; c. Choose any additional differentiation rule that you like,
;;    such as the one for exponents (*Note Exercise 2-56::), and
;;    install it in this data-directed system.

(define (install-exponent-package)
  ;; internal procedures
  (define (deriv-exponent exp var)
    (make-product (make-product (exponent exp)
				(make-exponentiation (base exp)
						     (- (exponent exp) 1)))
		  (deriv (base exp) var)))
  (define (base e) (car e))
  (define (exponent e) (caddr e))

  ;; interface to the rest of system
  (define (tag x) (attach-tag '**))
  (put 'deriv '(**) deriv-exponent)
  (put 'make-product '**
       (lambda (x y) (tag (make-product x y))))
  (put 'make-exponentiation '**
       (lambda (x y) (tag (make-exponentiation x y))))
  'done)

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	((and (number? base) (number? exponent)) (expt base exponent))
	(else (list base '** exponent))))

  
;; d. In this simple algebraic manipulator the type of an
;;    expression is the algebraic operator that binds it together.
;;    Suppose, however, we indexed the procedures in the opposite
;;    way, so that the dispatch line in `deriv' looked like

;;         ((get (operator exp) 'deriv) (operands exp) var)

;;    What corresponding changes to the derivative system are
;;    required?


;; We would need to make the corresponding changes to the put calls


;; *Exercise 2.74:* Insatiable Enterprises, Inc., is a highly
;; decentralized conglomerate company consisting of a large number of
;; independent divisions located all over the world.  The company's
;; computer facilities have just been interconnected by means of a
;; clever network-interfacing scheme that makes the entire network
;; appear to any user to be a single computer.  Insatiable's
;; president, in her first attempt to exploit the ability of the
;; network to extract administrative information from division files,
;; is dismayed to discover that, although all the division files have
;; been implemented as data structures in Scheme, the particular data
;; structure used varies from division to division.  A meeting of
;; division managers is hastily called to search for a strategy to
;; integrate the files that will satisfy headquarters' needs while
;; preserving the existing autonomy of the divisions.

;; Show how such a strategy can be implemented with data-directed
;; programming.  As an example, suppose that each division's
;; personnel records consist of a single file, which contains a set
;; of records keyed on employees' names.  The structure of the set
;; varies from division to division.  Furthermore, each employee's
;; record is itself a set (structured differently from division to
;; division) that contains information keyed under identifiers such
;; as `address' and `salary'.  In particular:

;;   a. Implement for headquarters a `get-record' procedure that
;;      retrieves a specified employee's record from a specified
;;      personnel file.  The procedure should be applicable to any
;;      division's file.  Explain how the individual divisions' files
;;      should be structured.  In particular, what type information
;;      must be supplied?

(define (get-record personnel-file)
  (apply-generic 'get-record personnel-file))

;; individual divisions' files need to include their own procedures for get-record and
;; the auxillary code to install them in the table used by the get-record procedure above.
;; For example, the specific method to use an employee's name or key to access a record
;; must be specified by the package issued by each division.

;;   b. Implement for headquarters a `get-salary' procedure that
;;      returns the salary information from a given employee's record
;;      from any division's personnel file.  How should the record be
;;      structured in order to make this operation work?

(define (get-salary personnel-file)
  (apply-generic 'get-salary personnel-file))

;; The division's files will contain the methods for displaying the salary, and any calculations
;; ie, formatting as currency $123,456.99.  Also, as in the get-record procedure, all other
;; specific methods for file access are handled at the division-level.

;;   c. Implement for headquarters a `find-employee-record'
;;      procedure.  This should search all the divisions' files for
;;      the record of a given employee and return the record.  Assume
;;      that this procedure takes as arguments an employee's name and
;;      a list of all the divisions' files.

(define (find-employee-record employee-name files)
  (let ((this-record) (car files))
    (let ((this-record-name) (apply-generic 'get-record-name this-record)
	  (this-record-key) (apply-generic 'get-record-key this-record))
      (cond ((null? files) #f)
	    ((eq? this-record-name employee-name) this-record-key)
	    (else (find-employee-record employee-name (cdr files)))))))

;;   d. When Insatiable takes over a new company, what changes must
;;      be made in order to incorporate the new personnel information
;;      into the central system?

;; The company needs to install its get-record and get-salary procedures in the lookup table.
+
0.
