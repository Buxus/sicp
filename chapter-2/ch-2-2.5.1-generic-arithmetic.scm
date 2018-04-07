;; put and get functions from ch2support.scm

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
       q                           (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; Coercion Table

(define coercion-table (make-table)) 
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))


(define (install-coercion-package)
  (define (scheme-number->rational n)
    (make-rational (contents n) 1))
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))

  (put-coercion 'scheme-number 'rational scheme-number->rational)
  (put-coercion 'scheme-number 'complex scheme-number->complex))
;; start code from SICP

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))
  (define (equ? x y)
    (and (= (numer x) (numer y))
	 (= (denom x) (denom y))))
  (define (=zero? x)
    (= 0 (numer x)))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

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
  (define (equ? x y)
    (and (= (magnitude x) (magnitude y))
	 (= (angle x) (angle y))))
  (define (=zero? z)
    (= 0 (magnitude z)))

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
  (put 'equ? '(polar polar) equ?)
  (put '=zero? '(polar) =zero?)
a  'done)

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
  (define (equ? x y)
    (and (= (real-part x) (real-part y))
	 (= (imag-part x) (imag-part y))))
  (define (=zero? z)
    (and (= 0 (real-part z))
	 (= 0 (imag-part z))))

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
  (put 'equ? '(rectangular) equ?)
  (put '=zero? '(rectangular) =zero?)
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))
  (define (=zero? z)
    (and (= 0 (real-part z)) (= 0 (imag-part z))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


;; *Exercise 2.77:
;; The methods for complex-number arithmetic were not installed in the table of operations.
;; After adding the suggested lines, the methods are installed in the table and the APPLY-GENERIC
;; procedure is able to lookup the necessary operations for the complex tag.

;; (define z (make-complex-from-real-imag 3 4))
;; ('complex ('rectangular (3 4)))

;; (magnitude z)
;; (apply-generic 'magnitude ('complex ('rectangular (3 4))))
;; (let ((type-tags (map type-tag args))))
;; (let ((type-tags 'complex)))
;; (let ((proc (get op type-tags))))
;; (let ((proc (get 'magnitude 'complex))))
;; ;; before changes
;; (error
;;  "No method for these types -- APPLY-GENERIC"
;;  (list op type-tags))
;; (error
;;  "No method for these types -- APPLY-GENERIC"
;;  (list 'magnitude 'complex))
;; ;; after changes
;; (apply (get 'magnitude 'complex) (contents ('complex ('rectangular (3 4)))))
;; (magnitude ('rectangular (3 4)))
;; (apply-generic 'magnitude ('rectangular (3 4)))
;; (type-tags 'rectangular)
;; (proc (get 'magnitude 'rectangular))
;; (apply (get 'magnitude 'rectangular) ('rectangular (3 4)))
;; (magnitude (3 4))
;;   (define (magnitude z)
;;     (sqrt (+ (square (real-part z))
;; 	     (square (imag-part z)))))
;; (sqrt (+ (square 3)
;; 	 (square 4)))
;; 5

;;apply-generic was called twice, once for the 'complex 'magnitude lookup and once for the
;;'rectangular 'magnitude lookup

;; *Exercise 2.78:* The internal procedures in the `scheme-number'
;; package are essentially nothing more than calls to the primitive
;; procedures `+', `-', etc.  It was not possible to use the
;; primitives of the language directly because our type-tag system
;; requires that each data object have a type attached to it.  In
;; fact, however, all Lisp implementations do have a type system,
;; which they use internally.  Primitive predicates such as `symbol?'
;; and `number?'  determine whether data objects have particular
;; types.  Modify the definitions of `type-tag', `contents', and
;; `attach-tag' from section *Note 2-4-2:: so that our generic system
;; takes advantage of Scheme's internal type system.  That is to say,
;; the system should work as before except that ordinary numbers
;; should be represented simply as Scheme numbers rather than as
;; pairs whose `car' is the symbol `scheme-number'.

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number) 
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
	((pair? datum) (car datum))
	(else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
	((pair? datum) (cdr datum))
	(else (error "Bad tagged datum -- CONTENTS" datum))))

;; *Exercise 2.79:* Define a generic equality predicate `equ?' that
;; tests the equality of two numbers, and install it in the generic
;; arithmetic package.  This operation should work for ordinary
;; numbers, rational numbers, and complex numbers.

;; *Exercise 2.80:* Define a generic predicate `=zero?' that tests if
;;  its argument is zero, and install it in the generic arithmetic
;;  package.  This operation should work for ordinary numbers, rational
;;  numbers, and complex numbers.


;; (define (scheme-number->complex n)
;;   (make-complex-from-real-imag (contents n) 0))

;; (put-coercion 'scheme-number 'complex scheme-number->complex)

;; *Exercise 2.81:* Louis Reasoner has noticed that `apply-generic'
;; may try to coerce the arguments to each other's type even if they
;; already have the same type.  Therefore, he reasons, we need to put
;; procedures in the coercion table to "coerce" arguments of each
;; type to their own type.  For example, in addition to the
;; `scheme-number->complex' coercion shown above, he would do:

;; (define (scheme-number->scheme-number n) n)
;; (define (complex->complex z) z)
;; (put-coercion 'scheme-number 'scheme-number
;; 	      scheme-number->scheme-number)
;; (put-coercion 'complex 'complex complex->complex)


;; a. With Louis's coercion procedures installed, what happens if
;; `apply-generic' is called with two arguments of type
;; `scheme-number' or two arguments of type `complex' for an
;; operation that is not found in the table for those types?
;; For example, assume that we've defined a generic
;; exponentiation operation:

;; (define (exp x y) (apply-generic 'exp x y))

;; and have put a procedure for exponentiation in the
;; Scheme-number package but not in any other package:

;; following added to Scheme-number package
;; (put 'exp '(scheme-number scheme-number)
;;      (lambda (x y) (tag (expt x y)))) 

					; using primitive `expt'

;; What happens if we call `exp' with two complex numbers as
;; arguments?

;; This will cause an infinite loop.  If the desired operation is not found for types t1 and t2,
;; the procedures will coerce t1 -> t2 and t2 -> t1.  The desired operations will then be tried
;; again on the same types, and the coercion will be forced, repeating in an infinite loop.

;; b. Is Louis correct that something had to be done about coercion
;;    with arguments of the same type, or does `apply-generic' work
;;    correctly as is?

;;  apply-generic will work correctly as designed, but only when the proper coerce
;;  arguments are installed in the operations table.

;; c. Modify `apply-generic' so that it doesn't try coercion if the
;;    two arguments have the same type.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(if (not (eq? type1 type2))
		    (let ((t1->t2 (get-coercion type1 type2))
			  (t2->t1 (get-coercion type2 type1)))
		      (cond ((not (null? t1->t2)) (apply-generic op (t1->t2 a1) a2))
			    ((not (null? t2->t1)) (apply-generic op a1 (t2->t1 a2)))
			    (else
			     (error "No coercion method for these types"
				    (list op type-tags)))))
		    (error "No method for these types"
			   (list op type-tags))))
	      (error "No method for these types"
		     (list op type-tags)))))))

;; *Exercise 2.82:* Show how to generalize `apply-generic' to handle
;; coercion in the general case of multiple arguments.  One strategy
;; is to attempt to coerce all the arguments to the type of the first
;; argument, then to the type of the second argument, and so on.
;; Give an example of a situation where this strategy (and likewise
;; the two-argument version given above) is not sufficiently general.
;; (Hint: Consider the case where there are some suitable mixed-type
;; operations present in the table that will not be tried.)

;; this strategy is not sufficiently general in the case where and intermediate
;; coercion is possible  between two types, but where the two types could not
;; be coerced directly to the other. I.e., Type A -> Type B-> Type C -> exists
;; but no Type A -> Type C.

;; (define (coerce-list-to target-type args)
;; ;; Recursively build a list of coereced types
;;   (if (null? args)
;;       '()
;;       (let ((this-arg (car args))
;; 	    (rest (cdr args)))
;; 	(let ((this-type (type-tag this-arg)))
;; 	  (let ((this-type->target-type (get-coercion this-type target-type)))
;; 	    (if this-type->target-type
;; 		(cons (this-type->target-type this-arg)
;; 		      (coerce-list-to target-type rest))
;; 		#f))))))


;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc 
;; 	  (apply proc (map contents args))
;; 	  (coerce-all type-tags op args)))))

;; (define (coerce-all type-tags op args)
;;   (if (not (null? type-tags))
;;       (let ((first-tag (car type-tags)))
;; 	(let ((coerced-set (coerce-to first-tag args)))
;; 	  (if (not (memq #f coerced-set))
;; 	      (map (apply-generic op coerced-set))
;; 	      (coerce-all (cdr type-tags) op args))))
;; 	(error "No method for these types"
;; 	       (list op type-tags))))

;; Plagiarized from
;; https://jots-jottings.blogspot.com/2012/02/sicp-exercise-282-multi-argument.html
(define (apply-generic op . args)
  (define (uniquify l)
    (if (null? l)
        '()
        (let ((head (car l))
              (tail (cdr l)))
          (if (memq head tail)
              (uniquify tail)
              (cons head (uniquify tail))))))
  (define (coerce-to target-type remaining-args result)
    (if (null? remaining-args)
        result
        (let* ((arg (car remaining-args))
               (original-type (type-tag arg)))
          (if (eq? original-type target-type)
              (coerce-to target-type
                         (cdr remaining-args)
                         (append result (list arg)))
              (let ((original->target (get-coercion (type-tag arg) target-type)))
                (if original->target
                    (coerce-to target-type
                               (cdr remaining-args)
                               (append result (list (original->target arg))))
                    #f))))))
  (define (apply-generic-iter coercion-types)
    (if (null? coercion-types)
        (error "No method for these types, and could not coerce"
               (list op (map type-tag args)))
        (let ((coerced-args (coerce-to (car coercion-types) args '())))
          (if coerced-args
              (let ((proc (get op (map type-tag coerced-args))))
                (if proc
                    (apply proc (map contents coerced-args))
                    (apply-generic-iter (cdr coercion-types))))
              (apply-generic-iter (cdr coercion-types))))))
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (let ((unique-types (uniquify type-tags)))
          (if (> (length unique-types) 1)
              (apply-generic-iter unique-types)
              (else (error "No method for this type"
                           (list op type-tags))))))))

;; *Exercise 2.83:* Suppose you are designing a generic arithmetic
;; system for dealing with the tower of types shown in *Note Figure
;; 2-25::: integer-> rational-> real-> complex.  For each type (except
;; complex), design a procedure that raises objects of that type one
;; level in the tower.  Show how to install a generic `raise'
;; operation that will work for each type (except complex).
(define (raise-integer n)
  (apply (get-coercion 'scheme-number 'rational) n))
(define (raise-rational n)
  (apply (get-coercion 'rational 'real) n))
(define (raise-real n)
  (apply (get-coercion 'real 'complex) n))


;; *Exercise 2.84:* Using the `raise' operation of *Note Exercise
;; 2-83::, modify the `apply-generic' procedure so that it coerces
;; its arguments to have the same type by the method of successive
;; raising, as discussed in this section.  You will need to devise a
;; way to test which of two types is higher in the tower.  Do this in
;; a manner that is "compatible" with the rest of the system and will
;; not lead to problems in adding new levels to the tower.

;; *Exercise 2.85:* This section mentioned a method for "simplifying"
;; a data object by lowering it in the tower of types as far as
;; possible.  Design a procedure `drop' that accomplishes this for the
;; tower described in *Note Exercise 2-83::.  The key is to decide,
;; in some general way, whether an object can be lowered.  For
;; example, the complex number 1.5 + 0i can be lowered as far as
;; `real', the complex number 1 + 0i can be lowered as far as
;; `integer', and the complex number 2 + 3i cannot be lowered at all.
;; Here is a plan for determining whether an object can be lowered:
;; Begin by defining a generic operation `project' that "pushes" an
;; object down in the tower.  For example, projecting a complex
;; number would involve throwing away the imaginary part.  Then a
;; number can be dropped if, when we `project' it and `raise' the
;; result back to the type we started with, we end up with something
;; equal to what we started with.  Show how to implement this idea in
;; detail, by writing a `drop' procedure that drops an object as far
;; as possible.  You will need to design the various projection
;; operations(5) and install `project' as a generic operation in the
;; system.  You will also need to make use of a generic equality
;; predicate, such as described in *Note Exercise 2-79::.  Finally,
;; use `drop' to rewrite `apply-generic' from *Note Exercise 2-84::
;; so that it "simplifies" its answers.

;; *Exercise 2.86:* Suppose we want to handle complex numbers whose
;; real parts, imaginary parts, magnitudes, and angles can be either
;; ordinary numbers, rational numbers, or other numbers we might wish
;; to add to the system.  Describe and implement the changes to the
;; system needed to accommodate this.  You will have to define
;; operations such as `sine' and `cosine' that are generic over
;; ordinary numbers and rational numbers.



(install-coercion-package)
(install-scheme-number-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)
(install-rational-package)

(define test-scheme-number 5)
(define test-polar-number
  (make-complex-from-mag-ang 3 4))
(define test-rectangular-number
  (make-complex-from-real-imag 5 6))
(define test-rational-number
  (make-rational 7 8))

