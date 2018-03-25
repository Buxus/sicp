;; *Exercise 2.75:* Implement the constructor `make-from-mag-ang' in
;; message-passing style.  This procedure should be analogous to the
;; `make-from-real-imag' procedure given above.

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) m)
	  ((eq? op 'angle) a)
	  ((eq? op 'real-part)
	   (* m (cos a)))
	  ((eq? op 'imag-part)
	   (* m (sin a)))
	  (else
	   (error "Unknown op -- MAKE-FROM-MAG-ANG op"))))
  dispatch)

;; *Exercise 2.76:* As a large system with generic operations
;; evolves, new types of data objects or new operations may be needed.
;; For each of the three strategies--generic operations with explicit
;; dispatch, data-directed style, and message-passing-style--describe
;; the changes that must be made to a system in order to add new
;; types or new operations.  Which organization would be most
;; appropriate for a system in which new types must often be added?
;; Which would be most appropriate for a system in which new
;; operations must often be added?

;; Generic operations with explicit dispatch
;; - need to modify all generic selectors each time a new type is added.  The Generic
;;   interface procedures must know about all the different representations.  This would be a poor
;;   choice in a system where new types were often added.
;;
;; Data-directed style
;; - Only add new entries to the table of operations for each new type added.  This is a good choice
;;   in a system where new types are often added.
;;
;; Message-passing style
;; - The procedures that define the objects themselves contain the dispatch procedures.
;;   If a new type is added only the procedure that defines its objects needs to be modified.
;;   The rest of the system can remain unmodified.  This works well in systems in which types
;;   must often be added.
;; - Downsides: only allows single argument generic operations.
