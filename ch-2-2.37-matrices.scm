;; Exercise 2.37
;; .............

;; Suppose we represent vectors v = (v_i) as sequences of numbers, and
;; matrices m = (m_(ij)) as sequences of vectors (the rows of the matrix).
;; For example, the matrix

;;      +-         -+
;;      |  1 2 3 4  |
;;      |  4 5 6 6  |
;;      |  6 7 8 9  |
;;      +-         -+

;; is represented as the sequence `((1 2 3 4) (4 5 6 6) (6 7 8 9))'.  With
;; this representation, we can use sequence operations to concisely
;; express the basic matrix and vector operations.  These operations
;; (which are described in any book on matrix algebra) are the following:

(dot-product v w)     ; returns the sum >_i v_i w_i
(matrix-*-vector m v) ; returns the vector t,
                      ; where t_i = >_j m_(ij) v_j
(matrix-*-matrix m n)
		; returns the matrix p,
	      	; where p_(ij) = >_k m_(ik) n_(kj)
(transpose m)   ; returns the matrix n,
    		; where n_(ij) = m_(ji)

(define test-matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9))
  
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambra (x) * v x) m))

(matrix-*-vector test-matrix 5)
