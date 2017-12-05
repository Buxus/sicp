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
(matrix-*-matrix m n) ; returns the matrix p,
                      ; where p_(ij) = >_k m_(ik) n_(kj)
(transpose m)         ; returns the matrix n,
		      ; where n_(ij) = m_(ji)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define test-matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 7 8 9 9)))
  
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
	;; (accumulate + 0 (map * v w)))
	 (dot-product row v))
       m))

(matrix-*-vector test-matrix (list 1 2 3 4))
(5 11)

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))


(transpose test-matrix)
((1 4 7) (2 5 8) (3 6 9) (4 6 9))

(define test-matrix (list (list 1 2) (list 3 4)))
(define test-matrix2 (list (list 1 2 3) (list 3 4 5)))

(matrix-*-matrix test-matrix test-matrix2)
((7 10 13) (15 22 29))
