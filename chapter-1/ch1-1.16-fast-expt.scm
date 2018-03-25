;; Chapter 1
;; Exercise 1.16
;; Iterative fast-expt

;; Design a procedure that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps, as does fast-expt. (Hint: Using the observation that (bn/2)2=(b2)n/2, keep, along with the exponent n and the base b, an additional state variable a, and define the state transformation in such a way that the product abn is unchanged from state to state. At the beginning of the process a is taken to be 1, and the answer is given by the value of a at the end of the process. In general, the technique of defining an invariant quantity that remains unchanged from state to state is a powerful way to think about the design of iterative algorithms.)



(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
	  ((even? n) (iter a (square b) (/ n 2)))
	  (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))
      
      ;; (* a b)
      ;; (if (even? counter)
      ;; 	  (fast-iter b
      ;; 		     (/ counter 2)
      ;; 		     (* a b))
      ;; 	  (fast-iter b
      ;; 		     (- counter 1)
      ;; 		     (* a b)))))

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))


(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product))))
