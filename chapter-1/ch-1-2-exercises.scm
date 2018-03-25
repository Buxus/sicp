;;; Exercise 1.11

;;;Recursive f
(define (recur-f n)
  (define (f-iter n)
    (if (< n 3)
	n
	(+ (f-iter (- n 1))
	   (* 2
	      (f-iter (- n 2)))
	   (* 3
	      (f-iter (- n 3))))))
  (f-iter n))

;;;Iterative f
(define (iter-f n)
  (define (f-iter a b c count)
    (if (= count 0)
	a
	(iter b c (+ c (*2 b) (*3 a)))))
  (f-iter 0 1 2 n))

  ;; (if (< count 3)
  ;; 	sum
  ;; 	(f-iter
  ;; 	 (+ sum (f count))
  ;; 	 (- count 1))))

  ;; (define (f n)
  ;;   (+ (- n 1)
  ;;      (* 2 (- n 2))
  ;;      (* 3 (- n 3))))
  
  ;; (f-iter 0 n))
