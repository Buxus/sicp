;; Exercise 3.1

(define (make-accumulator sum)
  (define (accumulate amount)
    (set! sum (+ sum amount))
    sum)
  accumulate)

;; Exercise 3.2

(define (make-monitored f)
  (let ((call-counter (make-accumulator 0)))
    (define (mf message)
      (cond ((eq? message 'how-many-calls?) (call-counter 0))
	    ((eq? message 'reset-count) (set! call-counter (make-accumulator 0)))
	    (else (begin (call-counter 1)
			 (f message)))))
    mf))

;; Exercise 3.3

(define (make-account balance password)
  (let ((account-password password))
    (define (withdraw amount)
      (if (>= balance amount) 
          (begin (set! balance (- balance amount))
		 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define check-password
      (make-monitored
       (lambda (password)
	 (eq? password account-password))))
    (define call-the-cops
      (lambda args "The police have been notified of this unauthorized access attempt."))
    (define (dispatch password m)
      (if (check-password password)
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		(else (error "Unknown request -- MAKE-ACCOUNT"
			     m)))
	  (if (> (check-password 'how-many-calls?) 6)
	      call-the-cops
	      (lambda args "Authentication error - incorrect password"))))
    dispatch))

