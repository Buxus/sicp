 ;; Exercise 1.22. Most Lisp implementations include a primitive called runtime that returns an integer that specifies the amount of time the system has been running (measured, for example, in microseconds). The following timed-prime-test procedure, when called with an integer n, prints n and checks to see if n is prime. If n is prime, the procedure prints three asterisks followed by the amount of time used in performing the test. 

;; Using this procedure, write a procedure search-for-primes that checks the primality of consecutive odd integers in a specified range. Use your procedure to find the three smallest primes larger than 1000; larger than 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to test each prime. Since the testing algorithm has order of growth of Θ(n−−√), you should expect that testing for primes around 10,000 should take about 1–√0 times as long as testing for primes around 1000. Do your timing data bear this out? How well do the data for 100,000 and 1,000,000 support the n−−√ prediction? Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps required for the computation?

;; Exercise 1.23. The smallest-divisor procedure shown at the start of this section does lots of needless testing: After it checks to see if the number is divisible by 2 there is no point in checking to see if it is divisible by any larger even numbers. This suggests that the values used for test-divisor should not be 2,3,4,5,6,…, but rather 2,3,5,7,9,… To implement this change, define a procedure next that returns 3 if its input is equal to 2 and otherwise returns its input plus 2. Modify the smallest-divisor procedure to use (next test-divisor) instead of (+ test-divisor 1). With timed-prime-test incorporating this modified version of smallest-divisor, run the test for each of the 12 primes found in exercise 1.22. Since this modification halves the number of test steps, you should expect it to run about twice as fast. Is this expectation confirmed? If not, what is the observed ratio of the speeds of the two algorithms, and how do you explain the fact that it is different from 2?

;; Exercise 1.24. Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime? (the Fermat method), and test each of the 12 primes you found in that exercise. Since the Fermat test has Θ(logn) growth, how would you expect the time to test primes near 1,000,000 to compare with the time needed to test primes near 1000? Do your data bear this out? Can you explain any discrepancy you find?

;; Exercise 1.27. Demonstrate that the Carmichael numbers listed in footnote 47 really do fool the Fermat test. That is, write a procedure that takes an integer n and tests whether an is congruent to a modulo n for every a<n, and try your procedure on the given Carmichael numbers.

;; Numbers that fool the Fermat test are called Carmichael numbers, and little is known about them other than that they are extremely rare. There are 255 Carmichael numbers below 100,000,000. The smallest few are 561, 1105, 1729, 2465, 2821, and 6601. In testing primality of very large numbers chosen at random, the chance of stumbling upon a value that fools the Fermat test is less than the chance that cosmic radiation will cause the computer to make an error in carrying out a “correct” algorithm. Considering an algorithm to be inadequate for the first reason but not for the second illustrates the difference between mathematics and engineering.

 ;; Exercise 1.28. One variant of the Fermat test that cannot be fooled is called the Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an alternate form of Fermat's Little Theorem, which states that if n is a prime number and a is any positive integer less than n, then a raised to the (n−1)st power is congruent to 1 modulo n. To test the primality of a number n by the Miller-Rabin test, we pick a random number a<n and raise a to the (n−1)st power modulo n using the expmod procedure. However, whenever we perform the squaring step in expmod, we check to see if we have discovered a “nontrivial square root of 1 modulo n,” that is, a number not equal to 1 or n−1 whose square is equal to 1 modulo n. It is possible to prove that if such a nontrivial square root of 1 exists, then n is not prime. It is also possible to prove that if n is an odd number that is not prime, then, for at least half the numbers a<n, computing an−1 in this way will reveal a nontrivial square root of 1 modulo n. (This is why the Miller-Rabin test cannot be fooled.) Modify the expmod procedure to signal if it discovers a nontrivial square root of 1, and use this to implement the Miller-Rabin test with a procedure analogous to fermat-test. Check your procedure by testing various known primes and non-primes. Hint: One convenient way to make expmod signal is to have it return 0. 

(define (test-carmichael n)
  (define (report-fail a)
    (display a)
    (display " fail")
    (newline))
  (define (helper a)
   ; (display a)
    ;; (if (= (expmod a n n) a)
    ;; 	(display " pass")
    ;; 	(display " fail"))
    ;; (newline)
    (if (not (= (expmod a n n) a))
	 (report-fail a))
    (if (< a n)
	(helper (+ a 1))))
  (helper 1))



(define (timed-prime-test n)
  ;(newline)
  ;(display n)
  (start-prime-test n (runtime)))

;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (square (expmod base (/ exp 2) m))
;;                     m))
;;         (else
;;          (remainder (* base (expmod base (- exp 1) m))
;;                     m))))

;; (define (expmod-miller base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;; 	 (square-check (expmod base (/ exp 2) m) m))
;;         (else
;;          (remainder (* base (expmod-miller base (- exp 1) m))
;;                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; (define (miller-rabin-test n)
;;   (define (try-it a)
;;     (= (expmod-miller a n n) a))
;;   (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 50)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (even? n)
  (= (remainder n 2) 0))

(define (search-for-primes begin end)
  (define (helper count)
    (timed-prime-test count)
    (if (< count end)
	(helper (next count))))
  (if (even? begin)
      (helper (+ 1 begin))
      (helper begin)))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define square (lambda n) (* n n))

;; (define (square-check x m)
;;   (if (and (not (or (= x 1) (= x (- m 1))))
;; 	   (= (remainder (* x x) m) 1))
;;       0
;;       (remainder (* x x) m)))



(define (square-check x m)
  (if (and (not (or (= x 1) (= x (- m 1))))
           (= (remainder (* x x) m) 1))
      0
      (remainder (* x x) m)))

;; (define (even? n)
;;   (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (square-check (expmod base (/ exp 2) m) m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (miller-rabin-test n)
  (define (try-it a)
     (= (expmod a (- n 1) n) 1))
  (try-it (+ 2 (random-integer (- n 2)))))  


