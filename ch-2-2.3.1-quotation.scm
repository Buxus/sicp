;; *Exercise 2.53:* What would the interpreter print in response to
;; evaluating each of the following expressions?

(list 'a 'b 'c)
(a b c)

(list (list 'george))
((george))

(cdr '((x1 x2) (y1 y2)))
((y1 y2))

(cadr '((x1 x2) (y1 y2)))
(y1 y2)

(pair? (car '(a short list)))
#f

(pair? '(a short list))
#t

(memq 'red '((red shoes) (blue socks)))
#f

(memq 'red '(red shoes blue socks))
(red shoes blue socks)

;; *Exercise 2.54:* Two lists are said to be `equal?' if they contain
;; equal elements arranged in the same order.  For example,

;; (equal? '(this is a list) '(this is a list))

;; is true, but

;; (equal? '(this is a list) '(this (is a) list))

;; is false.  To be more precise, we can define `equal?'  recursively
;; in terms of the basic `eq?' equality of symbols by saying that `a'
;; and `b' are `equal?' if they are both symbols and the symbols are
;; `eq?', or if they are both lists such that `(car a)' is `equal?'
;; to `(car b)' and `(cdr a)' is `equal?' to `(cdr b)'.  Using this
;; idea, implement `equal?' as a procedure.(5)

(define (equal? a b)
  (if (or (symbol? a) (symbol? b) (null? a) (null? b))
      (eq? a b)
      (and (equal? (car a) (car b))
	   (equal? (cdr a) (cdr b)))))

(equal? '(this is a list) '(this is a list))
#t

(equal? '(this is a list) '(this (is a) list))
#f


;; *Exercise 2.55:* Eva Lu Ator types to the interpreter the
;; expression

(car ''abracadabra)
quote

;; To her subrprise, the interpreter prints back `quote'.  Explain.
''abracadabira
(quote abracadabra)

(cdr ''abracadabra)
(abracadabra)

(car '(abracadabra))
abracadabra

(cdr '(abracadabra))
()

;; ''abracadabra is itself a list where the car is the symbol quote and the cdr is (abracadabra)
