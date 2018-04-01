;;;; RSA.SCM

;;;; fast modular exponentiation. From the textbook, section 1.2.4

(define (expmod b e m)
  (cond ((zero? e) 1)
        ((even? e)
         (remainder
	  (square (expmod b (/ e 2) m))
                    m))
        (else
         (remainder (* b (expmod b (- e 1) m))
                    m))))

(define (square x) (* x x))


;;; An RSA key consists of a modulus and an exponent.

(define make-key cons)
(define key-modulus car)
(define key-exponent cdr)

(define (RSA-transform number key)
  (expmod number (key-exponent key) (key-modulus key)))

;;; The following routine compresses a list of numbers to a single
;;; number for use in creating digital signatures.

(define (compress intlist)
  (define (add-loop l)
    (if (null? l)
        0
        (+ (car l) (add-loop (cdr l)))))
  (modulo (add-loop intlist) (expt 2 28)))
;;;; generating RSA keys

;;; To choose a prime, we start searching at a random odd number in a
;;; specifed range

(define (choose-prime smallest range)
  (let ((start (+ smallest (choose-random range))))
    (search-for-prime (if (even? start) (+ start 1) start))))

(define (search-for-prime guess)
  (if (fast-prime? guess 2)
      guess
      (search-for-prime (+ guess 2))))

;;; The following procedure picks a random number in a given range,
;;; but makes sure that the specified range is not too big for
;;; Scheme's RANDOM primitive.

(define choose-random
  ;; restriction of Scheme RANDOM primitive
  (let ((max-random-number (expt 10 18))) 
    (lambda (n)
      (random (floor->exact (min n max-random-number))))))


;;; The Fermat test for primality, from the texbook section 1.2.6

(define (fermat-test n)
    (let ((a (choose-random n)))
      (= (expmod a n n) a)))

(define (fast-prime? n times)
    (cond ((zero? times) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))
;;; RSA key pairs are pairs of keys

(define make-key-pair cons)
(define key-pair-public car)
(define key-pair-private cdr)

;;; generate an RSA key pair (k1, k2).  This has the property that
;;; transforming by k1 and transforming by k2 are inverse operations.
;;; Thus, we can use one key as the public key and one as the private key.

(define (generate-RSA-key-pair)
  (let ((size (expt 2 14)))
    ;; we choose p and q in the range from 2^14 to 2^15.  This insures
    ;; that the pq will be in the range 2^28 to 2^30, which is large
    ;; enough to encode four characters per number.
    (let ((p (choose-prime size size))
          (q (choose-prime size size)))
    (if (= p q)       ;check that we haven't chosen the same prime twice
        (generate-RSA-key-pair)     ;(VERY unlikely)
        (let ((n (* p q))
              (m (* (- p 1) (- q 1))))
          (let ((e (select-exponent m)))
            (let ((d (invert-modulo e m)))
              (make-key-pair (make-key n e) (make-key n d)))))))))


;;; The RSA exponent can be any random number relatively prime to m

(define (select-exponent m)
  (let ((try (choose-random m)))
    (if (= (gcd try m) 1)
        try
        (select-exponent m))))


;;; Invert e modulo m

(define (invert-modulo e m)
  (if (= (gcd e m) 1)
      (let ((y (cdr (solve-ax+by=1 m e))))
        (modulo y m))                   ;just in case y was negative
      (error "gcd not 1" e m)))


;;; solve ax+by=1
;;; The idea is to let a=bq+r and solve bx+ry=1 recursively

;;;(define (solve-ax+by=1 a b)...) you must complete this procedure


;;; Actual RSA encryption and decryption

(define (RSA-encrypt string key1)
  (RSA-convert-list (string->intlist string) key1))

(define (RSA-convert-list intlist key)
  (let ((n (key-modulus key)))
    (define (convert l sum)
      (if (null? l)
          '()
          (let ((x (RSA-transform (modulo (- (car l) sum) n)
                                  key)))
            (cons x (convert (cdr l) x)))))
    (convert intlist 0)))

(define (RSA-decrypt intlist key2)
  (intlist->string (RSA-unconvert-list intlist key2)))

;;;(define (RSA-unconvert-list intlist key) ...)
;;;  You must complete this procedure 

(define (RSA-unconvert-list intlist key)
  (let ((n (key-modulus key)))
    (define (unconvert l sum)
      (if (null? l)
	  '()
	  (let ((x (modulo (+ sum
			      (RSA-transform (car l) key))
			   n)))
	    (cons x (unconvert (cdr l) (car l))))))
    (unconvert intlist 0)))

;;;; searching for divisors.

;;; The following procedure is very much like the find-divisor
;;; procedure of section 1.2.6 of the test, except that it increments
;;; the test divisor by 2 each time (compare exercise 1.18 of the
;;; text).  You should be careful to call it only with odd numbers n.

(define (smallest-divisor n)
  (find-divisor n 3))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 2)))))

(define (divides? a b)
  (= (remainder b a) 0))


;;;; converting between strings and numbers

;;; The following procedures are used to convert between strings, and
;;; lists of integers in the range 0 through 2^28.  You are not
;;; responsible for studying this code -- just use it.

;;; Convert a string into a list of integers, where each integer
;;; encodes a block of characters.  Pad the string with spaces if the
;;; length of the string is not a multiple of the blocksize.

(define (string->intlist string)
  (let ((blocksize 4))
    (let ((padded-string (pad-string string blocksize)))
      (let ((length (string-length padded-string)))
        (block-convert padded-string 0 length blocksize)))))

(define (block-convert string start-index end-index blocksize)
  (if (= start-index end-index)
      '()
      (let ((block-end (+ start-index blocksize)))
        (cons (charlist->integer
	       (string->list (substring string start-index block-end)))
              (block-convert string block-end end-index blocksize)))))

(define (pad-string string blocksize)
  (let ((rem (remainder (string-length string) blocksize)))
    (if (= rem 0)
        string
        (string-append string (make-string (- blocksize rem) #\Space)))))

;;; Encode a list of characters as a single number
;;; Each character gets converted to an ascii code between 0 and 127.
;;; Then the resulting number is c[0]+c[1]*128+c[2]*128^2,...

(define (charlist->integer charlist)
  (let ((n (char->integer (car charlist))))
    (if (null? (cdr charlist))
        n
        (+ n (* 128 (charlist->integer (cdr charlist)))))))

;;; Convert a list of integers to a string. (Inverse of
;;; string->intlist, except for the padding.)

(define (intlist->string intlist)
  (list->string
   (apply
    append
    (map integer->charlist intlist))))
;;; Decode an integer into a list of characters.  (This is essentially
;;; writing the integer in base 128, and converting each "digit" to a
;;; character.)

(define (integer->charlist integer)
  (if (< integer 128)
      (list (ascii->char integer))
      (cons (ascii->char (remainder integer 128))
            (integer->charlist (quotient integer 128)))))

;;;; the following procedure is handy for timing things

(define (timed f . args)
  (let ((init (runtime)))
    (let ((v (apply f args)))
      (write-line (list 'time: (- (runtime) init)))
      v)))

;;;; Some initial test data

(define test-key-pair1
  (make-key-pair
   (make-key 816898139 180798509)
   (make-key 816898139 301956869)))

(define test-key-pair2
  (make-key-pair
   (make-key 513756253 416427023)
   (make-key 513756253 462557987)))

;;;public keys for political figures

(define bill-clinton-public-key (make-key 833653283 583595407))
(define al-gore-public-key (make-key 655587853 463279441))
(define bob-dole-public-key (make-key 507803083 445001911))
(define ross-perot-public-key (make-key 865784123 362279729))
(define hillary-clinton-public-key (make-key 725123713 150990017))
(define tipper-gore-public-key (make-key 376496027 270523157))
(define chuck-vest-public-key (make-key 780450379 512015071))
(define rupert-murdoch-public-key (make-key 412581307 251545759))
(define newt-gingrich-public-key (make-key 718616329 290820109))
(define newt-gingrich-private-key (make-key 718616329 129033029))

(define political-public-keys
  (list bill-clinton-public-key al-gore-public-key bob-dole-public-key ross-perot-public-key hillary-clinton-public-key tipper-gore-public-key chuck-vest-public-key rupert-murdoch-public-key newt-gingrich-public-key))

;;;message received by Newt Gingrich -- Who sent it?
;; (define received-mystery-message
;;   '(510560918 588076790 115222453 249656722 408910590 69814552
;;     690687967 281490047 41430131 256420885 184791295 75938032
;;     693840839 663727111 593617709 335351412))

;; (define received-mystery-signature 65732336)

;; (define received-mystery-message-signed
;;   (signed-message-create received-mystery-message
;; 			 (list received-mystery-signature)))

;; (RSA-decrypt received-mystery-message newt-gingrich-private-key)
;; "The time has come for the Pineapple to stir up the Whitewater.  "

;; (define (find-sender signed-mystery-message public-keys-list private-key)
;;   (map (lambda (public-key)
;; 	 (cons public-key (cdr (authenticate-and-decrypt signed-mystery-message public-key private-key))))
;;        public-keys-list))
	      
;; (find-sender received-mystery-message-signed political-public-keys
;; newt-gingrich-private-key) (((833653283 . 583595407) . #f)
;; ((655587853 . 463279441) . #f) ((507803083 . 445001911) . #f)
;; ((865784123 . 362279729) . #f) ((725123713 . 150990017) . #f)
;; ((376496027 . 270523157) . #f) ((780450379 . 512015071) . #f)
;; ((412581307 . 251545759) . #t) ((718616329 . 290820109) . #f))


;; This message came from, (412581307 . 251545759), which appears to
;; be Rupert Murdoch.


;;; Exercises
;; Exercise 1

;; The two are effectively equivalent, the first form being syntactic
;; sugar for the second.  Additionally, the method used to call them
;; is slightly different.  The first method is called as (choose-prime
;; smallest range) and the second as ((choose-prime) smallest range),
;; as you need to return the lambda function before it can be
;; evaluated.

;; Exercise 2

;; generate-rsa-key-pair -> ((n . e) n . d)

;; [ ][ ] ->   [ ][ ] -> d
;; |            |
;; |           \/
;; \/           n
;; [ ][ ] -> e
;;  |
;; \/
;; n


;; Exercise 3
;; Using encrypt->sign, one can not necessarily be sure
;; that the sender is aware of the plaintext content of the message.
;; However, anyone (not just the recipient) can verify the message.  On
;; the other hand, sign->encrypt prevents anyone but the recipient from
;; verifying the message, however, one can be sure that the sender was
;; aware of the plaintext content of the message.  In practice, best
;; security can be obtained by sign->encrypt->sign or sign->encrypt,
;; while prepending the identity of the recipient before signing.


;; Exercise 4

;; (define (RSA-decrypt intlist key2)
;;   (map (lambda (int) (rsa-transform int key2))
;;        intlist))

;; In this scheme, the value of one number does not depend on all the
;; others.  Therefore, sections could be changed and when decrypted,
;; the recipient would not be aware the message was edited.  The
;; proper implementation ensures that the entire message is intact.

;;; Assignment Exercises
;;; Exercise 2

(define signed-message-create cons)

(define signed-message-message car)

(define signed-message-signature cdr)

(define (encrypt-and-sign message private-key public-key)
  (let* ((encrypted-message (RSA-encrypt message public-key))
	 (unencrypted-signature (list (compress encrypted-message)))
	 (encrypted-signature (RSA-convert-list unencrypted-signature private-key)))
    (signed-message-create encrypted-message encrypted-signature)))
	 

(define test-public-key2 (key-pair-public test-key-pair2))
(define test-private-key2 (key-pair-private test-key-pair2))

;; (define result2
;;   (encrypt-and-sign "Test message from user 1 to user 2"
;;   test-private-key1
;;   test-public-key2))

;; result2
;; ((499609777 242153055 12244841 376031918 242988502 31156692
;; 221535122 463709109 468341391) 15378444)

(define (authenticate-and-decrypt signed-message public-key private-key)
  (let ((encrypted-message (signed-message-message signed-message))
	(encrypted-signature (signed-message-signature signed-message)))
    (let ((decrypted-message (RSA-decrypt encrypted-message private-key))
	  (unencrypted-signature (list (compress encrypted-message)))
	  (decrypted-signature (RSA-unconvert-list encrypted-signature public-key)))
      (if (= (car unencrypted-signature) (car decrypted-signature))
	  (cons decrypted-message #t)
	  (cons decrypted-message #f)))))

;; (authenticate-and-decrypt result2 test-public-key1 test-private-key2)
;; ("Test message from user 1 to user 2  " . #t)

;; Exercise 4


;; (define (solve-ax+by=1 a b)
;;   (if (= b 0)
;;       (cons 1 0)
;;       (let ((q (quotient a b))
;; 	    (r (remainder a b)))
;; 	(let ((res (solve-ax+by=1 b r)))
;; 	  (cons (cdr res) (+ (car res) (* (- q) (cdr res))))))))

(define (ax+by=1 a b)
        (if (= b 0)
            (cons 1 0)
            (let* ((q (quotient a b))
                   (r (remainder a b))
                   (e (ax+by=1 b r))
                   (s (car e))
                   (t (cdr e)))
           (cons t (- s (* q t))))))

;; (solve-ax+by=1 233987973 41111687)
;; (-11827825 . 67318298)  

(define test-answer
  (let* ((a 233987973)
	 (b 41111687)
	 (res (solve-ax+by=1 a b))
	 (x (car res))
	 (y (cdr res)))
    (= 1 (+ (* a x)
	    (* b y)))))

;; test-answer
;; #t

(define (crack-rsa public-key)
  (let ((n (key-modulus public-key))
	(e (key-exponent public-key)))
    (let* ((p (smallest-divisor n))
	   (q (/ n p)))
      (let ((m (* (- p 1) (- q 1))))
	(cdr (solve-ax+by=1 m e))))))

;; (crack-rsa newt-gingrich-public-key)
;; 129033029
;; (key-exponent newt-gingrich-private-key)
;; 129033029

;; (crack-rsa (key-pair-public test-key-pair1))
;; 301956869
;; (key-pair-private test-key-pair1)
;; (816898139 . 301956869)

;; (crack-rsa bill-clinton-public-key)
;; -130787105


;; Exercise 6
(define (forge-message message sender-public-key recipient-public-key)
  (let ((cracked-key (make-key (key-modulus sender-public-key)
			       (crack-rsa sender-public-key))))
    (RSA-encrypt message cracked-key)))

;; (define forged-message-to-gore
;;   (forge-message "Dear Mr. Gore, Please raise taxes.  From President clinton." bill-clinton-public-key al-gore-public-key))

;; Exercise 8

;; (timed smallest-divisor 780450379)
;; 25057


;; (timed smallest-divisor 
