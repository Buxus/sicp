(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)    ; symbol
			       (cadr pair))  ; frequency
		    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; (decode sample-message sample-tree)
;; (a d a b b c a)

;; *Exercise 2.68:* The `encode' procedure takes as arguments a
;; message and a tree and produces the list of bits that gives the
;; encoded message.

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

;; `Encode-symbol' is a procedure, which you must write, that returns
;; the list of bits that encodes a given symbol according to a given
;; tree.  You should design `encode-symbol' so that it signals an
;; error if the symbol is not in the tree at all.  Test your
;; procedure by encoding the result you obtained in *Note Exercise
;; 2-67:: with the sample tree and seeing whether it is the same as
;; the original sample message.

(define (encode-symbol symbol tree)
  (cond ((not (memq symbol (symbols tree))) (error "Symbol not in tree: " symbol))
	((and (leaf? (left-branch tree)) (eq? symbol (car (symbols (left-branch tree))))) '(0))
	((and (leaf? (right-branch tree)) (eq? symbol (car (symbols (right-branch tree))))) '(1))
	(else (if (memq symbol (symbols (left-branch tree)))
		  (append '(0) (encode-symbol symbol (left-branch tree)))
		  (append '(1) (encode-symbol symbol (right-branch tree)))))))


;; *Exercise 2.69:* The following procedure takes as its argument a
;; list of symbol-frequency pairs (where no symbol appears in more
;; than one pair) and generates a Huffman encoding tree according to
;; the Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; `Make-leaf-set' is the procedure given above that transforms the
;; list of pairs into an ordered set of leaves.  `Successive-merge'
;; is the procedure you must write, using `make-code-tree' to
;; successively merge the smallest-weight elements of the set until
;; there is only one element left, which is the desired Huffman tree.
;; (This procedure is slightly tricky, but not really complicated.
;;       If you find yourself designing a complex procedure, then you are
;;       almost certainly doing something wrong.  You can take significant
;;       advantage of the fact that we are using an ordered set
;;       representation.)

(define test-leafs
  (make-leaf-set '((A 4) (B 2) (D 1) (C 1))))
;; test-leafs
;; ((leaf c 1) (leaf d 1) (leaf b 2) (leaf a 4))


(define (successive-merge leaf-set)
  (if (= (size-of-set leaf-set) 1)
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set)
						    (cadr leaf-set))
				    (cddr leaf-set)))))
			 
(define size-of-set length)


;; (successive-merge test-leafs)
;; ((leaf a 4) ((leaf b 2) ((leaf c 1) (leaf d 1) (c d) 2) (b c d) 4) (a b c d) 8)


;; *Exercise 2.70:* The following eight-symbol alphabet with
;; associated relative frequencies was designed to efficiently encode
;; the lyrics of 1950s rock songs.  (Note that the "symbols" of an
;; "alphabet" need not be individual letters.)

;;      A     2 NA   16
;;      BOOM  1 SHA  3
;;      GET   2 YIP  9
;;      JOB   2 WAH  1

;; Use `generate-huffman-tree' (*Note Exercise 2-69::) to generate a
;; corresponding Huffman tree, and use `encode' (*Note Exercise
;; 2-68::) to encode the following message:

(define rock-symbols (generate-huffman-tree  '((A 2)
					       (BOOM 1)
					       (GET 2)
					       (JOB 2)
					       (NA 16)
					       (SHA 3)
					       (YIP 9)
					       (WAH 1))))


;;      Get a job

;;      Sha na na na na na na na na

;;      Get a job

;;      Sha na na na na na na na na

;;      Wah yip yip yip yip yip yip yip yip yip

;;      Sha boom

;; (encode '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom) rock-symbols)
;; (length '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1))
;; 84


 
;; How many bits are required for the encoding?  What is the smallest
;; number of bits that would be needed to encode this song if we used
;; a fixed-length code for the eight-symbol alphabet?

;; It takes 84 bits to encode this message.  If using a fixed length code, using 3 bits per character, this 36 character long message would take 3*36=108 bits to encode, and increase of 28%.

;; *Exercise 2.71:* Suppose we have a Huffman tree for an alphabet of
;; n symbols, and that the relative frequencies of the symbols are 1,
;; 2, 4, ..., 2^(n-1).  Sketch the tree for n=5; for n=10.  In such a
;; tree (for general n) how may bits are required to encode the most
;; frequent symbol?  the least frequent symbol?

;n=5
;abcde
;; (map (lambda (x) (expt 2 (- x 1))) '(1 2 3 4 5))
;; (1 2 4 8 16)

(define huffman-tree-5 (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16))))
(((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (a b c d) 15) (leaf e 16) (a b c d e) 31)


;; {A B C D E} 31
;;    |     \
;;    E 16   {A B C D} 15
;;           |           \
;;           D 8         {A B C} 7
;;                      /       \
;;                     {A B} 3   C 4
;;                    /   \
;;                  A 1    B 2

;; (map (lambda (x) (encode-symbol x huffman-tree-5)) '(a b c d e))
;; ((0 0 0 0) (0 0 0 1) (0 0 1) (0 1) (1))
;; ;; most frequent = 1 bit, least frequent = 4 bits

;; ;;n=10
;; ;;abcdefghij
;; ;; (map (lambda (x) (expt 2 (- x 1))) '(1 2 3 4 5 6 7 8 9 10))
;; ;; (1 2 4 8 16 32 64 128 256 512)

 (define huffman-tree-10 (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512))))
;; ((((((((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (a b c d) 15) (leaf e 16) (a b c d e) 31) (leaf f 32) (a b c d e f) 63) (leaf g 64) (a b c d e f g) 127) (leaf h 128) (a b c d e f g h) 255) (leaf i 256) (a b c d e f g h i) 511) (leaf j 512) (a b c d e f g h i j) 1023)

;; (map (lambda (x) (encode-symbol x huffman-tree-10)) '(a b c d e f g h i j))
;; ((0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 1) (0 0 0 0 0 0 1) (0 0 0 0 0 1) (0 0 0 0 1) (0 0 0 1) (0 0 1) (0 1) (1))
;; ;; least frequent = 1 bit, most frequent = 9 bits

;; ;; {A B C D E F G H I J} 1023
;; ;;     |                     \
;; ;; {A B C D E F G H I} 511  J 512
;; ;;     |                 \
;; ;; {A B C D E F G H} 255  I 256
;; ;;     |                \
;; ;; {A B C D E F G} 127   H 128
;; ;;     |             \
;; ;; {A B C D E F} 63   G 64
;; ;;     |          \
;; ;; {A B C D E} 31  F 32
;; ;;     |         \
;; ;; {A B C D} 15   E 16
;; ;;     |       \
;; ;; {A B C} 7   D 8
;; ;;     |    \
;; ;; {A B} 3   C 4
;; ;;     |  \
;; ;;  A 1    B 2

;; *Exercise 2.72:* Consider the encoding procedure that you designed
;; in *Note Exercise 2-68::.  What is the order of growth in the
;; number of steps needed to encode a symbol?  Be sure to include the
;; number of steps needed to search the symbol list at each node
;; encountered.  To answer this question in general is difficult.
;; Consider the special case where the relative frequencies of the n
;; symbols are as described in *Note Exercise 2-71::, and give the
;; order of growth (as a function of n) of the number of steps needed
;; to encode the most frequent and least frequent symbols in the
;; alphabet.

(define (encode-symbol symbol tree)
  (cond ((not (memq symbol (symbols tree))) (error "Symbol not in tree: " symbol)) ;; searches whole (symbols tree), could be optimized by only checking once then using helper function to run through rest of conditionals. O(n) growth
	((and (leaf? (left-branch tree)) (eq? symbol (car (symbols (left-branch tree))))) '(0)) ;; O(1)
	((and (leaf? (right-branch tree)) (eq? symbol (car (symbols (right-branch tree))))) '(1)) ;; O(1)
	(else (if (memq symbol (symbols (left-branch tree))) ;; O(n)
		  (append '(0) (encode-symbol symbol (left-branch tree)))
		  (append '(1) (encode-symbol symbol (right-branch tree)))))))

;; For most frequent symbol, the algorithm only has to search the symbols list of the top level node.
;; This should be O(n) growth.  For the least frequent symbol, the algorithm has to search the
;; (shrinking) symbol list at each non-leaf node.  This will result in
;; O(n^2) growth.  
