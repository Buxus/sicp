;; *Exercise 2.42:* The "eight-queens puzzle" asks how to place eight
;;    queens on a chessboard so that no queen is in check from any other
;;    (i.e., no two queens are in the same row, column, or diagonal).
;;    One possible solution is shown in *Note Figure 2-8::.  One way to
;;    solve the puzzle is to work across the board, placing a queen in
;;    each column.  Once we have placed k - 1 queens, we must place the
;;    kth queen in a position where it does not check any of the queens
;;    already on the board.  We can formulate this approach recursively:
;;    Assume that we have already generated the sequence of all possible
;;    ways to place k - 1 queens in the first k - 1 columns of the
;;    board.  For each of these ways, generate an extended set of
;;    positions by placing a queen in each row of the kth column.  Now
;;    filter these, keeping only the positions for which the queen in
;;    the kth column is safe with respect to the other queens.  This
;;    produces the sequence of all ways to place k queens in the first k
;;    columns.  By continuing this process, we will produce not only one
;;    solution, but all solutions to the puzzle.

;;    We implement this solution as a procedure `queens', which returns a
;;    sequence of all solutions to the problem of placing n queens on an
;;    n*n chessboard.  `Queens' has an internal procedure `queen-cols'
;;    that returns the sequence of all ways to place queens in the first
;;    k columns of the board.

;; *Figure 2.8:* A solution to the eight-queens puzzle.

;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   |   |   | Q |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   | Q |   |   |   |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      | Q |   |   |   |   |   |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   |   |   |   | Q |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   |   | Q |   |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   |   |   |   |   | Q |
;;      +---+---+---+---+---+---+---+---+
;;      |   | Q |   |   |   |   |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   | Q |   |   |   |   |
;;      +---+---+---+---+---+---+---+---+

;; In this procedure `rest-of-queens' is a way to place k - 1 queens
;; in the first k - 1 columns, and `new-row' is a proposed row in
;; which to place the queen for the kth column.  Complete the program
;; by implementing the representation for sets of board positions,
;; including the procedure `adjoin-position', which adjoins a new
;; row-column position to a set of positions, and `empty-board',
;; which represents an empty set of positions.  You must also write
;; the procedure `safe?', which determines for a set of positions,
;; whether the queen in the kth column is safe with respect to the
;; others.  (Note that we need only check whether the new queen is
;; safe--the other queens are already guaranteed safe with respect to
;; each other.)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (make-position row col)
  (cons row col))

(define (position-row position)
  (car position))

(define (position-col position)
  (cdr position))

(define (adjoin-position new-row column rest-of-queens)
  ;; adjoins a row-column position to a set of positions
  (append rest-of-queens (list (make-position new-row column))))

(define test-queens
  (adjoin-position 3 3
		   (adjoin-position 2 2
				    (list (make-position 1 1)))))

;; test-queens
;; ((1 . 1) (2 . 2) (3 . 3))
;; (list-ref test-queens 1)
;; (2 . 2)
;; (position-row (list-ref test-queens 1))
;; 2

(define test-queens-fail-horiz
  (adjoin-position 3 3
		   (adjoin-position 1 2
				    (list (make-position 1 1)))))

;; test-queens-fail-horiz
;; ((1 . 1) (1 . 2) (3 . 3))

;; (define test-position (make-position 1 1))
;; test-position (1 . 1)


;; (member test-position test-queens)
;; ((1 . 1) (2 . 2) (3 . 3))

;; (map position-row test-queens)
;; (1 2 3)

;; (not (member (position-row test-position)
;; 	     (map position-row test-queens)))

;; (take test-queens
;;       (- (position-row test-position) 1))
;; ()



(define empty-board '())
;; represents an empty set of positions

(define (safe? column positions)
  ;; determines for a set of positions,
  ;; whether the queen in the kth column is safe with respect to the
  ;; others.  (Note that we need only check whether the new queen is
  ;; safe--the other queens are already guaranteed safe with respect to
  ;; each other.)
  (let ((kth-queen (list-ref positions (- column 1))))
    (let ((rest-queens (take positions (- (position-col kth-queen) 1))))
      (define (safe-horizontal? position rest-positions)
	(not (member (position-row position)
		     (map position-row rest-positions))))
      (define (safe-up-left-diag? position rest-positions)
	(cond ((or (< (position-col position) 0)
		   (< (position-row position) 0)) #t)
	      ((member position rest-positions) #f)
	      (else (safe-up-left-diag? (make-position (- (position-row position) 1)
						       (- (position-col position) 1))
					rest-positions))))      
      (define (safe-down-left-diag? position rest-positions)
	(cond ((< (position-col position) 0) #t) ;; need to add position-row out-of-bounds condition?
	      ((member position rest-positions) #f)
	      (else (safe-down-left-diag? (make-position (+ (position-row position) 1)
							 (- (position-col position) 1))
					  rest-positions))))
      
      (if (and (safe-horizontal? kth-queen rest-queens)
	       (safe-up-left-diag? kth-queen rest-queens)
	       (safe-down-left-diag? kth-queen rest-queens))
	  #t
	  #f))))

;; (queens 1)
;; (((1 . 1)))

;; (queens 3)
;; ()

;; (queens 4)
;; (((2 . 1) (4 . 2) (1 . 3) (3 . 4)) ((3 . 1) (1 . 2) (4 . 3) (2 . 4)))

;; (length (queens 8))
;; 92

;; (length (queens 10))
;; 724

;; (length (queens 11))
;; ERRORError: retort-syntax


;; *Exercise 2.43:* Louis Reasoner is having a terrible time doing
;; *Note Exercise 2-42::.  His `queens' procedure seems to work, but
;; it runs extremely slowly.  (Louis never does manage to wait long
;; enough for it to solve even the 6*6 case.)  When Louis asks Eva Lu
;; Ator for help, she points out that he has interchanged the order
;; of the nested mappings in the `flatmap', writing it as

;; (flatmap
;;  (lambda (new-row)
;;    (map (lambda (rest-of-queens)
;; 	  (adjoin-position new-row k rest-of-queens))
;; 	(queen-cols (- k 1))))
;;  (enumerate-interval 1 board-size))

;; Explain why this interchange makes the program run slowly.
;; Estimate how long it will take Louis's program to solve the
;; eight-queens puzzle, assuming that the program in *Note Exercise
;; 2-42:: solves the puzzle in time T.

;; Louis's program generates for each queen the board configurations for each
;; rest-of-queens not each-new-row.  The result will be boards with queens placed
;; in/not on every possible square.  the puzzle will therefore take (board-size ^ board*size) * T
