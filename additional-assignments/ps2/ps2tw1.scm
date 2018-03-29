;; Scheme code for Twenty-One Simulator [PS2 Fall '90]

(define (twenty-one player-strategy house-strategy)
  (let ((house-initial-hand (make-new-hand (deal))))
    (let ((player-hand
           (play-hand player-strategy
                      (make-new-hand (deal))
                      (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
          0                                ; ``bust'': player loses
          (let ((house-hand 
                 (play-hand house-strategy
                            house-initial-hand
                            (hand-up-card player-hand))))
            (cond ((> (hand-total house-hand) 21)
                   1)                      ; ``bust'': house loses
                  ((> (hand-total player-hand)
                      (hand-total house-hand))
                   1)                      ; house loses
                  (else 0)))))))           ; player loses

(define (play-hand strategy my-hand opponent-up-card)
  (cond ((> (hand-total my-hand) 21) my-hand) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
         (play-hand strategy
                    (hand-add-card my-hand (deal))
                    opponent-up-card))
        (else my-hand)))                ; stay


(define (deal) (+ 1 (random 10)))

(define (hit? your-hand opponent-up-card)
  (newline)
  (display "Opponent up card ")
  (display opponent-up-card)
  (newline)
  (display "Your Total: ")
  (display (hand-total your-hand))
  (newline)
  (display "Hit? ")
  (user-says-y?))


(define (user-says-y?) (eq? (read-char) 'y))

(define (stupid-strategy my-hand opponent-up-card)
  (> opponent-up-card 5))

(define (stop-at num)
  (lambda (your-hand opponent-up-card)
    (if (< (hand-total your-hand) num)
	#t
	#f)))

(define (test-strategy strat1 strat2 num-games)
  (define (iter strat1 strat2 num-games player-wins)
    (if (= 0 num-games)
	player-wins
	(iter strat1
	      strat2
	      (- num-games 1)
	      (+ player-wins
		 (twenty-one strat1 strat2)))))
  (iter strat1 strat2 num-games 0))

;; (test-strategy (stop-at 16) (stop-at 21) 10)
;;  7

(define (watch-player strategy)
  (lambda (your-hand opponent-up-card)
    (newline)
    (display "Opponent up card ")
    (display opponent-up-card)
    (newline)
    (display "Your total: ")
    (display (hand-total your-hand))
    (newline)
    (display "Decision: ")
    (display (if (strategy your-hand opponent-up-card)
		 "Hit"
		 "Stand"))
    (strategy your-hand opponent-up-card)))

;; TODO This doesn't work right yet
;; 
;; (test-strategy (watch-player (stop-at 16))
;; 	       (watch-player (stop-at 15))
;; 	       2)

;; Opponent up card 8
;; Your total: 10
;; Decision: Hit
;; Opponent up card 8
;; Your total: 13
;; Decision: Hit
;; Opponent up card 8
;; Your total: 20
;; Decision: Stand
;; Opponent up card 10
;; Your total: 8
;; Decision: Hit
;; Opponent up card 10
;; Your total: 18
;; Decision: Stand
;; Opponent up card 2
;; Your total: 3
;; Decision: Hit
;; Opponent up card 2
;; Your total: 9
;; Decision: Hit
;; Opponent up card 2
;; Your total: 18
;; Decision: Stand
;; Opponent up card 3
;; Your total: 2
;; Decision: Hit
;; Opponent up card 3
;; Your total: 5
;; Decision: Hit
;; Opponent up card 3
;; Your total: 12
;; Decision: Hit
;; Opponent up card 3
;; Your total: 20
;; Decision: Stand
;; ;Value: 1

(define (louis your-hand opponent-up-card)
  (cond ((< (hand-total your-hand) 12) #t)
	((> (hand-total your-hand) 16) #f)
	((and (= (hand-total your-hand) 16) (= opponent-up-card 10)) #f)
	(else
	 (if (> opponent-up-card 6)
	     #t #f))))

;; (test-strategy louis (stop-at 15) 10)
;; 5

;; (test-strategy louis (stop-at 16) 10)
;; 2

;; (test-strategy louis (stop-at 17) 10)
;; 4

	   
(define (both strat1 strat2)
  (lambda (your-hand opponent-up-card)
    (and (strat1 your-hand opponent-up-card)
	 (strat2 your-hand opponent-up-card))))

;; Tutorial Exercise 1
;; System to keep track of actual cards in hand

(define (make-card value suit)
  (cons value suit))

(define (card-value card)
  (car card))

(define (card-suit card)
  (cdr card))

(define (add-card-to-set card set)
  (append set (list card)))

(define (make-new-hand first-card)
  (make-hand first-card '()))

(define (make-hand up-card card-set)
  (cons up-card card-set))

(define (hand-up-card hand)
  (car hand))

(define (hand-card-set hand)
  (cdr hand))

(define (hand-total hand)
  (card-set-value (hand-card-set hand)))

(define (accumulate op base func ls)
  (if (null? ls)
      base
      (op (func (car ls))
	  (accumulate op base func (cdr ls)))))

(define (card-set-value card-set)
  (accumulate + 0 card-value card-set))

;; (card-set-value (add-card-to-set (make-card 9 "spades") (make-new-hand (make-card 3 "clubs"))))
;;  12

(define (hand-add-card hand new-card card-set)
  (make-hand (hand-up-card hand)
             (add-card-to-set new-card card-set)))

