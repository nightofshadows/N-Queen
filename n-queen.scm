(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter (lambda (positions) (safe? k positions))
	      (flatmap (lambda (rest-of-queens)
			 (map (lambda (new-row)
				(adjoin-position
				  new-row k rest-of-queens))
			      (enumerate-interval 1 board-size)))
		       (queen-cols (- k 1))))))
  (queen-cols board-size))
	      
(define empty-board nil)

(define (adjoin-position row column rest-of-queens)
  (cons (list row column) rest-of-queens))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq) (accumulate op initial (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ 1 low) high))))

(define (filter predicate sequence)
  (if (null? sequence)
    nil
    (if (predicate (car sequence))
      (cons (car sequence) (filter predicate (cdr sequence)))
      (filter predicate (cdr sequence)))))

;; Conditions for the safety of new queen
;; 1. No same row = Check with car of new queen and second queen
;; 2. No same column = Check with cadr of new queen and second queen
;; 3. Checking the Diagonals.
;;    = It is clear that the diagonals in a cube make a 45 degree angle with
;; 	the vertices which means that cubes lying on the diagonal have the
;; 	the same slope when we take x2 - x1 / y2 - y1.
;; 	As it is clear that the slope is either 1 or -1.
;; 	That means the condition for diagonals is abs (x2-x1) = abs (y2-y1)

(define (row queen) (car queen))
(define (col queen) (cadr queen))

(define (queen-place? new-queen old-queen)
  (let((nx (row new-queen))
	(ny (col new-queen))
	(ox (row old-queen))
	(oy (col old-queen)))
    (cond ((or (= nx ox) (= ny oy)) #f)                ;;Checks col and row
	  ((= (abs (- nx ox)) (abs (- ny oy))) #f)     ;;Checks diagonals left and right
	  (else #t))))
	   
(define (safe? k positions) (= 0 (accumulate + 0 (map (lambda(x) (if (queen-place? (car positions) x) 0 1)) (cdr positions)))))

  

