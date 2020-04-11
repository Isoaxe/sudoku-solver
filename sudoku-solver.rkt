;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname sudoku-solver) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list) ;gets list-ref, take and drop
        
;; sudoku-v3.rkt

;;
;; Brute force Sudoku solver
;;
;; In Sudoku, the board is a 9x9 grid of SQUARES.
;; There are 9 ROWS and 9 COLUMNS, there are also 9
;; 3x3 BOXES.  Rows, columns and boxes are all UNITs.
;; So there are 27 units.
;;
;; The idea of the game is to fill each square with
;; a Natural[1, 9] such that no unit contains a duplicate
;; number.
;;

;; =================
;; Data definitions:


;; Val is Natural[1, 9]

;; Board is (listof Val|false)   that is 81 elements long
;; interp.
;;  Visually a board is a 9x9 array of squares, where each square
;;  has a row and column number (r, c).  But we represent it as a
;;  single flat list, in which the rows are layed out one after
;;  another in a linear fashion. (See interp. of Pos below for how
;;  we convert back and forth between (r, c) and position in a board.)

;; Pos is Natural[0, 80]
;; interp.
;;  the position of a square on the board, for a given p, then
;;    - the row    is (quotient p 9)
;;    - the column is (remainder p 9)


;; Convert 0-based row and column to Pos
(define (r-c->pos r c) (+ (* r 9) c))  ;helpful for writing tests


;; Unit is (listof Pos) of length 9
;; interp. 
;;  The position of every square in a unit. There are
;;  27 of these for the 9 rows, 9 columns and 9 boxes.


;; NOTE: This solution is more computationally efficient than the solution
;; presented by Gregor. Call (time (solve BD6)) for comparison

;; =================
;; Constants:

(define ALL-VALS (list 1 2 3 4 5 6 7 8 9))

(define B false) ;B stands for blank


(define BD1 
  (list B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define BD2 
  (list 1 2 3 4 5 6 7 8 9 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define BD3 
  (list 1 B B B B B B B B
        2 B B B B B B B B
        3 B B B B B B B B
        4 B B B B B B B B
        5 B B B B B B B B
        6 B B B B B B B B
        7 B B B B B B B B
        8 B B B B B B B B
        9 B B B B B B B B))

(define BD4                ;easy
  (list 2 7 4 B 9 1 B B 5
        1 B B 5 B B B 9 B
        6 B B B B 3 2 8 B
        B B 1 9 B B B B 8
        B B 5 1 B B 6 B B
        7 B B B 8 B B B 3
        4 B 2 B B B B B 9
        B B B B B B B 7 B
        8 B B 3 4 9 B B B))

(define BD4s               ;solution to 4
  (list 2 7 4 8 9 1 3 6 5
        1 3 8 5 2 6 4 9 7
        6 5 9 4 7 3 2 8 1
        3 2 1 9 6 4 7 5 8
        9 8 5 1 3 7 6 4 2
        7 4 6 2 8 5 9 1 3
        4 6 2 7 5 8 1 3 9
        5 9 3 6 1 2 8 7 4
        8 1 7 3 4 9 5 2 6))

(define BD5                ;hard
  (list 5 B B B B 4 B 7 B
        B 1 B B 5 B 6 B B
        B B 4 9 B B B B B
        B 9 B B B 7 5 B B
        1 8 B 2 B B B B B 
        B B B B B 6 B B B 
        B B 3 B B B B B 8
        B 6 B B 8 B B B 9
        B B 8 B 7 B B 3 1))

(define BD5s               ;solution to 5
  (list 5 3 9 1 6 4 8 7 2
        8 1 2 7 5 3 6 9 4
        6 7 4 9 2 8 3 1 5
        2 9 6 4 1 7 5 8 3
        1 8 7 2 3 5 9 4 6
        3 4 5 8 9 6 1 2 7
        9 2 3 5 4 1 7 6 8
        7 6 1 3 8 2 4 5 9
        4 5 8 6 7 9 2 3 1))

(define BD6                ;hardest ever? (Dr Arto Inkala)
  (list B B 5 3 B B B B B 
        8 B B B B B B 2 B
        B 7 B B 1 B 5 B B 
        4 B B B B 5 3 B B
        B 1 B B 7 B B B 6
        B B 3 2 B B B 8 B
        B 6 B 5 B B B B 9
        B B 4 B B B B 3 B
        B B B B B 9 7 B B))

(define BD7                 ; no solution 
  (list 1 2 3 4 5 6 7 8 B 
        B B B B B B B B 2 
        B B B B B B B B 3 
        B B B B B B B B 4 
        B B B B B B B B 5
        B B B B B B B B 6
        B B B B B B B B 7
        B B B B B B B B 8
        B B B B B B B B 9))




;; Positions of all the rows, columns and boxes:

(define ROWS
  (list (list  0  1  2  3  4  5  6  7  8)
        (list  9 10 11 12 13 14 15 16 17)
        (list 18 19 20 21 22 23 24 25 26)
        (list 27 28 29 30 31 32 33 34 35)
        (list 36 37 38 39 40 41 42 43 44)
        (list 45 46 47 48 49 50 51 52 53)
        (list 54 55 56 57 58 59 60 61 62)
        (list 63 64 65 66 67 68 69 70 71)
        (list 72 73 74 75 76 77 78 79 80)))

(define COLS
  (list (list 0  9 18 27 36 45 54 63 72)
        (list 1 10 19 28 37 46 55 64 73)
        (list 2 11 20 29 38 47 56 65 74)
        (list 3 12 21 30 39 48 57 66 75)
        (list 4 13 22 31 40 49 58 67 76)
        (list 5 14 23 32 41 50 59 68 77)
        (list 6 15 24 33 42 51 60 69 78)
        (list 7 16 25 34 43 52 61 70 79)
        (list 8 17 26 35 44 53 62 71 80)))

(define BOXES
  (list (list  0  1  2  9 10 11 18 19 20)
        (list  3  4  5 12 13 14 21 22 23)
        (list  6  7  8 15 16 17 24 25 26)
        (list 27 28 29 36 37 38 45 46 47)
        (list 30 31 32 39 40 41 48 49 50)
        (list 33 34 35 42 43 44 51 52 53)
        (list 54 55 56 63 64 65 72 73 74)
        (list 57 58 59 66 67 68 75 76 77)
        (list 60 61 62 69 70 71 78 79 80)))

(define UNITS (append ROWS COLS BOXES))


;; =================
;; Functions:

;; Board -> Board or false
;; produce a solution for bd; or false if bd is unsolvable
;; Assume: bd is valid
(check-expect (solve BD4) BD4s)
(check-expect (solve BD5) BD5s)
(check-expect (solve BD7) false)

;(define (solve bd) false) ;stub

(define (solve bd)
  (local [(define (solve--bd bd)
            (if (solved? bd)
                bd
                (solve--lobd (next-boards bd))))
          
          (define (solve--lobd lobd)
            (cond [(empty? lobd) false]
                  [else 
                   (local [(define try (solve--bd (first lobd)))]
                     (if (not (false? try))
                         try
                         (solve--lobd (rest lobd))))]))]
    
    (solve--bd bd))) 

;; -------------------------------------

;; Board -> Boolean
;; produce true if board is solved
;; Assume: board is valid, so it is solved if it is full
(check-expect (solved? BD1) false)
(check-expect (solved? BD2) false)
(check-expect (solved? BD4s) true)
 
;(define (solved? bd) false) ; stub

(define (solved? bd)
  (andmap number? bd)) 

;; -------------------------------------

;; Board -> (listof Board)
;; produce list of valid next boards from board
;; finds first empty square, fills it with Natural[1, 9], keeps only valid boards
(check-expect (next-boards (cons 1 (rest BD1)))
              (list (cons 1 (cons 2 (rest (rest BD1))))
                    (cons 1 (cons 3 (rest (rest BD1))))
                    (cons 1 (cons 4 (rest (rest BD1))))
                    (cons 1 (cons 5 (rest (rest BD1))))
                    (cons 1 (cons 6 (rest (rest BD1))))
                    (cons 1 (cons 7 (rest (rest BD1))))
                    (cons 1 (cons 8 (rest (rest BD1))))
                    (cons 1 (cons 9 (rest (rest BD1))))))

;(define (next-boards bd) empty) ;stub

(define (next-boards bd)
  (local [(define pos (find-blank bd))]
    (keep-only-valid pos (fill-with-1-9 pos bd))))

;; -------------------------------------

;; Board -> Pos
;; produces the position of the first blank square
;; ASSUME: the board has at least one blank square
(check-expect (find-blank BD1) 0)
(check-expect (find-blank (cons 2 (rest BD1))) 1)
(check-expect (find-blank (cons 2 (cons 4 (rest (rest BD1))))) 2)

;(define (find-blank bd) 0) ;stub

;<Template is a natural recursion for (listof X)>
(define (find-blank bd)
  (cond [(empty? bd) (error "Blank space not found.")]
        [else
         (if (false? (first bd))
             0
             (+ 1 (find-blank (rest bd))))]))

;; -------------------------------------

;; Pos Board -> (listof Board)
;; produce 9 boards, with blank filled with Natural[1, 9]
(check-expect (fill-with-1-9 0 BD1)
              (list (cons 1 (rest BD1))
                    (cons 2 (rest BD1))
                    (cons 3 (rest BD1))
                    (cons 4 (rest BD1))
                    (cons 5 (rest BD1))
                    (cons 6 (rest BD1))
                    (cons 7 (rest BD1))
                    (cons 8 (rest BD1))
                    (cons 9 (rest BD1))))

;(define (fill-with-1-9 p bd) empty) ;stub

(define (fill-with-1-9 p bd)
  (local [(define (build-one n)
            (fill-square bd p (+ n 1)))]
    (build-list 9 build-one)))

;; -------------------------------------

;; Pos (listof Board) -> (listof Board)
;; produce list containing only valid boards
(check-expect (keep-only-valid 0 (list (cons 1 (cons 1 (rest (rest BD1))))))
              empty)

;(define (keep-only-valid pos lobd) empty) ;stub

(define (keep-only-valid pos lobd)
  (if (empty? lobd)
      empty
      (if (valid-board? pos (first lobd))
          (cons (first lobd)
                (keep-only-valid pos (rest lobd)))
          (keep-only-valid pos (rest lobd)))))

;; -------------------------------------

;; Pos Board -> Boolean
;; produce true if no unit has the same value twice for given position and board, false otherwise
(check-expect (valid-board? 12 BD1) true)
(check-expect (valid-board?  5 BD2) true)
(check-expect (valid-board? 36 BD3) true)
(check-expect (valid-board? 57 BD4) true)
(check-expect (valid-board? 11 BD5) true)
(check-expect (valid-board?  1 (cons 2 (rest BD2))) false)   ;row fails test
(check-expect (valid-board?  9 (cons 2 (rest BD3))) false)   ;column fails test
(check-expect (valid-board?  0 (cons 2 (rest BD3))) false)   ;column and box fail
(check-expect (valid-board?  1 (fill-square BD4 1 6)) false) ;box fails test
(check-expect (valid-board?  3              BD2      )  true)
(check-expect (valid-board? 36              BD3      )  true)
(check-expect (valid-board?  3 (fill-square BD2  3 3)) false)  ;row test fails
(check-expect (valid-board? 27 (fill-square BD3 27 3)) false)  ;column test fails
(check-expect (valid-board? 10 (fill-square BD4 10 2)) false)  ;box test fails

;(define (valid-board? p bd) false) ;stub

(define (valid-board? p bd)
  (local [(define val (read-square bd p))]
    (or (equal? val false)
        (and (valid-row? p bd)
             (valid-col? p bd)
             (valid-box? p bd)))))

;; -------------------------------------

;; Pos Board -> Boolean
;; true if row does not contain Val at given Pos twice, false otherwise
(check-expect (valid-row?  3 BD2) true)
(check-expect (valid-row? 45 BD4) true)
(check-expect (valid-row?  2 (fill-square BD2 7 3)) false) ;3 appears twice in row 0

;(define (valid-row? p bd) false) ;stub

(define (valid-row? p bd)
  (local [(define val (read-square bd p))
          (define row (floor (/ p 9)))
          (define lov (value-list (list-ref ROWS row) bd))]
    (not (member val (remove val lov)))))

;; -------------------------------------

;; Pos Board -> Boolean
;; true if column does not contain Val at given Pos twice, false otherwise
(check-expect (valid-col? 36 BD3) true)
(check-expect (valid-col? 36 (fill-square BD3 0 5)) false) ;5 appears twice in col 0

;(define (valid-col? p bd) false) ;stub

(define (valid-col? p bd)
  (local [(define val (read-square bd p))
          (define col (modulo p 9))
          (define lov (value-list (list-ref COLS col) bd))]
    (not (member val (remove val lov)))))

;; -------------------------------------

;; Pos Board -> Boolean
;; true if box does not contain Val at given Pos twice, false otherwise
(check-expect (valid-box? 1 BD2) true)
(check-expect (valid-box? 1 (fill-square BD2  9 2)) false) ;2 appears twice in box 0
(check-expect (valid-box? 7 (fill-square BD2 15 8)) false) ;8 appears twice in box 2

;(define (valid-box? p bd) false) ;stub

(define (valid-box? p bd)
  (local [(define val (read-square bd p))
          (define row (floor (/ p 9)))
          (define col (modulo p 9))
          (define box (+ (* (floor (/ row 3)) 3) (floor (/ col 3))))
          (define lov (value-list (list-ref BOXES box) bd))]
    (not (member val (remove val lov)))))

;; -------------------------------------

;; (listof Pos) Board -> (listof Val)
;; produce the list of values for a given list of positions on a given board
(check-expect (value-list (list-ref ROWS 0) BD2) ALL-VALS)

;(define (value-list lop bd) empty) ;stub

(define (value-list lop bd)
  (cond [(empty? lop) empty]
        [else
         (cons (read-square bd (first lop))
               (value-list (rest lop) bd))]))

;; -------------------------------------

;; Board Pos -> Val or false
;; Produce value at given position on board.
(check-expect (read-square BD2 (r-c->pos 0 5)) 6)
(check-expect (read-square BD3 (r-c->pos 7 0)) 8)
(check-expect (read-square BD2 5) 6)

(define (read-square bd p)
  (list-ref bd p))

;; -------------------------------------

;; Board Pos Val -> Board
;; produce new board with val at given position
(check-expect (fill-square BD1 (r-c->pos 0 0) 1)
              (cons 1 (rest BD1)))

(define (fill-square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p)))
