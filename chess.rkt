;; PIECE
; A piece is a struct with 2 members
;   - type  -> a String which indicates the type
;   - moves -> a List of valid moves for the piece
(define-struct piece [type moves])

;; CELL
; A cell is a struct with a value and a color
;   - value -> one of EMPTY, PIECE
;   - color -> color of the cell (?)
(define-struct cell [value color])

;; BOARD
; A board is a vector of cells and a canvas
;    - cells  -> vector of 64 cells which are on the board
;    - canvas -> image result from the vector of cells
(define-struct board [cells canvas])

;; INITIAL DATA
; WHITE
(define WHITE 0)
; BLACK
(define BLACK 1)

; PAWN piece
(define PAWN-MOVES (list "TODO: see how to make moves"))
(define PAWN (make-piece "pawn" PAWN-MOVES))
; ROOK piece
(define ROOK-MOVES (list "TODO: see how to make moves"))
(define ROOK (make-piece "rook" ROOK-MOVES))
; BISHOP piece
(define BISHOP-MOVES (list "TODO: see how to make moves"))
(define BISHOP (make-piece "bishop" BISHOP-MOVES))
; KNIGHT piece
(define KNIGHT-MOVES (list "TODO: see how to make moves"))
(define KNIGHT (make-piece "knight" KNIGHT-MOVES))
; KING piece
(define KING-MOVES (list "TODO: see how to make moves"))
(define KING (make-piece "king" KING-MOVES))
; QUEEN piece
(define QUEEN-MOVES (list "TODO: see how to make moves"))
(define QUEEN (make-piece "queen" QUEEN-MOVES))
; EMPTY piece
(define EMPTY-MOVES '())
(define EMPTY (make-piece "empty" EMPTY-MOVES))

(define CELL1 (make-cell EMPTY WHITE))
(define CELL2 (make-cell EMPTY BLACK))
(define CELL3 (make-cell EMPTY WHITE))
(define CELL4 (make-cell EMPTY BLACK))
(define CELL5 (make-cell EMPTY WHITE))
(define CELL6 (make-cell EMPTY BLACK))
(define CELL7 (make-cell EMPTY WHITE))
(define CELL8 (make-cell EMPTY BLACK))
; ...

(define BOARD (vector CELL1 CELL2 CELL3 CELL4
                      CELL5 CELL6 CELL7 CELL8)) ;...

