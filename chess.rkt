(requite htdp/universe)
(require "board.rkt")

;; INITIAL VALUES
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

