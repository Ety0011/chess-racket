require 2htdp/image)
(require htdp/universe)

(define BLACK #f)
(define WHITE #t)

;; DATA TYPES
A Type is one of those Strings:
; - "king"
; - "queen"
; - "rook"
; - "bishop"
; - "knight"
; - "pawn"

; A Piece is a struct (make-piece type color) where
;   - type      -> a String which indicates the type
;   - color     -> Boolean where is BLACK or WHITE
(define-struct piece [type color])


; Definition of the white pieces
(define WHITE_KING      (make-piece     "king"      WHITE))
(define WHITE_QUEEN     (make-piece     "queen"     WHITE))
(define WHITE_ROOK      (make-piece     "rook"      WHITE))
(define WHITE_BISHOP    (make-piece     "bishop"    WHITE))
(define WHITE_KNIGHT    (make-piece     "knight"    WHITE))
(define WHITE_PAWN      (make-piece     "pawn"      WHITE))

; Definition of the black pieces
(define BLACK_KING      (make-piece     "king"      BLACK))
(define BLACK_QUEEN     (make-piece     "queen"     BLACK))
(define BLACK_ROOK      (make-piece     "rook"      BLACK))
(define BLACK_BISHOP    (make-piece     "bishop"    BLACK))
(define BLACK_KNIGHT    (make-piece     "knight"    BLACK))
(define BLACK_PAWN      (make-piece     "pawn"      BLACK))

(define EMPTY "empty")

; Cell is one of:
;   - Piece
;   - EMPTY

A Matrix is a Vector of Vectors of Cells
(define CHESSBOARD_MATRIX
  (vector
   (vector BLACK_ROOK       BLACK_KNIGHT     BLACK_BISHOP       BLACK_QUEEN         BLACK_KING      BLACK_BISHOP        BLACK_KNIGHT        BLACK_ROOK)
   (vector BLACK_PAWN       BLACK_PAWN       BLACK_PAWN         BLACK_PAWN          BLACK_PAWN      BLACK_PAWN          BLACK_PAWN          BLACK_PAWN)
   (vector EMPTY            EMPTY            EMPTY              EMPTY               EMPTY           EMPTY               EMPTY               EMPTY)
   (vector EMPTY            EMPTY            EMPTY              EMPTY               EMPTY           EMPTY               EMPTY               EMPTY)
   (vector EMPTY            EMPTY            EMPTY              EMPTY               EMPTY           EMPTY               EMPTY               EMPTY)
   (vector EMPTY            EMPTY            EMPTY              EMPTY               EMPTY           EMPTY               EMPTY               EMPTY)
   (vector WHITE_PAWN       WHITE_PAWN       WHITE_PAWN         WHITE_PAWN          WHITE_PAWN      WHITE_PAWN          WHITE_PAWN          WHITE_PAWN)
   (vector WHITE_ROOK       WHITE_KNIGHT     WHITE_BISHOP       WHITE_QUEEN         WHITE_KING      WHITE_BISHOP        WHITE_KNIGHT        WHITE_ROOK)))

(define CHESSBOARD)






(define PAWN-MOVES (list "TODO: see how to make moves"))
(define ROOK-MOVES (list "TODO: see how to make moves"))
(define BISHOP-MOVES (list "TODO: see how to make moves"))
(define KNIGHT-MOVES (list "TODO: see how to make moves"))
(define KING-MOVES (list "TODO: see how to make moves"))
(define QUEEN-MOVES (list "TODO: see how to make moves"))
(define EMPTY-MOVES '())



; cell
; A cell is one of:
; - 

;; BOARD
; A board is a vector of cells and a canvas
;    - cells  -> vector of 64 cells which are on the board
;    - canvas -> image result from the vector of cells
(define-struct board [cells canvas])














;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTIONS

;; draw-board : board -> board
; Takes a board as input and updates its canvas member based on its
; current state. This should be called everytime a move is made (so
; after the end of every turn)
(define (draw-board board) #f)

;; template
; (define (draw-board board)
;   (... board-cells ...
;    ... board-canvas ... ))

;;TODO: write tests for draw-board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-move : board Number Move -> board
; Takes a board as input along with the target cell (given by the
; Number, which is a number between 1 and 64 indicating the target cell)
; and a Move. When called, a function validate-move is called to
; see if the move is legal
(define (make-move board target mov) #f)

;; template
; (define (make-move board target mov)
;    (... board-cells ... target ...
;     ... mov ...))

;;TODO: write tests for make-move
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; validate-move : board Number Move -> Boolean
; Takes a board, a number signifying the target cell and a move. If
; the move is legal it returns #t, if it's not it returns #f
(define (validate-move board target mov) #f)

;;TODO: write template and tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO: a way to check for checks/checkmate

;;PROVIDES
(provide piece)
(provide cell)
(provide board)

(provide draw-board)
(provide make-move)
(provide validate-move)