(require racket/base)

;; DATA TYPES

;; PIECE
; A piece is a struct with 2 members
;   - type  -> a String which indicates the type
;   - moves -> a List of valid moves for the piece
(define-struct piece [type moves])

;; CELL
; A cell is a struct with a value
;   - value -> one of EMPTY, PIECE
(define-struct cell [value])

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

;;PROVIDES
(provide piece)
(provide cell)
(provide board)

(provide draw-board)
(provide make-move)
(provide validate-move)