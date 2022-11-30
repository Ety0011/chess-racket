#lang racket
;(require 2htdp/universe)
;(require board.rkt)


;DATA TYPE
;A piece-move is a bitwise operation where: 
;   - there are the initial coordinates
;   - the bitshift represents the direction of where the piece can go
;   - there are the new coordinates of the pieces after the bitshift

; Mossa Pietr0
;(define (PAWN-MOVES bitmap lom))

; Mossa Ety











; Mossa Pietro
;(define (KNIGHT-MOVES))

; Mossa Leo
;;ROOK
;; rook-horizontal : Number (Number) -> Number
; Returns all horizontal moves for the rook (doesnt account for other pieces)
(define (rook-horizontal bit-board row)
    (local [(define bitRow (arithmetic-shift #b11111111 (* 8 row)))]
        (bitwise-xor bit-board bitRow)))

;; rook-vertical : Number (Number) -> Number
; Returns all vertical moves for the rook (doesnt account for other pieces)
(define (rook-vertical bit-board col)
    (local [(define bitCol (arithmetic-shift #b0000000100000001000000010000000100000001000000010000000100000001 col))]
        (bitwise-xor bit-board bitCol)))

;; rook-basic : Number (Number Number) -> Number
; Takes the bitboard where there is only one bit set, which is the rook. The other
; two numbers are temporary and will be taken from the matrix
;
; The result of this will be used for further calculations that will account for
; other pieces / possible captures
(define (rook-basic bit-board row col)
    (bitwise-xor (rook-horizontal bit-board row) (rook-vertical bit-board col) ))

;test
(define rook-test-bitboard #b0000000000000000000000000000000000010000000000000000000000000000)
(define rook-basic-test (rook-basic rook-test-bitboard 3 4))


;;; (define (ROOK-MOVES)
;;; 
;;; )

; Mossa Leo
;(define BISHOP-MOVES())

; Mossa Ety
;(define (QUEEN-MOVES))
