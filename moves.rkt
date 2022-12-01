;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname moves) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
#reader(lib "htdp-advanced-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(require 2htdp/universe)



;DATA TYPE
;A piece-move is a bitwise operation where: 
;   - there are the initial coordinates
;   - the bitshift represents the direction of where the piece can go
;   - there are the new coordinates of the pieces after the bitshift

; Mossa Pietr0
;;Pawn
;(define (PAWN-MOVES bitmap lom))

; Mossa Ety
(define (reverseBinary2 b k_acc total_sum)
  (cond
    [(equal? 64 k_acc) total_sum]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift b (- k_acc 63))))
       (reverseBinary2 b (add1 k_acc) (+ total_sum (arithmetic-shift 1 k_acc)))]
    [else (reverseBinary2 b (add1 k_acc) total_sum)]))

(define (reverseBinary b)
  (reverseBinary2 b 0 #b0000000000000000000000000000000000000000000000000000000000000000))


(define (bitBoardsXOR BITBOARDS k_acc)
  (cond
    [(equal? 11 k_acc) (vector-ref BITBOARDS k_acc)]
    [else
     (bitwise-xor (vector-ref BITBOARDS k_acc) (bitBoardsXOR BITBOARDS (add1 k_acc)))]))

(define OCCUPIED
  (bitBoardsXOR BITBOARDS 0))


(define RANKMASKS
  (vector
   #b1111111100000000000000000000000000000000000000000000000000000000
   #b0000000011111111000000000000000000000000000000000000000000000000
   #b0000000000000000111111110000000000000000000000000000000000000000
   #b0000000000000000000000001111111100000000000000000000000000000000
   #b0000000000000000000000000000000011111111000000000000000000000000
   #b0000000000000000000000000000000000000000111111110000000000000000
   #b0000000000000000000000000000000000000000000000001111111100000000
   #b0000000000000000000000000000000000000000000000000000000011111111))

(define FILEMASKS
  (vector
   #b1000000010000000100000001000000010000000100000001000000010000000
   #b0100000001000000010000000100000001000000010000000100000001000000
   #b0010000000100000001000000010000000100000001000000010000000100000
   #b0001000000010000000100000001000000010000000100000001000000010000
   #b0000100000001000000010000000100000001000000010000000100000001000
   #b0000010000000100000001000000010000000100000001000000010000000100
   #b0000001000000010000000100000001000000010000000100000001000000010
   #b0000000100000001000000010000000100000001000000010000000100000001))


(define (horizontalVerticalMoves matrixPosition)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 matrixPosition)))
    (define horizontalMoves
      (bitwise-xor (- OCCUPIED (* 2 binaryPosition))
                   (reverseBinary (- (reverseBinary OCCUPIED) (* 2 (reverseBinary binaryPosition))))))
    (define verticalMoves
      (bitwise-xor (- (bitwise-and OCCUPIED (vector-ref FILEMASKS (modulo matrixPosition 8))) (* 2 binaryPosition))
                   (reverseBinary (- (reverseBinary (bitwise-and OCCUPIED (vector-ref FILEMASKS (modulo matrixPosition 8)))) (* 2 (reverseBinary binaryPosition)))))))
    (bitwise-ior (bitwise-and horizontalMoves (vector-ref RANKMASKS (floor (/ matrixPosition 8)))) (bitwise-and verticalMoves (vector-ref FILEMASKS (modulo matrixPosition 8))))))











; Mossa Pietro
;;Knight
; returns the moveset of the knight
(define (KNIGHT-NW bit-board row col)
    (cond
    [(and(= col 1)(= row 2)) (bit-board(and(arithmetic-shift col 1)(arithmetic-shift row 2)))]
    [(and(= col -1)(= row 2)) (bit-board(and(arithmetic-shift col -1)(arithmetic-shift row 2)))]
    [(and(= col 2)(= row 1)) (bit-board(and(arithmetic-shift col 2)(arithmetic-shift row 1)))]
    [(and(= col 2)(= row -1)) (bit-board(and(arithmetic-shift col 2)(arithmetic-shift row -1)))]
    [(and(= col -2)(= row 1)) (bit-board(and(arithmetic-shift col -2)(arithmetic-shift row 1)))]
    [(and(= col -2)(= row -1)) (bit-board(and(arithmetic-shift col -2)(arithmetic-shift row -1)))]
    [(and(= col 1)(= row -2)) (bit-board(and(arithmetic-shift col 1)(arithmetic-shift row -2)))]
    [(and(= col -1)(= row -2)) (bit-board(and(arithmetic-shift col -1)(arithmetic-shift row -2)))]
    )
)






; Mossa Leo
;;ROOK

;; CONSTANTS
;; rook-column / rook-line
; Are used for the rook-vertical and rook-horizontal functions
(define rook-column #b0000000100000001000000010000000100000001000000010000000100000001)
(define rook-line   #b11111111)

;; rook-horizontal : Number (Number) -> Number
; Returns all horizontal moves for the rook (doesnt account for other pieces)
(define (rook-horizontal bit-board row)
    (local [(define bitRow (arithmetic-shift rook-line (* 8 row)))]
        (bitwise-xor bit-board bitRow)))

;; rook-vertical : Number (Number) -> Number
; Returns all vertical moves for the rook (doesnt account for other pieces)
(define (rook-vertical bit-board col)
    (local [(define bitCol (arithmetic-shift rook-column col))]
        (bitwise-xor bit-board bitCol)))

;; rook-basic : Number (Number Number) -> Number
; Takes the bitboard where there is only one bit set, which is the rook. The other
; two numbers are temporary and will be taken from the matrix
;
; The result of this will be used for further calculations that will account for
; other pieces / possible captures
(define (rook-basic bit-board row col)
    (bitwise-xor (rook-horizontal bit-board row) (rook-vertical bit-board col) ))


;;; (define (ROOK-MOVES)
;;; 
;;; )

; Mossa Leo
;(define BISHOP-MOVES())
;; tutto copia incollato da drracket perchè non ho sbatti ora

;; le due diagonali della scacchiera
(define main-diag #b1000000001000000001000000001000000001000000001000000001000000001)
(define anti-diag #b0000000100000010000001000000100000010000001000000100000010000000)

; questi numeri vengono shiftati a destra e sinistra per "pulire" le diagonali, in modo
; tale da non avere bit in eccesso 
(define cleanupleft    #b0000011111111111111111111111111111111111111111111111111111111111)
(define cleanupright   #b1111111111111111111111111111111111111111111111111111111111100000)

(define (bishop-main-left bb clean shl/acc)
  (local [(define diag (bitwise-and (arithmetic-shift main-diag shl/acc) (arithmetic-shift clean (- shl/acc))))]
  (cond
    [(= 8 shl/acc) 0]
    [(not (zero? (bitwise-and bb diag))) diag]
    [else
     (bishop-main-left bb (arithmetic-shift clean -4) (add1 shl/acc))])))



(define (bishop-main-right bb clean shr/acc)
  (local [(define diag (bitwise-and (arithmetic-shift main-diag (- shr/acc)) (arithmetic-shift clean shr/acc)))]
     (cond
       [(= 8 shr/acc) 0]
       [(not (zero? (bitwise-and bb diag))) diag]
       [else
        (bishop-main-right bb (arithmetic-shift clean 2) (add1 shr/acc))])))



(define (bishop-anti-left bb clean shl/acc)
  (local [(define diag (bitwise-and (arithmetic-shift anti-diag shl/acc) (arithmetic-shift clean shl/acc) ))]
    (cond
      [(= 8 shl/acc) 0]
      [(not (zero? (bitwise-and bb diag))) diag]
      [else
       (bishop-anti-left bb (arithmetic-shift clean 8) (add1 shl/acc))])))


(define (bishop-anti-right bb clean shr/acc)
  (local [(define diag (bitwise-and (arithmetic-shift anti-diag (- shr/acc)) (arithmetic-shift clean shr/acc)))]
    (cond
      [(= 8 shr/acc) 0]
      [(not (zero? (bitwise-and bb diag))) diag]
      [else
       (bishop-anti-right bb (arithmetic-shift clean -8) (add1 shr/acc))])))

(define (bishop-main bb)
  (bitwise-xor (bitwise-ior (bishop-main-left bb cleanupleft 0) (bishop-main-right bb cleanupright 0))
               (bitwise-ior (bishop-anti-left bb cleanupleft 0) (bishop-anti-right bb cleanupright 0)))) 




;; bishop-basics : Number (Number Number) -> Number
; Takes the bitboard where here is only one bit set, which is the bishop. The other
; two numbers are temporary and will be taken from the matrix
;
; The result of this will be used for further calculations that will account for
; other pieces / possible captures
(define (bishop-basic bit-board row col)
    #f)

; Mossa Ety
;(define (QUEEN-MOVES))
