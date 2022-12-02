;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname chess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/base)
(require racket/format)
(require 2htdp/image)


(define SQUARE_SIDE 100)
(define DARK_WOOD (make-color 191 108 58))
(define LIGHT_WOOD (make-color 238 202 160))
(define LIGHT_SQUARE (square SQUARE_SIDE "solid" LIGHT_WOOD))
(define DARK_SQUARE (square SQUARE_SIDE "solid" DARK_WOOD))



;; Data type
;  Background is an Image
;  Interpretation: it rapresents a chessboard, composed by 64 squares (an 8x8 square) alternating in a dark and a light color
(define BACKGROUND
  (above (beside LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE)
         (beside DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE)
         (beside LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE)
         (beside DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE)
         (beside LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE)
         (beside DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE)
         (beside LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE)
         (beside DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE)))

;; Costants
(define WK_IMG (scale (/ SQUARE_SIDE 98) (bitmap "img/WHITE_KING.png")))
(define WQ_IMG (scale (/ SQUARE_SIDE 98)(bitmap "img/WHITE_QUEEN.png")))
(define WR_IMG (scale (/ SQUARE_SIDE 98)(bitmap "img/WHITE_ROOK.png")))
(define WB_IMG (scale (/ SQUARE_SIDE 98)(bitmap "img/WHITE_BISHOP.png")))
(define WN_IMG (scale (/ SQUARE_SIDE 98)(bitmap "img/WHITE_KNIGHT.png")))
(define WP_IMG (scale (/ SQUARE_SIDE 98)(bitmap "img/WHITE_PAWN.png")))
(define BK_IMG (scale (/ SQUARE_SIDE 98)(bitmap "img/BLACK_KING.png")))
(define BQ_IMG (scale (/ SQUARE_SIDE 98)(bitmap "img/BLACK_QUEEN.png")))
(define BR_IMG (scale (/ SQUARE_SIDE 98)(bitmap "img/BLACK_ROOK.png")))
(define BB_IMG (scale (/ SQUARE_SIDE 98)(bitmap "img/BLACK_BISHOP.png")))
(define BN_IMG (scale (/ SQUARE_SIDE 98)(bitmap "img/BLACK_KNIGHT.png")))
(define BP_IMG (scale (/ SQUARE_SIDE 98)(bitmap "img/BLACK_PAWN.png")))

(define EMTPY_CHESSBOARD
  (vector
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")))

(define STANDARD_CHESSBOARD
  (vector
   (vector "r" "n" "b" "q" "k" "b" "n" "r")
   (vector "p" "p" "p" "p" "p" "p" "p" "p")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " "Q" " " "K" " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector "P" "P" "P" "P" "P" "P" "P" "P")
   (vector "R" "N" "B" "Q" "K" "B" "N" "R")))

(define TEST_CHESSBOARD
  (vector
   (vector " " " " " " " " "r" " " " " "p")
   (vector " " " " " " " " " " " " " " " ")
   (vector "-" " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector "p" "n" " " " " "R" " " "p" "p")
   (vector " " " " " " " " "p" " " " " " ")
   (vector " " " " " " " " "p" " " " " " ")
   (vector " " " " " " " " " " " " " " " ")))

(define (chessboardGet matrix row col)
  (vector-ref (vector-ref matrix row) col))

(define (chessboardSet matrix row col value)
  (vector-set! (vector-ref matrix row) col value))

(define (draw chessboard ChessboardIndex)
  (local
    ((define (getPiece chessboard ChessboardIndex)
       (chessboardGet chessboard (floor (/ ChessboardIndex 8)) (modulo ChessboardIndex 8)))
     (define (drawPiece pieceIMG ChessboardIndex)
       (place-image pieceIMG (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (modulo ChessboardIndex 8))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ ChessboardIndex 8)))) (draw chessboard (add1 ChessboardIndex))))) 
    (if (equal? 64 ChessboardIndex) BACKGROUND
        (cond
          [(equal? "K" (getPiece chessboard ChessboardIndex))
           (drawPiece WK_IMG ChessboardIndex)]
          [(equal? "Q" (getPiece chessboard ChessboardIndex))
           (drawPiece WQ_IMG ChessboardIndex)]
          [(equal? "R" (getPiece chessboard ChessboardIndex))
           (drawPiece WR_IMG ChessboardIndex)]
          [(equal? "B" (getPiece chessboard ChessboardIndex))
           (drawPiece WB_IMG ChessboardIndex)]
          [(equal? "N" (getPiece chessboard ChessboardIndex))
           (drawPiece WN_IMG ChessboardIndex)]
          [(equal? "P" (getPiece chessboard ChessboardIndex))
           (drawPiece WP_IMG ChessboardIndex)]
          [(equal? "k" (getPiece chessboard ChessboardIndex))
           (drawPiece BK_IMG ChessboardIndex)]
          [(equal? "r" (getPiece chessboard ChessboardIndex))
           (drawPiece BR_IMG ChessboardIndex)]
          [(equal? "q" (getPiece chessboard ChessboardIndex))
           (drawPiece BQ_IMG ChessboardIndex)]
          [(equal? "b" (getPiece chessboard ChessboardIndex))
           (drawPiece BB_IMG ChessboardIndex)]
          [(equal? "n" (getPiece chessboard ChessboardIndex))
           (drawPiece BN_IMG ChessboardIndex)]
          [(equal? "p" (getPiece chessboard ChessboardIndex))
           (drawPiece BP_IMG ChessboardIndex)]
          [(equal? " " (getPiece chessboard ChessboardIndex))
           (draw chessboard (add1 ChessboardIndex))]))))

(draw STANDARD_CHESSBOARD 0)

(define WK #b0000000000000000000000000000000000000000000000000000000000000000)
(define WQ #b0000000000000000000000000000000000000000000000000000000000000000)
(define WR #b0000000000000000000000000000000000000000000000000000000000000000)
(define WB #b0000000000000000000000000000000000000000000000000000000000000000)
(define WN #b0000000000000000000000000000000000000000000000000000000000000000)
(define WP #b0000000000000000000000000000000000000000000000000000000000000000)
(define BK #b0000000000000000000000000000000000000000000000000000000000000000)
(define BQ #b0000000000000000000000000000000000000000000000000000000000000000)
(define BR #b0000000000000000000000000000000000000000000000000000000000000000)
(define BB #b0000000000000000000000000000000000000000000000000000000000000000)
(define BN #b0000000000000000000000000000000000000000000000000000000000000000)
(define BP #b0000000000000000000000000000000000000000000000000000000000000000)

(define BITBOARDS
  (vector WK WQ WR WB WN WP BK BQ BR BB BN BP))

(define (chessboardToBitboards chessboard ChessboardIndex)
  (local
    ((define (getPiece chessboard ChessboardIndex)
       (chessboardGet chessboard (floor (/ ChessboardIndex 8)) (modulo ChessboardIndex 8)))
     (define (writeBitBoard bitboard ChessboardIndex)
       (begin (vector-set! BITBOARDS bitboard (+ (vector-ref BITBOARDS bitboard) (arithmetic-shift 1 (- 63 ChessboardIndex)))))
       (begin (chessboardToBitboards chessboard (add1 ChessboardIndex)))))
    (if (equal? 64 ChessboardIndex) BITBOARDS
        (cond
          [(equal? "K" (getPiece chessboard ChessboardIndex))
           (writeBitBoard 0 ChessboardIndex)]
          [(equal? "Q" (getPiece chessboard ChessboardIndex))
           (writeBitBoard 1 ChessboardIndex)]
          [(equal? "R" (getPiece chessboard ChessboardIndex))
           (writeBitBoard 2 ChessboardIndex)]
          [(equal? "B" (getPiece chessboard ChessboardIndex))
           (writeBitBoard 3 ChessboardIndex)]
          [(equal? "N" (getPiece chessboard ChessboardIndex))
           (writeBitBoard 4 ChessboardIndex)]
          [(equal? "P" (getPiece chessboard ChessboardIndex))
           (writeBitBoard 5 ChessboardIndex)]
          [(equal? "k" (getPiece chessboard ChessboardIndex))
           (writeBitBoard 6 ChessboardIndex)]
          [(equal? "q" (getPiece chessboard ChessboardIndex))
           (writeBitBoard 7 ChessboardIndex)]
          [(equal? "r" (getPiece chessboard ChessboardIndex))
           (writeBitBoard 8 ChessboardIndex)]
          [(equal? "b" (getPiece chessboard ChessboardIndex))
           (writeBitBoard 9 ChessboardIndex)]
          [(equal? "n" (getPiece chessboard ChessboardIndex))
           (writeBitBoard 10 ChessboardIndex)]
          [(equal? "p" (getPiece chessboard ChessboardIndex))
           (writeBitBoard 11 ChessboardIndex)]
          [(equal? " " (getPiece chessboard ChessboardIndex))
           (chessboardToBitboards chessboard (add1 ChessboardIndex))]))))

(define (bitboardsToChessboard chessboard ChessboardIndex)
  (local
    ((define (getBit bitboard ChessboardIndex)
       (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS bitboard) (- ChessboardIndex 63))))
     (define (writeChessBoard chessboard ChessboardIndex value)
       (begin (chessboardSet chessboard (floor (/ ChessboardIndex 8)) (modulo ChessboardIndex 8) value))
       (begin (bitboardsToChessboard chessboard (add1 ChessboardIndex)))))
    (if (equal? 64 ChessboardIndex) chessboard
        (cond
          [(equal? 1 (getBit 0 ChessboardIndex))
           (writeChessBoard chessboard ChessboardIndex "K")]
          [(equal? 1 (getBit 1 ChessboardIndex))
           (writeChessBoard chessboard ChessboardIndex "Q")]
          [(equal? 1 (getBit 2 ChessboardIndex))
           (writeChessBoard chessboard ChessboardIndex "R")]
          [(equal? 1 (getBit 3 ChessboardIndex))
           (writeChessBoard chessboard ChessboardIndex "B")]
          [(equal? 1 (getBit 4 ChessboardIndex))
           (writeChessBoard chessboard ChessboardIndex "N")]
          [(equal? 1 (getBit 5 ChessboardIndex))
           (writeChessBoard chessboard ChessboardIndex "P")]
          [(equal? 1 (getBit 6 ChessboardIndex))
           (writeChessBoard chessboard ChessboardIndex "k")]
          [(equal? 1 (getBit 7 ChessboardIndex))
           (writeChessBoard chessboard ChessboardIndex "q")]
          [(equal? 1 (getBit 8 ChessboardIndex))
           (writeChessBoard chessboard ChessboardIndex "r")]
          [(equal? 1 (getBit 9 ChessboardIndex))
           (writeChessBoard chessboard ChessboardIndex "b")]
          [(equal? 1 (getBit 10 ChessboardIndex))
           (writeChessBoard chessboard ChessboardIndex "n")]
          [(equal? 1 (getBit 11 ChessboardIndex))
           (writeChessBoard chessboard ChessboardIndex "p")]
          [else
           (writeChessBoard chessboard ChessboardIndex " ")]))))

(chessboardToBitboards STANDARD_CHESSBOARD 0)
(bitboardsToChessboard STANDARD_CHESSBOARD 0)


(define (printBitBoard2 BITBOARD ChessboardIndex)
  (cond
    [(equal? 8 ChessboardIndex) (void)]
    [else
     (begin (writeln (~r (bitwise-and (arithmetic-shift BITBOARD (* -8 (- 7 ChessboardIndex))) #b11111111) #:base 2 #:min-width 8 #:pad-string "0")))
     (begin (printBitBoard2 BITBOARD (add1 ChessboardIndex)))]))

(define (printBitBoard BITBOARD)
  (printBitBoard2 BITBOARD 0))


(define (printBitboards2 BITBOARDS ChessboardIndex)
  (cond
    [(equal? (vector-length BITBOARDS) ChessboardIndex) (void)]
    [else
     (begin (writeln "        "))
     (begin  (printBitBoard (vector-ref BITBOARDS ChessboardIndex)))
     (begin (printBitboards2 BITBOARDS (add1 ChessboardIndex)))]))

(define (printBitboards BITBOARDS)
  (printBitboards2 BITBOARDS 0))

;=============================================================================================

;DATA TYPE
;A piece-move is a bitwise operation where: 
;   - there are the initial coordinates
;   - the bitshift represents the direction of where the piece can go
;   - there are the new coordinates of the pieces after the bitshift

; Mossa Pietro
;;Pawn

(define (PawnMoves ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex))))
    (bitwise-ior
      ;Capture Right
      (arithmetic-shift (bitwise-and binaryPosition FILE_A RANKMASKS) 7)
      ;Capture Left
      (arithmetic-shift (bitwise-and binaryPosition FILE_H RANKMASKS) 9))


    ;Pietro non puoi mettere due body diversi nella stessa funzione. Ho commentato il secondo body
    ; perche altrimenti mi da errore
    
    ;Move either 2 or 1 forward 
    ;(cond
      ;move 1 forward 
      ;[(not(equal? 2 (modulo binaryPosition 8)) (arithmetic-shift (bitwise-and binaryPosition RANKMASKS) 8))]
      ;move 2 forward from the 2nd line
      ;[(equal? 2 (modulo binaryPosition 8)) (arithmetic-shift (bitwise-and binaryPosition RANKMASKS) 16)]
    )
  )
;)
      
     
(define (BPawnMoves ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex))))
    (bitwise-ior
      ;Capture left
      (arithmetic-shift (bitwise-and binaryPosition FILE_H RANKMASKS) -7)
      ;Capture right
      (arithmetic-shift (bitwise-and binaryPosition FILE_A RANKMASKS) -9))

    ; QUESTA FUNZIONE E ROTTA, NON PUOI SCRIVERE UN SECONDO BODY... ESCE ERRORE
    
    ;Move either 2 or 1 forward 
    ;(cond
      ;move 1 forward 
      ;[(not(equal? 2 (modulo binaryPosition 8)) (arithmetic-shift (bitwise-and binaryPosition RANKMASKS) -8))]
      ;move 2 forward from the 2nd line
      ;[(equal? 2 (modulo binaryPosition 8)) (arithmetic-shift (bitwise-and binaryPosition RANKMASKS) -16)]
    )
  )
;)
     

; Mossa Ety
(define (reverseBinary2 b ChessboardIndex total_sum)
  (cond
    [(equal? 64 ChessboardIndex) total_sum]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift b (- ChessboardIndex 63))))
       (reverseBinary2 b (add1 ChessboardIndex) (+ total_sum (arithmetic-shift 1 ChessboardIndex)))]
    [else (reverseBinary2 b (add1 ChessboardIndex) total_sum)]))

(define (reverseBinary b)
  (reverseBinary2 b 0 #b0000000000000000000000000000000000000000000000000000000000000000))


(define (bitboardsXOR BITBOARDS ChessboardIndex)
  (cond
    [(equal? 11 ChessboardIndex) (vector-ref BITBOARDS ChessboardIndex)]
    [else
     (bitwise-xor (vector-ref BITBOARDS ChessboardIndex) (bitboardsXOR BITBOARDS (add1 ChessboardIndex)))]))

(define OCCUPIED
  (bitboardsXOR BITBOARDS 0))


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

(define DIAGONALMASKS
  (vector
   #b1000000000000000000000000000000000000000000000000000000000000000
   #b0100000010000000000000000000000000000000000000000000000000000000
   #b0010000001000000100000000000000000000000000000000000000000000000
   #b0001000000100000010000001000000000000000000000000000000000000000
   #b0000100000010000001000000100000010000000000000000000000000000000
   #b0000010000001000000100000010000001000000100000000000000000000000
   #b0000001000000100000010000001000000100000010000001000000000000000
   #b0000000100000010000001000000100000010000001000000100000010000000
   #b0000000000000001000000100000010000001000000100000010000001000000
   #b0000000000000000000000010000001000000100000010000001000000100000
   #b0000000000000000000000000000000100000010000001000000100000010000
   #b0000000000000000000000000000000000000001000000100000010000001000
   #b0000000000000000000000000000000000000000000000010000001000000100
   #b0000000000000000000000000000000000000000000000000000000100000010
   #b0000000000000000000000000000000000000000000000000000000000000001))

(define ANTIDIAGONALMASKS
  (vector
   #b0000000100000000000000000000000000000000000000000000000000000000
   #b0000001000000001000000000000000000000000000000000000000000000000
   #b0000010000000010000000010000000000000000000000000000000000000000
   #b0000100000000100000000100000000100000000000000000000000000000000
   #b0001000000001000000001000000001000000001000000000000000000000000
   #b0010000000010000000010000000010000000010000000010000000000000000
   #b0100000000100000000100000000100000000100000000100000000100000000
   #b1000000001000000001000000001000000001000000001000000001000000001
   #b0000000010000000010000000010000000010000000010000000010000000010
   #b0000000000000000100000000100000000100000000100000000100000000100
   #b0000000000000000000000001000000001000000001000000001000000001000
   #b0000000000000000000000000000000010000000010000000010000000010000
   #b0000000000000000000000000000000000000000100000000100000000100000
   #b0000000000000000000000000000000000000000000000001000000001000000
   #b0000000000000000000000000000000000000000000000000000000010000000))


(define (horizontalVerticalMoves ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex)))
    (define horizontalMoves
      (bitwise-xor (- OCCUPIED (* 2 binaryPosition))
                   (reverseBinary (- (reverseBinary OCCUPIED) (* 2 (reverseBinary binaryPosition))))))
    (define verticalMoves
      (bitwise-xor (- (bitwise-and OCCUPIED (vector-ref FILEMASKS (modulo ChessboardIndex 8))) (* 2 binaryPosition))
                   (reverseBinary (- (reverseBinary (bitwise-and OCCUPIED (vector-ref FILEMASKS (modulo ChessboardIndex 8)))) (* 2 (reverseBinary binaryPosition)))))))
    (bitwise-ior (bitwise-and horizontalMoves (vector-ref RANKMASKS (floor (/ ChessboardIndex 8)))) (bitwise-and verticalMoves (vector-ref FILEMASKS (modulo ChessboardIndex 8))))))


(define (DiagonalAntiDiagonalMoves ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex)))
    (define DiagonalMoves
      (bitwise-xor (- (bitwise-and OCCUPIED (vector-ref DIAGONALMASKS (+ (floor (/ ChessboardIndex 8)) (modulo ChessboardIndex 8)))) (* 2 binaryPosition))
                   (reverseBinary (- (reverseBinary (bitwise-and OCCUPIED (vector-ref DIAGONALMASKS (+ (floor (/ ChessboardIndex 8)) (modulo ChessboardIndex 8))))) (* 2 (reverseBinary binaryPosition))))))
    (define AntiDiagonalMoves
      (bitwise-xor (- (bitwise-and OCCUPIED (vector-ref ANTIDIAGONALMASKS (+ (floor (/ ChessboardIndex 8)) (- 7 (modulo ChessboardIndex 8))))) (* 2 binaryPosition))
                   (reverseBinary (- (reverseBinary (bitwise-and OCCUPIED (vector-ref ANTIDIAGONALMASKS (+ (floor (/ ChessboardIndex 8)) (- 7 (modulo ChessboardIndex 8)))))) (* 2 (reverseBinary binaryPosition)))))))
    (bitwise-ior (bitwise-and DiagonalMoves (vector-ref DIAGONALMASKS (+ (floor (/ ChessboardIndex 8)) (modulo ChessboardIndex 8)))) (bitwise-and AntiDiagonalMoves (vector-ref ANTIDIAGONALMASKS (+ (floor (/ ChessboardIndex 8)) (- 7 (modulo ChessboardIndex 8))))))))




(define FILE_A  #b0111111101111111011111110111111101111111011111110111111101111111)
(define FILE_AB #b0011111100111111001111110011111100111111001111110011111100111111)
(define FILE_GH #b1111110011111100111111001111110011111100111111001111110011111100)
(define FILE_H  #b1111111011111110111111101111111011111110111111101111111011111110)

(define (knightMoves ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex))))
    (bitwise-ior
     (arithmetic-shift (bitwise-and binaryPosition FILE_AB) 10)
     (arithmetic-shift (bitwise-and binaryPosition FILE_A) 17)
     (arithmetic-shift (bitwise-and binaryPosition FILE_H) 15)
     (arithmetic-shift (bitwise-and binaryPosition FILE_GH) 6)
     (arithmetic-shift (bitwise-and binaryPosition FILE_GH) -10)
     (arithmetic-shift (bitwise-and binaryPosition FILE_H) -17)
     (arithmetic-shift (bitwise-and binaryPosition FILE_A) -15)
     (arithmetic-shift (bitwise-and binaryPosition FILE_AB) -6))))

(define (kingMoves ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex))))
    (bitwise-ior
     (arithmetic-shift (bitwise-and binaryPosition FILE_A) 1)
     (arithmetic-shift (bitwise-and binaryPosition FILE_A) 9)
     (arithmetic-shift              binaryPosition         8)
     (arithmetic-shift (bitwise-and binaryPosition FILE_H) 7)
     (arithmetic-shift (bitwise-and binaryPosition FILE_H) -1)
     (arithmetic-shift (bitwise-and binaryPosition FILE_H) -9)
     (arithmetic-shift              binaryPosition         -8)
     (arithmetic-shift (bitwise-and binaryPosition FILE_A) -7))))





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

(define (numberOfTrailingZeros bb no-zeroes)
  (if (equal? 1 (bitwise-and bb (arithmetic-shift 1 no-zeroes))) no-zeroes
      (numberOfTrailingZeros bb (add1 no-zeroes))))



;;; TEST ;;;
(define (isKingChecked kingBitBoard enemyAttacks)
  (if (not (zero? (bitwise-and kingBitBoard enemyAttacks))) #t
      #f))


(define enemyRook #b0000000000000000000000010000000000000000000000000000000000000000)
(define rookAttacks (horizontalVerticalMoves 23))
(define testKing  #b0000000000000000000010000000000000000000000000000000000000000000)
(isKingChecked testKing rookAttacks)