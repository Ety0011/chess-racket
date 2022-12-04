;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname chess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/base)
(require racket/format)
(require 2htdp/image)
(require 2htdp/universe)


(define WHITE #true)
(define BLACK #false)


; The color of the dark and the light squares ot the chessboard
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

;  The different images of the pieces
(define WK_IMG (scale (/ SQUARE_SIDE 162) (bitmap "img/WK.png")))
(define WQ_IMG (scale (/ SQUARE_SIDE 162) (bitmap "img/WQ.png")))
(define WR_IMG (scale (/ SQUARE_SIDE 162) (bitmap "img/WR.png")))
(define WB_IMG (scale (/ SQUARE_SIDE 162) (bitmap "img/WB.png")))
(define WN_IMG (scale (/ SQUARE_SIDE 162) (bitmap "img/WN.png")))
(define WP_IMG (scale (/ SQUARE_SIDE 162) (bitmap "img/WP.png")))
(define BK_IMG (scale (/ SQUARE_SIDE 162) (bitmap "img/BK.png")))
(define BQ_IMG (scale (/ SQUARE_SIDE 162) (bitmap "img/BQ.png")))
(define BR_IMG (scale (/ SQUARE_SIDE 162) (bitmap "img/BR.png")))
(define BB_IMG (scale (/ SQUARE_SIDE 162) (bitmap "img/BB.png")))
(define BN_IMG (scale (/ SQUARE_SIDE 162) (bitmap "img/BN.png")))
(define BP_IMG (scale (/ SQUARE_SIDE 162) (bitmap "img/BP.png")))


;; Data type
;  A chessboard is an image made of vectors
;  Interpretation: The chessboard contain the different pieces

; An empty chessboard is a vector made from 64 empty squares.
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



;  A standart chessboard contain the following pieces in these position.
; Interpretation: For example the rooks "R" are on the corners as the starting position in a standart game 
(define STANDARD_CHESSBOARD
  (vector
   (vector "r" "n" "b" "q" "k" "b" "n" "r")
   (vector "p" "p" "p" "p" " " "p" "p" "p")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " "p" "P" " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector "P" "P" "P" "P" "P" " " "P" "P")
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

(define (drawPieces2 chessboard ChessboardIndex)
  (local
    ((define (getPiece chessboard ChessboardIndex)
       (chessboardGet chessboard (floor (/ ChessboardIndex 8)) (modulo ChessboardIndex 8)))
     (define (drawPiece pieceIMG ChessboardIndex)
       (place-image pieceIMG (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (modulo ChessboardIndex 8))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ ChessboardIndex 8)))) (drawPieces2 chessboard (add1 ChessboardIndex))))) 
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
           (drawPieces2 chessboard (add1 ChessboardIndex))]))))



(define (drawPieces chessboard)
  (drawPieces2 chessboard 0))

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


;; Data type
;  Bitboars are vectors
;  Interpretation: a structure of array of bits based on the position of a piece in a board cell. 
;  The name are based on the type of piece and on its color. "WK" is the white king

(define BITBOARDS
  (vector WK WQ WR WB WN WP BK BQ BR BB BN BP))

(define (chessboardToBitboards2 chessboard ChessboardIndex)
  (local
    ((define (getPiece chessboard ChessboardIndex)
       (chessboardGet chessboard (floor (/ ChessboardIndex 8)) (modulo ChessboardIndex 8)))
     (define (writeBitBoard bitboard ChessboardIndex)
       (begin (vector-set! BITBOARDS bitboard (+ (vector-ref BITBOARDS bitboard) (arithmetic-shift 1 (- 63 ChessboardIndex)))))
       (begin (chessboardToBitboards2 chessboard (add1 ChessboardIndex)))))
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
           (chessboardToBitboards2 chessboard (add1 ChessboardIndex))]))))

(define (chessboardToBitboards chessboard)
  (chessboardToBitboards2 chessboard 0))

(define (bitboardsToChessboard2 chessboard ChessboardIndex)
  (local
    ((define (getBit bitboard ChessboardIndex)
       (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS bitboard) (- ChessboardIndex 63))))
     (define (writeChessBoard chessboard ChessboardIndex value)
       (begin (chessboardSet chessboard (floor (/ ChessboardIndex 8)) (modulo ChessboardIndex 8) value))
       (begin (bitboardsToChessboard2 chessboard (add1 ChessboardIndex)))))
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

(define (bitboardsToChessboard chessboard)
  (bitboardsToChessboard2 chessboard 0))

(chessboardToBitboards STANDARD_CHESSBOARD)
(bitboardsToChessboard STANDARD_CHESSBOARD)


(define (printBitboard2 bitboard ChessboardIndex)
  (cond
    [(equal? 8 ChessboardIndex) (void)]
    [else
     (begin (writeln (~r (bitwise-and (arithmetic-shift bitboard (* -8 (- 7 ChessboardIndex))) #b11111111) #:base 2 #:min-width 8 #:pad-string "0")))
     (begin (printBitboard2 bitboard (add1 ChessboardIndex)))]))

(define (printBitboard bitboard)
  (printBitboard2 bitboard 0))


(define (printBitboards2 bitboards ChessboardIndex)
  (cond
    [(equal? (vector-length bitboards) ChessboardIndex) (void)]
    [else
     (begin (writeln "        "))
     (begin  (printBitboard (vector-ref bitboards ChessboardIndex)))
     (begin (printBitboards2 bitboards (add1 ChessboardIndex)))]))

(define (printBitboards bitboards)
  (printBitboards2 bitboards 0))

;=============================================================================================

;DATA TYPE
;A piece-move is a bitwise operation where: 
;   - there are the initial coordinates
;   - the bitshift represents the direction of where the piece can go
;   - there are the new coordinates of the pieces after the bitshift

     

; Mossa Ety
(define (reverseBinary2 b ChessboardIndex totalSum)
  (cond
    [(equal? 64 ChessboardIndex) totalSum]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift b (- ChessboardIndex 63))))
       (reverseBinary2 b (add1 ChessboardIndex) (+ totalSum (arithmetic-shift 1 ChessboardIndex)))]
    [else (reverseBinary2 b (add1 ChessboardIndex) totalSum)]))

(define (reverseBinary b)
  (reverseBinary2 b 0 #b0000000000000000000000000000000000000000000000000000000000000000))


(define (bitboardsXOR BITBOARDS ChessboardIndex)
  (cond
    [(equal? 11 ChessboardIndex) (vector-ref BITBOARDS ChessboardIndex)]
    [else
     (bitwise-xor (vector-ref BITBOARDS ChessboardIndex) (bitboardsXOR BITBOARDS (add1 ChessboardIndex)))]))

(define OCCUPIED
  (bitboardsXOR BITBOARDS 0))

(define NOT_OCCUPIED
  (bitwise-not OCCUPIED))


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


(define (rookMoves occupied ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex)))
    (define horizontalMoves
      (bitwise-xor (- occupied (* 2 binaryPosition))
                   (reverseBinary (- (reverseBinary occupied) (* 2 (reverseBinary binaryPosition))))))
    (define verticalMoves
      (bitwise-xor (- (bitwise-and occupied (vector-ref FILEMASKS (modulo ChessboardIndex 8))) (* 2 binaryPosition))
                   (reverseBinary (- (reverseBinary (bitwise-and occupied (vector-ref FILEMASKS (modulo ChessboardIndex 8)))) (* 2 (reverseBinary binaryPosition)))))))
    (bitwise-ior (bitwise-and horizontalMoves (vector-ref RANKMASKS (floor (/ ChessboardIndex 8)))) (bitwise-and verticalMoves (vector-ref FILEMASKS (modulo ChessboardIndex 8))))))


(define (bishopMoves occupied ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex)))
    (define DiagonalMoves
      (bitwise-xor (- (bitwise-and occupied (vector-ref DIAGONALMASKS (+ (floor (/ ChessboardIndex 8)) (modulo ChessboardIndex 8)))) (* 2 binaryPosition))
                   (reverseBinary (- (reverseBinary (bitwise-and occupied (vector-ref DIAGONALMASKS (+ (floor (/ ChessboardIndex 8)) (modulo ChessboardIndex 8))))) (* 2 (reverseBinary binaryPosition))))))
    (define AntiDiagonalMoves
      (bitwise-xor (- (bitwise-and occupied (vector-ref ANTIDIAGONALMASKS (+ (floor (/ ChessboardIndex 8)) (- 7 (modulo ChessboardIndex 8))))) (* 2 binaryPosition))
                   (reverseBinary (- (reverseBinary (bitwise-and occupied (vector-ref ANTIDIAGONALMASKS (+ (floor (/ ChessboardIndex 8)) (- 7 (modulo ChessboardIndex 8)))))) (* 2 (reverseBinary binaryPosition)))))))
    (bitwise-ior (bitwise-and DiagonalMoves (vector-ref DIAGONALMASKS (+ (floor (/ ChessboardIndex 8)) (modulo ChessboardIndex 8)))) (bitwise-and AntiDiagonalMoves (vector-ref ANTIDIAGONALMASKS (+ (floor (/ ChessboardIndex 8)) (- 7 (modulo ChessboardIndex 8))))))))


(define FILE_A #b1000000010000000100000001000000010000000100000001000000010000000)
(define FILE_AB #b1100000011000000110000001100000011000000110000001100000011000000)
(define FILE_GH #b0000001100000011000000110000001100000011000000110000001100000011)
(define FILE_H #b0000000100000001000000010000000100000001000000010000000100000001)
(define NOT_FILE_A (bitwise-not FILE_A))
(define NOT_FILE_AB (bitwise-not FILE_AB))
(define NOT_FILE_GH (bitwise-not FILE_GH))
(define NOT_FILE_H (bitwise-not FILE_H))

(define (knightMoves ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex))))
    (bitwise-ior
     (bitwise-and (arithmetic-shift binaryPosition  10) NOT_FILE_GH)
     (bitwise-and (arithmetic-shift binaryPosition  17) NOT_FILE_H)
     (bitwise-and (arithmetic-shift binaryPosition  15) NOT_FILE_A)
     (bitwise-and (arithmetic-shift binaryPosition  6)  NOT_FILE_AB)
     (bitwise-and (arithmetic-shift binaryPosition -10) NOT_FILE_AB)
     (bitwise-and (arithmetic-shift binaryPosition -17) NOT_FILE_A)
     (bitwise-and (arithmetic-shift binaryPosition -15) NOT_FILE_H)
     (bitwise-and (arithmetic-shift binaryPosition -6)  NOT_FILE_GH))))

(define (kingMoves ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex))))
    (bitwise-ior
     (bitwise-and (arithmetic-shift binaryPosition  1) NOT_FILE_H)
     (bitwise-and (arithmetic-shift binaryPosition  9) NOT_FILE_H)
                  (arithmetic-shift binaryPosition  8)
     (bitwise-and (arithmetic-shift binaryPosition  7) NOT_FILE_A)
     (bitwise-and (arithmetic-shift binaryPosition -1) NOT_FILE_A)
     (bitwise-and (arithmetic-shift binaryPosition -9) NOT_FILE_A)
                  (arithmetic-shift binaryPosition -8)
     (bitwise-and (arithmetic-shift binaryPosition -7) NOT_FILE_H))))


(define RANK_1 #b0000000000000000000000000000000000000000000000000000000011111111)
(define NOT_RANK_1 (bitwise-not RANK_1))
(define RANK_4 #b0000000000000000000000000000000011111111000000000000000000000000)
(define RANK_5 #b0000000000000000000000001111111100000000000000000000000000000000)
(define RANK_8 #b1111111100000000000000000000000000000000000000000000000000000000)
(define NOT_RANK_8 (bitwise-not RANK_8))


(define (whitePawnMoves ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex))))
    (bitwise-ior
     (bitwise-and (arithmetic-shift binaryPosition 9) NOT_FILE_H NOT_RANK_8)
     (bitwise-and (arithmetic-shift binaryPosition 7) NOT_FILE_A NOT_RANK_8)
     (bitwise-and (arithmetic-shift binaryPosition 8) NOT_OCCUPIED NOT_RANK_8)
     (bitwise-and (arithmetic-shift binaryPosition 16) (arithmetic-shift NOT_OCCUPIED 8) NOT_OCCUPIED RANK_4))))

(define (whitePawnAttacks ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex))))
    (bitwise-ior
     (bitwise-and (arithmetic-shift binaryPosition 9) NOT_FILE_H NOT_RANK_8)
     (bitwise-and (arithmetic-shift binaryPosition 7) NOT_FILE_A NOT_RANK_8))))

(define (blackPawnMoves ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex))))
    (bitwise-ior
     (bitwise-and (arithmetic-shift binaryPosition -9) NOT_FILE_A NOT_RANK_1)
     (bitwise-and (arithmetic-shift binaryPosition -7) NOT_FILE_H NOT_RANK_1)
     (bitwise-and (arithmetic-shift binaryPosition -8) NOT_OCCUPIED NOT_RANK_1)
     (bitwise-and (arithmetic-shift binaryPosition -16) (arithmetic-shift NOT_OCCUPIED -8) NOT_OCCUPIED RANK_5))))

(define (blackPawnAttacks ChessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 ChessboardIndex))))
    (bitwise-ior
     (bitwise-and (arithmetic-shift binaryPosition -9) NOT_FILE_A NOT_RANK_1)
     (bitwise-and (arithmetic-shift binaryPosition -7) NOT_FILE_H NOT_RANK_1))))

;(define (whitePawnPromotion ChessboardIndex)
;  (if (equal? RANK_8 ) 
;    (cond
;      ;Promotion to queen 
;      [()(make-WQ ChessboardIndex)]
;      ;Promotion to Knight
;      [()(make-WN ChessboardIndex)]
;      ;Promotion to Rook
;      [()(make-WR ChessboardIndex)]
;      ;Promotion to Bishop
;      [()(make-WB ChessboardIndex)]
;    )
;    (ChessboardIndex)
;  )
;)
;(define (blackPawnPromotion ChessboardIndex)
;  (if (equal? RANK_1 ) 
;    (cond
;      ;Promotion to queen 
;      [()(make-BQ ChessboardIndex)]
;      ;Promotion to Knight
;      [()(make-BN ChessboardIndex)]
;      ;Promotion to Rook
;      [()(make-BR ChessboardIndex)]
;      ;Promotion to Bishop
;      [()(make-BB ChessboardIndex)]
;    )
;    (ChessboardIndex)
;  )
;)

(define (numberOfTrailingZeros bb no-zeroes)
  (if (equal? 1 (bitwise-and bb (arithmetic-shift 1 no-zeroes))) no-zeroes
      (numberOfTrailingZeros bb (add1 no-zeroes))))



;; King Safety v2
; get rooks attacks. Takes the bitboard of all rooks of a certain color as input along with the
; bitboard of occupied pieces, and returns a bitboard with all attacks. It works by iterating through
; the bitboard untill it finds a 1, and then gets all the attacks for that position. 
(define (getRookAttacks-backend rb occupied chessboardIndex attacks)
  (cond
    [(equal? 64 chessboardIndex) attacks]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift rb (- chessboardIndex 63))))
     (getRookAttacks-backend rb occupied (add1 chessboardIndex) (bitwise-ior attacks (rookMoves occupied chessboardIndex)))]
    [else (getRookAttacks-backend rb occupied (add1 chessboardIndex) attacks)]))
    
; get bishop attacks. Takes the bitboard of all bishops of a certain color as input along with the
; bitboard of occupied pieces, and returns a bitboard with all attacks. It works by iterating through
; the bitboard untill it finds a 1, and then gets all the attacks for that position. 
(define (getBishopAttacks-backend bb occupied chessboardIndex attacks)
  (cond
    [(equal? 64 chessboardIndex) attacks]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift bb (- chessboardIndex 63))))
     (getBishopAttacks-backend bb occupied (add1 chessboardIndex) (bitwise-ior attacks (bishopMoves occupied chessboardIndex)))]
    [else (getBishopAttacks-backend bb occupied (add1 chessboardIndex) attacks)]))

; get knights attacks. Takes the bitboard of all knights of a certain color as input along with the
; bitboard of occupied pieces, and returns a bitboard with all attacks. It works by iterating through
; the bitboard untill it finds a 1, and then gets all the attacks for that position. 
(define (getKnightAttacks-backend nb occupied chessboardIndex attacks)
  (cond
    [(equal? 64 chessboardIndex) attacks]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift nb (- chessboardIndex 63))))
     (getKnightAttacks-backend nb occupied (add1 chessboardIndex) (bitwise-ior attacks (knightMoves occupied chessboardIndex)))]
    [else (getBishopAttacks-backend nb occupied (add1 chessboardIndex) attacks)]))

; get black pawn attacks. Takes the bitboard of all black pawns as input along with the bitboard
; of occupied pieces, and returns a bitboard with all attacks. It works by iterating thorugh the
; bitboard until it finds a 1, and then gets all the attacks for that position
(define (getBPawnAttacks-backend pb occupied chessboardIndex attacks)
  (cond
    [(equal? 64 chessboardIndex) attacks]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift pb (- chessboardIndex 63))))
     (getBPawnAttacks-backend pb occupied (add1 chessboardIndex) (bitwise-ior attacks (blackPawnAttacks chessboardIndex)))]
    [else (getBPawnAttacks-backend pb occupied (add1 chessboardIndex) attacks)]))

; get white pawn attacks. Takes the bitboard of all white pawns as input along with the bitboard
; of occupied pieces, and returns a bitboard with all attacks. It works by iterating thorugh the
; bitboard until it finds a 1, and then gets all the attacks for that position
(define (getWPawnAttacks-backend pb occupied chessboardIndex attacks)
  (cond
    [(equal? 64 chessboardIndex) attacks]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift pb (- chessboardIndex 63))))
     (getWPawnAttacks-backend pb occupied (add1 chessboardIndex) (bitwise-ior attacks (whitePawnAttacks chessboardIndex)))]
    [else (getWPawnAttacks-backend pb occupied (add1 chessboardIndex) attacks)]))


; USE THESE FUNCTIONS
; - rook attacks frontend
; Calls getRookAttacks-backend and automatically passes the accumulators/const bitboards
(define (getRookAttacks rookBitBoard occupied)
  (getRookAttacks-backend rookBitBoard occupied 0 0))

; - bishop attacks frontend
; Calls getBishopAttacks-backend and automatically passes the accumulators/const bitboards
(define (getBishopAttacks bishopBitBoard occupied)
  (getBishopAttacks-backend bishopBitBoard occupied 0 0))

; - knight attacks frontend
; Calls getKnightAttacks-backend and automatically passes the accumulators/const bitboards
(define (getKnightAttacks knightBitBoard occupied)
  (getKnightAttacks-backend knightBitBoard occupied 0 0))

; - black pawn attacks frontend
; Calls getBPawnAttacks-backend and automatically passes the accumulators/const bitboards
(define (getBPawnAttacks pawnBitBoard occupied)
  (getBPawnAttacks-backend pawnBitBoard occupied 0 0))

; - white pawn attacks frontend
; Calls getWPawnAttacks-backend and automatically passes the accumulators/const bitboards
(define (getWPawnAttacks pawnBitBoard occupied)
  (getWPawnAttacks-backend pawnBitBoard occupied 0 0))

;; Gets the attacks of all black pieces and returns a bitboard of the combined attacks
(define (getBlackAttacks BR BB BN BP occupied)
  (bitwise-ior (bitwise-ior (getRookAttacks   BR occupied) 
                            (getBishopAttacks BB occupied)) 
               (bitwise-ior (getKnightAttacks BN occupied) 
                            (getBPawnAttacks  BP occupied)))) ; add king and queen later

;; Gets the attacks of all white pieces and returns a bitboard of the combined attacks
(define (getWhiteAttacks WR WB WN WP occupied)
  (bitwise-ior (bitwise-ior (getRookAttacks   WR occupied)
                            (getBishopAttacks WB occupied))
               (bitwise-ior (getKnightAttacks WN occupied)
                            (getWPawnAttacks  WP occupied)))) ; add king and queen later

;; Performs an and between the WK bitboard and the result of getBlackAttacks. If it returns
;  0 then the king is safe, else it is in check
(define (isWhiteKingSafe WK BP BR BB BN BQ BK occupied)
  (local [(define blackAttacks (getBlackAttacks BR BB BN BP occupied))] ; TODO: add king and queen later
    (if (zero? (bitwise-and WK blackAttacks)) #t 
        #f)))

;; Performs an and between the BK bitboard and the result of getWhiteAttacks. If it returns
;  0 then the king is safe, else it is in check
(define (isBlackKingSafe BK WP WR WB WN WQ WK occupied)
  (local [(define whiteAttacks (getWhiteAttacks WR WB WN WP occupied))] ; TODO: add king and queen later
    (if (zero? (bitwise-and BK whiteAttacks)) #t
        #f))) 

; TODO: add checkmate
;   POSSIBLE IMPLEMENTATION: iterate trhough all possible moves for a certain color. If none of
;                            them produce a legal move (king is unsafe in all positions) then
;                            it is a checkmate. This function gets called when the king is 
;                            detected to be in check. 
;                            TODO: find a way to iterate through all moves



; A CurrentMove is a Structure (make-currentMove image type color start end) where:
; -
(define-struct currentMove [image type color start end])

; A Maybe<CurrentMove> is one of:
; - CurrentMove: the CurrentMove exists
; - #false : the CurrentMove is missing
; A CurrentMove that may be missing

; A worldState is a Structure (make-worldState image chessboard bitboards currentMove quit) where:
; - chessboard: Vector of Vectors of Strings
; - bitboards: Vector of Numbers
; - CurrentMove: CurrentMove
; - quit: Boolean
; A state of the application where:
; - 'canvas' is an Image of the drawing canvas, which is initially empty and then includes the lines that are drawn.
; - 'line' is the currently drawn Line2D (if there is any) which goes from an initial point to the current end point
; - 'quit' is a Boolean that stores the information of whether the application has quit or not
(define-struct worldState [image chessboard bitboards currentMove quit])


(define (draw worldState)
  (if (currentMove? (worldState-currentMove worldState))
      (place-image (currentMove-image (worldState-currentMove worldState))
                   (posn-x (currentMove-end (worldState-currentMove worldState)))
                   (posn-y (currentMove-end (worldState-currentMove worldState)))
                   (worldState-image worldState))
      (worldState-image worldState)))


(define (startMove worldState new-x new-y)
  (make-worldState (hideSelectedPiece worldState new-x new-y)
                   (worldState-chessboard worldState)
                   (worldState-bitboards worldState)
                   (newCurrentMove worldState new-x new-y)
                   (worldState-quit worldState)))

(define (hideSelectedPiece worldState new-x new-y)
  (if (equal? 0 (modulo (+ (floor (/ new-y SQUARE_SIDE)) (floor (/ new-x SQUARE_SIDE))) 2))
      (place-image LIGHT_SQUARE (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ new-x SQUARE_SIDE)))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ new-y SQUARE_SIDE)))) (worldState-image worldState))
      (place-image DARK_SQUARE (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ new-x SQUARE_SIDE)))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ new-y SQUARE_SIDE)))) (worldState-image worldState))))

(define (newCurrentMove worldState new-x new-y)
  (local
    ((define piece
       (chessboardGet (worldState-chessboard worldState) (floor (/ new-y SQUARE_SIDE)) (floor (/ new-x SQUARE_SIDE)))))
  (cond
    [(equal? "K" piece)
     (make-currentMove WK_IMG "K" WHITE (make-posn new-x new-y) (make-posn new-x new-y))]
    [(equal? "Q" piece)
     (make-currentMove WQ_IMG "Q" WHITE (make-posn new-x new-y) (make-posn new-x new-y))]
    [(equal? "R" piece)
     (make-currentMove WR_IMG "R" WHITE (make-posn new-x new-y) (make-posn new-x new-y))]
    [(equal? "B" piece)
     (make-currentMove WB_IMG "B" WHITE (make-posn new-x new-y) (make-posn new-x new-y))]
    [(equal? "N" piece)
     (make-currentMove WN_IMG "N" WHITE (make-posn new-x new-y) (make-posn new-x new-y))]
    [(equal? "P" piece)
     (make-currentMove WP_IMG "P" WHITE (make-posn new-x new-y) (make-posn new-x new-y))]
    [(equal? "k" piece)
     (make-currentMove BK_IMG "k" BLACK (make-posn new-x new-y) (make-posn new-x new-y))]
    [(equal? "q" piece)
     (make-currentMove BQ_IMG "q" BLACK (make-posn new-x new-y) (make-posn new-x new-y))]
    [(equal? "r" piece)
     (make-currentMove BR_IMG "r" BLACK (make-posn new-x new-y) (make-posn new-x new-y))]
    [(equal? "b" piece)
     (make-currentMove BB_IMG "b" BLACK (make-posn new-x new-y) (make-posn new-x new-y))]
    [(equal? "n" piece)
     (make-currentMove BN_IMG "n" BLACK (make-posn new-x new-y) (make-posn new-x new-y))]
    [(equal? "p" piece)
     (make-currentMove BP_IMG "p" BLACK (make-posn new-x new-y) (make-posn new-x new-y))]
    [(equal? " " piece)
     #false])))

(define (changeMove worldState new-x new-y)
  (make-worldState (worldState-image worldState)
                   (worldState-chessboard worldState)
                   (worldState-bitboards worldState)
                   (make-currentMove (currentMove-image (worldState-currentMove worldState))
                                     (currentMove-type (worldState-currentMove worldState))
                                     (currentMove-color (worldState-currentMove worldState))
                                     (currentMove-start (worldState-currentMove worldState))
                                     (make-posn new-x new-y))
                   (worldState-quit worldState)))

(define (makeMove worldState new-x new-y)
  (if (currentMove? (worldState-currentMove worldState))
      (make-worldState (drawPieces (worldState-chessboard worldState))
                       (worldState-chessboard worldState)
                       (worldState-bitboards worldState)
                       #false
                       (worldState-quit worldState))
      (make-worldState (worldState-image worldState)
                   (worldState-chessboard worldState)
                   (worldState-bitboards worldState)
                   (worldState-currentMove worldState)
                   (worldState-quit worldState))))

(define (quit worldState)
  (make-worldState (worldState-image worldState)
                   (worldState-chessboard worldState)
                   (worldState-bitboards worldState)
                   (worldState-currentMove worldState)
                   #true))

(define (quit? worldState)
  (if (equal? #t (worldState-quit worldState))
      #t
      #f))

(define (handle-key worldState key-event)
  (cond
    [(string=? "q" key-event) (quit worldState)]
    [else worldState]))

(define (handle-mouse worldState x-mouse y-mouse mouse-event)
  (cond
    [(string=? "button-down" mouse-event) (startMove worldState x-mouse y-mouse)]
    [(and (string=? "drag" mouse-event)
          (currentMove? (worldState-currentMove worldState))) (changeMove worldState x-mouse y-mouse)]
    [(string=? "button-up" mouse-event) (makeMove worldState x-mouse y-mouse)]
    [else worldState]))





(define initialState (make-worldState
                        (drawPieces STANDARD_CHESSBOARD)
                        STANDARD_CHESSBOARD
                        BITBOARDS
                        #false
                        #false))

(define (drawing-app initialState)
  (big-bang initialState
    [to-draw draw]
    [on-mouse handle-mouse]
    [on-key handle-key]
    [stop-when quit?]
    [close-on-stop #true]))

(drawing-app initialState)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(define (handle-mouse2 appstate x-mouse y-mouse mouse-event)
;  (cond
;    [(string=? "button-down" mouse-event) (move-start appstate x-mouse y-mouse)]
;    [(and (string=? "drag" mouse-event)
;          (line2D? (appstate-current_line appstate))) (move-end appstate x-mouse y-mouse)]
;    [(string=? "button-up" mouse-event) (add-line-to-canvas appstate)]
;    [else appstate]))

