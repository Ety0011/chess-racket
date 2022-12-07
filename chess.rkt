;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname chess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; =========
; Libraries
; =========

(require racket/base)
(require racket/format)
(require 2htdp/image)
(require 2htdp/universe)
(require racket/dict)


; ========
; Costants
; ========

; The color of the dark and the light squares ot the chessboard
(define SQUARE_SIDE 100)
(define DARK_WOOD (make-color 191 108 58))
(define LIGHT_WOOD (make-color 238 202 160))
(define LIGHT_SQUARE (square SQUARE_SIDE "solid" LIGHT_WOOD))
(define DARK_SQUARE (square SQUARE_SIDE "solid" DARK_WOOD))
(define WHITE_SQUARE (square SQUARE_SIDE "solid" "white"))

(define WHITE #true)
(define BLACK #false)

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


; An empty matrix is a vector made from 64 empty squares.
(define EMPTY_MATRIX
  (vector
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")))


;  A standard matrix contains the following pieces in these position.
; Interpretation: For example the rooks "R" are on the corners as the starting position in a standart game 
(define STANDARD_MATRIX
  (vector
   (vector "r" "n" "b" "q" "k" "b" "n" "r")
   (vector "p" "p" "p" "p" "p" "p" "p" "p")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector "P" "P" "P" "P" "P" "P" "P" "P")
   (vector "R" "N" "B" "Q" "K" "B" "N" "R")))

(define TEST_MATRIX
  (vector
   (vector " " " " " " " " "r" " " " " "p")
   (vector " " " " " " " " " " " " " " " ")
   (vector "-" " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector "p" "n" " " " " "R" " " "p" "p")
   (vector " " " " " " " " "p" " " " " " ")
   (vector " " " " " " " " "p" " " " " " ")
   (vector " " " " " " " " " " " " " " " ")))

;; Data type
;  Bitboars are a Dictionary
;  Interpretation: A dictionary is an instance of a datatype that maps keys to values. 
;  The names are based on the type of piece and on its color. "WK" is the white king
;  Assign uppercase letter to white pieces, and lower case letter to black pieces
(define BITBOARDS
       (list
        (cons "K" 0)
        (cons "Q" 0)
        (cons "R" 0)
        (cons "B" 0)
        (cons "N" 0)
        (cons "P" 0)
        (cons "k" 0)
        (cons "q" 0)
        (cons "r" 0)
        (cons "b" 0)
        (cons "n" 0)
        (cons "p" 0)))


; =========
; Functions
; =========

; Signature
; matrixGet: Matrix Number Number -> String
; Intepretation: takes the coordinates of 'roB' and 'column' of a 'matrix' to access the stored value in the corresponding element
; Header: (define (matrixGet matrix row col) " ")

; Checks
(check-expect (matrixGet STANDARD_MATRIX 0 0) "r")
(check-expect (matrixGet STANDARD_MATRIX 4 3) " ")
(check-expect (matrixGet STANDARD_MATRIX 7 4) "K")

; Implementation
(define (matrixGet matrix row col)
  (vector-ref (vector-ref matrix row) col))


; Signature
; matrixSet: Matrix Number Number -> Matrix
; Intepretation: takes the coordinates of 'row' and 'column' of a 'matrix' and returns the same matrix with the updated value in the corresponding coordinate
; Header: (define (matrixSet matrix row col value))
; Additional infos: not in library, adapted from https://stackoverflow.com/questions/38421007/update-element-of-immutable-vector-vector-set

; Implementation
(define (matrixSet matrix row column value)
   (for/vector ([j (in-range (vector-length matrix))])
     (for/vector ([k (in-range (vector-length (vector-ref matrix j)))])
       (if (and (= j row) (= k column))
           value
           (vector-ref (vector-ref matrix j) k)))))


; Signature
; matrixToBitboards: Matrix -> Dictionary<Bitboard>
; Interpretation: assignes every piece in the matrix to the corresponding "1" in the bitboards. Both boards are accessed with matrixIndex which is a number that ranges from 0 to 63
; Header: (define (matrixToBitboards matrix) BITBOARDS_OF_STANDARD_MATRIX)

; Examples
(define BITBOARDS_OF_STANDARD_MATRIX
       (list
        (cons "K" 8)
        (cons "Q" 16)
        (cons "R" 129)
        (cons "B" 36)
        (cons "N" 66)
        (cons "P" 65280)
        (cons "k" 576460752303423488)
        (cons "q" 1152921504606846976)
        (cons "r" 9295429630892703744)
        (cons "b" 2594073385365405696)
        (cons "n" 4755801206503243776)
        (cons "p" 71776119061217280)))

; Checks
(check-expect (matrixToBitboards STANDARD_MATRIX) BITBOARDS_OF_STANDARD_MATRIX)

; Implementation
(define (matrixToBitboards matrix)
  (local
    ((define newBitboards
       (list
        (cons "K" 0)
        (cons "Q" 0)
        (cons "R" 0)
        (cons "B" 0)
        (cons "N" 0)
        (cons "P" 0)
        (cons "k" 0)
        (cons "q" 0)
        (cons "r" 0)
        (cons "b" 0)
        (cons "n" 0)
        (cons "p" 0)))
     ; Matrix -> String
     (define (getPiece matrixIndex)
       (matrixGet matrix (floor (/ matrixIndex 8)) (modulo matrixIndex 8)))
     ; Dictionary<Bitboard> -> Dictionary<Bitboard>
     (define (writeBitBoard bitboards bitboard matrixIndex)
       (matrixToBitboardsAcc (dict-set bitboards bitboard (+ (dict-ref bitboards bitboard) (arithmetic-shift 1 (- 63 matrixIndex)))) (add1 matrixIndex)))
     ; Matrix Accumulator -> Dictionary<Bitboard>
     (define (matrixToBitboardsAcc bitboards matrixIndex)
       (if (equal? 64 matrixIndex) bitboards
           (cond
             [(equal? "K" (getPiece matrixIndex))
              (writeBitBoard bitboards "K" matrixIndex)]
             [(equal? "Q" (getPiece matrixIndex))
              (writeBitBoard bitboards "Q" matrixIndex)]
             [(equal? "R" (getPiece matrixIndex))
              (writeBitBoard bitboards "R" matrixIndex)]
             [(equal? "B" (getPiece matrixIndex))
              (writeBitBoard bitboards "B" matrixIndex)]
             [(equal? "N" (getPiece matrixIndex))
              (writeBitBoard bitboards "N" matrixIndex)]
             [(equal? "P" (getPiece matrixIndex))
              (writeBitBoard bitboards "P" matrixIndex)]
             [(equal? "k" (getPiece matrixIndex))
              (writeBitBoard bitboards "k" matrixIndex)]
             [(equal? "q" (getPiece matrixIndex))
              (writeBitBoard bitboards "q" matrixIndex)]
             [(equal? "r" (getPiece matrixIndex))
              (writeBitBoard bitboards "r" matrixIndex)]
             [(equal? "b" (getPiece matrixIndex))
              (writeBitBoard bitboards "b" matrixIndex)]
             [(equal? "n" (getPiece matrixIndex))
              (writeBitBoard bitboards "n" matrixIndex)]
             [(equal? "p" (getPiece matrixIndex))
              (writeBitBoard bitboards "p" matrixIndex)]
             [(equal? " " (getPiece matrixIndex))
              (matrixToBitboardsAcc bitboards (add1 matrixIndex))]))))
    (matrixToBitboardsAcc newBitboards 0)))


; Signature
; bitboardsToMatrix: Dictionary<Bitboard> -> Matrix
; Interpretation: assignes every "1" in the bitboards to the corresponding piece of the matrix. Both boards are accessed with matrixIndex which is a number that ranges from 0 to 63
; Header (define (bitboardsToMatrix STANDARD_MATRIX)

; Examples
; STANDARD_MATRIX

; Checks
(check-expect (bitboardsToMatrix BITBOARDS_OF_STANDARD_MATRIX) STANDARD_MATRIX)

; Implementation
(define (bitboardsToMatrix bitboards)
  (local
    ((define newMatrix
       (vector
        (vector " " " " " " " " " " " " " " " ")
        (vector " " " " " " " " " " " " " " " ")
        (vector " " " " " " " " " " " " " " " ")
        (vector " " " " " " " " " " " " " " " ")
        (vector " " " " " " " " " " " " " " " ")
        (vector " " " " " " " " " " " " " " " ")
        (vector " " " " " " " " " " " " " " " ")
        (vector " " " " " " " " " " " " " " " ")))
     ; Bitboard -> Number
     (define (getBit bitboard matrixIndex)
       (bitwise-and 1 (arithmetic-shift (dict-ref bitboards bitboard) (- matrixIndex 63))))
     ; Matrix -> Matrix
     (define (writeMatrix matrix matrixIndex value)
       (bitboardsToMatrixAcc (matrixSet matrix (floor (/ matrixIndex 8)) (modulo matrixIndex 8) value) (add1 matrixIndex)))
     ; Dictionary<Bitboard> Accumulator -> Matrix
     (define (bitboardsToMatrixAcc matrix matrixIndex)
       (if (equal? 64 matrixIndex) matrix
           (cond
             [(equal? 1 (getBit "K" matrixIndex))
              (writeMatrix matrix matrixIndex "K")]
             [(equal? 1 (getBit "Q" matrixIndex))
              (writeMatrix matrix matrixIndex "Q")]
             [(equal? 1 (getBit "R" matrixIndex))
              (writeMatrix matrix matrixIndex "R")]
             [(equal? 1 (getBit "B" matrixIndex))
              (writeMatrix matrix matrixIndex "B")]
             [(equal? 1 (getBit "N" matrixIndex))
              (writeMatrix matrix matrixIndex "N")]
             [(equal? 1 (getBit "P" matrixIndex))
              (writeMatrix matrix matrixIndex "P")]
             [(equal? 1 (getBit "k" matrixIndex))
              (writeMatrix matrix matrixIndex "k")]
             [(equal? 1 (getBit "q" matrixIndex))
              (writeMatrix matrix matrixIndex "q")]
             [(equal? 1 (getBit "r" matrixIndex))
              (writeMatrix matrix matrixIndex "r")]
             [(equal? 1 (getBit "b" matrixIndex))
              (writeMatrix matrix matrixIndex "b")]
             [(equal? 1 (getBit "n" matrixIndex))
              (writeMatrix matrix matrixIndex "n")]
             [(equal? 1 (getBit "p" matrixIndex))
              (writeMatrix matrix matrixIndex "p")]
             [else
              (writeMatrix matrix matrixIndex " ")]))))
    (bitboardsToMatrixAcc newMatrix 0)))


; Signature
; drawPieces: Matrix -> Image
; Interpretation: draws all the pieces found in 'matrix' on the constant BACKGROUND
; Header: (drawPieces matrix) STANDARD_MATRIX_IMG)

; Examples
(define STANDARD_MATRIX_IMG
  (above (beside (overlay BR_IMG LIGHT_SQUARE) (overlay BN_IMG DARK_SQUARE)  (overlay BB_IMG LIGHT_SQUARE) (overlay BQ_IMG DARK_SQUARE)  (overlay BK_IMG LIGHT_SQUARE) (overlay BB_IMG DARK_SQUARE)  (overlay BN_IMG LIGHT_SQUARE) (overlay BR_IMG DARK_SQUARE))
         (beside (overlay BP_IMG DARK_SQUARE)  (overlay BP_IMG LIGHT_SQUARE) (overlay BP_IMG DARK_SQUARE)  (overlay BP_IMG LIGHT_SQUARE) (overlay BP_IMG DARK_SQUARE)  (overlay BP_IMG LIGHT_SQUARE) (overlay BP_IMG DARK_SQUARE)  (overlay BP_IMG LIGHT_SQUARE))
         (beside LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE)
         (beside DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE)
         (beside LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE)
         (beside DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE)
         (beside (overlay WP_IMG LIGHT_SQUARE) (overlay WP_IMG DARK_SQUARE)  (overlay WP_IMG LIGHT_SQUARE) (overlay WP_IMG DARK_SQUARE)  (overlay WP_IMG LIGHT_SQUARE) (overlay WP_IMG DARK_SQUARE)  (overlay WP_IMG LIGHT_SQUARE) (overlay WP_IMG DARK_SQUARE))
         (beside (overlay WR_IMG DARK_SQUARE)  (overlay WN_IMG LIGHT_SQUARE) (overlay WB_IMG DARK_SQUARE)  (overlay WQ_IMG LIGHT_SQUARE) (overlay WK_IMG DARK_SQUARE)  (overlay WB_IMG LIGHT_SQUARE) (overlay WN_IMG DARK_SQUARE)  (overlay WR_IMG LIGHT_SQUARE))))

; Checks
(check-expect (drawPieces STANDARD_MATRIX) STANDARD_MATRIX_IMG)

; Implementation
(define (drawPieces matrix)
  (local
    (; Matrix Accumulator -> String
     (define (getPiece matrix matrixIndex)
       (matrixGet matrix (floor (/ matrixIndex 8)) (modulo matrixIndex 8)))
     ; Image Accumulator -> Image
     (define (drawPiece pieceIMG matrixIndex)
       (place-image pieceIMG (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (modulo matrixIndex 8))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ matrixIndex 8)))) (drawPiecesAcc matrix (add1 matrixIndex))))
     ; Matrix Accumulator -> Image
     (define (drawPiecesAcc matrix matrixIndex)
       (if (equal? 64 matrixIndex) BACKGROUND
           (cond
             [(equal? "K" (getPiece matrix matrixIndex))
              (drawPiece WK_IMG matrixIndex)]
             [(equal? "Q" (getPiece matrix matrixIndex))
              (drawPiece WQ_IMG matrixIndex)]
             [(equal? "R" (getPiece matrix matrixIndex))
              (drawPiece WR_IMG matrixIndex)]
             [(equal? "B" (getPiece matrix matrixIndex))
              (drawPiece WB_IMG matrixIndex)]
             [(equal? "N" (getPiece matrix matrixIndex))
              (drawPiece WN_IMG matrixIndex)]
             [(equal? "P" (getPiece matrix matrixIndex))
              (drawPiece WP_IMG matrixIndex)]
             [(equal? "k" (getPiece matrix matrixIndex))
              (drawPiece BK_IMG matrixIndex)]
             [(equal? "r" (getPiece matrix matrixIndex))
              (drawPiece BR_IMG matrixIndex)]
             [(equal? "q" (getPiece matrix matrixIndex))
              (drawPiece BQ_IMG matrixIndex)]
             [(equal? "b" (getPiece matrix matrixIndex))
              (drawPiece BB_IMG matrixIndex)]
             [(equal? "n" (getPiece matrix matrixIndex))
              (drawPiece BN_IMG matrixIndex)]
             [(equal? "p" (getPiece matrix matrixIndex))
              (drawPiece BP_IMG matrixIndex)]
             [(equal? " " (getPiece matrix matrixIndex))
              (drawPiecesAcc matrix (add1 matrixIndex))]))))
    (drawPiecesAcc matrix 0)))


; Signature
; printBitboard: Bitboard -> Matrix<Number>
; Intepretation: prints the bitboard as a matrix 
; Header:
; (define (printBitboard bitboard)
;   (vector
;    (vector 0 0 0 0 0 0 0 0)
;    (vector 0 0 0 0 0 0 0 0)
;    (vector 0 0 0 0 0 0 0 0)
;    (vector 0 0 0 0 0 0 0 0)
;    (vector 0 0 0 0 0 0 0 0)
;    (vector 0 0 0 0 0 0 0 0)
;    (vector 0 0 0 0 0 0 0 0)
;    (vector 0 0 0 0 0 0 0 0))))

; Checks
(check-expect (printBitboard (dict-ref BITBOARDS_OF_STANDARD_MATRIX "K"))
              (vector
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 1 0 0 0)))
(check-expect (printBitboard (dict-ref BITBOARDS_OF_STANDARD_MATRIX "r"))
              (vector
               (vector 1 0 0 0 0 0 0 1)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)))
(check-expect (printBitboard (dict-ref BITBOARDS_OF_STANDARD_MATRIX "P"))
              (vector
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 1 1 1 1 1 1 1 1)
               (vector 0 0 0 0 0 0 0 0)))

; Implementation
(define (printBitboard bitboard)
  (local
    ((define newMatrix
       (vector
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0))))
   (for/vector ([j (in-range (vector-length newMatrix))])
     (for/vector ([k (in-range (vector-length (vector-ref newMatrix j)))])
       (if (equal? 1 (bitwise-and 1 (arithmetic-shift bitboard (- (+ k (* 8 j)) 63))))
           1
           0)))))


; Signature
; reverseBinary: Number -> String
; Interpretation: takes a 'binary' and returns it with all the bits reversed

; Checks
(check-expect (reverseBinary #b1000000000000000000000000000000000000000000000000000000000000000)
                             #b0000000000000000000000000000000000000000000000000000000000000001)
(check-expect (reverseBinary #b1101010000000000000000000000000000000000000000000000000000000000)
                             #b0000000000000000000000000000000000000000000000000000000000101011)
(check-expect (reverseBinary #b0000000000000000000111000000000010000000000000000000000000000011)
                             #b1100000000000000000000000000000100000000001110000000000000000000)

; Implementation
(define (reverseBinary binary)
  (local
    ((define (reverseBinaryAcc binary matrixIndex totalSum)
       (cond
         [(equal? 64 matrixIndex) totalSum]
         [(equal? 1 (bitwise-and 1 (arithmetic-shift binary (- matrixIndex 63))))
          (reverseBinaryAcc binary (add1 matrixIndex) (+ totalSum (arithmetic-shift 1 matrixIndex)))]
         [else (reverseBinaryAcc binary (add1 matrixIndex) totalSum)])))
     (reverseBinaryAcc binary 0 0)))


; ============
; PIECES MOVES
; ============

; for King and Knight
(define FILE_A #b1000000010000000100000001000000010000000100000001000000010000000)
(define FILE_AB #b1100000011000000110000001100000011000000110000001100000011000000)
(define FILE_GH #b0000001100000011000000110000001100000011000000110000001100000011)
(define FILE_H #b0000000100000001000000010000000100000001000000010000000100000001)
(define NOT_FILE_A (bitwise-not FILE_A))
(define NOT_FILE_AB (bitwise-not FILE_AB))
(define NOT_FILE_GH (bitwise-not FILE_GH))
(define NOT_FILE_H (bitwise-not FILE_H))

; for Pawn
(define RANK_1 #b0000000000000000000000000000000000000000000000000000000011111111)
(define NOT_RANK_1 (bitwise-not RANK_1))
(define RANK_4 #b0000000000000000000000000000000011111111000000000000000000000000)
(define RANK_5 #b0000000000000000000000001111111100000000000000000000000000000000)
(define RANK_8 #b1111111100000000000000000000000000000000000000000000000000000000)
(define NOT_RANK_8 (bitwise-not RANK_8))

; for Rook
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


; define the diagonals that cover all the 8 rows
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

; for Bishop
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

; for Bishop
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

(define ALLPIECES
  (bitwise-xor
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "K")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "Q")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "R")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "B")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "N")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "P")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "k")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "q")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "r")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "b")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "n")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "p")))

(define WHITEPIECES
  (bitwise-xor
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "K")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "Q")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "R")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "B")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "N")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "P")))

(define BLACKPIECES
  (bitwise-xor
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "k")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "q")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "r")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "b")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "n")
   (dict-ref BITBOARDS_OF_STANDARD_MATRIX "p")))


; Signature
; kingMoves: Boolean Bitboard Bitboard Bitboard -> Bitboard
; Interpreation: starting from "positionBitboard", which is a bitboard with the position of the king written as a 1, another bitboard is
; generated where every 1 represents all the possible positions a king can move to. Depending on the color of the king, the result is then
; ANDed with another bitboard full of 1s except for the positions occupied by the other pieces of the same color. In this way we avoid pieces
; of the same color capturing each other. Notes: the result is ANDed with 18446744073709551615 to avoid 1s over the 64th bit.
; Header: (define (kingMoves color whitePieces blackPieces positionBitboard) 0)

; Examples
(define WKPOSITION1
  #b0000000000000000000000000000000000000000000000000000000000001000)
(define WKMOVES1
  #b0000000000000000000000000000000000000000000000000000000000000000)
(define WKPOSITION2
  #b0000000000000000000000000000000000001000000000000000000000000000)
(define WKMOVES2
  #b0000000000000000000000000001110000010100000111000000000000000000)

(define BKPOSITION1
  #b0000100000000000000000000000000000000000000000000000000000000000)
(define BKMOVES1
  #b0000000000000000000000000000000000000000000000000000000000000000)
(define BKPOSITION2
  #b0000000000000000000000000000000000000001000000000000000000000000)
(define BKMOVES2
  #b0000000000000000000000000000001100000010000000110000000000000000)

; Checks
(check-expect (kingMoves WHITE WHITEPIECES BLACKPIECES WKPOSITION1) WKMOVES1)
(check-expect (kingMoves WHITE WHITEPIECES BLACKPIECES WKPOSITION2) WKMOVES2)
(check-expect (kingMoves BLACK WHITEPIECES BLACKPIECES BKPOSITION1) BKMOVES1)
(check-expect (kingMoves BLACK WHITEPIECES BLACKPIECES BKPOSITION2) BKMOVES2)

; Implementation
(define (kingMoves color whitePieces blackPieces positionBitboard)
  (local
    ((define moves
       (bitwise-ior
        (bitwise-and (arithmetic-shift positionBitboard  1) NOT_FILE_H)
        (bitwise-and (arithmetic-shift positionBitboard  9) NOT_FILE_H)
        (arithmetic-shift positionBitboard  8)
        (bitwise-and (arithmetic-shift positionBitboard  7) NOT_FILE_A)
        (bitwise-and (arithmetic-shift positionBitboard -1) NOT_FILE_A)
        (bitwise-and (arithmetic-shift positionBitboard -9) NOT_FILE_A)
        (arithmetic-shift positionBitboard -8)
        (bitwise-and (arithmetic-shift positionBitboard -7) NOT_FILE_H))))
    (if (equal? #true color)
        (bitwise-and moves (bitwise-not (bitwise-and moves whitePieces)) 18446744073709551615)
        (bitwise-and moves (bitwise-not (bitwise-and moves blackPieces)) 18446744073709551615))))


; Signature
; rookMoves: Boolean Bitboard Bitboard Bitboard Bitboard Number -> Bitboard
; Interpretation: starting from "positionBitboard", which is a bitboard with the position of the rook written as a 1, another bitboard is
; generated where every 1 represents all the possible positions a rook can move to. Depending on the color of the rook, the result is then
; ANDed with another bitboard full of 1s except for the positions occupied by the other pieces of the same color. In this way we avoid pieces
; of the same color capturing each other.
; Header: (define (rookMoves color allPieces whitePieces blackPieces positionBitboard positionIndex) 0)

; Examples
(define WRPOSITION1
  #b0000000000000000000000000000000000000000000000000000000000000001)
(define WRPOSITIONINDEX1
  63)
(define WRMOVES1
  #b0000000000000000000000000000000000000000000000000000000000000000)
(define WRPOSITION2
  #b0000000000000000000000000000001000000000000000000000000000000000)
(define WRPOSITIONINDEX2
  30)
(define WRMOVES2
  #b0000000000000010000000101111110100000010000000100000000000000000)


(define BRPOSITION1
  #b1000000000000000000000000000000000000000000000000000000000000000)
(define BRPOSITIONINDEX1
  0)
(define BRMOVES1
  #b0000000000000000000000000000000000000000000000000000000000000000)
(define BRPOSITION2
  #b0000000000000000000000000000000000000000100000000000000000000000)
(define BRPOSITIONINDEX2
  40)
(define BRMOVES2
  #b0000000000000000100000001000000010000000011111111000000000000000)

; Checks
(check-expect (rookMoves WHITE ALLPIECES WHITEPIECES BLACKPIECES WRPOSITION1 WRPOSITIONINDEX1) WRMOVES1)
(check-expect (rookMoves WHITE ALLPIECES WHITEPIECES BLACKPIECES WRPOSITION2 WRPOSITIONINDEX2) WRMOVES2)
(check-expect (rookMoves BLACK ALLPIECES WHITEPIECES BLACKPIECES BRPOSITION1 BRPOSITIONINDEX1) BRMOVES1)
(check-expect (rookMoves BLACK ALLPIECES WHITEPIECES BLACKPIECES BRPOSITION2 BRPOSITIONINDEX2) BRMOVES2)

; Implementation
(define (rookMoves color allPieces whitePieces blackPieces positionBitboard positionIndex)
  (local
    ((define horizontalMoves
       (bitwise-xor (- allPieces (* 2 positionBitboard))
                    (reverseBinary (- (reverseBinary allPieces) (* 2 (reverseBinary positionBitboard))))))
     (define verticalMoves
       (bitwise-xor (- (bitwise-and allPieces (vector-ref FILEMASKS (modulo positionIndex 8))) (* 2 positionBitboard))
                    (reverseBinary (- (reverseBinary (bitwise-and allPieces (vector-ref FILEMASKS (modulo positionIndex 8)))) (* 2 (reverseBinary positionBitboard))))))
     (define moves
       (bitwise-ior (bitwise-and horizontalMoves (vector-ref RANKMASKS (floor (/ positionIndex 8)))) (bitwise-and verticalMoves (vector-ref FILEMASKS (modulo positionIndex 8))))))
     (if (equal? #true color)
         (bitwise-and moves (bitwise-not (bitwise-and moves whitePieces)) 18446744073709551615)
         (bitwise-and moves (bitwise-not (bitwise-and moves blackPieces)) 18446744073709551615))))

; Examples
(define WBPOSITION1
  #b0000000000000000000000000000000000000000000000000000000000000100)
(define WBPOSITIONINDEX1
  61)
(define WBMOVES1
  #b0000000000000000000000000000000000000000000000000000000000000000)
(define WBPOSITION2
  #b0000000000000000000000000000000000100000000000000000000000000000)
(define WBPOSITIONINDEX2
  34)
(define WBMOVES2
  #b0000000000000100100010000101000000000000010100000000000000000000)

(define BBPOSITION1
  #b0010000000000000000000000000000000000000000000000000000000000000)
(define BBPOSITIONINDEX1
  2)
(define BBMOVES1
  #b0000000000000000000000000000000000000000000000000000000000000000)
(define BBPOSITION2
  #b0000000000000000000000000000100000000000000000000000000000000000)
(define BBPOSITIONINDEX2
  28)
(define BBMOVES2
  #b0000000000000000000101000000000000010100001000100100000100000000)

; Checks
(check-expect (bishopMoves WHITE ALLPIECES WHITEPIECES BLACKPIECES WBPOSITION1 WBPOSITIONINDEX1) WBMOVES1)
(check-expect (bishopMoves WHITE ALLPIECES WHITEPIECES BLACKPIECES WBPOSITION2 WBPOSITIONINDEX2) WBMOVES2)
(check-expect (bishopMoves BLACK ALLPIECES WHITEPIECES BLACKPIECES BBPOSITION1 BBPOSITIONINDEX1) BBMOVES1)
(check-expect (bishopMoves BLACK ALLPIECES WHITEPIECES BLACKPIECES BBPOSITION2 BBPOSITIONINDEX2) BBMOVES2)

; Signature
; bishopMoves Boolean Bitboard Bitboard Bitboard Bitboard Number -> Bitboard
; Interpretation: starting from "positionBitboard", which is a bitboard with the position of the bishop written as a 1, another bitboard is
; generated where every 1 represents all the possible positions a bishop can move to. Depending on the color of the bishop, the result is then
; ANDed with another bitboard full of 1s except for the positions occupied by the other pieces of the same color. In this way we avoid pieces
; of the same color capturing each other.
; Header: (define (bishopMoves color allPieces whitePieces blackPieces positionBitboard positionIndex) 0)

; Implementation
(define (bishopMoves color allPieces whitePieces blackPieces positionBitboard positionIndex)
  (local
    ((define DiagonalMoves
       (bitwise-xor (- (bitwise-and allPieces (vector-ref DIAGONALMASKS (+ (floor (/ positionIndex 8)) (modulo positionIndex 8)))) (* 2 positionBitboard))
                    (reverseBinary (- (reverseBinary (bitwise-and allPieces (vector-ref DIAGONALMASKS (+ (floor (/ positionIndex 8)) (modulo positionIndex 8))))) (* 2 (reverseBinary positionBitboard))))))
     (define AntiDiagonalMoves
       (bitwise-xor (- (bitwise-and allPieces (vector-ref ANTIDIAGONALMASKS (+ (floor (/ positionIndex 8)) (- 7 (modulo positionIndex 8))))) (* 2 positionBitboard))
                    (reverseBinary (- (reverseBinary (bitwise-and allPieces (vector-ref ANTIDIAGONALMASKS (+ (floor (/ positionIndex 8)) (- 7 (modulo positionIndex 8)))))) (* 2 (reverseBinary positionBitboard))))))
     (define moves
       (bitwise-ior (bitwise-and DiagonalMoves (vector-ref DIAGONALMASKS (+ (floor (/ positionIndex 8)) (modulo positionIndex 8)))) (bitwise-and AntiDiagonalMoves (vector-ref ANTIDIAGONALMASKS (+ (floor (/ positionIndex 8)) (- 7 (modulo positionIndex 8))))))))
    (if (equal? #true color)
        (bitwise-and moves (bitwise-not (bitwise-and moves whitePieces)) 18446744073709551615)
        (bitwise-and moves (bitwise-not (bitwise-and moves blackPieces)) 18446744073709551615))))


; Signature
; queenMoves Boolean Bitboard Bitboard Bitboard Bitboard Number -> Bitboard
; Interpretation: starting from "positionBitboard", which is a bitboard with the position of the queen written as a 1, another bitboard is
; generated where every 1 represents all the possible positions a queen can move to. Depending on the color of the queen, the result is then
; ANDed with another bitboard full of 1s except for the positions occupied by the other pieces of the same color. In this way we avoid pieces
; of the same color capturing each other.
; Header: (define (queenMoves color allPieces whitePieces blackPieces positionBitboard positionIndex) 0)

; Examples
(define WQPOSITION1
  #b0000000000000000000000000000000000000000000000000000000000010000)
(define WQPOSITIONINDEX1
  59)
(define WQMOVES1
  #b0000000000000000000000000000000000000000000000000000000000000000)
(define WQPOSITION2
  #b0000000000000000000000000000000000000000000100000000000000000000)
(define WQPOSITIONINDEX2
  43)
(define WQMOVES2
  #b0000000000010001100100100101010000111000111011110000000000000000)

(define BQPOSITION1
  #b0001000000000000000000000000000000000000000000000000000000000000)
(define BQPOSITIONINDEX1
  3)
(define BQMOVES1
  #b0000000000000000000000000000000000000000000000000000000000000000)
(define BQPOSITION2
  #b0000000000000000000000000000010000000000000000000000000000000000)
(define BQPOSITIONINDEX2
  29)
(define BQMOVES2
  #b0000000000000000000011101111101100001110000101010010010000000000)

; Checks
(check-expect (queenMoves WHITE ALLPIECES WHITEPIECES BLACKPIECES WQPOSITION1 WQPOSITIONINDEX1) WQMOVES1)
(check-expect (queenMoves WHITE ALLPIECES WHITEPIECES BLACKPIECES WQPOSITION2 WQPOSITIONINDEX2) WQMOVES2)
(check-expect (queenMoves BLACK ALLPIECES WHITEPIECES BLACKPIECES BQPOSITION1 BQPOSITIONINDEX1) BQMOVES1)
(check-expect (queenMoves BLACK ALLPIECES WHITEPIECES BLACKPIECES BQPOSITION2 BQPOSITIONINDEX2) BQMOVES2)

; Implementation
(define (queenMoves color allPieces whitePieces blackPieces positionBitboard positionIndex)
  (bitwise-ior (rookMoves color allPieces whitePieces blackPieces positionBitboard positionIndex) (bishopMoves color allPieces whitePieces blackPieces positionBitboard positionIndex)))


; Signature
; knightMoves Boolean Bitboard Bitboard Bitboard Number -> Bitboard
; Interpretation: starting from "positionBitboard", which is a bitboard with the position of the knight written as a 1, another bitboard is
; generated where every 1 represents all the possible positions a knight can move to. Depending on the color of the knight, the result is then
; ANDed with another bitboard full of 1s except for the positions occupied by the other pieces of the same color. In this way we avoid pieces
; of the same color capturing each other.
; Header: (define (knightMoves color allPieces whitePieces blackPieces positionBitboard positionIndex) #b0000000000000000000000000000000000000000000001010000000000000000)

; Examples
(define WNPOSITION1
  #b0000000000000000000000000000000000000000000000000000000000000010)
(define WNMOVES1
  #b0000000000000000000000000000000000000000000001010000000000000000)
(define WNPOSITION2
  #b0000000000000000010000000000000000000000000000000000000000000000)
(define WNMOVES2
  #b1010000000010000000000000001000010100000000000000000000000000000)

(define BNPOSITION1
  #b0100000000000000000000000000000000000000000000000000000000000000)
(define BNMOVES1
  #b0000000000000000101000000000000000000000000000000000000000000000)
(define BNPOSITION2
  #b0000000000000000000000000000000100000000000000000000000000000000)
(define BNMOVES2
  #b0000000000000000000001000000000000000100000000100000000000000000)

; Checks
(check-expect (knightMoves WHITE WHITEPIECES BLACKPIECES WNPOSITION1) WNMOVES1)
(check-expect (knightMoves WHITE WHITEPIECES BLACKPIECES WNPOSITION2) WNMOVES2)
(check-expect (knightMoves BLACK WHITEPIECES BLACKPIECES BNPOSITION1) BNMOVES1)
(check-expect (knightMoves BLACK WHITEPIECES BLACKPIECES BNPOSITION2) BNMOVES2)

; Implementation
(define (knightMoves color whitePieces blackPieces positionBitboard)
  (local
    ((define moves
       (bitwise-ior
       (bitwise-and (arithmetic-shift positionBitboard  10) NOT_FILE_GH)
       (bitwise-and (arithmetic-shift positionBitboard  17) NOT_FILE_H)
       (bitwise-and (arithmetic-shift positionBitboard  15) NOT_FILE_A)
       (bitwise-and (arithmetic-shift positionBitboard  6)  NOT_FILE_AB)
       (bitwise-and (arithmetic-shift positionBitboard -10) NOT_FILE_AB)
       (bitwise-and (arithmetic-shift positionBitboard -17) NOT_FILE_A)
       (bitwise-and (arithmetic-shift positionBitboard -15) NOT_FILE_H)
       (bitwise-and (arithmetic-shift positionBitboard -6)  NOT_FILE_GH))))
    (if (equal? #true color)
        (bitwise-and moves (bitwise-not (bitwise-and moves whitePieces)) 18446744073709551615)
        (bitwise-and moves (bitwise-not (bitwise-and moves blackPieces)) 18446744073709551615))))


; Signature
; pawnMoves Boolean Bitboard Bitboard Bitboard Number -> Bitboard
; Interpretation: starting from "positionBitboard", which is a bitboard with the position of the pawn written as a 1, another bitboard is
; generated where every 1 represents all the possible positions a pawn can move to. Depending on the color of the pawn, the result is then
; ANDed with another bitboard full of 1s except for the positions occupied by the other pieces of the same color. In this way we avoid pieces
; of the same color capturing each other.
; Header: (define (pawnMoves color whitePieces blackPieces positionBitboard) 0)

; Examples
(define WPPOSITION1
  #b0000000000000000000000000000000000000000000000000000100000000000)
(define WPMOVES1
  #b0000000000000000000000000000000000001000000010000000000000000000)
(define WPPOSITION2
  #b0000000000000000000000000000000000100000000000000000000000000000)
(define WPMOVES2
  #b0000000000000000000000000010000000000000000000000000000000000000)
(define WPPOSITION3
  #b0000000000000000000000100000000000000000000000000000000000000000)
(define WPMOVES3
  #b0000000000000101000000000000000000000000000000000000000000000000)

(define BPPOSITION1
  #b0000000000010000000000000000000000000000000000000000000000000000)
(define BPMOVES1
  #b0000000000000000000100000001000000000000000000000000000000000000)
(define BPPOSITION2
  #b0000000000000000000000000000000000000001000000000000000000000000)
(define BPMOVES2
  #b0000000000000000000000000000000000000000000000010000000000000000)
(define BPPOSITION3
  #b0000000000000000000000000000000000000000000010000000000000000000)
(define BPMOVES3
  #b0000000000000000000000000000000000000000000000000001010000000000)

; Checks
(check-expect (pawnMoves WHITE ALLPIECES WHITEPIECES BLACKPIECES WPPOSITION1) WPMOVES1)
(check-expect (pawnMoves WHITE ALLPIECES WHITEPIECES BLACKPIECES WPPOSITION2) WPMOVES2)
(check-expect (pawnMoves WHITE ALLPIECES WHITEPIECES BLACKPIECES WPPOSITION3) WPMOVES3)
(check-expect (pawnMoves BLACK ALLPIECES WHITEPIECES BLACKPIECES BPPOSITION1) BPMOVES1)
(check-expect (pawnMoves BLACK ALLPIECES WHITEPIECES BLACKPIECES BPPOSITION2) BPMOVES2)
(check-expect (pawnMoves BLACK ALLPIECES WHITEPIECES BLACKPIECES BPPOSITION3) BPMOVES3)

(define (pawnMoves color allPieces whitePieces blackPieces positionBitboard)
  (local
    ((define whiteMoves
       (bitwise-ior
        (bitwise-and (arithmetic-shift positionBitboard 9) NOT_FILE_H NOT_RANK_8 blackPieces)
        (bitwise-and (arithmetic-shift positionBitboard 7) NOT_FILE_A NOT_RANK_8 blackPieces)
        (bitwise-and (arithmetic-shift positionBitboard 8) (bitwise-not allPieces) NOT_RANK_8)
        (bitwise-and (arithmetic-shift positionBitboard 16) (arithmetic-shift (bitwise-not allPieces) 8) (bitwise-not allPieces) RANK_4)))
     (define blackMoves
       (bitwise-ior
        (bitwise-and (arithmetic-shift positionBitboard -9) NOT_FILE_A NOT_RANK_1 whitePieces)
        (bitwise-and (arithmetic-shift positionBitboard -7) NOT_FILE_H NOT_RANK_1 whitePieces)
        (bitwise-and (arithmetic-shift positionBitboard -8) (bitwise-not allPieces) NOT_RANK_1)
        (bitwise-and (arithmetic-shift positionBitboard -16) (arithmetic-shift (bitwise-not allPieces) -8) (bitwise-not allPieces) RANK_5))))
    (if (equal? #true color)
        (bitwise-and whiteMoves (bitwise-not (bitwise-and whiteMoves whitePieces)) 18446744073709551615)
        (bitwise-and blackMoves (bitwise-not (bitwise-and blackMoves blackPieces)) 18446744073709551615))))




;=============================================================================================



(define (pawnAttacks color positionIndex)
  (local
    ((define whiteAttacks
       (bitwise-ior
        (bitwise-and (arithmetic-shift positionIndex 9) NOT_FILE_H NOT_RANK_8)
        (bitwise-and (arithmetic-shift positionIndex 7) NOT_FILE_A NOT_RANK_8)))
     (define blackAttacks
       (bitwise-ior
        (bitwise-and (arithmetic-shift positionIndex -9) NOT_FILE_A NOT_RANK_1)
        (bitwise-and (arithmetic-shift positionIndex -7) NOT_FILE_H NOT_RANK_1))))
    (if (equal? #true color)
        (bitwise-and whiteAttacks 18446744073709551615)
        (bitwise-and blackAttacks 18446744073709551615))))



(define PROMOTION_MENU (rectangle 100 100 "solid" "white"))     

;(define X (above (overlay WQ_IMG MENU) (overlay WR_IMG MENU)(overlay WB_IMG MENU)(overlay WN_IMG MENU)))

;(define (PawnPromotion color positionIndex)
;  (if (equal? color #true)
;    (above (overlay WQ_IMG MENU) (overlay WR_IMG MENU)(overlay WB_IMG MENU)(overlay WN_IMG MENU))
;  )
;  (else 
;    (above (overlay BQ_IMG MENU) (overlay BR_IMG MENU)(overlay BB_IMG MENU)(overlay BN_IMG MENU))
;  )
;)



(define (numberOfTrailingZeros bb no-zeroes)
  (if (equal? 1 (bitwise-and bb (arithmetic-shift 1 no-zeroes))) no-zeroes
      (numberOfTrailingZeros bb (add1 no-zeroes))))



;; King Safety v2
; get rooks attacks. Takes the bitboard of all rooks of a certain color as input along with the
; bitboard of allPieces pieces, and returns a bitboard with all attacks. It works by iterating through
; the bitboard untill it finds a 1, and then gets all the attacks for that position. 
(define (getRookAttacks-backend rb allPieces chessboardIndex attacks color whitePieces blackPieces positionBitboard)
  (cond
    [(equal? 64 chessboardIndex) attacks]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift rb (- chessboardIndex 63))))
     (getRookAttacks-backend rb allPieces (add1 chessboardIndex) (bitwise-ior attacks (rookMoves color allPieces whitePieces blackPieces positionBitboard chessboardIndex)))]
    [else (getRookAttacks-backend rb allPieces (add1 chessboardIndex) attacks)]))
    
; get bishop attacks. Takes the bitboard of all bishops of a certain color as input along with the
; bitboard of allPieces pieces, and returns a bitboard with all attacks. It works by iterating through
; the bitboard untill it finds a 1, and then gets all the attacks for that position. 
(define (getBishopAttacks-backend bb allPieces chessboardIndex attacks color whitePieces blackPieces positionBitboard)
  (cond
    [(equal? 64 chessboardIndex) attacks]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift bb (- chessboardIndex 63))))
     (getBishopAttacks-backend bb allPieces (add1 chessboardIndex) (bitwise-ior attacks (bishopMoves color allPieces whitePieces blackPieces positionBitboard chessboardIndex)))]
    [else (getBishopAttacks-backend bb allPieces (add1 chessboardIndex) attacks)]))

; get knights attacks. Takes the bitboard of all knights of a certain color as input along with the
; bitboard of allPieces pieces, and returns a bitboard with all attacks. It works by iterating through
; the bitboard untill it finds a 1, and then gets all the attacks for that position. 
(define (getKnightAttacks-backend nb allPieces chessboardIndex attacks color whitePieces blackPieces positionBitboard)
  (cond
    [(equal? 64 chessboardIndex) attacks]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift nb (- chessboardIndex 63))))
     (getKnightAttacks-backend nb allPieces (add1 chessboardIndex) (bitwise-ior attacks (knightMoves color whitePieces blackPieces positionBitboard)))]
    [else (getBishopAttacks-backend nb allPieces (add1 chessboardIndex) attacks)]))

; get black pawn attacks. Takes the bitboard of all black pawns as input along with the bitboard
; of allPieces pieces, and returns a bitboard with all attacks. It works by iterating thorugh the
; bitboard until it finds a 1, and then gets all the attacks for that position
(define (getBPawnAttacks-backend pb allPieces chessboardIndex attacks)
  (cond
    [(equal? 64 chessboardIndex) attacks]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift pb (- chessboardIndex 63))))
     (getBPawnAttacks-backend pb allPieces (add1 chessboardIndex) (bitwise-ior attacks (pawnAttacks #f chessboardIndex)))]
    [else (getBPawnAttacks-backend pb allPieces (add1 chessboardIndex) attacks)]))

; get white pawn attacks. Takes the bitboard of all white pawns as input along with the bitboard
; of allPieces pieces, and returns a bitboard with all attacks. It works by iterating thorugh the
; bitboard until it finds a 1, and then gets all the attacks for that position
(define (getWPawnAttacks-backend pb allPieces chessboardIndex attacks)
  (cond
    [(equal? 64 chessboardIndex) attacks]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift pb (- chessboardIndex 63))))
     (getWPawnAttacks-backend pb allPieces (add1 chessboardIndex) (bitwise-ior attacks (pawnAttacks #t chessboardIndex)))]
    [else (getWPawnAttacks-backend pb allPieces (add1 chessboardIndex) attacks)]))

; USE THESE FUNCTIONS
; - rook attacks frontend
; Calls getRookAttacks-backend and automatically passes the accumulators/const bitboards
(define (getRookAttacks rookBitBoard allPieces color whitePieces blackPieces positionBitboard)
  (getRookAttacks-backend rookBitBoard allPieces 0 0 color whitePieces blackPieces positionBitboard))

; - bishop attacks frontend
; Calls getBishopAttacks-backend and automatically passes the accumulators/const bitboards
(define (getBishopAttacks bishopBitBoard allPieces color whitePieces blackPieces positionBitboard)
  (getBishopAttacks-backend bishopBitBoard allPieces 0 0 color whitePieces blackPieces positionBitboard))

; - knight attacks frontend
; Calls getKnightAttacks-backend and automatically passes the accumulators/const bitboards
(define (getKnightAttacks knightBitBoard allPieces whitePieces blackPieces positionBitboard)
  (getKnightAttacks-backend knightBitBoard allPieces 0 0 whitePieces blackPieces positionBitboard))

; - black pawn attacks frontend
; Calls getBPawnAttacks-backend and automatically passes the accumulators/const bitboards
(define (getBPawnAttacks pawnBitBoard allPieces)
  (getBPawnAttacks-backend pawnBitBoard allPieces 0 0))

; - white pawn attacks frontend
; Calls getWPawnAttacks-backend and automatically passes the accumulators/const bitboards
(define (getWPawnAttacks pawnBitBoard allPieces)
  (getWPawnAttacks-backend pawnBitBoard allPieces 0 0))

;; Gets the attacks of all black pieces and returns a bitboard of the combined attacks
(define (getBlackAttacks BR BB BN BP allPieces whitePieces blackPieces positionBitboard)
  (bitwise-ior (bitwise-ior (getRookAttacks   BR allPieces #f whitePieces blackPieces positionBitboard) 
                            (getBishopAttacks BB allPieces #f whitePieces blackPieces positionBitboard)) 
               (bitwise-ior (getKnightAttacks BN allPieces #f whitePieces blackPieces positionBitboard) 
                            (getBPawnAttacks  BP allPieces)))) ; add king and queen later

;; Gets the attacks of all white pieces and returns a bitboard of the combined attacks
(define (getWhiteAttacks WR WB WN WP allPieces whitePieces blackPieces positionBitboard)
  (bitwise-ior (bitwise-ior (getRookAttacks   WR allPieces #t whitePieces blackPieces positionBitboard)
                            (getBishopAttacks WB allPieces #t whitePieces blackPieces positionBitboard))
               (bitwise-ior (getKnightAttacks WN allPieces #t whitePieces blackPieces positionBitboard)
                            (getWPawnAttacks  WP allPieces)))) ; add king and queen later

;; Performs an and between the WK bitboard and the result of getBlackAttacks. If it returns
;  0 then the king is safe, else it is in check
(define (isWhiteKingSafe WK BP BR BB BN BQ BK allPieces whitePieces blackPieces positionBitboard)
  (local [(define blackAttacks (getBlackAttacks BR BB BN BP allPieces whitePieces blackPieces positionBitboard))] ; TODO: add king and queen later
    (if (zero? (bitwise-and WK blackAttacks)) #t 
        #f)))

;; Performs an and between the BK bitboard and the result of getWhiteAttacks. If it returns
;  0 then the king is safe, else it is in check
(define (isBlackKingSafe BK WP WR WB WN WQ WK allPieces whitePieces blackPieces positionBitboard)
  (local [(define whiteAttacks (getWhiteAttacks WR WB WN WP allPieces whitePieces blackPieces positionBitboard))] ; TODO: add king and queen later
    (if (zero? (bitwise-and BK whiteAttacks)) #t
        #f))) 

; checkmate test ;

; when the king is in check call this function
; see if the king can move out to safety 
; see if the queen can save the king, then the rook...
; against every scenario, check for isxKingSafe

;; checks if the white king can safely move (used for checkmate detection)
(define (canWhiteKingMove WK BQ BR BB BN BP BK allPieces whitePieces blackPieces positionBitboard)
  (local [(define kingMoves    (kingMoves #t whitePieces blackPieces positionBitboard))
          (define blackAttacks (getBlackAttacks BR BB BN BP allPieces whitePieces blackPieces positionBitboard))]
  (if (= kingMoves (bitwise-and kingMoves blackAttacks)) #f
      #t)))

;; checks if the black king can safely move (used for checkmate detection)
(define (canBlackKingMove BK WQ WR WB WN WP WK allPieces whitePieces blackPieces positionBitboard)
  (local [(define kingMoves    (kingMoves #f whitePieces blackPieces positionBitboard))
          (define whiteAttacks (getWhiteAttacks WR WB WN WP allPieces whitePieces blackPieces positionBitboard))]
  (if (= kingMoves (bitwise-and kingMoves whiteAttacks)) #f
      #t)))

;; checks if the white pieces can protect the white king
(define (canWhitePiecesProtect WK WQ WR WB WN WP BK BQ BR BB BN BP allPieces whitePieces blackPieces positionBitboard)
  (local [(define whiteAttacks (getWhiteAttacks WR WB WN WP allPieces whitePieces blackPieces positionBitboard))
          (define blackAttacks (getBlackAttacks BR BB BN BP allPieces whitePieces blackPieces positionBitboard))]
  (if (zero? (bitwise-and WK (bitwise-and whiteAttacks blackAttacks))) #t 
      #f)))

(define (canBlackPiecesProtect WK WQ WR WB WN WP BK BQ BR BB BN BP allPieces whitePieces blackPieces positionBitboard)
  (local [(define whiteAttacks (getWhiteAttacks WR WB WN WP allPieces whitePieces blackPieces positionBitboard))
          (define blackAttacks (getBlackAttacks BR BB BN BP allPieces whitePieces blackPieces positionBitboard))]
  (if (zero? (bitwise-and BK (bitwise-and whiteAttacks blackAttacks))) #t 
      #f)))

(define (isWhiteKingCheckmate WK WQ WR WB WN WP BK BQ BR BB BN BP allPieces whitePieces blackPieces positionBitboard)
  (local [(define canKingMove     (canWhiteKingMove WK BQ BR BB BN BP BK allPieces whitePieces blackPieces positionBitboard))
          (define canWhiteProtect (canWhitePiecesProtect WK WQ WR WB WN WP BK BQ BR BB BN BP allPieces whitePieces blackPieces positionBitboard))]
  (if (or canKingMove canWhiteProtect) #f
    #t)))

(define (isBlackKingCheckmate WK WQ WR WB WN WP BK BQ BR BB BN BP allPieces whitePieces blackPieces positionBitboard)
  (local [(define canKingMove     (canBlackKingMove WK BQ BR BB BN BP BK allPieces whitePieces blackPieces positionBitboard))
          (define canBlackProtect (canBlackPiecesProtect WK WQ WR WB WN WP BK BQ BR BB BN BP allPieces whitePieces blackPieces positionBitboard))]
  (if (or canKingMove canBlackProtect) #f
    #t)))
;;;;;;;;;;;;;;;;;;



(define (draw worldState)
  (place-image (currentMove-icon (worldState-currentMove worldState))
               (posn-x (currentMove-end (worldState-currentMove worldState)))
               (posn-y (currentMove-end (worldState-currentMove worldState)))
               (worldState-chessboard worldState)))


(define (startMove worldState newX newY)
  (if (equal? #true (history-promotion (worldState-history worldState)))
      (make-worldState (worldState-chessboard worldState)
                       (worldState-matrix worldState)
                       (worldState-bitboards worldState)
                       (make-currentMove empty-image " " BLACK  (make-posn newX newY) (make-posn newX newY))
                       (worldState-history worldState)
                       (worldState-quit worldState))
      (make-worldState (hideSelectedPiece worldState newX newY)
                       (worldState-matrix worldState)
                       (worldState-bitboards worldState)
                       (newCurrentMove worldState newX newY)
                       (worldState-history worldState)
                       (worldState-quit worldState))))

(define (hideSelectedPiece worldState newX newY)
  (if (equal? 0 (modulo (+ (floor (/ newY SQUARE_SIDE)) (floor (/ newX SQUARE_SIDE))) 2))
      (place-image LIGHT_SQUARE (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newX SQUARE_SIDE)))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newY SQUARE_SIDE)))) (worldState-chessboard worldState))
      (place-image DARK_SQUARE (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newX SQUARE_SIDE)))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newY SQUARE_SIDE)))) (worldState-chessboard worldState))))

(define (newCurrentMove worldState newX newY)
  (local
    ((define piece
       (matrixGet (worldState-matrix worldState) (floor (/ newY SQUARE_SIDE)) (floor (/ newX SQUARE_SIDE)))))
  (cond
    [(equal? "K" piece)
     (make-currentMove WK_IMG "K" WHITE (make-posn newX newY) (make-posn newX newY))]
    [(equal? "Q" piece)
     (make-currentMove WQ_IMG "Q" WHITE (make-posn newX newY) (make-posn newX newY))]
    [(equal? "R" piece)
     (make-currentMove WR_IMG "R" WHITE (make-posn newX newY) (make-posn newX newY))]
    [(equal? "B" piece)
     (make-currentMove WB_IMG "B" WHITE (make-posn newX newY) (make-posn newX newY))]
    [(equal? "N" piece)
     (make-currentMove WN_IMG "N" WHITE (make-posn newX newY) (make-posn newX newY))]
    [(equal? "P" piece)
     (make-currentMove WP_IMG "P" WHITE (make-posn newX newY) (make-posn newX newY))]
    [(equal? "k" piece)
     (make-currentMove BK_IMG "k" BLACK (make-posn newX newY) (make-posn newX newY))]
    [(equal? "q" piece)
     (make-currentMove BQ_IMG "q" BLACK (make-posn newX newY) (make-posn newX newY))]
    [(equal? "r" piece)
     (make-currentMove BR_IMG "r" BLACK (make-posn newX newY) (make-posn newX newY))]
    [(equal? "b" piece)
     (make-currentMove BB_IMG "b" BLACK (make-posn newX newY) (make-posn newX newY))]
    [(equal? "n" piece)
     (make-currentMove BN_IMG "n" BLACK (make-posn newX newY) (make-posn newX newY))]
    [(equal? "p" piece)
     (make-currentMove BP_IMG "p" BLACK (make-posn newX newY) (make-posn newX newY))]
    [(equal? " " piece)
     (make-currentMove empty-image " " BLACK  (make-posn 0 0) (make-posn 0 0))])))

(define (changeMove worldState newX newY)
  (make-worldState (worldState-chessboard worldState)
                   (worldState-matrix worldState)
                   (worldState-bitboards worldState)
                   (make-currentMove (currentMove-icon (worldState-currentMove worldState))
                                     (currentMove-piece (worldState-currentMove worldState))
                                     (currentMove-color (worldState-currentMove worldState))
                                     (currentMove-start (worldState-currentMove worldState))
                                     (make-posn newX newY))
                   (worldState-history worldState)
                   (worldState-quit worldState)))











;POSITION INDEX BITBOARD...

;MATRIX

;PIECE ICON














; Color is a Boolean
; - #true:  white
; - #false: black

; Piece is a String of the following group:
; - "K": white king
; - "Q": white queen
; - "R": white rook
; - "B": white bishop
; - "N": white knight
; - "P": white pawn
; - "k": black king
; - "q": black queen
; - "r": black rook
; - "b": black bishop
; - "n": black knight
; - "p": black pawn
; - " ": non-existent
; It represents all the possible pieces

; CurrentMove is a Structure (make-currentMove icon color piece start end)
; - icon:  Image
; - color: Color
; - piece: Piece
; - start: Posn
; - end:   Posn
; A Posn represents 2 coordinate Numbers
; An icon is an image of Piece
(define-struct currentMove [icon piece color start end])

; Matrix<String> is a Vector of Vectors of Strings where all the vectors have length 8.
; It represents a 8x8 chessboard with elements of type String

; Bitboard is a binary number with 64 bits.
; It represents all the positions that a Piece can occupy as a "1".
; Since there are in total 12 different Pieces, 6 from white and 6 from black, 12 Bitboards are needed to fully define a Matrix

; Dictionary<Bitboard> is a dictionary that maps keys to values as follows:
; - "K": Bitboard white king
; - "Q": Bitboard white queen
; - "R": Bitboard white rook
; - "B": Bitboard white bishop
; - "N": Bitboard white knight 
; - "P": Bitboard white pawn
; - "k": Bitboard black king
; - "q": Bitboard black queen
; - "r": Bitboard black rook
; - "b": Bitboard black bishop
; - "n": Bitboard black knight
; - "p": Bitboard black pawn

; History is a Structure (make-history castle enPassant promotion previousEndPosition)
; - castle: ???
; - enPassant: ???
; - previousEndPosition: Posn
(define-struct history [castle enPassant promotion previousEndPosition])

; WorldState is a Structure (make-worldState chessboard matrix bitboards currentMove history quit)
; - chessboard:  Image
; - matrix:      Matrix
; - bitboards:   Dictionary<Bitboard>
; - currentMove: CurrentMove
; - history:     History
; - quit:        Boolean
(define-struct worldState [chessboard matrix bitboards currentMove history quit])




(define (makeMove worldState)
  (local
    ((define matrix
       (worldState-matrix worldState))
     (define bitboards
       (worldState-bitboards worldState))
     (define piece
       (currentMove-piece (worldState-currentMove worldState)))
     (define color
       (currentMove-color (worldState-currentMove worldState)))
     (define startPositionIndex
       (+ (floor (/ (posn-x (currentMove-start (worldState-currentMove worldState))) SQUARE_SIDE)) (* 8 (floor (/ (posn-y (currentMove-start (worldState-currentMove worldState))) SQUARE_SIDE)))))
     (define startPositionBitboard
       (arithmetic-shift 1 (- 63 startPositionIndex)))
     (define endPositionIndex
       (+ (floor (/ (posn-x (currentMove-end (worldState-currentMove worldState))) SQUARE_SIDE)) (* 8 (floor (/ (posn-y (currentMove-end (worldState-currentMove worldState))) SQUARE_SIDE)))))
     (define endPositionBitboard
       (arithmetic-shift 1 (- 63 endPositionIndex)))
     (define capturedPiece
       (matrixGet matrix (floor (/ (posn-y (currentMove-end (worldState-currentMove worldState))) SQUARE_SIDE)) (floor (/ (posn-x (currentMove-end (worldState-currentMove worldState))) SQUARE_SIDE))))
     (define allPieces
      (bitwise-xor
        (dict-ref bitboards "K")
        (dict-ref bitboards "Q")
        (dict-ref bitboards "R")
        (dict-ref bitboards "B")
        (dict-ref bitboards "N")
        (dict-ref bitboards "P")
        (dict-ref bitboards "k")
        (dict-ref bitboards "q")
        (dict-ref bitboards "r")
        (dict-ref bitboards "b")
        (dict-ref bitboards "n")
        (dict-ref bitboards "p")))
     (define whitePieces
       (bitwise-xor
        (dict-ref bitboards "K")
        (dict-ref bitboards "Q")
        (dict-ref bitboards "R")
        (dict-ref bitboards "B")
        (dict-ref bitboards "N")
        (dict-ref bitboards "P")))
     (define blackPieces
       (bitwise-xor
        (dict-ref bitboards "k")
        (dict-ref bitboards "q")
        (dict-ref bitboards "r")
        (dict-ref bitboards "b")
        (dict-ref bitboards "n")
        (dict-ref bitboards "p"))))
    (cond
      [(equal? #true (history-promotion (worldState-history worldState)))
       (cond
         [(equal? endPositionIndex (history-previousEndPosition (worldState-history worldState)))
          (make-worldState (drawPieces (bitboardsToMatrix (updatePieceAtPosition bitboards "Q" (arithmetic-shift 1 (- 63 endPositionIndex)))))
                           (bitboardsToMatrix (updatePieceAtPosition bitboards "Q" (arithmetic-shift 1 (- 63 endPositionIndex))))
                           (updatePieceAtPosition bitboards "Q" (arithmetic-shift 1 (- 63 endPositionIndex)))
                           (make-currentMove empty-image " " BLACK  (make-posn 0 0) (make-posn 0 0))
                           (make-history (history-castle (worldState-history worldState))
                                      (history-enPassant (worldState-history worldState))
                                      #false
                                      endPositionIndex)
                           (worldState-quit worldState))]
         [else
          (make-worldState (drawPromotionMenu (drawPieces (worldState-matrix worldState)) WHITE (history-previousEndPosition (worldState-history worldState)))
                           (worldState-matrix worldState)
                           (worldState-bitboards worldState)
                           (make-currentMove empty-image " " BLACK  (make-posn 0 0) (make-posn 0 0))
                           (worldState-history worldState)
                           (worldState-quit worldState))])]
      
      [(and (equal? "P" piece) (equal? 1 (floor (/ startPositionIndex 8))) (equal? 0 (floor (/ endPositionIndex 8))))
       (make-worldState (drawPromotionMenu (drawPieces (bitboardsToMatrix (updatePieceAtPosition (updatePieceAtPosition bitboards "P" startPositionBitboard) capturedPiece endPositionBitboard))) WHITE endPositionIndex)
                        (bitboardsToMatrix (updatePieceAtPosition (updatePieceAtPosition bitboards "P" startPositionBitboard) capturedPiece endPositionBitboard))
                        (updatePieceAtPosition (updatePieceAtPosition bitboards "P" startPositionBitboard) capturedPiece endPositionBitboard)
                        (make-currentMove empty-image " " BLACK  (make-posn 0 0) (make-posn 0 0))
                        (make-history (history-castle (worldState-history worldState))
                                      (history-enPassant (worldState-history worldState))
                                      #true
                                      endPositionIndex)
                        (worldState-quit worldState))]





                                  
      [(equal? 1 (arithmetic-shift (bitwise-and (getMovesPiece piece color startPositionBitboard startPositionIndex allPieces whitePieces blackPieces) endPositionBitboard) (- endPositionIndex 63)))
       (make-worldState (drawPieces (bitboardsToMatrix (updateBitboards bitboards piece startPositionBitboard endPositionBitboard capturedPiece)))
                        (bitboardsToMatrix (updateBitboards bitboards piece startPositionBitboard endPositionBitboard capturedPiece))
                        (updateBitboards bitboards piece startPositionBitboard endPositionBitboard capturedPiece)
                        (make-currentMove empty-image " " BLACK  (make-posn 0 0) (make-posn 0 0))
                        (make-history (history-castle (worldState-history worldState))
                                      (history-enPassant (worldState-history worldState))
                                      (history-promotion (worldState-history worldState))
                                      endPositionIndex)
                        (worldState-quit worldState))]
      [else
       (make-worldState (drawPieces (worldState-matrix worldState))
                        (worldState-matrix worldState)
                        (worldState-bitboards worldState)
                        (make-currentMove empty-image " " BLACK  (make-posn 0 0) (make-posn 0 0))
                        (worldState-history worldState)
                        (worldState-quit worldState))])))




(define (drawPromotionMenu chessboard color positionIndex)
  (local
    ((define whitePromotionMenu
       (above (overlay WQ_IMG WHITE_SQUARE) (overlay WR_IMG WHITE_SQUARE)(overlay WB_IMG WHITE_SQUARE)(overlay WN_IMG WHITE_SQUARE)))
     (define blackPromotionMenu
       (above (overlay BN_IMG WHITE_SQUARE) (overlay BB_IMG WHITE_SQUARE)(overlay BR_IMG WHITE_SQUARE)(overlay BQ_IMG WHITE_SQUARE))))
    (if (equal? #true color)
        (place-image whitePromotionMenu (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (modulo positionIndex 8))) (* SQUARE_SIDE 2) chessboard)
        (place-image blackPromotionMenu (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (modulo positionIndex 8))) (* SQUARE_SIDE 6) chessboard))))
 




(define (updateBitboards bitboards piece startPositionBitboard endPositionBitboard capturedPiece)
  (if (equal? " " capturedPiece)
      (updatePieceAtPosition (updatePieceAtPosition bitboards piece startPositionBitboard) piece endPositionBitboard)
      (updatePieceAtPosition (updatePieceAtPosition (updatePieceAtPosition bitboards piece startPositionBitboard) piece endPositionBitboard) capturedPiece endPositionBitboard)))

(define (updatePieceAtPosition bitboards piece positionBitboard)
  (dict-set bitboards piece (bitwise-xor (dict-ref bitboards piece) positionBitboard)))


;(define (capturedPiece worldState)
;       (chessboardGet (worldState-matrix worldState) (floor (/ (posn-y (currentMove-end (worldState-currentMove worldState))) SQUARE_SIDE)) (floor (/ (posn-x (currentMove-end (worldState-currentMove worldState))) SQUARE_SIDE))))
;(begin (if (not (equal? " " (capturedPiece worldState)))
;                      (dict-set BITBOARDS (capturedPiece worldState) (bitwise-xor (dict-ref BITBOARDS capturedPiece (arithmetic-shift 1 (- 63 (endIndex worldState))))))
;                      (void)))

(define (getMovesPiece piece color positionBitboard positionIndex allPieces whitePieces blackPieces)
  (cond
    [(or (equal? "K" piece) (equal? "k" piece))
             (kingMoves color whitePieces blackPieces positionBitboard)]
    [(or (equal? "Q" piece) (equal? "q" piece))
             (queenMoves color allPieces whitePieces blackPieces positionBitboard positionIndex)]
    [(or (equal? "R" piece) (equal? "r" piece))
             (rookMoves color allPieces whitePieces blackPieces positionBitboard positionIndex)]
    [(or (equal? "B" piece) (equal? "b" piece))
             (bishopMoves color allPieces whitePieces blackPieces positionBitboard positionIndex)]
    [(or (equal? "N" piece) (equal? "n" piece))
             (knightMoves color whitePieces blackPieces positionBitboard)]
    [(or (equal? "P" piece) (equal? "p" piece))
             (pawnMoves color allPieces whitePieces blackPieces positionBitboard)]
    [else 0]))







; TEST SECTION
(define testState (make-worldState
                        (drawPieces EMPTY_MATRIX)
                        EMPTY_MATRIX
                        BITBOARDS
                        (make-currentMove
                         WP_IMG
                         "P"
                         WHITE
                         (make-posn 25 650)
                         (make-posn 25 550))
                        #false
                        #false))









(define (quit worldState)
  (make-worldState (worldState-chessboard worldState)
                   (worldState-matrix worldState)
                   (worldState-bitboards worldState)
                   (worldState-currentMove worldState)
                   (worldState-history worldState)
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
    [(string=? "button-up" mouse-event) (makeMove worldState)]
    [else worldState]))





(define initialState (make-worldState
                        (drawPieces STANDARD_MATRIX)
                        STANDARD_MATRIX
                        (matrixToBitboards STANDARD_MATRIX)
                        (make-currentMove empty-image " " BLACK  (make-posn 0 0) (make-posn 0 0))
                        (make-history #false
                                      #false
                                      #false
                                      0)
                        #false))

(define (drawing-app initialState)
  (big-bang initialState
    [to-draw draw]
    [on-mouse handle-mouse]
    [on-key handle-key]
    [stop-when quit?]
    [close-on-stop #true]))

(drawing-app initialState)