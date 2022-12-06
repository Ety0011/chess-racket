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


;; Data type
;  A chessboard is an image made of vectors
;  Interpretation: The chessboard contain the different pieces

; An empty chessboard is a vector made from 64 empty squares.
(define EMPTY_CHESSBOARD
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
   (vector "p" "p" "p" "p" "p" "p" "p" "p")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " " ")
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
; chessboardGet: Chessboard Number Number -> String
; Intepretation: takes the coordinates of 'roB' and 'column' of a 'chessboard' to access the stored value in the corresponding element
; Header: (define (chessboardGet matrix row col) " ")

; Checks
(check-expect (chessboardGet STANDARD_CHESSBOARD 0 0) "r")
(check-expect (chessboardGet STANDARD_CHESSBOARD 4 3) " ")
(check-expect (chessboardGet STANDARD_CHESSBOARD 7 4) "K")

; Implementation
(define (chessboardGet chessboard row col)
  (vector-ref (vector-ref chessboard row) col))


; Signature
; chessboardSet: Chessboard Number Number -> Chessboard
; Intepretation: takes the coordinates of 'row' and 'column' of a 'chessboard' and returns the same chessboard with the updated value in the corresponding coordinate
; Header: (define (chessboardSet matrix row col value))
; Additional infos: not in library, adapted from https://stackoverflow.com/questions/38421007/update-element-of-immutable-vector-vector-set

; Implementation
(define (chessboardSet chessboard row column value)
   (for/vector ([j (in-range (vector-length chessboard))])
     (for/vector ([k (in-range (vector-length (vector-ref chessboard j)))])
       (if (and (= j row) (= k column))
           value
           (vector-ref (vector-ref chessboard j) k)))))


; Signature
; chessboardToBitboards: Chessboard -> Dictionary<Bitboard>
; Interpretation: assignes every piece in the chessboard to the corresponding "1" in the bitboards. Both boards are accessed with chessboardIndex which is a number that ranges from 0 to 63
; Header: (define (chessboardToBitboards chessboard) BITBOARDS_OF_STANDARD_CHESSBOARD)

; Examples
(define BITBOARDS_OF_STANDARD_CHESSBOARD
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
(check-expect (chessboardToBitboards STANDARD_CHESSBOARD) BITBOARDS_OF_STANDARD_CHESSBOARD)

; Implementation
(define (chessboardToBitboards chessboard)
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
     ; Chessboard -> String
     (define (getPiece chessboardIndex)
       (chessboardGet chessboard (floor (/ chessboardIndex 8)) (modulo chessboardIndex 8)))
     ; Dictionary<Bitboard> -> Dictionary<Bitboard>
     (define (writeBitBoard bitboards bitboard chessboardIndex)
       (chessboardToBitboardsAcc (dict-set bitboards bitboard (+ (dict-ref bitboards bitboard) (arithmetic-shift 1 (- 63 chessboardIndex)))) (add1 chessboardIndex)))
     ; Chessboard Accumulator -> Dictionary<Bitboard>
     (define (chessboardToBitboardsAcc bitboards chessboardIndex)
       (if (equal? 64 chessboardIndex) bitboards
           (cond
             [(equal? "K" (getPiece chessboardIndex))
              (writeBitBoard bitboards "K" chessboardIndex)]
             [(equal? "Q" (getPiece chessboardIndex))
              (writeBitBoard bitboards "Q" chessboardIndex)]
             [(equal? "R" (getPiece chessboardIndex))
              (writeBitBoard bitboards "R" chessboardIndex)]
             [(equal? "B" (getPiece chessboardIndex))
              (writeBitBoard bitboards "B" chessboardIndex)]
             [(equal? "N" (getPiece chessboardIndex))
              (writeBitBoard bitboards "N" chessboardIndex)]
             [(equal? "P" (getPiece chessboardIndex))
              (writeBitBoard bitboards "P" chessboardIndex)]
             [(equal? "k" (getPiece chessboardIndex))
              (writeBitBoard bitboards "k" chessboardIndex)]
             [(equal? "q" (getPiece chessboardIndex))
              (writeBitBoard bitboards "q" chessboardIndex)]
             [(equal? "r" (getPiece chessboardIndex))
              (writeBitBoard bitboards "r" chessboardIndex)]
             [(equal? "b" (getPiece chessboardIndex))
              (writeBitBoard bitboards "b" chessboardIndex)]
             [(equal? "n" (getPiece chessboardIndex))
              (writeBitBoard bitboards "n" chessboardIndex)]
             [(equal? "p" (getPiece chessboardIndex))
              (writeBitBoard bitboards "p" chessboardIndex)]
             [(equal? " " (getPiece chessboardIndex))
              (chessboardToBitboardsAcc bitboards (add1 chessboardIndex))]))))
    (chessboardToBitboardsAcc newBitboards 0)))


; Signature
; bitboardsToChessboard: Dictionary<Bitboard> -> Chessboard
; Interpretation: assignes every "1" in the bitboards to the corresponding piece of the chessboard. Both boards are accessed with chessboardIndex which is a number that ranges from 0 to 63
; Header (define (bitboardsToChessboard chessboard)

; Examples
; STANDARD_CHESSBOARD

; Checks
(check-expect (bitboardsToChessboard BITBOARDS_OF_STANDARD_CHESSBOARD) STANDARD_CHESSBOARD)

; Implementation
(define (bitboardsToChessboard bitboards)
  (local
    ((define newChessboard
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
     (define (getBit bitboard chessboardIndex)
       (bitwise-and 1 (arithmetic-shift (dict-ref bitboards bitboard) (- chessboardIndex 63))))
     ; Chessboard -> Chessboard
     (define (writeChessBoard chessboard chessboardIndex value)
       (bitboardsToChessboardAcc (chessboardSet chessboard (floor (/ chessboardIndex 8)) (modulo chessboardIndex 8) value) (add1 chessboardIndex)))
     ; Dictionary<Bitboard> Accumulator -> Chessboard
     (define (bitboardsToChessboardAcc chessboard chessboardIndex)
       (if (equal? 64 chessboardIndex) chessboard
           (cond
             [(equal? 1 (getBit "K" chessboardIndex))
              (writeChessBoard chessboard chessboardIndex "K")]
             [(equal? 1 (getBit "Q" chessboardIndex))
              (writeChessBoard chessboard chessboardIndex "Q")]
             [(equal? 1 (getBit "R" chessboardIndex))
              (writeChessBoard chessboard chessboardIndex "R")]
             [(equal? 1 (getBit "B" chessboardIndex))
              (writeChessBoard chessboard chessboardIndex "B")]
             [(equal? 1 (getBit "N" chessboardIndex))
              (writeChessBoard chessboard chessboardIndex "N")]
             [(equal? 1 (getBit "P" chessboardIndex))
              (writeChessBoard chessboard chessboardIndex "P")]
             [(equal? 1 (getBit "k" chessboardIndex))
              (writeChessBoard chessboard chessboardIndex "k")]
             [(equal? 1 (getBit "q" chessboardIndex))
              (writeChessBoard chessboard chessboardIndex "q")]
             [(equal? 1 (getBit "r" chessboardIndex))
              (writeChessBoard chessboard chessboardIndex "r")]
             [(equal? 1 (getBit "b" chessboardIndex))
              (writeChessBoard chessboard chessboardIndex "b")]
             [(equal? 1 (getBit "n" chessboardIndex))
              (writeChessBoard chessboard chessboardIndex "n")]
             [(equal? 1 (getBit "p" chessboardIndex))
              (writeChessBoard chessboard chessboardIndex "p")]
             [else
              (writeChessBoard chessboard chessboardIndex " ")]))))
    (bitboardsToChessboardAcc newChessboard 0)))


; Signature
; drawPieces: Chessboard -> Image
; Interpretation: draws all the pieces found in 'chessboard' on the constant BACKGROUND
; Header: (drawPieces chessboard) STANDARD_CHESSBOARD_IMG)

; Examples
(define STANDARD_CHESSBOARD_IMG
  (above (beside (overlay BR_IMG LIGHT_SQUARE) (overlay BN_IMG DARK_SQUARE)  (overlay BB_IMG LIGHT_SQUARE) (overlay BQ_IMG DARK_SQUARE)  (overlay BK_IMG LIGHT_SQUARE) (overlay BB_IMG DARK_SQUARE)  (overlay BN_IMG LIGHT_SQUARE) (overlay BR_IMG DARK_SQUARE))
         (beside (overlay BP_IMG DARK_SQUARE)  (overlay BP_IMG LIGHT_SQUARE) (overlay BP_IMG DARK_SQUARE)  (overlay BP_IMG LIGHT_SQUARE) (overlay BP_IMG DARK_SQUARE)  (overlay BP_IMG LIGHT_SQUARE) (overlay BP_IMG DARK_SQUARE)  (overlay BP_IMG LIGHT_SQUARE))
         (beside LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE)
         (beside DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE)
         (beside LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE)
         (beside DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE)
         (beside (overlay WP_IMG LIGHT_SQUARE) (overlay WP_IMG DARK_SQUARE)  (overlay WP_IMG LIGHT_SQUARE) (overlay WP_IMG DARK_SQUARE)  (overlay WP_IMG LIGHT_SQUARE) (overlay WP_IMG DARK_SQUARE)  (overlay WP_IMG LIGHT_SQUARE) (overlay WP_IMG DARK_SQUARE))
         (beside (overlay WR_IMG DARK_SQUARE)  (overlay WN_IMG LIGHT_SQUARE) (overlay WB_IMG DARK_SQUARE)  (overlay WQ_IMG LIGHT_SQUARE) (overlay WK_IMG DARK_SQUARE)  (overlay WB_IMG LIGHT_SQUARE) (overlay WN_IMG DARK_SQUARE)  (overlay WR_IMG LIGHT_SQUARE))))

; Checks
(check-expect (drawPieces STANDARD_CHESSBOARD) STANDARD_CHESSBOARD_IMG)

; Implementation
(define (drawPieces chessboard)
  (local
    (; Chessboard Accumulator -> String
     (define (getPiece chessboard chessboardIndex)
       (chessboardGet chessboard (floor (/ chessboardIndex 8)) (modulo chessboardIndex 8)))
     ; Image Accumulator -> Image
     (define (drawPiece pieceIMG chessboardIndex)
       (place-image pieceIMG (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (modulo chessboardIndex 8))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ chessboardIndex 8)))) (drawPiecesAcc chessboard (add1 chessboardIndex))))
     ; Chessboard Accumulator -> Image
     (define (drawPiecesAcc chessboard chessboardIndex)
       (if (equal? 64 chessboardIndex) BACKGROUND
           (cond
             [(equal? "K" (getPiece chessboard chessboardIndex))
              (drawPiece WK_IMG chessboardIndex)]
             [(equal? "Q" (getPiece chessboard chessboardIndex))
              (drawPiece WQ_IMG chessboardIndex)]
             [(equal? "R" (getPiece chessboard chessboardIndex))
              (drawPiece WR_IMG chessboardIndex)]
             [(equal? "B" (getPiece chessboard chessboardIndex))
              (drawPiece WB_IMG chessboardIndex)]
             [(equal? "N" (getPiece chessboard chessboardIndex))
              (drawPiece WN_IMG chessboardIndex)]
             [(equal? "P" (getPiece chessboard chessboardIndex))
              (drawPiece WP_IMG chessboardIndex)]
             [(equal? "k" (getPiece chessboard chessboardIndex))
              (drawPiece BK_IMG chessboardIndex)]
             [(equal? "r" (getPiece chessboard chessboardIndex))
              (drawPiece BR_IMG chessboardIndex)]
             [(equal? "q" (getPiece chessboard chessboardIndex))
              (drawPiece BQ_IMG chessboardIndex)]
             [(equal? "b" (getPiece chessboard chessboardIndex))
              (drawPiece BB_IMG chessboardIndex)]
             [(equal? "n" (getPiece chessboard chessboardIndex))
              (drawPiece BN_IMG chessboardIndex)]
             [(equal? "p" (getPiece chessboard chessboardIndex))
              (drawPiece BP_IMG chessboardIndex)]
             [(equal? " " (getPiece chessboard chessboardIndex))
              (drawPiecesAcc chessboard (add1 chessboardIndex))]))))
    (drawPiecesAcc chessboard 0)))


; Signature
; printBitboard: Bitboard -> Chessboard<Number>
; Intepretation: prints the bitboard as a Chessboard 
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
(check-expect (printBitboard (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "K"))
              (vector
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 1 0 0 0)))
(check-expect (printBitboard (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "r"))
              (vector
               (vector 1 0 0 0 0 0 0 1)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)
               (vector 0 0 0 0 0 0 0 0)))
(check-expect (printBitboard (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "P"))
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
    ((define chessboard
       (vector
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0)
        (vector 0 0 0 0 0 0 0 0))))
   (for/vector ([j (in-range (vector-length chessboard))])
     (for/vector ([k (in-range (vector-length (vector-ref chessboard j)))])
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
    ((define (reverseBinaryAcc binary chessboardIndex totalSum)
       (cond
         [(equal? 64 chessboardIndex) totalSum]
         [(equal? 1 (bitwise-and 1 (arithmetic-shift binary (- chessboardIndex 63))))
          (reverseBinaryAcc binary (add1 chessboardIndex) (+ totalSum (arithmetic-shift 1 chessboardIndex)))]
         [else (reverseBinaryAcc binary (add1 chessboardIndex) totalSum)])))
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
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "K")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "Q")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "R")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "B")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "N")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "P")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "k")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "q")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "r")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "b")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "n")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "p")))

(define WHITEPIECES
  (bitwise-xor
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "K")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "Q")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "R")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "B")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "N")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "P")))

(define BLACKPIECES
  (bitwise-xor
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "k")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "q")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "r")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "b")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "n")
   (dict-ref BITBOARDS_OF_STANDARD_CHESSBOARD "p")))


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

    

(define (blackPawnAttacks chessboardIndex)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 chessboardIndex))))
    


(define PROMOTION_MENU (rectangle 100 100 "solid" "white"))     

(define X (above (overlay WQ_IMG MENU) (overlay WR_IMG MENU)(overlay WB_IMG MENU)(overlay WN_IMG MENU)))

(define (PawnPromotion color positionIndex)
  (if (equal? color #true)
    (above (overlay WQ_IMG MENU) (overlay WR_IMG MENU)(overlay WB_IMG MENU)(overlay WN_IMG MENU))
  )
  (else 
    (above (overlay BQ_IMG MENU) (overlay BR_IMG MENU)(overlay BB_IMG MENU)(overlay BN_IMG MENU))
  )
)



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
(define (getKnightAttacks-backend nb chessboardIndex attacks color whitePieces blackPieces positionBitboard)
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
     (getBPawnAttacks-backend pb allPieces (add1 chessboardIndex) (bitwise-ior attacks (blackPawnAttacks chessboardIndex)))]
    [else (getBPawnAttacks-backend pb allPieces (add1 chessboardIndex) attacks)]))

; get white pawn attacks. Takes the bitboard of all white pawns as input along with the bitboard
; of allPieces pieces, and returns a bitboard with all attacks. It works by iterating thorugh the
; bitboard until it finds a 1, and then gets all the attacks for that position
(define (getWPawnAttacks-backend pb allPieces chessboardIndex attacks)
  (cond
    [(equal? 64 chessboardIndex) attacks]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift pb (- chessboardIndex 63))))
     (getWPawnAttacks-backend pb allPieces (add1 chessboardIndex) (bitwise-ior attacks (whitePawnAttacks chessboardIndex)))]
    [else (getWPawnAttacks-backend pb allPieces (add1 chessboardIndex) attacks)]))


; USE THESE FUNCTIONS
; - rook attacks frontend
; Calls getRookAttacks-backend and automatically passes the accumulators/const bitboards
(define (getRookAttacks rookBitBoard allPieces)
  (getRookAttacks-backend rookBitBoard allPieces 0 0))

; - bishop attacks frontend
; Calls getBishopAttacks-backend and automatically passes the accumulators/const bitboards
(define (getBishopAttacks bishopBitBoard allPieces)
  (getBishopAttacks-backend bishopBitBoard allPieces 0 0))

; - knight attacks frontend
; Calls getKnightAttacks-backend and automatically passes the accumulators/const bitboards
(define (getKnightAttacks knightBitBoard allPieces)
  (getKnightAttacks-backend knightBitBoard allPieces 0 0))

; - black pawn attacks frontend
; Calls getBPawnAttacks-backend and automatically passes the accumulators/const bitboards
(define (getBPawnAttacks pawnBitBoard allPieces)
  (getBPawnAttacks-backend pawnBitBoard allPieces 0 0))

; - white pawn attacks frontend
; Calls getWPawnAttacks-backend and automatically passes the accumulators/const bitboards
(define (getWPawnAttacks pawnBitBoard allPieces)
  (getWPawnAttacks-backend pawnBitBoard allPieces 0 0))

;; Gets the attacks of all black pieces and returns a bitboard of the combined attacks
(define (getBlackAttacks BR BB BN BP allPieces)
  (bitwise-ior (bitwise-ior (getRookAttacks   BR allPieces) 
                            (getBishopAttacks BB allPieces)) 
               (bitwise-ior (getKnightAttacks BN allPieces) 
                            (getBPawnAttacks  BP allPieces)))) ; add king and queen later

;; Gets the attacks of all white pieces and returns a bitboard of the combined attacks
(define (getWhiteAttacks WR WB WN WP allPieces)
  (bitwise-ior (bitwise-ior (getRookAttacks   WR allPieces)
                            (getBishopAttacks WB allPieces))
               (bitwise-ior (getKnightAttacks WN allPieces)
                            (getWPawnAttacks  WP allPieces)))) ; add king and queen later

;; Performs an and between the WK bitboard and the result of getBlackAttacks. If it returns
;  0 then the king is safe, else it is in check
(define (isWhiteKingSafe WK BP BR BB BN BQ BK allPieces)
  (local [(define blackAttacks (getBlackAttacks BR BB BN BP allPieces))] ; TODO: add king and queen later
    (if (zero? (bitwise-and WK blackAttacks)) #t 
        #f)))

;; Performs an and between the BK bitboard and the result of getWhiteAttacks. If it returns
;  0 then the king is safe, else it is in check
(define (isBlackKingSafe BK WP WR WB WN WQ WK allPieces)
  (local [(define whiteAttacks (getWhiteAttacks WR WB WN WP allPieces))] ; TODO: add king and queen later
    (if (zero? (bitwise-and BK whiteAttacks)) #t
        #f))) 

; TODO: add checkmate
;   POSSIBLE IMPLEMENTATION: iterate trhough all possible moves for a certain color. If none of
;                            them produce a legal move (king is unsafe in all positions) then
;                            it is a checkmate. This function gets called when the king is 
;                            detected to be in check. 
;                            TODO: find a way to iterate through all moves

; checkmate test ;
; iterate through all black attacks and check
;(define (isWhiteCheckMate WK WQ WR WB WN WP BK BQ BR BB BN BP allPieces)
;  ())
;;;;;;;;;;;;;;;;;;



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


(define (startMove worldState newX newY)
  (make-worldState (hideSelectedPiece worldState newX newY)
                   (worldState-chessboard worldState)
                   (worldState-bitboards worldState)
                   (newCurrentMove worldState newX newY)
                   (worldState-quit worldState)))

(define (hideSelectedPiece worldState newX newY)
  (if (equal? 0 (modulo (+ (floor (/ newY SQUARE_SIDE)) (floor (/ newX SQUARE_SIDE))) 2))
      (place-image LIGHT_SQUARE (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newX SQUARE_SIDE)))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newY SQUARE_SIDE)))) (worldState-image worldState))
      (place-image DARK_SQUARE (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newX SQUARE_SIDE)))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newY SQUARE_SIDE)))) (worldState-image worldState))))

(define (newCurrentMove worldState newX newY)
  (local
    ((define piece
       (chessboardGet (worldState-chessboard worldState) (floor (/ newY SQUARE_SIDE)) (floor (/ newX SQUARE_SIDE)))))
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
     (make-currentMove empty-image " " WHITE (make-posn newX newY) (make-posn newX newY))])))

(define (changeMove worldState newX newY)
  (make-worldState (worldState-image worldState)
                   (worldState-chessboard worldState)
                   (worldState-bitboards worldState)
                   (make-currentMove (currentMove-image (worldState-currentMove worldState))
                                     (currentMove-type (worldState-currentMove worldState))
                                     (currentMove-color (worldState-currentMove worldState))
                                     (currentMove-start (worldState-currentMove worldState))
                                     (make-posn newX newY))
                   (worldState-quit worldState)))







(define (makeMove worldState)
  (local
    ((define chessboard
       (worldState-chessboard worldState))
     (define bitboards
       (worldState-bitboards worldState))
     (define type
       (currentMove-type (worldState-currentMove worldState)))
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
     (define capturedPieceType
       (chessboardGet chessboard (floor (/ (posn-y (currentMove-end (worldState-currentMove worldState))) SQUARE_SIDE)) (floor (/ (posn-x (currentMove-end (worldState-currentMove worldState))) SQUARE_SIDE))))
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
     (if (currentMove? (worldState-currentMove worldState))
        (cond
          [(equal? 1 (arithmetic-shift (bitwise-and (getMovesPiece type color startPositionBitboard startPositionIndex allPieces whitePieces blackPieces)
                                                    endPositionBitboard)
                                       (- endPositionIndex 63)))
           (make-worldState (drawPieces (bitboardsToChessboard (updateBitboards bitboards type startPositionBitboard endPositionBitboard capturedPieceType)))
                            (bitboardsToChessboard (updateBitboards bitboards type startPositionBitboard endPositionBitboard capturedPieceType))
                            (updateBitboards bitboards type startPositionBitboard endPositionBitboard capturedPieceType)
                            #false
                            (worldState-quit worldState))]
          [else
           (make-worldState (drawPieces (worldState-chessboard worldState))
                            (worldState-chessboard worldState)
                            (worldState-bitboards worldState)
                            #false
                            (worldState-quit worldState))])
        
        (make-worldState (worldState-image worldState)
                         (worldState-chessboard worldState)
                         (worldState-bitboards worldState)
                         (worldState-currentMove worldState)
                         (worldState-quit worldState)))))

(define (updateBitboards bitboards type startPositionBitboard endPositionBitboard capturedPieceType)
  (if (equal? " " capturedPieceType)
      (updatePieceAtPosition (updatePieceAtPosition bitboards type startPositionBitboard) type endPositionBitboard)
      (updatePieceAtPosition (updatePieceAtPosition (updatePieceAtPosition bitboards type startPositionBitboard) type endPositionBitboard) capturedPieceType endPositionBitboard)))

(define (updatePieceAtPosition bitboards type positionBitboard)
  (dict-set bitboards type (bitwise-xor (dict-ref bitboards type) positionBitboard)))



(define (getMovesPiece type color positionBitboard positionIndex allPieces whitePieces blackPieces)
  (cond
    [(or (equal? "K" type) (equal? "k" type))
             (kingMoves color whitePieces blackPieces positionBitboard)]
    [(or (equal? "Q" type) (equal? "q" type))
             (queenMoves color allPieces whitePieces blackPieces positionBitboard positionIndex)]
    [(or (equal? "R" type) (equal? "r" type))
             (rookMoves color allPieces whitePieces blackPieces positionBitboard positionIndex)]
    [(or (equal? "B" type) (equal? "b" type))
             (bishopMoves color allPieces whitePieces blackPieces positionBitboard positionIndex)]
    [(or (equal? "N" type) (equal? "n" type))
             (knightMoves color whitePieces blackPieces positionBitboard)]
    [(or (equal? "P" type) (equal? "p" type))
             (pawnMoves color allPieces whitePieces blackPieces positionBitboard)]
    [else 0]))







; TEST SECTION
(define testState (make-worldState
                        (drawPieces EMPTY_CHESSBOARD)
                        EMPTY_CHESSBOARD
                        BITBOARDS
                        #false
                        #false))









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
    [(string=? "button-up" mouse-event) (makeMove worldState)]
    [else worldState]))





(define initialState (make-worldState
                        (drawPieces STANDARD_CHESSBOARD)
                        STANDARD_CHESSBOARD
                        (chessboardToBitboards STANDARD_CHESSBOARD)
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