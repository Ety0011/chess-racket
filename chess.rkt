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


; ==========
; Data Types
; ==========

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

; CurrentMove is a Structure (make-currentMove startPieceIcon color piece start end)
; - icon:  Image
; - color: Color
; - piece: Piece
; - start: Posn
; - end:   Posn
; A Posn represents 2 coordinate Numbers
; An startPieceIcon is an image of Piece
(define-struct currentMove [startPieceIcon piece start end])

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

; PreviousMove is a Structure (make-previousMove endPiece endPositionIndex)
; - endPiece:         Piece
; - endPositionIndex: Number
(define-struct previousMove [startPieceColor endPiece endPositionIndex])

; CastleRights is a Structure (make-castleRights castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleBlackQueenSide)
; - castleWhiteKingSide:  Boolean
; - castleWhiteQueenSide: Boolean
; - castleBlackKingSide:  Boolean
; - castleBlackQueenSide: Boolean
(define-struct castleRights [castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleBlackQueenSide])

; History is a Structure (make-history castle enPassant promotion previousEndPosition)
; - castle: ???
; - enPassant: ???
; - previousEndPosition: Posn
(define-struct history [castleRights enPassant promotion previousMove])

; WorldState is a Structure (make-worldState chessboard matrix bitboards currentMove history quit)
; - chessboard:  Image
; - matrix:      Matrix
; - bitboards:   Dictionary<Bitboard>
; - currentMove: CurrentMove
; - history:     History
; - quit:        Boolean
(define-struct worldState [chessboard matrix bitboards currentMove history quit])


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
(define WK_ICON (scale (/ SQUARE_SIDE 162) (bitmap "img/WK.png")))
(define WQ_ICON (scale (/ SQUARE_SIDE 162) (bitmap "img/WQ.png")))
(define WR_ICON (scale (/ SQUARE_SIDE 162) (bitmap "img/WR.png")))
(define WB_ICON (scale (/ SQUARE_SIDE 162) (bitmap "img/WB.png")))
(define WN_ICON (scale (/ SQUARE_SIDE 162) (bitmap "img/WN.png")))
(define WP_ICON (scale (/ SQUARE_SIDE 162) (bitmap "img/WP.png")))
(define BK_ICON (scale (/ SQUARE_SIDE 162) (bitmap "img/BK.png")))
(define BQ_ICON (scale (/ SQUARE_SIDE 162) (bitmap "img/BQ.png")))
(define BR_ICON (scale (/ SQUARE_SIDE 162) (bitmap "img/BR.png")))
(define BB_ICON (scale (/ SQUARE_SIDE 162) (bitmap "img/BB.png")))
(define BN_ICON (scale (/ SQUARE_SIDE 162) (bitmap "img/BN.png")))
(define BP_ICON (scale (/ SQUARE_SIDE 162) (bitmap "img/BP.png")))


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
   (vector "r" "n" " " " " " " "r" "k" " ")
   (vector "p" "b" "p" "p" "q" " " "p" "p")
   (vector " " "p" " " " " " " "b" " " " ")
   (vector " " " " " " " " "N" " " " " "Q")
   (vector " " " " " " "P" " " " " " " " ")
   (vector " " " " " " "B" " " " " " " " ")
   (vector "P" "P" "P" " " " " "P" "P" "P")
   (vector "R" " " " " " " "K" " " " " "R")))





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
; Header: (drawPieces matrix) STANDARD_CHESSBOARD

; Examples
(define STANDARD_CHESSBOARD
  (above (beside (overlay BR_ICON LIGHT_SQUARE) (overlay BN_ICON DARK_SQUARE)  (overlay BB_ICON LIGHT_SQUARE) (overlay BQ_ICON DARK_SQUARE)  (overlay BK_ICON LIGHT_SQUARE) (overlay BB_ICON DARK_SQUARE)  (overlay BN_ICON LIGHT_SQUARE) (overlay BR_ICON DARK_SQUARE))
         (beside (overlay BP_ICON DARK_SQUARE)  (overlay BP_ICON LIGHT_SQUARE) (overlay BP_ICON DARK_SQUARE)  (overlay BP_ICON LIGHT_SQUARE) (overlay BP_ICON DARK_SQUARE)  (overlay BP_ICON LIGHT_SQUARE) (overlay BP_ICON DARK_SQUARE)  (overlay BP_ICON LIGHT_SQUARE))
         (beside LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE)
         (beside DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE)
         (beside LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE)
         (beside DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE DARK_SQUARE  LIGHT_SQUARE)
         (beside (overlay WP_ICON LIGHT_SQUARE) (overlay WP_ICON DARK_SQUARE)  (overlay WP_ICON LIGHT_SQUARE) (overlay WP_ICON DARK_SQUARE)  (overlay WP_ICON LIGHT_SQUARE) (overlay WP_ICON DARK_SQUARE)  (overlay WP_ICON LIGHT_SQUARE) (overlay WP_ICON DARK_SQUARE))
         (beside (overlay WR_ICON DARK_SQUARE)  (overlay WN_ICON LIGHT_SQUARE) (overlay WB_ICON DARK_SQUARE)  (overlay WQ_ICON LIGHT_SQUARE) (overlay WK_ICON DARK_SQUARE)  (overlay WB_ICON LIGHT_SQUARE) (overlay WN_ICON DARK_SQUARE)  (overlay WR_ICON LIGHT_SQUARE))))

; Checks
(check-expect (drawPieces STANDARD_MATRIX) STANDARD_CHESSBOARD)

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
              (drawPiece WK_ICON matrixIndex)]
             [(equal? "Q" (getPiece matrix matrixIndex))
              (drawPiece WQ_ICON matrixIndex)]
             [(equal? "R" (getPiece matrix matrixIndex))
              (drawPiece WR_ICON matrixIndex)]
             [(equal? "B" (getPiece matrix matrixIndex))
              (drawPiece WB_ICON matrixIndex)]
             [(equal? "N" (getPiece matrix matrixIndex))
              (drawPiece WN_ICON matrixIndex)]
             [(equal? "P" (getPiece matrix matrixIndex))
              (drawPiece WP_ICON matrixIndex)]
             [(equal? "k" (getPiece matrix matrixIndex))
              (drawPiece BK_ICON matrixIndex)]
             [(equal? "r" (getPiece matrix matrixIndex))
              (drawPiece BR_ICON matrixIndex)]
             [(equal? "q" (getPiece matrix matrixIndex))
              (drawPiece BQ_ICON matrixIndex)]
             [(equal? "b" (getPiece matrix matrixIndex))
              (drawPiece BB_ICON matrixIndex)]
             [(equal? "n" (getPiece matrix matrixIndex))
              (drawPiece BN_ICON matrixIndex)]
             [(equal? "p" (getPiece matrix matrixIndex))
              (drawPiece BP_ICON matrixIndex)]
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
    ((define possibleMoves
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
        (bitwise-and possibleMoves (bitwise-not (bitwise-and possibleMoves whitePieces)) 18446744073709551615)
        (bitwise-and possibleMoves (bitwise-not (bitwise-and possibleMoves blackPieces)) 18446744073709551615))))


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
     (define allMoves
       (bitwise-ior (bitwise-and horizontalMoves (vector-ref RANKMASKS (floor (/ positionIndex 8)))) (bitwise-and verticalMoves (vector-ref FILEMASKS (modulo positionIndex 8))))))
     (if (equal? #true color)
         (bitwise-and allMoves (bitwise-not (bitwise-and allMoves whitePieces)) 18446744073709551615)
         (bitwise-and allMoves (bitwise-not (bitwise-and allMoves blackPieces)) 18446744073709551615))))

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
     (define possibleMoves
       (bitwise-ior (bitwise-and DiagonalMoves (vector-ref DIAGONALMASKS (+ (floor (/ positionIndex 8)) (modulo positionIndex 8)))) (bitwise-and AntiDiagonalMoves (vector-ref ANTIDIAGONALMASKS (+ (floor (/ positionIndex 8)) (- 7 (modulo positionIndex 8))))))))
    (if (equal? #true color)
        (bitwise-and possibleMoves (bitwise-not (bitwise-and possibleMoves whitePieces)) 18446744073709551615)
        (bitwise-and possibleMoves (bitwise-not (bitwise-and possibleMoves blackPieces)) 18446744073709551615))))


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
    ((define possibleMoves
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
        (bitwise-and possibleMoves (bitwise-not (bitwise-and possibleMoves whitePieces)) 18446744073709551615)
        (bitwise-and possibleMoves (bitwise-not (bitwise-and possibleMoves blackPieces)) 18446744073709551615))))


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
    ((define possibleWhiteMoves
       (bitwise-ior
        (bitwise-and (arithmetic-shift positionBitboard 9) NOT_FILE_H blackPieces)
        (bitwise-and (arithmetic-shift positionBitboard 7) NOT_FILE_A blackPieces)
        (bitwise-and (arithmetic-shift positionBitboard 8) (bitwise-not allPieces))
        (bitwise-and (arithmetic-shift positionBitboard 16) (arithmetic-shift (bitwise-not allPieces) 8) (bitwise-not allPieces) RANK_4)))
     (define possibleBlackMoves
       (bitwise-ior
        (bitwise-and (arithmetic-shift positionBitboard -9) NOT_FILE_A whitePieces)
        (bitwise-and (arithmetic-shift positionBitboard -7) NOT_FILE_H whitePieces)
        (bitwise-and (arithmetic-shift positionBitboard -8) (bitwise-not allPieces))
        (bitwise-and (arithmetic-shift positionBitboard -16) (arithmetic-shift (bitwise-not allPieces) -8) (bitwise-not allPieces) RANK_5))))
    (if (equal? #true color)
        (bitwise-and possibleWhiteMoves (bitwise-not (bitwise-and possibleWhiteMoves whitePieces)) 18446744073709551615)
        (bitwise-and possibleBlackMoves (bitwise-not (bitwise-and possibleBlackMoves blackPieces)) 18446744073709551615))))



;=============================================================================================



(define (pawnAttacks color positionBitboard)
  (local
    ((define whiteAttacks
       (bitwise-ior
        (bitwise-and (arithmetic-shift positionBitboard 9) NOT_FILE_H NOT_RANK_8)
        (bitwise-and (arithmetic-shift positionBitboard 7) NOT_FILE_A NOT_RANK_8)))
     (define blackAttacks
       (bitwise-ior
        (bitwise-and (arithmetic-shift positionBitboard -9) NOT_FILE_A NOT_RANK_1)
        (bitwise-and (arithmetic-shift positionBitboard -7) NOT_FILE_H NOT_RANK_1))))
    (if (equal? #true color)
        (bitwise-and whiteAttacks 18446744073709551615)
        (bitwise-and blackAttacks 18446744073709551615))))



(define (numberOfTrailingZeros bb no-zeroes)
  (if (equal? 1 (bitwise-and bb (arithmetic-shift 1 no-zeroes))) no-zeroes
      (numberOfTrailingZeros bb (add1 no-zeroes))))


;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;

;Signature 
;draw: WorldState-> worldState
;Interpretation: it draws the current pieces on the chessboard with the newly moved pieces
;(define (draw worldState) (place-image (currentMove-startPieceIcon)))

;(define (draw worldState)
;  (place-image (currentMove-startPieceIcon (worldState-currentMove ...))
;               (posn-x (currentMove-end (worldState-currentMove ...)))
;               (posn-y (currentMove-end (worldState-currentMove ...)))
;               (worldState-chessboard ...)))

(define (draw worldState)
  (place-image (currentMove-startPieceIcon (worldState-currentMove worldState))
               (posn-x (currentMove-end (worldState-currentMove worldState)))
               (posn-y (currentMove-end (worldState-currentMove worldState)))
               (worldState-chessboard worldState)))

;Signature 
;hidePieceStartPosition: WorldState NewX NewY -> WorldState 
;hidePieceStartPosition is a function (hidePieceStartPosition worldState newX newY) where:
;- worldState: structure
;- newX:       number   
;- newY:       number 
;Interpretation: the function updates the chessboard with the starting position of the piece hidden.
;(define (hidePieceStartPosition worldState newX newY) worldState)

;(define (hidePieceStartPosition worldState newX newY)
;  (if (equal? 0 (modulo (+ (floor (/ newY ...)) (floor (/ newX ...))) 2))
;      (place-image LIGHT_SQUARE (+ (/ ... 2) (* ... (floor (/ newX ...)))) (+ (/ ... 2) (* ... (floor (/ newY ...)))) (worldState-chessboard ...))
;      (place-image DARK_SQUARE  (+ (/ ... 2) (* ... (floor (/ newX ...)))) (+ (/ ... 2) (* ... (floor (/ newY ...)))) (worldState-chessboard ...))))

(define (hidePieceStartPosition worldState newX newY)
  (if (equal? 0 (modulo (+ (floor (/ newY SQUARE_SIDE)) (floor (/ newX SQUARE_SIDE))) 2))
      (place-image LIGHT_SQUARE (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newX SQUARE_SIDE)))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newY SQUARE_SIDE)))) (worldState-chessboard worldState))
      (place-image DARK_SQUARE  (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newX SQUARE_SIDE)))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newY SQUARE_SIDE)))) (worldState-chessboard worldState))))


;Signature
;newMove: worldState newX newY -> worldState
;newMove is a structure (newMove worldState newX newY) where: 
;- worldState: structure
;- newX:       number
;- newY:       number 
;Interpretation: returns the worldstate once the button down function from the mouse handler is called and the piece has been placed on the new coordinates 
;(define (newMove worldState newX newY)(make-worldState (worldState-chessboard)))

;(define (newMove worldState newX newY)
;  (if (equal? #true (history-promotion (worldState-history worldState)))
;      (make-worldState (worldState-chessboard ...)
;                       (worldState-matrix ...)
;                       (worldState-bitboards ...)
;                       (make-currentMove ...)
;                       (worldState-history ...)
;                       (worldState-quit ...))
;      (make-worldState (hidePieceStartPosition ...)
;                       (worldState-matrix ...)
;                       (worldState-bitboards ...)
;                       (newCurrentMove ...)
;                       (worldState-history ...)
;                       (worldState-quit ...))))

                       
(define (newMove worldState newX newY)
  (if (equal? #true (history-promotion (worldState-history worldState)))
      (make-worldState (worldState-chessboard worldState)
                       (worldState-matrix worldState)
                       (worldState-bitboards worldState)
                       (make-currentMove empty-image " " (make-posn newX newY) (make-posn newX newY))
                       (worldState-history worldState)
                       (worldState-quit worldState))
      (make-worldState (hidePieceStartPosition worldState newX newY)
                       (worldState-matrix worldState)
                       (worldState-bitboards worldState)
                       (newCurrentMove worldState newX newY)
                       (worldState-history worldState)
                       (worldState-quit worldState))))


;newCurrentMove: WorldState NewX NewY -> WorldState
;newCurrentMove is a structure (newCurrentMove worldState newX newY) where: 
;- worldState: worldState
;- newX:       number
;- newY:       number
;interpretation: finds the selected piece and generates a currentMove with the defined parameters
;(define (newCurrentMove worldState newX newY)(make-currentMove))

;(define (newCurrentMove worldState newX newY)
;  (local
;    ((define piece
;       (matrixGet (worldState-matrix ...) (floor (/ newY ...)) (floor (/ newX ...)))))
;  (cond
;    [(equal? ...)
;     (make-currentMove ... )]
;    [(equal? ...) 
;     (make-currentMove ... )]
;    [(equal? ...) 
;     (make-currentMove ... )]
;    [(equal? ...) 
;     (make-currentMove ... )]
;    [(equal? ...) 
;     (make-currentMove ... )]
;    [(equal? ...) 
;     (make-currentMove ... )]
;    [(equal? ...) 
;     (make-currentMove ... )]
;    [(equal? ...) 
;     (make-currentMove ... )]
;    [(equal? ...) 
;     (make-currentMove ... )]
;    [(equal? ...) 
;     (make-currentMove ... )]
;    [(equal? ...) 
;     (make-currentMove ... )]
;    [(equal? ...)
;     (make-currentMove ...)]
;    [(equal? ...)
;     (make-currentMove ...)])))


(define (newCurrentMove worldState newX newY)
  (local
    ((define piece
       (matrixGet (worldState-matrix worldState) (floor (/ newY SQUARE_SIDE)) (floor (/ newX SQUARE_SIDE)))))
  (cond
    [(equal? "K" piece)
     (make-currentMove WK_ICON "K" (make-posn newX newY) (make-posn newX newY))]
    [(equal? "Q" piece)
     (make-currentMove WQ_ICON "Q" (make-posn newX newY) (make-posn newX newY))]
    [(equal? "R" piece)
     (make-currentMove WR_ICON "R" (make-posn newX newY) (make-posn newX newY))]
    [(equal? "B" piece)
     (make-currentMove WB_ICON "B" (make-posn newX newY) (make-posn newX newY))]
    [(equal? "N" piece)
     (make-currentMove WN_ICON "N" (make-posn newX newY) (make-posn newX newY))]
    [(equal? "P" piece)
     (make-currentMove WP_ICON "P" (make-posn newX newY) (make-posn newX newY))]
    [(equal? "k" piece)
     (make-currentMove BK_ICON "k" (make-posn newX newY) (make-posn newX newY))]
    [(equal? "q" piece)
     (make-currentMove BQ_ICON "q" (make-posn newX newY) (make-posn newX newY))]
    [(equal? "r" piece)
     (make-currentMove BR_ICON "r" (make-posn newX newY) (make-posn newX newY))]
    [(equal? "b" piece)
     (make-currentMove BB_ICON "b" (make-posn newX newY) (make-posn newX newY))]
    [(equal? "n" piece)
     (make-currentMove BN_ICON "n" (make-posn newX newY) (make-posn newX newY))]
    [(equal? "p" piece)
     (make-currentMove BP_ICON "p" (make-posn newX newY) (make-posn newX newY))]
    [(equal? " " piece)
     (make-currentMove empty-image " " (make-posn 0 0) (make-posn 0 0))])))


;changeMove: WorldState NewX NewY -> WorldState
;changeMove is a structure (changeMove worldState newX newY) where:
;- worldState: worldState
;- newX:       number
;- newY:       number
; Interpretation: when moving the mouse it draws the selected piece until the user lets go and updates its coordinates 
;(define (changeMove worldState newX newY) (make-worldState chessboard))

;(define (changeMove worldState newX newY)
;  (make-worldState (worldState-chessboard ...)
;                   (worldState-matrix ...)
;                   (worldState-bitboards ...)
;                   (make-currentMove (currentMove-startPieceIcon (worldState-currentMove ...))
;                                     (currentMove-piece (worldState-currentMove ...))
;                                     (currentMove-start (worldState-currentMove ...))
;                                     (make-posn ...))
;                   (worldState-history ...)
;                   (worldState-quit ...)))


(define (changeMove worldState newX newY)
  (make-worldState (worldState-chessboard worldState)
                   (worldState-matrix worldState)
                   (worldState-bitboards worldState)
                   (make-currentMove (currentMove-startPieceIcon (worldState-currentMove worldState))
                                     (currentMove-piece (worldState-currentMove worldState))
                                     (currentMove-start (worldState-currentMove worldState))
                                     (make-posn newX newY))
                   (worldState-history worldState)
                   (worldState-quit worldState)))


;makeMove: WorldState -> WorldState
;returns the image of the chessboard after with the move executed where it 
;(define (makeMove worldState) worldState)

(define (makeMove worldState)
  (local
    ((define matrix
       (worldState-matrix worldState))
     (define bitboards
       (worldState-bitboards worldState))
     (define startPiece
       (currentMove-piece (worldState-currentMove worldState)))
     (define startPieceColor
       (getColor startPiece))
     (define startPositionIndex
       (+ (floor (/ (posn-x (currentMove-start (worldState-currentMove worldState))) SQUARE_SIDE)) (* 8 (floor (/ (posn-y (currentMove-start (worldState-currentMove worldState))) SQUARE_SIDE)))))
     (define startPositionBitboard
       (arithmetic-shift 1 (- 63 startPositionIndex)))
     (define endPositionIndex
       (+ (floor (/ (posn-x (currentMove-end (worldState-currentMove worldState))) SQUARE_SIDE)) (* 8 (floor (/ (posn-y (currentMove-end (worldState-currentMove worldState))) SQUARE_SIDE)))))
     (define endPositionBitboard
       (arithmetic-shift 1 (- 63 endPositionIndex)))
     (define endPiece
       (matrixGet matrix (floor (/ (posn-y (currentMove-end (worldState-currentMove worldState))) SQUARE_SIDE)) (floor (/ (posn-x (currentMove-end (worldState-currentMove worldState))) SQUARE_SIDE))))
     (define previousStartPieceColor
       (previousMove-startPieceColor (history-previousMove (worldState-history worldState))))
     (define previousEndPiece
       (previousMove-endPiece (history-previousMove (worldState-history worldState))))
     (define previousEndPositionIndex
       (previousMove-endPositionIndex (history-previousMove (worldState-history worldState))))
     (define allPieces
       (allPiecesBitboard bitboards))
     (define whitePieces
       (whitePiecesBitboard bitboards))
     (define blackPieces
       (blackPiecesBitboard bitboards))
     (define castleRights
       (history-castleRights (worldState-history worldState)))
     (define castleWhiteKingSide
       (castleRights-castleWhiteKingSide (history-castleRights (worldState-history worldState))))
     (define castleWhiteQueenSide
       (castleRights-castleWhiteQueenSide (history-castleRights (worldState-history worldState))))
     (define castleBlackKingSide
       (castleRights-castleBlackKingSide (history-castleRights (worldState-history worldState))))
     (define castleBlackQueenSide
       (castleRights-castleBlackQueenSide (history-castleRights (worldState-history worldState))))
     (define enPassant
       (history-enPassant (worldState-history worldState))))

     
    (cond
      [(equal? #true (history-promotion (worldState-history worldState)))
       (makePromotion worldState previousStartPieceColor bitboards endPiece endPositionIndex previousEndPiece previousEndPositionIndex)]
      [(preparePromotion? startPiece startPieceColor startPositionBitboard startPositionIndex allPieces whitePieces blackPieces endPositionBitboard endPositionIndex)
       (preparePromotion worldState startPieceColor bitboards startPositionBitboard endPiece endPositionIndex)]


      [(makeCastle? matrix bitboards (not startPieceColor) startPiece endPositionIndex allPieces whitePieces blackPieces castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleWhiteQueenSide)
       (makeCastle worldState bitboards startPiece startPieceColor endPiece endPositionIndex castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleWhiteQueenSide)]

      [(makeEnPassant? startPiece startPositionIndex endPositionIndex previousStartPieceColor previousEndPositionIndex enPassant)
       (makeEnPassant worldState bitboards startPiece startPieceColor startPositionIndex endPiece endPositionIndex previousEndPositionIndex)]



       
               
      [(equal? 1 (arithmetic-shift (bitwise-and (getMovesPiece startPiece startPieceColor startPositionBitboard startPositionIndex allPieces whitePieces blackPieces) endPositionBitboard) (- endPositionIndex 63)))
       (makeRegularMove worldState startPieceColor bitboards startPiece startPositionBitboard startPositionIndex endPiece endPositionBitboard endPositionIndex castleRights castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleWhiteQueenSide)]
      [else
       (moveNotValid worldState)])))

;========================================================================================================================================================

(define (makeEnPassant worldState bitboards startPiece startPieceColor startPositionIndex endPiece endPositionIndex previousEndPositionIndex)
  (local
    ((define (newBitboard capture)
       (cond
         [(and (equal? "P" startPiece) (equal? "right" capture))
          (updatePieceAtPosition
            (updatePieceAtPosition bitboards startPiece (bitwise-ior (arithmetic-shift 1 (- 63 startPositionIndex)) (arithmetic-shift 1 (- 63 endPositionIndex))))
            "p"
            (arithmetic-shift 1 (- 63 (+ startPositionIndex 1))))]
         [(and (equal? "P" startPiece) (equal? "left" capture))
          (updatePieceAtPosition
            (updatePieceAtPosition bitboards startPiece (bitwise-ior (arithmetic-shift 1 (- 63 startPositionIndex)) (arithmetic-shift 1 (- 63 endPositionIndex))))
            "p"
            (arithmetic-shift 1 (- 63 (- startPositionIndex 1))))]
         [(and (equal? "p" startPiece) (equal? "right" capture))
          (updatePieceAtPosition
            (updatePieceAtPosition bitboards startPiece (bitwise-ior (arithmetic-shift 1 (- 63 startPositionIndex)) (arithmetic-shift 1 (- 63 endPositionIndex))))
            "P"
            (arithmetic-shift 1 (- 63 (+ startPositionIndex 1))))]
         [(and (equal? "p" startPiece) (equal? "left" capture))
          (updatePieceAtPosition
            (updatePieceAtPosition bitboards startPiece (bitwise-ior (arithmetic-shift 1 (- 63 startPositionIndex)) (arithmetic-shift 1 (- 63 endPositionIndex))))
            "P"
            (arithmetic-shift 1 (- 63 (- startPositionIndex 1))))]))
     (define (newWorldState newBitboard)
       (make-worldState (drawPieces (bitboardsToMatrix newBitboard))
                        (bitboardsToMatrix newBitboard)
                        newBitboard
                        (make-currentMove empty-image " " (make-posn 0 0) (make-posn 0 0))
                        (make-history (history-castleRights (worldState-history worldState))
                                      #false
                                      #false
                                      (make-previousMove startPieceColor
                                                         endPiece
                                                         endPositionIndex))
                        (worldState-quit worldState))))
    (cond
      [(and (equal? "P" startPiece) (equal? endPositionIndex (- startPositionIndex 7)))
       (newWorldState (newBitboard "right"))]
      [(and (equal? "P" startPiece) (equal? endPositionIndex (- startPositionIndex 9)))
       (newWorldState (newBitboard "left"))]
      [(and (equal? "p" startPiece) (equal? endPositionIndex (+ startPositionIndex 9)))
       (newWorldState (newBitboard "right"))]
      [(and (equal? "p" startPiece) (equal? endPositionIndex (+ startPositionIndex 7)))
       (newWorldState (newBitboard "left"))])))

;makeCastle: WorldState Bitboards StartPiece EndPositionIndex CastleWhiteKingSide CastleWhiteQueenSide CastleBlackKingSide CastleBlackQueenSide -> Bitboard
;generates a new bitboard with the king and rook after they have been castled 
;(define (makeCastle worldState bitboards startPiece endPositionIndex castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleBlackQueenSide) bitboard)
  



























(define (makeEnPassant? startPiece startPositionIndex endPositionIndex previousStartPieceColor previousEndPositionIndex enPassant)
  (local
    ((define enPassantWhiteRight
       (and (equal? "P" startPiece)
            (equal? BLACK previousStartPieceColor)
            (equal? (- previousEndPositionIndex 1) startPositionIndex)
            (equal? (- previousEndPositionIndex 8) endPositionIndex)
            (equal? 3 (floor (/ startPositionIndex 8)))
            (not (equal? 7 (modulo startPositionIndex 8)))
            enPassant))
     (define enPassantWhiteLeft
       (and (equal? "P" startPiece)
            (equal? BLACK previousStartPieceColor)
            (equal? (+ previousEndPositionIndex 1) startPositionIndex)
            (equal? (- previousEndPositionIndex 8) endPositionIndex)
            (equal? 3 (floor (/ startPositionIndex 8)))
            (not (equal? 0 (modulo startPositionIndex 8)))
            enPassant))
     (define enPassantBlackRight
       (and (equal? "p" startPiece)
            (equal? WHITE previousStartPieceColor)
            (equal? (- previousEndPositionIndex 1) startPositionIndex)
            (equal? (+ previousEndPositionIndex 8) endPositionIndex)
            (equal? 4 (floor (/ startPositionIndex 8)))
            (not (equal? 7 (modulo startPositionIndex 8)))
            enPassant))
     (define enPassantBlackLeft
       (and (equal? "p" startPiece)
            (equal? WHITE previousStartPieceColor)
            (equal? (+ previousEndPositionIndex 1) startPositionIndex)
            (equal? (+ previousEndPositionIndex 8) endPositionIndex)
            (equal? 4 (floor (/ startPositionIndex 8)))
            (not (equal? 0 (modulo startPositionIndex 8)))
            enPassant)))
    (or enPassantWhiteRight enPassantWhiteLeft enPassantBlackRight enPassantBlackLeft)))


(define (makeCastle worldState bitboards startPiece startPieceColor endPiece endPositionIndex castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleBlackQueenSide)
  (local
    ((define (newBitboard side)
       (cond
         [(and (equal? "K" startPiece) (equal? "kingSide" side))
          (updatePieceAtPosition
            (updatePieceAtPosition bitboards startPiece (bitwise-ior (arithmetic-shift 1 (- 63 60)) (arithmetic-shift 1 (- 63 62))))
            "R"
            (bitwise-ior (arithmetic-shift 1 (- 63 63)) (arithmetic-shift 1 (- 63 61))))]
         [(and (equal? "K" startPiece) (equal? "queenSide" side))
          (updatePieceAtPosition
            (updatePieceAtPosition bitboards startPiece (bitwise-ior (arithmetic-shift 1 (- 63 60)) (arithmetic-shift 1 (- 63 58))))
            "R"
            (bitwise-ior (arithmetic-shift 1 (- 63 56)) (arithmetic-shift 1 (- 63 59))))]
         [(and (equal? "k" startPiece) (equal? "kingSide" side))
          (updatePieceAtPosition
            (updatePieceAtPosition bitboards startPiece (bitwise-ior (arithmetic-shift 1 (- 63 4)) (arithmetic-shift 1 (- 63 6))))
            "r"
            (bitwise-ior (arithmetic-shift 1 (- 63 7)) (arithmetic-shift 1 (- 63 5))))]
         [(and (equal? "k" startPiece) (equal? "queenSide" side))
          (updatePieceAtPosition
            (updatePieceAtPosition bitboards startPiece (bitwise-ior (arithmetic-shift 1 (- 63 4)) (arithmetic-shift 1 (- 63 2))))
            "r"
            (bitwise-ior (arithmetic-shift 1 (- 63 0)) (arithmetic-shift 1 (- 63 3))))]))
     (define newCastleRights
       (if (equal? "K" startPiece)
           (make-castleRights #false
                              #false
                              castleBlackKingSide
                              castleBlackQueenSide)
           (make-castleRights castleWhiteKingSide
                              castleWhiteQueenSide
                              #false
                              #false)))
     (define (newWorldState newBitboard)
       (make-worldState (drawPieces (bitboardsToMatrix newBitboard))
                        (bitboardsToMatrix newBitboard)
                        newBitboard
                        (make-currentMove empty-image " " (make-posn 0 0) (make-posn 0 0))
                        (make-history newCastleRights
                                      #false
                                      #false
                                      (make-previousMove startPieceColor
                                                         endPiece
                                                         endPositionIndex))
                        (worldState-quit worldState))))
    (cond
      [(and (equal? "K" startPiece) (equal? 62 endPositionIndex))
       (newWorldState (newBitboard "kingSide"))]
      [(and (equal? "K" startPiece) (equal? 58 endPositionIndex))
       (newWorldState (newBitboard "queenSide"))]
      [(and (equal? "k" startPiece) (equal? 6 endPositionIndex))
       (newWorldState (newBitboard "kingSide"))]
      [(and (equal? "k" startPiece) (equal? 2 endPositionIndex))
       (newWorldState (newBitboard "queenSide"))])))



;makeCastle? Matrix Bitboards Color StartPiece EndPositionIndex AllPieces WhitePieces BlackPieces CastleWhiteKingSide CastleWhiteQueenSide CastleBlackKingSide CastleBlackQueenSide -> Boolean
;determinates if a castle is possible based on the placement of all the pieces on the chessboard  
;(define (makeCastle? matrix bitboards color startPiece endPositionIndex allPieces whitePieces blackPieces castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleBlackQueenSide) #t)

(define (makeCastle? matrix bitboards color startPiece endPositionIndex allPieces whitePieces blackPieces castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleBlackQueenSide)
  (local
    ((define (positionClear? positionIndex)
       (equal? 0 (bitwise-and 1 (arithmetic-shift allPieces (- positionIndex 63)))))
     (define (positionSafe? positionIndex)
       (equal? 0 (bitwise-and 1 (arithmetic-shift (allAttacks matrix color allPieces whitePieces blackPieces) (- positionIndex 63)))))
     (define makeCastleWhiteKingSide?
       (and (equal? "K" startPiece)
            (equal? 62 endPositionIndex)
            (positionClear? 61) (positionSafe? 61)
            (positionClear? 62) (positionSafe? 62)
            castleWhiteKingSide))
     (define makeCastleWhiteQueenSide?
       (and (equal? "K" startPiece)
            (equal? 58 endPositionIndex)
            (positionClear? 59) (positionSafe? 59)
            (positionClear? 58) (positionSafe? 58)
            (positionClear? 57)
            castleWhiteQueenSide))
     (define makeCastleBlackKingSide?
       (and (equal? "k" startPiece)
            (equal? 6 endPositionIndex)
            (positionClear? 5) (positionSafe? 5)
            (positionClear? 6) (positionSafe? 6)
            castleBlackKingSide))
     (define makeCastleBlackQueenSide?
       (and (equal? "k" startPiece)
            (equal? 2 endPositionIndex)
            (positionClear? 3) (positionSafe? 3)
            (positionClear? 2) (positionSafe? 2)
            (positionClear? 1)
            castleWhiteQueenSide)))
    (or makeCastleWhiteKingSide? makeCastleWhiteQueenSide? makeCastleBlackKingSide? makeCastleBlackQueenSide?)))


; allAttacks: Matrix Color AllPieces WhitePieces BlackPieces -> Bitboard
; evaluates all the possible attacks inside the chessboard for both the white and black pieces  
; (define (allAttacks matrix color allPieces whitePieces blackPieces) bitboard)
(define (allAttacks matrix color allPieces whitePieces blackPieces)
  (local
    ((define (getPiece positionIndex)
       (matrixGet matrix (floor (/ positionIndex 8)) (modulo positionIndex 8)))
     (define (allAttacksAcc bitboard positionIndex)
       (cond
         [(equal? 64 positionIndex) bitboard]
         [(or (equal? (not color) (getColor (getPiece positionIndex))) (equal? " " (getPiece positionIndex)))
          (allAttacksAcc bitboard (add1 positionIndex))]
         [else
          (allAttacksAcc (bitwise-ior bitboard
                                      (getAttacksPiece (getPiece positionIndex)
                                                       color
                                                       (arithmetic-shift 1 (- 63  positionIndex))
                                                       positionIndex
                                                       allPieces
                                                       whitePieces
                                                       blackPieces))
                         (add1 positionIndex))])))
    (allAttacksAcc 0 0)))

; getAttacksPiece: Piece StartPieceColor PositionBitboard PositionIndex AllPieces WhitePieces BlackPieces -> Bitboard
; acquires the attacks bitboard of the piece based on the relative position in the Bitboard
; (define (getAttacksPiece piece startPieceColor positionBitboard positionIndex allPieces whitePieces blackPieces) Bitboard) 
(define (getAttacksPiece piece startPieceColor positionBitboard positionIndex allPieces whitePieces blackPieces)
  (cond
    [(or (equal? "K" piece) (equal? "k" piece))
             (kingMoves startPieceColor whitePieces blackPieces positionBitboard)]
    [(or (equal? "Q" piece) (equal? "q" piece))
             (queenMoves startPieceColor allPieces whitePieces blackPieces positionBitboard positionIndex)]
    [(or (equal? "R" piece) (equal? "r" piece))
             (rookMoves startPieceColor allPieces whitePieces blackPieces positionBitboard positionIndex)]
    [(or (equal? "B" piece) (equal? "b" piece))
             (bishopMoves startPieceColor allPieces whitePieces blackPieces positionBitboard positionIndex)]
    [(or (equal? "N" piece) (equal? "n" piece))
             (knightMoves startPieceColor whitePieces blackPieces positionBitboard)]
    [(or (equal? "P" piece) (equal? "p" piece))
             (pawnAttacks color positionBitboard)]
    [else 0]))



(define (getColor piece)
  (if (or (equal? "K" piece) (equal? "Q" piece) (equal? "R" piece) (equal? "B" piece) (equal? "N" piece) (equal? "P" piece))
            WHITE
            BLACK))



(define (allPiecesBitboard bitboards)
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

(define (whitePiecesBitboard bitboards)
  (bitwise-xor
   (dict-ref bitboards "K")
   (dict-ref bitboards "Q")
   (dict-ref bitboards "R")
   (dict-ref bitboards "B")
   (dict-ref bitboards "N")
   (dict-ref bitboards "P")))

(define (blackPiecesBitboard bitboards)
  (bitwise-xor
   (dict-ref bitboards "k")
   (dict-ref bitboards "q")
   (dict-ref bitboards "r")
   (dict-ref bitboards "b")
   (dict-ref bitboards "n")
   (dict-ref bitboards "p")))

(define (preparePromotion? startPiece startPieceColor startPositionBitboard startPositionIndex allPieces whitePieces blackPieces endPositionBitboard endPositionIndex)
  (local
    ((define prepareWhitePromotion?
       (and (equal? "P" startPiece)
            (equal? 1 (arithmetic-shift (bitwise-and (getMovesPiece startPiece startPieceColor startPositionBitboard startPositionIndex allPieces whitePieces blackPieces) endPositionBitboard) (- endPositionIndex 63)))
            (equal? 1 (floor (/ startPositionIndex 8)))
            (equal? 0 (floor (/ endPositionIndex 8)))))
     (define prepareBlackPromotion?
       (and (equal? "p" startPiece)
            (equal? 1 (arithmetic-shift (bitwise-and (getMovesPiece startPiece startPieceColor startPositionBitboard startPositionIndex allPieces whitePieces blackPieces) endPositionBitboard) (- endPositionIndex 63)))
            (equal? 6 (floor (/ startPositionIndex 8)))
            (equal? 7 (floor (/ endPositionIndex 8))))))
    (or prepareWhitePromotion? prepareBlackPromotion?)))

(define (preparePromotion worldState startPieceColor bitboards startPositionBitboard endPiece endPositionIndex)
  (local
    ((define prepareWhitePromotion
       (make-worldState (drawPromotionMenu (drawPieces (bitboardsToMatrix (updatePieceAtPosition bitboards "P" startPositionBitboard))) WHITE endPositionIndex)
                        (bitboardsToMatrix (updatePieceAtPosition bitboards "P" startPositionBitboard))
                        (updatePieceAtPosition bitboards "P" startPositionBitboard)
                        (make-currentMove empty-image " " (make-posn 0 0) (make-posn 0 0))
                        (make-history (history-castleRights (worldState-history worldState))
                                      #false
                                      #true
                                      (make-previousMove startPieceColor
                                                         endPiece
                                                         endPositionIndex))
                        (worldState-quit worldState)))
     (define prepareBlackPromotion
       (make-worldState (drawPromotionMenu (drawPieces (bitboardsToMatrix (updatePieceAtPosition bitboards "p" startPositionBitboard))) BLACK endPositionIndex)
                        (bitboardsToMatrix (updatePieceAtPosition bitboards "p" startPositionBitboard))
                        (updatePieceAtPosition bitboards "p" startPositionBitboard)
                        (make-currentMove empty-image " " (make-posn 0 0) (make-posn 0 0))
                        (make-history (history-castleRights (worldState-history worldState))
                                   #false
                                   #true
                                   (make-previousMove startPieceColor
                                                      endPiece
                                                      endPositionIndex))
                        (worldState-quit worldState))))
    (if (equal? #true startPieceColor)
        prepareWhitePromotion
        prepareBlackPromotion)))

; makePromotion: WorldState PreviousStartPieceColor Bitboards EndPiece EndPositionIndex PreviousEndPiece PreviousEndPositionIndex -> Piece
; promotes the pawn to the new selected piece from the promotion menu and place the new piece to where the pawn lied on the chessboard updating its moveset
;(define (makePromotion worldState previousStartPieceColor bitboards endPiece endPositionIndex previousEndPiece previousEndPositionIndex) (newWorldState (newBitboard "Q")))

(define (makePromotion worldState previousStartPieceColor bitboards endPiece endPositionIndex previousEndPiece previousEndPositionIndex)
  (local
    ((define (newBitboard promotedPiece)
       (if (equal? " " previousEndPiece)
           (updatePieceAtPosition bitboards promotedPiece (arithmetic-shift 1 (- 63 previousEndPositionIndex)))
           (updatePieceAtPosition
            (updatePieceAtPosition bitboards promotedPiece (arithmetic-shift 1 (- 63 previousEndPositionIndex)))
            previousEndPiece
            (arithmetic-shift 1 (- 63 previousEndPositionIndex)))))
     (define (newWorldState newBitboard)
       (make-worldState (drawPieces (bitboardsToMatrix newBitboard))
                        (bitboardsToMatrix newBitboard)
                        newBitboard
                        (make-currentMove empty-image " " (make-posn 0 0) (make-posn 0 0))
                        (make-history (history-castleRights (worldState-history worldState))
                                      #false
                                      #false
                                      (history-previousMove (worldState-history worldState)))
                        (worldState-quit worldState))))
    (cond
      [(and (equal? WHITE previousStartPieceColor) (equal? endPositionIndex previousEndPositionIndex))
       (newWorldState (newBitboard "Q"))]
      [(and (equal? WHITE previousStartPieceColor) (equal? endPositionIndex (+ previousEndPositionIndex 8)))
       (newWorldState (newBitboard "R"))]
      [(and (equal? WHITE previousStartPieceColor) (equal? endPositionIndex (+ previousEndPositionIndex 16)))
       (newWorldState (newBitboard "B"))]
      [(and (equal? WHITE previousStartPieceColor) (equal? endPositionIndex (+ previousEndPositionIndex 24)))
       (newWorldState (newBitboard "N"))]
      [(and (equal? BLACK previousStartPieceColor) (equal? endPositionIndex previousEndPositionIndex))
       (newWorldState (newBitboard "q"))]
      [(and (equal? BLACK previousStartPieceColor) (equal? endPositionIndex (- previousEndPositionIndex 8)))
       (newWorldState (newBitboard "r"))]
      [(and (equal? BLACK previousStartPieceColor) (equal? endPositionIndex (- previousEndPositionIndex 16)))
       (newWorldState (newBitboard "b"))]
      [(and (equal? BLACK previousStartPieceColor) (equal? endPositionIndex (- previousEndPositionIndex 24)))
       (newWorldState (newBitboard "n"))]
      [else
       (make-worldState (drawPromotionMenu (drawPieces (worldState-matrix worldState)) WHITE previousEndPositionIndex)
                        (worldState-matrix worldState)
                        (worldState-bitboards worldState)
                        (make-currentMove empty-image " " (make-posn 0 0) (make-posn 0 0))
                        (worldState-history worldState)
                        (worldState-quit worldState))])))

; drawPromotionMenu: Chessboard StartPieceColor PositionIndex -> Image
; draws a promotion selection menu ontop of the chessboard and the pawn once a pawn has make it to the corresponding last rank  
;(define (drawPromotionMenu chessboard startPieceColor positionIndex) place-image whitePromotionMenu)


(define (drawPromotionMenu chessboard startPieceColor positionIndex)
  (local
    ((define whitePromotionMenu
       (above (overlay WQ_ICON WHITE_SQUARE) (overlay WR_ICON WHITE_SQUARE)(overlay WB_ICON WHITE_SQUARE)(overlay WN_ICON WHITE_SQUARE)))
     (define blackPromotionMenu
       (above (overlay BN_ICON WHITE_SQUARE) (overlay BB_ICON WHITE_SQUARE)(overlay BR_ICON WHITE_SQUARE)(overlay BQ_ICON WHITE_SQUARE))))
    (if (equal? #true startPieceColor)
        (place-image whitePromotionMenu (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (modulo positionIndex 8))) (* SQUARE_SIDE 2) chessboard)
        (place-image blackPromotionMenu (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (modulo positionIndex 8))) (* SQUARE_SIDE 6) chessboard))))


(define (makeRegularMove worldState startPieceColor bitboards startPiece startPositionBitboard startPositionIndex endPiece endPositionBitboard endPositionIndex castleRights castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleBlackQueenSide)
  (local
    ((define newBitboard
       (if (equal? " " endPiece)
           (updatePieceAtPosition bitboards startPiece (bitwise-ior startPositionBitboard endPositionBitboard))
           (updatePieceAtPosition
            (updatePieceAtPosition bitboards startPiece (bitwise-ior startPositionBitboard endPositionBitboard))
            endPiece endPositionBitboard)))
     (define newCastleRights
       (cond
         [(equal? "K" startPiece)
          (make-castleRights #false
                             #false
                             castleBlackKingSide
                             castleBlackQueenSide)]
         [(and (equal? "R" startPiece) (equal? 63 startPositionIndex))
          (make-castleRights #false
                             castleWhiteQueenSide
                             castleBlackKingSide
                             castleBlackQueenSide)]
         [(and (equal? "R" startPiece) (equal? 56 startPositionIndex))
          (make-castleRights castleWhiteKingSide
                             #false
                             castleBlackKingSide
                             castleBlackQueenSide)]

         [(equal? "k" startPiece)
          (make-castleRights castleWhiteKingSide
                             castleWhiteQueenSide
                             #false
                             #false)]
         [(and (equal? "r" startPiece) (equal? 7 startPositionIndex))
          (make-castleRights castleWhiteKingSide
                             castleWhiteQueenSide
                             #false
                             castleBlackQueenSide)]
         [(and (equal? "r" startPiece) (equal? 0 startPositionIndex))
          (make-castleRights castleWhiteKingSide
                             castleWhiteQueenSide
                             castleBlackKingSide
                             #false)]
         [else
          castleRights]))
     (define newEnPassant
       (if (or (and (equal? "P" startPiece) (equal? 6 (floor (/ startPositionIndex 8))) (equal? 4 (floor (/ endPositionIndex 8))))
               (and (equal? "p" startPiece) (equal? 1 (floor (/ startPositionIndex 8))) (equal? 3 (floor (/ endPositionIndex 8)))))
           #true
           #false)))
    (make-worldState (drawPieces (bitboardsToMatrix newBitboard))
                     (bitboardsToMatrix newBitboard)
                     newBitboard
                     (make-currentMove empty-image " " (make-posn 0 0) (make-posn 0 0))
                     (make-history newCastleRights
                                   newEnPassant
                                   (history-promotion (worldState-history worldState))
                                   (make-previousMove startPieceColor
                                                      endPiece
                                                      endPositionIndex))
                     (worldState-quit worldState))))

; getMovesPiece: Piece StartPieceColor PositionBitboard PositionIndex AllPieces WhitePieces BlackPieces -> Bitboard
; gets the moveset and bitboard of the selected pieces from its position on the chessboard
; (define (getMovesPiece piece startPieceColor positionBitboard positionIndex allPieces whitePieces blackPieces) (kingMoves startPieceColor whitePieces blackPieces positionBitboard))
(define (getMovesPiece piece startPieceColor positionBitboard positionIndex allPieces whitePieces blackPieces)
  (cond
    [(or (equal? "K" piece) (equal? "k" piece))
             (kingMoves startPieceColor whitePieces blackPieces positionBitboard)]
    [(or (equal? "Q" piece) (equal? "q" piece))
             (queenMoves startPieceColor allPieces whitePieces blackPieces positionBitboard positionIndex)]
    [(or (equal? "R" piece) (equal? "r" piece))
             (rookMoves startPieceColor allPieces whitePieces blackPieces positionBitboard positionIndex)]
    [(or (equal? "B" piece) (equal? "b" piece))
             (bishopMoves startPieceColor allPieces whitePieces blackPieces positionBitboard positionIndex)]
    [(or (equal? "N" piece) (equal? "n" piece))
             (knightMoves startPieceColor whitePieces blackPieces positionBitboard)]
    [(or (equal? "P" piece) (equal? "p" piece))
             (pawnMoves startPieceColor allPieces whitePieces blackPieces positionBitboard)]
    [else 0]))


; moveNotValid: WorldState -> WorldState
; evaluates wether or not a move is valid once a piece is selected and its moveset is evaluated  
; (define (moveNotValid worldState) (drawPieces))

(define (moveNotValid worldState)
  (make-worldState (drawPieces (worldState-matrix worldState))
                   (worldState-matrix worldState)
                   (worldState-bitboards worldState)
                   (make-currentMove empty-image " " (make-posn 0 0) (make-posn 0 0))
                   (worldState-history worldState)
                   (worldState-quit worldState)))


; updatePieceAtPosition: Bitboards Piece PositionBitboard -> Bitboards
; updates the bitboards piece after is has been moved 
; (define (updatePieceAtPosition bitboards piece positionBitboard) bitboards)

(define (updatePieceAtPosition bitboards piece positionBitboard)
  (dict-set bitboards piece (bitwise-xor (dict-ref bitboards piece) positionBitboard)))


; quit: WorldState -> WorldState
; returns an worldState that records the information that the application has quit with the 'quit' parameter set to #true
; (define (quit worldState) (make-worldState chessboard #f #t))

(define (quit worldState)
  (make-worldState (worldState-chessboard worldState)
                   (worldState-matrix worldState)
                   (worldState-bitboards worldState)
                   (worldState-currentMove worldState)
                   (worldState-history worldState)
                   #true))



; quit?: WorldState -> Boolean
; returns a Boolean indicating whether the app has quit or not.
; (define (quit? appstate) #f)

(define (quit? worldState)
  (if (equal? #t (worldState-quit worldState))
      #t
      #f))


; handle-key: WorldState KeyEvent -> worldState
; handles the following key event and updates the worldState accordingly
; - "q": set the world state to quit
; (define (handle-key worldState key-event) worldState)

(define (handle-key worldState key-event)
  (cond
    [(string=? "q" key-event) (quit worldState)]
    [(string=? "r" key-event) initialState]
    [else worldState]))


; handle-mouse: worldState Number Number String -> worldState
; handles the following mouse events and updates the worldState accordingly
; - "button-down": selects the piece 
; - "drag": move the current piece end point
; - "button-up": add the current piece to the chessboard
; (define (handle-mouse worldState x-mouse y-mouse mouse-event) worldState)

(define (handle-mouse worldState x-mouse y-mouse mouse-event)
  (cond
    [(string=? "button-down" mouse-event) (newMove worldState x-mouse y-mouse)]
    [(and (string=? "drag" mouse-event)
          (currentMove? (worldState-currentMove worldState))) (changeMove worldState x-mouse y-mouse)]
    [(string=? "button-up" mouse-event) (makeMove worldState)]
    [else worldState]))

;================================================================================================================================================================
;MAIN FUNCTION
;================================================================================================================================================================

(define initialState (make-worldState
                        (drawPieces STANDARD_MATRIX)
                        STANDARD_MATRIX
                        (matrixToBitboards STANDARD_MATRIX)
                        (make-currentMove empty-image " " (make-posn 0 0) (make-posn 0 0))
                        (make-history (make-castleRights #true
                                                         #true
                                                         #true
                                                         #true)
                                      #false
                                      #false
                                      (make-previousMove BLACK
                                                         " "
                                                         0))
                        #false))

(define (drawing-app initialState)
  (big-bang initialState
    [to-draw draw]
    [on-mouse handle-mouse]
    [on-key handle-key]
    [stop-when quit?]
    [close-on-stop #true]))

(drawing-app initialState)