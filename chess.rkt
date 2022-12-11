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
(define-struct currentMove [piece start end])

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

; WorldState is a Structure (make-worldState chessboard matrix bitboards turn currentMove history quit)
; - chessboard:  Image
; - matrix:      Matrix
; - bitboards:   Dictionary<Bitboard>
; - turn:        Color
; - currentMove: CurrentMove
; - history:     History
; - quit:        Boolean
(define-struct worldState [chessboard matrix bitboards turn kingChecks legalMovesUnderCheck icon currentMove history quit])


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
   (vector "p" "b" "p" "p" " " " " "p" "p")
   (vector " " "p" " " " " " " " " " " " ")
   (vector " " " " " " " " " " " " " " "Q")
   (vector " " " " " " "P" " " " " " " " ")
   (vector " " " " " " "B" " " " " " " " ")
   (vector "P" "P" "P" "p" " " "P" "P" "P")
   (vector "R" " " " " " " "K" " " " " "R")))

(define TEST_MATRIX2
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

(define (putPieceBitboard positionIndex)
  (display (~r (arithmetic-shift 1 (- 63 positionIndex)) #:base 2 #:min-width 64 #:pad-string "0")))

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

;; TEST MATRICES
(define BITBOARDS_OF_TEST_MATRIX (matrixToBitboards TEST_MATRIX))

(define TEST_WHITEPIECES
  (bitwise-xor
   (dict-ref BITBOARDS_OF_TEST_MATRIX "K")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "Q")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "R")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "B")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "N")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "P")))

(define TEST_BLACKPIECES
  (bitwise-xor
   (dict-ref BITBOARDS_OF_TEST_MATRIX "k")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "q")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "r")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "b")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "n")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "p")))

(define TEST_ALLPIECES
  (bitwise-xor
   (dict-ref BITBOARDS_OF_TEST_MATRIX "K")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "Q")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "R")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "B")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "N")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "P")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "k")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "q")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "r")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "b")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "n")
   (dict-ref BITBOARDS_OF_TEST_MATRIX "p")))

; Signature
; kingMoves: Boolean Bitboard Bitboard Bitboard -> Bitboard
; Interpreation: starting from "positionBitboard", which is a bitboard with the position of the king written as a 1, another bitboard is
; generated where every 1 represents all the possible positions a king can move to. Depending on the color of the king, the result is then
; ANDed with another bitboard full of 1s except for the positions occupied by the other pieces of the same color. In this way we avoid pieces
; of the same color capturing each other. Notes: the result is ANDed with 18446744073709551615 to avoid 1s over the 64th bit.
; Header: (define (kingMoves color whitePieces blackPieces positionBitboard) 0)

; Examples
(define WKPOSITION
  #b0000000000000000000000000000000000000000000000000000000000001000)
(define WK_EXPECTED_MOVES
  #b0000000000000000000000000000000000000000000000000001100000010100)

(define BKPOSITION
  #b0000001000000000000000000000000000000000000000000000000000000000)
(define BK_EXPECTED_MOVES
  #b0000000100000010000000000000000000000000000000000000000000000000)

; Checks
;(check-expect (kingMoves WHITE TEST_WHITEPIECES TEST_BLACKPIECES WKPOSITION) WK_EXPECTED_MOVES)
;(check-expect (kingMoves BLACK TEST_WHITEPIECES TEST_BLACKPIECES BKPOSITION) BK_EXPECTED_MOVES)

; Implementation
(define (kingMoves positionBitboard)
  (bitwise-and (bitwise-ior (bitwise-and (arithmetic-shift positionBitboard  1) NOT_FILE_H)
                            (bitwise-and (arithmetic-shift positionBitboard  9) NOT_FILE_H)
                            (arithmetic-shift positionBitboard  8)
                            (bitwise-and (arithmetic-shift positionBitboard  7) NOT_FILE_A)
                            (bitwise-and (arithmetic-shift positionBitboard -1) NOT_FILE_A)
                            (bitwise-and (arithmetic-shift positionBitboard -9) NOT_FILE_A)
                            (arithmetic-shift positionBitboard -8)
                            (bitwise-and (arithmetic-shift positionBitboard -7) NOT_FILE_H))
               18446744073709551615))


; Signature
; rookMoves: Boolean Bitboard Bitboard Bitboard Bitboard Number -> Bitboard
; Interpretation: starting from "positionBitboard", which is a bitboard with the position of the rook written as a 1, another bitboard is
; generated where every 1 represents all the possible positions a rook can move to. Depending on the color of the rook, the result is then
; ANDed with another bitboard full of 1s except for the positions occupied by the other pieces of the same color. In this way we avoid pieces
; of the same color capturing each other.
; Header: (define (rookMoves color allPieces whitePieces blackPieces positionBitboard positionIndex) 0)

; Examples
(define WRPOSITION
  #b0000000000000000000000000000000000000000000000000000000000000001)
(define WRPOSITIONINDEX
  0)
(define WR_EXPECTED_MOVES
  #b0000000000000000000000000000000000000000000000000000000000000110)

(define BRPOSITION
  #b1000000000000000000000000000000000000000000000000000000000000000)
(define BRPOSITIONINDEX
  63)
(define BR_EXPECTED_MOVES
  #b0000000000000000000000000000000000000000000000000000000000000000)

; Checks
;(check-expect (rookMoves WHITE TEST_ALLPIECES TEST_WHITEPIECES TEST_BLACKPIECES WRPOSITION WRPOSITIONINDEX) WR_EXPECTED_MOVES)
;(check-expect (rookMoves BLACK TEST_ALLPIECES TEST_WHITEPIECES TEST_BLACKPIECES BRPOSITION BRPOSITIONINDEX) BR_EXPECTED_MOVES)

; Implementation
(define (rookMoves allPieces positionBitboard positionIndex)
  (local
    ((define horizontalMoves
       (bitwise-xor (- allPieces (* 2 positionBitboard))
                    (reverseBinary (- (reverseBinary allPieces) (* 2 (reverseBinary positionBitboard))))))
     (define verticalMoves
       (bitwise-xor (- (bitwise-and allPieces (vector-ref FILEMASKS (modulo positionIndex 8))) (* 2 positionBitboard))
                    (reverseBinary (- (reverseBinary (bitwise-and allPieces (vector-ref FILEMASKS (modulo positionIndex 8)))) (* 2 (reverseBinary positionBitboard)))))))
    (bitwise-and (bitwise-ior (bitwise-and horizontalMoves (vector-ref RANKMASKS (floor (/ positionIndex 8))))
                              (bitwise-and verticalMoves (vector-ref FILEMASKS (modulo positionIndex 8))))
                 18446744073709551615)))

; Examples
(define WBPOSITION
  #b0000000000000000000000000000000000000000000000000000000000000000)
(define WBPOSITIONINDEX
  61)
(define WB_EXPECTED_MOVES
  #b0000000000000000000000000000000000000000000000000000000000000000)

(define BBPOSITION
  #b0000000000000000000000000000000000000000000000000000000000000000)
(define BBPOSITIONINDEX
  2)
(define BB_EXPECTED_MOVES
  #b0000000000000000000000000000000000000000000000000000000000000000)

; Checks
;(check-expect (bishopMoves WHITE TEST_ALLPIECES TEST_WHITEPIECES TEST_BLACKPIECES WBPOSITION WBPOSITIONINDEX) WB_EXPECTED_MOVES)
;(check-expect (bishopMoves BLACK TEST_ALLPIECES TEST_WHITEPIECES TEST_BLACKPIECES BBPOSITION BBPOSITIONINDEX) BB_EXPECTED_MOVES)

; Signature
; bishopMoves Boolean Bitboard Bitboard Bitboard Bitboard Number -> Bitboard
; Interpretation: starting from "positionBitboard", which is a bitboard with the position of the bishop written as a 1, another bitboard is
; generated where every 1 represents all the possible positions a bishop can move to. Depending on the color of the bishop, the result is then
; ANDed with another bitboard full of 1s except for the positions occupied by the other pieces of the same color. In this way we avoid pieces
; of the same color capturing each other.
; Header: (define (bishopMoves color allPieces whitePieces blackPieces positionBitboard positionIndex) 0)

; Implementation
(define (bishopMoves allPieces positionBitboard positionIndex)
  (local
    ((define DiagonalMoves
       (bitwise-xor (- (bitwise-and allPieces (vector-ref DIAGONALMASKS (+ (floor (/ positionIndex 8)) (modulo positionIndex 8)))) (* 2 positionBitboard))
                    (reverseBinary (- (reverseBinary (bitwise-and allPieces (vector-ref DIAGONALMASKS (+ (floor (/ positionIndex 8)) (modulo positionIndex 8))))) (* 2 (reverseBinary positionBitboard))))))
     (define AntiDiagonalMoves
       (bitwise-xor (- (bitwise-and allPieces (vector-ref ANTIDIAGONALMASKS (+ (floor (/ positionIndex 8)) (- 7 (modulo positionIndex 8))))) (* 2 positionBitboard))
                    (reverseBinary (- (reverseBinary (bitwise-and allPieces (vector-ref ANTIDIAGONALMASKS (+ (floor (/ positionIndex 8)) (- 7 (modulo positionIndex 8)))))) (* 2 (reverseBinary positionBitboard)))))))
       (bitwise-and (bitwise-ior (bitwise-and DiagonalMoves (vector-ref DIAGONALMASKS (+ (floor (/ positionIndex 8)) (modulo positionIndex 8))))
                                 (bitwise-and AntiDiagonalMoves (vector-ref ANTIDIAGONALMASKS (+ (floor (/ positionIndex 8)) (- 7 (modulo positionIndex 8))))))
                    18446744073709551615)))

; Signature
; queenMoves Boolean Bitboard Bitboard Bitboard Bitboard Number -> Bitboard
; Interpretation: starting from "positionBitboard", which is a bitboard with the position of the queen written as a 1, another bitboard is
; generated where every 1 represents all the possible positions a queen can move to. Depending on the color of the queen, the result is then
; ANDed with another bitboard full of 1s except for the positions occupied by the other pieces of the same color. In this way we avoid pieces
; of the same color capturing each other.
; Header: (define (queenMoves color allPieces whitePieces blackPieces positionBitboard positionIndex) 0)

; Examples
(define WQPOSITION
  #b0000000000000000000000000000000100000000000000000000000000000000)
(define WQPOSITIONINDEX
  32)
(define WQ_EXPECTED_MOVES
  #b0000000000000000000000000000000000000001000000010000000000000000)

(define BQPOSITION
  #b0000000000000000000000000000000000000000000000000000000000000000)
(define BQPOSITIONINDEX
  3)
(define BQ_EXPECTED_MOVES
  #b0000000000000000000000000000000000000000000000000000000000000000)

; Checks
;(check-expect (queenMoves WHITE TEST_ALLPIECES TEST_WHITEPIECES TEST_BLACKPIECES WQPOSITION WQPOSITIONINDEX) WQ_EXPECTED_MOVES)
;(check-expect (queenMoves BLACK TEST_ALLPIECES TEST_WHITEPIECES TEST_BLACKPIECES BQPOSITION BQPOSITIONINDEX) BQ_EXPECTED_MOVES)

; Implementation
(define (queenMoves allPieces positionBitboard positionIndex)
  (bitwise-ior (rookMoves allPieces positionBitboard positionIndex) (bishopMoves allPieces positionBitboard positionIndex)))


; Signature
; knightMoves Boolean Bitboard Bitboard Bitboard Number -> Bitboard
; Interpretation: starting from "positionBitboard", which is a bitboard with the position of the knight written as a 1, another bitboard is
; generated where every 1 represents all the possible positions a knight can move to. Depending on the color of the knight, the result is then
; ANDed with another bitboard full of 1s except for the positions occupied by the other pieces of the same color. In this way we avoid pieces
; of the same color capturing each other.
; Header: (define (knightMoves color allPieces whitePieces blackPieces positionBitboard positionIndex) #b0000000000000000000000000000000000000000000001010000000000000000)

; Examples
(define WNPOSITION
  #b0000000000000000000000000000000000000000000000000000000000000010)
(define WN_EXPECTED_MOVES
  #b0000000000000000000000000000000000000000000001010000000000000000)

(define BNPOSITION
  #b0100000000000000000000000000000000000000000000000000000000000000)
(define BN_EXPECTED_MOVES
  #b0000000000000000101000000000000000000000000000000000000000000000)

; Checks
;(check-expect (knightMoves WHITE TEST_WHITEPIECES TEST_BLACKPIECES WNPOSITION) WN_EXPECTED_MOVES)
;(check-expect (knightMoves BLACK TEST_WHITEPIECES TEST_BLACKPIECES BNPOSITION) BN_EXPECTED_MOVES)

; Implementation
(define (knightMoves positionBitboard)
  (bitwise-and (bitwise-ior (bitwise-and (arithmetic-shift positionBitboard  10) NOT_FILE_GH)
                            (bitwise-and (arithmetic-shift positionBitboard  17) NOT_FILE_H)
                            (bitwise-and (arithmetic-shift positionBitboard  15) NOT_FILE_A)
                            (bitwise-and (arithmetic-shift positionBitboard  6)  NOT_FILE_AB)
                            (bitwise-and (arithmetic-shift positionBitboard -10) NOT_FILE_AB)
                            (bitwise-and (arithmetic-shift positionBitboard -17) NOT_FILE_A)
                            (bitwise-and (arithmetic-shift positionBitboard -15) NOT_FILE_H)
                            (bitwise-and (arithmetic-shift positionBitboard -6)  NOT_FILE_GH))
               18446744073709551615))


; Signature
; pawnMoves Boolean Bitboard Bitboard Bitboard Number -> Bitboard
; Interpretation: starting from "positionBitboard", which is a bitboard with the position of the pawn written as a 1, another bitboard is
; generated where every 1 represents all the possible positions a pawn can move to. Depending on the color of the pawn, the result is then
; ANDed with another bitboard full of 1s except for the positions occupied by the other pieces of the same color. In this way we avoid pieces
; of the same color capturing each other.
; Header: (define (pawnMoves color whitePieces blackPieces positionBitboard) 0)

; Examples
(define WPPOSITION
  #b0000000000000000000000000000000000000000000000000000000000000000)
(define WP_EXPECTED_MOVES
  #b0000000000000000000000000000000000001000000010000000000000000000)

(define BPPOSITION
  #b0000000000000000000000000000000000000000000000000000000000000000)
(define BP_EXPECTED_MOVES
  #b0000000000000000000100000001000000000000000000000000000000000000)

; Checks
;(check-expect (pawnMoves WHITE ALLPIECES WHITEPIECES BLACKPIECES WPPOSITION1) WPMOVES1)
;(check-expect (pawnMoves WHITE ALLPIECES WHITEPIECES BLACKPIECES WPPOSITION2) WPMOVES2)

;(check-expect (pawnMoves WHITE TEST_ALLPIECES TEST_WHITEPIECES TEST_BLACKPIECES WPPOSITION) WP_EXPECTED_MOVES)
;(check-expect (pawnMoves BLACK TEST_ALLPIECES TEST_WHITEPIECES TEST_BLACKPIECES BPPOSITION) BP_EXPECTED_MOVES)

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
  (place-image (worldState-icon worldState)
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

(define (hidePieceStartPosition chessboard newX newY)
  (if (equal? 0 (modulo (+ (floor (/ newY SQUARE_SIDE)) (floor (/ newX SQUARE_SIDE))) 2))
      (place-image LIGHT_SQUARE (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newX SQUARE_SIDE)))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newY SQUARE_SIDE)))) chessboard)
      (place-image DARK_SQUARE  (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newX SQUARE_SIDE)))) (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (floor (/ newY SQUARE_SIDE)))) chessboard)))


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
  (local
    ((define piece
       (matrixGet (worldState-matrix worldState) (floor (/ newY SQUARE_SIDE)) (floor (/ newX SQUARE_SIDE))))

     (define (newWorldStateParameters icon piece)
       (make-worldState (hidePieceStartPosition (worldState-chessboard worldState) newX newY)
                        (worldState-matrix worldState)
                        (worldState-bitboards worldState)
                        (worldState-turn worldState)
                        (worldState-kingChecks worldState)
                        (worldState-legalMovesUnderCheck worldState)
                        icon
                        (make-currentMove piece (make-posn newX newY) (make-posn newX newY))
                        (worldState-history worldState)
                        (worldState-quit worldState)))
     (define (newWorldState piece)
       (cond
         [(equal? "K" piece)
          (newWorldStateParameters WK_ICON "K")]
         [(equal? "Q" piece)
          (newWorldStateParameters WQ_ICON "Q")]
         [(equal? "R" piece)
          (newWorldStateParameters WR_ICON "R")]
         [(equal? "B" piece)
          (newWorldStateParameters WB_ICON "B")]
         [(equal? "N" piece)
          (newWorldStateParameters WN_ICON "N")]
         [(equal? "P" piece)
          (newWorldStateParameters WP_ICON "P")]
         [(equal? "k" piece)
          (newWorldStateParameters BK_ICON "k")]
         [(equal? "q" piece)
          (newWorldStateParameters BQ_ICON "q")]
         [(equal? "r" piece)
          (newWorldStateParameters BR_ICON "r")]
         [(equal? "b" piece)
          (newWorldStateParameters BB_ICON "b")]
         [(equal? "n" piece)
          (newWorldStateParameters BN_ICON "n")]
         [(equal? "p" piece)
          (newWorldStateParameters BP_ICON "p")]
         [(equal? " " piece)
          (newWorldStateParameters empty-image " ")])))
    (cond
      [(and (equal? (worldState-turn worldState) (getColor piece)) (equal? #false (history-promotion (worldState-history worldState))))
       (newWorldState piece)]
       [else
        (make-worldState (worldState-chessboard worldState)
                         (worldState-matrix worldState)
                         (worldState-bitboards worldState)
                         (worldState-turn worldState)
                         (worldState-kingChecks worldState)
                         (worldState-legalMovesUnderCheck worldState)
                         empty-image
                         (make-currentMove " " (make-posn newX newY) (make-posn newX newY))
                         (worldState-history worldState)
                         (worldState-quit worldState))])))


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
                   (worldState-turn worldState)
                   (worldState-kingChecks worldState)
                   (worldState-legalMovesUnderCheck worldState)
                   (worldState-icon worldState)
                   (make-currentMove (currentMove-piece (worldState-currentMove worldState))
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
       (history-enPassant (worldState-history worldState)))
     (define unsafeForKing
       (allAttacks matrix startPieceColor allPieces whitePieces blackPieces))
     (define kingChecks
       (newKingChecks matrix bitboards startPieceColor allPieces whitePieces blackPieces)))

     
    (cond
      [(equal? #true (history-promotion (worldState-history worldState)))
       (makePromotion worldState previousStartPieceColor bitboards endPiece endPositionIndex previousEndPiece previousEndPositionIndex)]
      [(preparePromotion? matrix startPiece startPieceColor startPositionBitboard startPositionIndex allPieces whitePieces blackPieces endPositionBitboard endPositionIndex)
       (preparePromotion worldState startPieceColor bitboards startPositionBitboard endPiece endPositionIndex)]


      [(makeCastle? matrix bitboards (not startPieceColor) startPiece endPositionIndex allPieces whitePieces blackPieces castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleWhiteQueenSide)
       (makeCastle worldState bitboards startPiece startPieceColor endPiece endPositionIndex castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleWhiteQueenSide)]

      [(makeEnPassant? startPiece startPositionIndex endPositionIndex previousStartPieceColor previousEndPositionIndex enPassant)
       (makeEnPassant worldState bitboards startPiece startPieceColor startPositionIndex endPiece endPositionIndex previousEndPositionIndex)]
      
      ; enPassantWhiteRight
      ;[(and (equal? "P" startPiece)
       ;     (equal? BLACK previousStartPieceColor)
        ;    (equal? (- previousEndPositionIndex 1) startPositionIndex)
         ;   (equal? (- previousEndPositionIndex 8) endPositionIndex)
          ;  (equal? 3 (floor (/ startPositionIndex 8)))
           ; (not (equal? 7 (modulo startPositionIndex 8)))
            ;enPassant)



       
               
      [(equal? 1 (arithmetic-shift (bitwise-and (getMovesPiece matrix startPiece startPieceColor startPositionBitboard startPositionIndex allPieces whitePieces blackPieces) endPositionBitboard) (- endPositionIndex 63)))
       (makeRegularMove worldState matrix bitboards startPiece startPieceColor startPositionBitboard startPositionIndex endPiece endPositionBitboard endPositionIndex allPieces whitePieces blackPieces castleRights castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleWhiteQueenSide kingChecks)]
      [else
       (moveNotLegal worldState)])))

;========================================================================================================================================================




(define (newKingChecks matrix bitboards color allPieces whitePieces blackPieces)
  (local
    ((define (getPiece positionIndex)
       (matrixGet matrix (floor (/ positionIndex 8)) (modulo positionIndex 8)))
     (define kingPiece
       (if (equal? color WHITE)
           "K"
           "k"))
     (define (newKingChecksAcc bitboards positionIndex totalSum)
       (cond
         [(equal? 64 positionIndex) totalSum]
         [(or (equal? (getColor (getPiece positionIndex)) color) (equal? (getPiece positionIndex) " " ))
          (newKingChecksAcc bitboards (add1 positionIndex) totalSum)]
         [(equal? 1 (bitwise-and 1 (arithmetic-shift (bitwise-and (dict-ref bitboards kingPiece)
                                                                  (getAttacksPiece (getPiece positionIndex)
                                                                                   (not color)
                                                                                   (arithmetic-shift 1 (- 63 positionIndex))
                                                                                   positionIndex
                                                                                   allPieces
                                                                                   whitePieces
                                                                                   blackPieces))
                                                     (- (bitboardToPositionIndex (dict-ref bitboards kingPiece)) 63))))
          (newKingChecksAcc bitboards (add1 positionIndex) (add1 totalSum))]
         [else
          (newKingChecksAcc bitboards (add1 positionIndex) totalSum)])))
    (newKingChecksAcc bitboards 0 0)))


(define (bitboardToPositionIndex bitboard)
  (local
    ((define (bitboardToPositionIndexAcc bitboard zerosNumber)
       (if (equal? 1 (bitwise-and 1 (arithmetic-shift bitboard (- zerosNumber))))
           zerosNumber
           (bitboardToPositionIndexAcc bitboard (add1 zerosNumber)))))
    (- 63 (bitboardToPositionIndexAcc bitboard 0))))




(define (getAttackRay matrix color startPositionBitboard startPositionIndex endPiece endPositionBitboard endPositionIndex allPieces whitePieces blackPieces)
  (local
    ((define rookPiece
       (if (equal? color WHITE)
           "R"
           "r"))
     (define bishopPiece
       (if (equal? color WHITE)
           "B"
           "b")))
    (if (equal? 1 (bitwise-and 1 (arithmetic-shift (bitwise-and endPositionBitboard
                                                                (getMovesPiece matrix rookPiece color startPositionBitboard startPositionIndex allPieces whitePieces blackPieces))
                                                   (- endPositionIndex 63))))
        (bitwise-and (getMovesPiece matrix rookPiece color startPositionBitboard startPositionIndex allPieces whitePieces blackPieces)
                     (getMovesPiece matrix endPiece (not color) endPositionBitboard endPositionIndex allPieces whitePieces blackPieces))
        (bitwise-and (getMovesPiece matrix bishopPiece color startPositionBitboard startPositionIndex allPieces whitePieces blackPieces)
                     (getMovesPiece matrix endPiece (not color) endPositionBitboard endPositionIndex allPieces whitePieces blackPieces)))))
  

(define (legalMovesUnderCheck matrix bitboards startPiece color endPositionBitboard endPositionIndex allPieces whitePieces blackPieces kingChecks)
  (local
    ((define kingPiece
       (if (equal? color WHITE)
           "K"
           "k"))
     (define (queenPiece color)
       (if (equal? color WHITE)
           "Q"
           "q"))
     (define (rookPiece color)
       (if (equal? color WHITE)
           "R"
           "r"))
     (define (bishopPiece color)
       (if (equal? color WHITE)
           "B"
           "b"))
     (define (knightPiece color)
       (if (equal? color WHITE)
           "N"
           "n"))
     (define (pawnPiece color)
       (if (equal? color WHITE)
           "P"
           "p")))
    (cond
      [(equal? 2 kingChecks)
       (if (equal? 0 (getMovesPiece matrix kingPiece color (arithmetic-shift 1 (- 63 (bitboardToPositionIndex (dict-ref bitboards kingPiece)))) (bitboardToPositionIndex (dict-ref bitboards kingPiece)) allPieces whitePieces blackPieces))
           (display "CHECKMATE")
           0)]
      [(and (equal? 1 kingChecks) (or (equal? startPiece (knightPiece (not color))) (equal? startPiece (pawnPiece (not color)))))
       (if (and (equal? 0 (getMovesPiece matrix kingPiece color (dict-ref bitboards kingPiece) (bitboardToPositionIndex (dict-ref bitboards kingPiece)) allPieces whitePieces blackPieces))
                (equal? 0 (bitwise-and (getMovesPiece matrix (queenPiece color) color (dict-ref bitboards (queenPiece color)) (bitboardToPositionIndex (dict-ref bitboards (queenPiece color))) allPieces whitePieces blackPieces)
                                       endPositionBitboard))
                (equal? 0 (bitwise-and (getMovesPiece matrix (rookPiece color) color (dict-ref bitboards (rookPiece color)) (bitboardToPositionIndex (dict-ref bitboards (rookPiece color))) allPieces whitePieces blackPieces)
                                       endPositionBitboard))
                (equal? 0 (bitwise-and (getMovesPiece matrix (bishopPiece color) color (dict-ref bitboards (bishopPiece color)) (bitboardToPositionIndex (dict-ref bitboards (bishopPiece color))) allPieces whitePieces blackPieces)
                                       endPositionBitboard))
                (equal? 0 (bitwise-and (getMovesPiece matrix (knightPiece color) color (dict-ref bitboards (knightPiece color)) (bitboardToPositionIndex (dict-ref bitboards (knightPiece color))) allPieces whitePieces blackPieces)
                                       endPositionBitboard))
                (equal? 0 (bitwise-and (getMovesPiece matrix (pawnPiece color) color (dict-ref bitboards (pawnPiece color)) (bitboardToPositionIndex (dict-ref bitboards (pawnPiece color))) allPieces whitePieces blackPieces)
                                       endPositionBitboard)))
           (display "CHECKMATE")
           endPositionBitboard)]
      [(and (equal? 1 kingChecks) (or (equal? startPiece (queenPiece (not color))) (equal? startPiece (rookPiece (not color))) (equal? startPiece (bishopPiece (not color)))))
       (if (and (equal? 0 (getMovesPiece matrix kingPiece color (dict-ref bitboards kingPiece) (bitboardToPositionIndex (dict-ref bitboards kingPiece)) allPieces whitePieces blackPieces))
                (equal? 0 (bitwise-and (getMovesPiece matrix (queenPiece color) color (dict-ref bitboards (queenPiece color)) (bitboardToPositionIndex (dict-ref bitboards (queenPiece color))) allPieces whitePieces blackPieces)
                                       (getAttackRay matrix color (dict-ref bitboards kingPiece) (bitboardToPositionIndex (dict-ref bitboards kingPiece)) startPiece endPositionBitboard endPositionIndex allPieces whitePieces blackPieces)
                                       endPositionBitboard))
                (equal? 0 (bitwise-and (getMovesPiece matrix (rookPiece color) color (dict-ref bitboards (rookPiece color)) (bitboardToPositionIndex (dict-ref bitboards (rookPiece color))) allPieces whitePieces blackPieces)
                                       (getAttackRay matrix color (dict-ref bitboards kingPiece) (bitboardToPositionIndex (dict-ref bitboards kingPiece)) startPiece endPositionBitboard endPositionIndex allPieces whitePieces blackPieces)
                                       endPositionBitboard))
                (equal? 0 (bitwise-and (getMovesPiece matrix (bishopPiece color) color (dict-ref bitboards (bishopPiece color)) (bitboardToPositionIndex (dict-ref bitboards (bishopPiece color))) allPieces whitePieces blackPieces)
                                       (getAttackRay matrix color (dict-ref bitboards kingPiece) (bitboardToPositionIndex (dict-ref bitboards kingPiece)) startPiece endPositionBitboard endPositionIndex allPieces whitePieces blackPieces)
                                       endPositionBitboard))
                (equal? 0 (bitwise-and (getMovesPiece matrix (knightPiece color) color (dict-ref bitboards (knightPiece color)) (bitboardToPositionIndex (dict-ref bitboards (knightPiece color))) allPieces whitePieces blackPieces)
                                       (getAttackRay matrix color (dict-ref bitboards kingPiece) (bitboardToPositionIndex (dict-ref bitboards kingPiece)) startPiece endPositionBitboard endPositionIndex allPieces whitePieces blackPieces)
                                       endPositionBitboard))
                (equal? 0 (bitwise-and (getMovesPiece matrix (pawnPiece color) color (dict-ref bitboards (pawnPiece color)) (bitboardToPositionIndex (dict-ref bitboards (pawnPiece color))) allPieces whitePieces blackPieces)
                                       (getAttackRay matrix color (dict-ref bitboards kingPiece) (bitboardToPositionIndex (dict-ref bitboards kingPiece)) startPiece endPositionBitboard endPositionIndex allPieces whitePieces blackPieces)
                                       endPositionBitboard)))
           (display "CHECKMATE")
           (bitwise-ior endPositionBitboard
                        (getAttackRay matrix color (dict-ref bitboards kingPiece) (bitboardToPositionIndex (dict-ref bitboards kingPiece)) startPiece endPositionBitboard endPositionIndex allPieces whitePieces blackPieces)))]
      [else 18446744073709551615])))































;makeCastle? Matrix Bitboards Color StartPiece EndPositionIndex AllPieces WhitePieces BlackPieces CastleWhiteKingSide CastleWhiteQueenSide CastleBlackKingSide CastleBlackQueenSide -> Boolean
;determinates if a castle is possible based on the placement of all the pieces on the chessboard  
;(define (makeCastle? matrix bitboards color startPiece endPositionIndex allPieces whitePieces blackPieces castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleBlackQueenSide) #t)

;(define (makeCastle? matrix bitboards color startPiece endPositionIndex allPieces whitePieces blackPieces castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleBlackQueenSide)
;  (local
;    ((define (positionClear? positionIndex)
;       (equal? 0 (bitwise-and 1 (arithmetic-shift ...))))
;     (define (positionSafe? positionIndex)
;       (equal? 0 (bitwise-and 1 (arithmetic-shift ...)))))
;     (define makeCastleWhiteKingSide?
;       (and (equal? ... startPiece)
;            (equal? ... endPositionIndex)
;            (positionClear? ...) (positionSafe? ...)
;            (positionClear? ...) (positionSafe? ...)
;            castleWhiteKingSide))
;     (define makeCastleWhiteQueenSide?
;       (and (equal? ... startPiece)
;            (equal? ... endPositionIndex)
;            (positionClear? ...) (positionSafe? ...)
;            (positionClear? ...) (positionSafe? ...)
;            (positionClear? ...)
;            castleWhiteQueenSide))
;     (define makeCastleBlackKingSide?
;       (and (equal? ... startPiece)
;            (equal? ... endPositionIndex)
;            (positionClear? ...) (positionSafe? ...)
;            (positionClear? ...) (positionSafe? ...)
;            castleBlackKingSide))
;     (define makeCastleBlackQueenSide?
;       (and (equal? ... startPiece)
;            (equal? ... endPositionIndex)
;            (positionClear? ...) (positionSafe? ...)
;            (positionClear? ...) (positionSafe? ...)
;            (positionClear? ...)
;            castleWhiteQueenSide)))
;    (or makeCastleWhiteKingSide? makeCastleWhiteQueenSide? makeCastleBlackKingSide? makeCastleBlackQueenSide?)))

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

;makeCastle: WorldState Bitboards StartPiece EndPositionIndex CastleWhiteKingSide CastleWhiteQueenSide CastleBlackKingSide CastleBlackQueenSide -> Bitboard
;generates a new bitboard with the king and rook after they have been castled 
;(define (makeCastle worldState bitboards startPiece endPositionIndex castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleBlackQueenSide) bitboard)

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
                        (not (worldState-turn worldState))
                        (worldState-kingChecks worldState)
                        (worldState-legalMovesUnderCheck worldState)
                        empty-image
                        (make-currentMove " " (make-posn 0 0) (make-posn 0 0))
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




(define (preparePromotion? matrix startPiece startPieceColor startPositionBitboard startPositionIndex allPieces whitePieces blackPieces endPositionBitboard endPositionIndex)
  (local
    ((define prepareWhitePromotion?
       (and (equal? "P" startPiece)
            (equal? 1 (arithmetic-shift (bitwise-and (getMovesPiece matrix startPiece startPieceColor startPositionBitboard startPositionIndex allPieces whitePieces blackPieces) endPositionBitboard) (- endPositionIndex 63)))
            (equal? 1 (floor (/ startPositionIndex 8)))
            (equal? 0 (floor (/ endPositionIndex 8)))))
     (define prepareBlackPromotion?
       (and (equal? "p" startPiece)
            (equal? 1 (arithmetic-shift (bitwise-and (getMovesPiece matrix startPiece startPieceColor startPositionBitboard startPositionIndex allPieces whitePieces blackPieces) endPositionBitboard) (- endPositionIndex 63)))
            (equal? 6 (floor (/ startPositionIndex 8)))
            (equal? 7 (floor (/ endPositionIndex 8))))))
    (or prepareWhitePromotion? prepareBlackPromotion?)))

(define (preparePromotion worldState startPieceColor bitboards startPositionBitboard endPiece endPositionIndex)
  (local
    ((define prepareWhitePromotion
       (make-worldState (drawPromotionMenu (drawPieces (bitboardsToMatrix (updatePieceAtPosition bitboards "P" startPositionBitboard))) WHITE endPositionIndex)
                        (bitboardsToMatrix (updatePieceAtPosition bitboards "P" startPositionBitboard))
                        (updatePieceAtPosition bitboards "P" startPositionBitboard)
                        (worldState-turn worldState)
                        (worldState-kingChecks worldState)
                        (worldState-legalMovesUnderCheck worldState)
                        empty-image
                        (make-currentMove " " (make-posn 0 0) (make-posn 0 0))
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
                        (worldState-turn worldState)
                        (worldState-kingChecks worldState)
                        (worldState-legalMovesUnderCheck worldState)
                        empty-image
                        (make-currentMove " " (make-posn 0 0) (make-posn 0 0))
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

;(define (makePromotion worldState previousStartPieceColor bitboards endPiece endPositionIndex previousEndPiece previousEndPositionIndex)
;  (local
;    ((define (newBitboard ...)
;       (if (equal? ...)
;           (updatePieceAtPosition ...(arithmetic-shift 1 (- 63 ...)))
;           (updatePieceAtPosition
;            (updatePieceAtPosition ... (arithmetic-shift 1 (- 63 ...)))
;            previousEndPiece
;            (arithmetic-shift 1 (- 63 ...)))))
;     (define (newWorldState newBitboard)
;       (make-worldState (drawPieces (bitboardsToMatrix newBitboard))
;                        (bitboardsToMatrix newBitboard)
;                        newBitboard
;                        (make-currentMove ...)
;                        (make-history (history-castleRights (worldState-history ...))
;                                      #false
;                                      #false
;                                      (history-previousMove (worldState-history ...)))
;                        (worldState-quit ...))))
;    (cond
;      [(and (equal? ...) (equal? ...))
;       (newWorldState (newBitboard ...))]
;       [(and (equal? ...) (equal? ...))
;       (newWorldState (newBitboard ...))]
;       [(and (equal? ...) (equal? ...))
;       (newWorldState (newBitboard ...))]
;       [(and (equal? ...) (equal? ...))
;       (newWorldState (newBitboard ...))]
;       [(and (equal? ...) (equal? ...))
;       (newWorldState (newBitboard ...))]
;       [(and (equal? ...) (equal? ...))
;       (newWorldState (newBitboard ...))]
;       [(and (equal? ...) (equal? ...))
;       (newWorldState (newBitboard ...))]
;       [(and (equal? ...) (equal? ...))
;       (newWorldState (newBitboard ...))]
;      [else
;       (make-worldState (drawPromotionMenu (drawPieces (worldState-matrix ...)) WHITE previousEndPositionIndex)
;                        (worldState-matrix ...)
;                        (worldState-bitboards ...)
;                        (make-currentMove ...)
;                        (worldState-history ...)
;                        (worldState-quit ...))])))


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
                        (not (worldState-turn worldState))
                        (worldState-kingChecks worldState)
                        (worldState-legalMovesUnderCheck worldState)
                        empty-image
                        (make-currentMove " " (make-posn 0 0) (make-posn 0 0))
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
       (make-worldState (drawPromotionMenu (drawPieces (worldState-matrix worldState)) previousStartPieceColor previousEndPositionIndex)
                        (worldState-matrix worldState)
                        (worldState-bitboards worldState)
                        (worldState-turn worldState)
                        (worldState-kingChecks worldState)
                        (worldState-legalMovesUnderCheck worldState)
                        empty-image
                        (make-currentMove " " (make-posn 0 0) (make-posn 0 0))
                        (worldState-history worldState)
                        (worldState-quit worldState))])))





;makeEnPassant? WorldState Bitboards StartPiece EndPositionIndex CastleWhiteKingSide CastleWhiteQueenSide CastleBlackKingSide CastleBlackQueenSide -> Bitboard
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
                        (not (worldState-turn worldState))
                        (worldState-kingChecks worldState)
                        (worldState-legalMovesUnderCheck worldState)
                        empty-image
                        (make-currentMove " " (make-posn 0 0) (make-posn 0 0))
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





;(define (makeRegularMove worldState startPieceColor bitboards startPiece startPositionBitboard startPositionIndex endPiece endPositionBitboard endPositionIndex castleRights castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleBlackQueenSide)
;  (local
;    ((define newBitboard
;       (if (equal? " " endPiece)
;           (updatePieceAtPosition ... (bitwise-ior ...))
;           (updatePieceAtPosition
;            (updatePieceAtPosition ...)))
;     (define newCastleRights
;       (cond
;         [(and (equal? ...) ())
;          (make-castleRights ...
;                             ...
;                             ...
;                             #false)]
;         [(and (equal? ...) (equal? ...))
;          (make-castleRights ...
;                             ...
;                             ...
;                             #false)]
;         [(and (equal? ...) (equal? ...))
;          (make-castleRights ...
;                             ...
;                             ...
;                             #false)]
;
;         [(and (equal? ...) (equal? ...))
;          (make-castleRights ...
;                             ...
;                             ...
;                             #false)]
;         [(and (equal? ...) (equal? ...))
;          (make-castleRights ...
;                             ...
;                             ...
;                             #false)]
;         [(and (equal? ...) (equal? ...))
;          (make-castleRights ...
;                             ...
;                             ...
;                             #false)]
;         [else
;          castleRights]))
;     (define newEnPassant
;       (if (or (and (equal? ...) (equal? ...)) (equal? ...))
;               (and (equal? ...) (equal? ...)) (equal? ...)))
;           #true
;           #false)))
;    (make-worldState (drawPieces (bitboardsToMatrix newBitboard))
;                     (bitboardsToMatrix ...)
;                     ...
;                     (make-currentMove ...)
;                     (make-history newCastleRights
;                                   newEnPassant
;                                   (history-promotion (worldState-history ...))
;                                   (make-previousMove ...))
;                     (worldState-quit ...))))

(define (makeRegularMove worldState matrix bitboards startPiece startPieceColor startPositionBitboard startPositionIndex endPiece endPositionBitboard endPositionIndex allPieces whitePieces blackPieces castleRights castleWhiteKingSide castleWhiteQueenSide castleBlackKingSide castleBlackQueenSide kingChecks)
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
           #false))
     (define newLegalMovesUnderCheck
       (legalMovesUnderCheck matrix newBitboard startPiece (not startPieceColor) endPositionBitboard endPositionIndex allPieces whitePieces blackPieces kingChecks)))
    (make-worldState (drawPieces (bitboardsToMatrix newBitboard))
                     (bitboardsToMatrix newBitboard)
                     newBitboard
                     (not (worldState-turn worldState))
                     (worldState-kingChecks worldState)
                     (display (printBitboard newLegalMovesUnderCheck))
                     empty-image
                     (make-currentMove " " (make-posn 0 0) (make-posn 0 0))
                     (make-history newCastleRights
                                   newEnPassant
                                   (history-promotion (worldState-history worldState))
                                   (make-previousMove startPieceColor
                                                      endPiece
                                                      endPositionIndex))
                     (worldState-quit worldState))))



















; moveNotLegal: WorldState -> WorldState
; evaluates wether or not a move is valid once a piece is selected and its moveset is evaluated  
; (define (moveNotValid worldState) (drawPieces))

;(define (moveNotValid worldState)
;  (make-worldState (drawPieces ...)
;                   (worldState-matrix ...)
;                   (worldState-bitboards ...)
;                   (make-currentMove ...)
;                   (worldState-history ...)
;                   (worldState-quit ...)))

(define (moveNotLegal worldState)
  (make-worldState (drawPieces (worldState-matrix worldState))
                   (worldState-matrix worldState)
                   (worldState-bitboards worldState)
                   (worldState-turn worldState)
                   (worldState-kingChecks worldState)
                   (worldState-legalMovesUnderCheck worldState)
                   empty-image
                   (make-currentMove " " (make-posn 0 0) (make-posn 0 0))
                   (worldState-history worldState)
                   (worldState-quit worldState)))





; drawPromotionMenu: Chessboard StartPieceColor PositionIndex -> Image
; draws a promotion selection menu ontop of the chessboard and the pawn once a pawn has make it to the corresponding last rank  
;(define (drawPromotionMenu chessboard startPieceColor positionIndex) place-image whitePromotionMenu)

;(define (drawPromotionMenu chessboard startPieceColor positionIndex)
;  (local
;    ((define whitePromotionMenu
;       (above (overlay WQ_ICON WHITE_SQUARE) (...)))
;     (define blackPromotionMenu
;       (above (overlay BN_ICON WHITE_SQUARE) (...))))
;    (if (equal? #true startPieceColor)
;        (place-image whitePromotionMenu (...) chessboard)
;        (place-image blackPromotionMenu (...) chessboard))))

(define (drawPromotionMenu chessboard startPieceColor positionIndex)
  (local
    ((define whitePromotionMenu
       (above (overlay WQ_ICON WHITE_SQUARE) (overlay WR_ICON WHITE_SQUARE)(overlay WB_ICON WHITE_SQUARE)(overlay WN_ICON WHITE_SQUARE)))
     (define blackPromotionMenu
       (above (overlay BN_ICON WHITE_SQUARE) (overlay BB_ICON WHITE_SQUARE)(overlay BR_ICON WHITE_SQUARE)(overlay BQ_ICON WHITE_SQUARE))))
    (if (equal? #true startPieceColor)
        (place-image whitePromotionMenu (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (modulo positionIndex 8))) (* SQUARE_SIDE 2) chessboard)
        (place-image blackPromotionMenu (+ (/ SQUARE_SIDE 2) (* SQUARE_SIDE (modulo positionIndex 8))) (* SQUARE_SIDE 6) chessboard))))




; getMovesPiece: Piece StartPieceColor PositionBitboard PositionIndex AllPieces WhitePieces BlackPieces -> Bitboard
; gets the moveset and bitboard of the selected pieces from its position on the chessboard
; (define (getMovesPiece piece startPieceColor positionBitboard positionIndex allPieces whitePieces blackPieces) (kingMoves startPieceColor whitePieces blackPieces positionBitboard))

;(define (getMovesPiece piece startPieceColor positionBitboard positionIndex allPieces whitePieces blackPieces)
;  (cond
;    [(or (equal? ...) (equal? ...))
;             ...]
;    [(or (equal? ...) (equal? ...))
;             ...]
;    [(or (equal? ...) (equal? ...))
;             ...]
;    [(or (equal? ...) (equal? ...))
;             ...]
;    [(or (equal? ...) (equal? ...))
;             ...]
;    [(or (equal? ...) (equal? ...))
;             ...]
;    [else ...]))

(define (getMovesPiece matrix piece startPieceColor positionBitboard positionIndex allPieces whitePieces blackPieces)
  (cond
    [(equal? "K" piece)
     (bitwise-and (kingMoves positionBitboard) (bitwise-not whitePieces) (bitwise-not (allAttacks matrix BLACK allPieces whitePieces blackPieces)))]
    [(equal? "k" piece)
     (bitwise-and (kingMoves positionBitboard) (bitwise-not blackPieces) (bitwise-not (allAttacks matrix WHITE allPieces whitePieces blackPieces)))]
    [(equal? "Q" piece)
     (bitwise-and (queenMoves allPieces positionBitboard positionIndex) (bitwise-not whitePieces))]
    [(equal? "q" piece)
     (bitwise-and (queenMoves allPieces positionBitboard positionIndex) (bitwise-not blackPieces))]
    [(equal? "R" piece)
     (bitwise-and (rookMoves allPieces positionBitboard positionIndex) (bitwise-not whitePieces))]
    [(equal? "r" piece)
     (bitwise-and (rookMoves allPieces positionBitboard positionIndex) (bitwise-not blackPieces))]
    [(equal? "B" piece)
     (bitwise-and (bishopMoves allPieces positionBitboard positionIndex) (bitwise-not whitePieces))]
    [(equal? "b" piece)
     (bitwise-and (bishopMoves allPieces positionBitboard positionIndex) (bitwise-not blackPieces))]
    [(equal? "N" piece)
     (bitwise-and (knightMoves positionBitboard) (bitwise-not whitePieces))]
    [(equal? "n" piece)
     (bitwise-and (knightMoves positionBitboard) (bitwise-not blackPieces))]
    [(or (equal? "P" piece) (equal? "p" piece))
             (pawnMoves startPieceColor allPieces whitePieces blackPieces positionBitboard)]
    [else 0]))




; allAttacks: Matrix Color AllPieces WhitePieces BlackPieces -> Bitboard
; evaluates all the possible attacks inside the chessboard for both the white and black pieces  
; (define (allAttacks matrix color allPieces whitePieces blackPieces) bitboard)

;(define (allAttacks matrix color allPieces whitePieces blackPieces)
;  (local
;    ((define (getPiece ...)
;       (matrixGet (...)))
;     (define (allAttacksAcc bitboard positionIndex)
;       (cond
;         [(equal? ...)]
;         [(or (equal? ...))
;          (allAttacksAcc ...)]
;         [else
;          (allAttacksAcc (bitwise-ior ...))])))
;    (allAttacksAcc ...)))

(define (allAttacks matrix startPieceColor allPieces whitePieces blackPieces)
  (local
    ((define (getPiece positionIndex)
       (matrixGet matrix (floor (/ positionIndex 8)) (modulo positionIndex 8)))
     (define (allAttacksAcc bitboard positionIndex)
       (cond
         [(equal? 64 positionIndex) bitboard]
         [(or (equal? (getColor (getPiece positionIndex)) (not startPieceColor)) (equal? (getPiece positionIndex) " " ))
          (allAttacksAcc bitboard (add1 positionIndex))]
         [else
          (allAttacksAcc (bitwise-ior bitboard
                                      (getAttacksPiece (getPiece positionIndex)
                                                       startPieceColor
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

;(define (getAttacksPiece piece startPieceColor positionBitboard positionIndex allPieces whitePieces blackPieces)
;  (cond
;    [(or (equal? ...) (...))
;             (...)]
;    [(or (equal? ...) (...))
;             (...)]
;    [(or (equal? ...) (...))
;             (...)]
;    [(or (equal? ...) (...))
;             (...)]
;    [(or (equal? ...) (...))
;             (...)]
;    [(or (equal? ...) (...))
;             (...)]
;    [else ...]))

(define (getAttacksPiece piece startPieceColor positionBitboard positionIndex allPieces whitePieces blackPieces)
  (cond
    [(or (equal? "K" piece) (equal? "k" piece))
             (kingMoves positionBitboard)]
    [(or (equal? "Q" piece) (equal? "q" piece))
             (queenMoves allPieces positionBitboard positionIndex)]
    [(or (equal? "R" piece) (equal? "r" piece))
             (rookMoves allPieces positionBitboard positionIndex)]
    [(or (equal? "B" piece) (equal? "b" piece))
             (bishopMoves allPieces positionBitboard positionIndex)]
    [(or (equal? "N" piece) (equal? "n" piece))
             (knightMoves positionBitboard)]
    [(or (equal? "P" piece) (equal? "p" piece))
             (pawnAttacks startPieceColor positionBitboard)]
    [else 0]))

;(define (allPiecesBitboard bitboards)
;  (bitwise-xor
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)))



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


;(define (whitePiecesBitboard bitboards)
;  (bitwise-xor
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)))

(define (whitePiecesBitboard bitboards)
  (bitwise-xor
   (dict-ref bitboards "K")
   (dict-ref bitboards "Q")
   (dict-ref bitboards "R")
   (dict-ref bitboards "B")
   (dict-ref bitboards "N")
   (dict-ref bitboards "P")))


;(define (blackPiecesBitboard bitboards)
;  (bitwise-xor
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)
;   (dict-ref ...)))

(define (blackPiecesBitboard bitboards)
  (bitwise-xor
   (dict-ref bitboards "k")
   (dict-ref bitboards "q")
   (dict-ref bitboards "r")
   (dict-ref bitboards "b")
   (dict-ref bitboards "n")
   (dict-ref bitboards "p")))



;(define (getColor piece)
;  (if (or (equal? ...) (equal? ...) (equal? ...) (equal? ...) (equal? ...) (equal? ...))
;            WHITE
;            BLACK))


(define (getColor piece)
  (if (or (equal? "K" piece) (equal? "Q" piece) (equal? "R" piece) (equal? "B" piece) (equal? "N" piece) (equal? "P" piece))
            WHITE
            BLACK))


; updatePieceAtPosition: Bitboards Piece PositionBitboard -> Bitboards
; updates the bitboards piece after is has been moved 
; (define (updatePieceAtPosition bitboards piece positionBitboard) bitboards)

;(define (updatePieceAtPosition bitboards piece positionBitboard)
;  (dict-set ... (bitwise-xor (dict-ref ...) ...)))

(define (updatePieceAtPosition bitboards piece positionBitboard)
  (dict-set bitboards piece (bitwise-xor (dict-ref bitboards piece) positionBitboard)))




; quit?: WorldState -> Boolean
; returns a Boolean indicating whether the app has quit or not.
; (define (quit? appstate) #f)

;(define (quit? worldState)
;  (if (equal? #t (... worldState ...))
;      ...))

(define (quit? worldState)
  (if (equal? #t (worldState-quit worldState))
      #t
      #f))

; quit: WorldState -> WorldState
; returns an worldState that records the information that the application has quit with the 'quit' parameter set to #true
; (define (quit worldState) (make-worldState chessboard #f #t))


; quit: WorldState -> WorldState
; returns an worldState that records the information that the application has quit with the 'quit' parameter set to #true
; (define (quit worldState) (make-worldState chessboard #f #t))

;(define (quit worldState)
;  (make-worldState (worldState-chessboard ...)
;                   (worldState-matrix ...)
;                   (worldState-bitboards ...)
;                   (worldState-currentMove ...)
;                   (worldState-history ...)
;                   #true))

(define (quit worldState)
  (make-worldState (worldState-chessboard worldState)
                   (worldState-matrix worldState)
                   (worldState-bitboards worldState)
                   (worldState-turn worldState)
                   (worldState-kingChecks worldState)
                   (worldState-legalMovesUnderCheck worldState)
                   (worldState-icon worldState)
                   (worldState-currentMove worldState)
                   (worldState-history worldState)
                   #true))




; handle-key: WorldState KeyEvent -> worldState
; handles the following key event and updates the worldState accordingly
; - "q": set the world state to quit
; (define (handle-key worldState key-event) worldState)

;(define handle-key worldState key-event) 
;  (cond
;    [(string=? "q" key-event) (... worldState ...)]
;    [(string=? "r" key-event) (... worldState ...)]
;    [else worldState]))

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

;(define (handle-mouse worldState x-mouse y-mouse mouse-event)
;  (cond
;    [(string=? "button-down" mouse-event) (... worldState ...)]
;    [(and (string=? "drag" mouse-event)
;          (line2D? (... worldState ...))) (... worldState ...)]
;    [(string=? "button-up" mouse-event) (... worldState ...)]
;    [else worldState]))

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
                        #true
                        0
                        18446744073709551615
                        empty-image
                        (make-currentMove " " (make-posn 0 0) (make-posn 0 0))
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

; things to do
; remove captured piece in prepare promotion, not after
; clean up the parameters in a consinstent order
; positionIndex before positionBitboard
; Refactor makeMove
; (equal? ordinare argomenti in modo logico cazzo
; rename moves to possibleMoves
; pawn moves inconsisten with other functions
; previous startpiececolor is obsolete
; kings are still capturable pieces...
; rayattacks is a mess with the names endpiece startpiece...