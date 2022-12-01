;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname chess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/base)
(require racket/format)
(require 2htdp/image)

(define WHITE_KING      (bitmap "img/WHITE_KING.png"))
(define WHITE_QUEEN     (bitmap "img/WHITE_QUEEN.png"))
(define WHITE_ROOK      (bitmap "img/WHITE_ROOK.png"))
(define WHITE_BISHOP    (bitmap "img/WHITE_BISHOP.png"))
(define WHITE_KNIGHT    (bitmap "img/WHITE_KNIGHT.png"))
(define WHITE_PAWN      (bitmap "img/WHITE_PAWN.png"))

; Definition of the black pieces
(define BLACK_KING      (bitmap "img/BLACK_KING.png"))
(define BLACK_QUEEN     (bitmap "img/BLACK_QUEEN.png"))
(define BLACK_ROOK      (bitmap "img/BLACK_ROOK.png"))
(define BLACK_BISHOP    (bitmap "img/BLACK_BISHOP.png"))
(define BLACK_KNIGHT    (bitmap "img/BLACK_KNIGHT.png"))
(define BLACK_PAWN      (bitmap "img/BLACK_PAWN.png"))

(define EMPTY           (text "EMPTY" 10 "white"))

(define dark_wood (make-color 191 108 58))
(define light_wood (make-color 238 202 160))

(define SQUARE_SIDE 100)
(define ODD_SQUARE (square SQUARE_SIDE "solid" light_wood))
(define EVEN_SQUARE (square SQUARE_SIDE "solid" dark_wood))

(define CHESSBOARD2
  (vector
   (vector BLACK_ROOK       BLACK_KNIGHT     BLACK_BISHOP       BLACK_QUEEN         BLACK_KING      BLACK_BISHOP        BLACK_KNIGHT        BLACK_ROOK)
   (vector BLACK_PAWN       BLACK_PAWN       BLACK_PAWN         BLACK_PAWN          BLACK_PAWN      BLACK_PAWN          BLACK_PAWN          BLACK_PAWN)
   (vector EMPTY            EMPTY            EMPTY              EMPTY               EMPTY           EMPTY               EMPTY               EMPTY)
   (vector EMPTY            EMPTY            EMPTY              EMPTY               EMPTY           EMPTY               EMPTY               EMPTY)
   (vector EMPTY            EMPTY            EMPTY              EMPTY               EMPTY           EMPTY               EMPTY               EMPTY)
   (vector EMPTY            EMPTY            EMPTY              EMPTY               EMPTY           EMPTY               EMPTY               EMPTY)
   (vector WHITE_PAWN       WHITE_PAWN       WHITE_PAWN         WHITE_PAWN          WHITE_PAWN      WHITE_PAWN          WHITE_PAWN          WHITE_PAWN)
   (vector WHITE_ROOK       WHITE_KNIGHT     WHITE_BISHOP       WHITE_QUEEN         WHITE_KING      WHITE_BISHOP        WHITE_KNIGHT        WHITE_ROOK)))


(define EMPTY_CHESSBOARD
  (above (beside ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE)
         (beside EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE)
         (beside ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE)
         (beside EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE)
         (beside ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE)
         (beside EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE)
         (beside ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE)
         (beside EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE EVEN_SQUARE ODD_SQUARE)))

(define (place_pieces pieces chessboard row_acc col_acc)
  (cond
    [(> row_acc 7) chessboard]
    [(> col_acc 7) (place_pieces pieces chessboard (add1 row_acc) 0)]
    [else (place-image (vector-ref (vector-ref pieces row_acc) col_acc) (+ 50 (* 100 col_acc)) (+ 50 (* 100 row_acc)) (place_pieces pieces chessboard row_acc (add1 col_acc)))]))



; New section with bitboards

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

(define A (vector 1 2 3))

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

(define CHESSBOARD
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


(define (matrix_get matrix row col)
  (vector-ref (vector-ref matrix row) col))

(define (matrix_set matrix row col value)
  (vector-set! (vector-ref matrix row) col value))

(define (matrixToBitBoards CHESSBOARD BITBOARDS k_acc)
  (cond
    [(equal? 64 k_acc) BITBOARDS]
    [(equal? "K" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
     (begin (vector-set! BITBOARDS 0 (+ (vector-ref BITBOARDS 0) (arithmetic-shift 1 (- 63 k_acc)))))
     (begin (matrixToBitBoards CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? "Q" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
     (begin (vector-set! BITBOARDS 1 (+ (vector-ref BITBOARDS 1) (arithmetic-shift 1 (- 63 k_acc)))))
     (begin (matrixToBitBoards CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? "R" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
     (begin (vector-set! BITBOARDS 2 (+ (vector-ref BITBOARDS 2) (arithmetic-shift 1 (- 63 k_acc)))))
     (begin (matrixToBitBoards CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? "B" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
     (begin (vector-set! BITBOARDS 3 (+ (vector-ref BITBOARDS 3) (arithmetic-shift 1 (- 63 k_acc)))))
     (begin (matrixToBitBoards CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? "N" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
     (begin (vector-set! BITBOARDS 4 (+ (vector-ref BITBOARDS 4) (arithmetic-shift 1 (- 63 k_acc)))))
     (begin (matrixToBitBoards CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? "P" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
     (begin (vector-set! BITBOARDS 5 (+ (vector-ref BITBOARDS 5) (arithmetic-shift 1 (- 63 k_acc)))))
     (begin (matrixToBitBoards CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? "k" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
     (begin (vector-set! BITBOARDS 6 (+ (vector-ref BITBOARDS 6) (arithmetic-shift 1 (- 63 k_acc)))))
     (begin (matrixToBitBoards CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? "q" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
     (begin (vector-set! BITBOARDS 7 (+ (vector-ref BITBOARDS 7) (arithmetic-shift 1 (- 63 k_acc)))))
     (begin (matrixToBitBoards CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? "r" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
     (begin (vector-set! BITBOARDS 8 (+ (vector-ref BITBOARDS 8) (arithmetic-shift 1 (- 63 k_acc)))))
     (begin (matrixToBitBoards CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? "b" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
     (begin (vector-set! BITBOARDS 9 (+ (vector-ref BITBOARDS 9) (arithmetic-shift 1 (- 63 k_acc)))))
     (begin (matrixToBitBoards CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? "n" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
     (begin (vector-set! BITBOARDS 10 (+ (vector-ref BITBOARDS 10) (arithmetic-shift 1 (- 63 k_acc)))))
     (begin (matrixToBitBoards CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? "p" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
     (begin (vector-set! BITBOARDS 11 (+ (vector-ref BITBOARDS 11) (arithmetic-shift 1 (- 63 k_acc)))))
     (begin (matrixToBitBoards CHESSBOARD BITBOARDS (add1 k_acc)))]
    [else
     (begin (matrixToBitBoards CHESSBOARD BITBOARDS (add1 k_acc)))]))

(define (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS k_acc)
  (cond
    [(equal? 64 k_acc) EMTPY_CHESSBOARD]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS 0) (- k_acc 63))))
     (begin (matrix_set EMTPY_CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8) "K"))
     (begin (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS 1) (- k_acc 63))))
     (begin (matrix_set EMTPY_CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8) "Q"))
     (begin (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS 2) (- k_acc 63))))
     (begin (matrix_set EMTPY_CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8) "R"))
     (begin (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS 3) (- k_acc 63))))
     (begin (matrix_set EMTPY_CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8) "B"))
     (begin (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS 4) (- k_acc 63))))
     (begin (matrix_set EMTPY_CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8) "N"))
     (begin (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS 5) (- k_acc 63))))
     (begin (matrix_set EMTPY_CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8) "P"))
     (begin (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS 6) (- k_acc 63))))
     (begin (matrix_set EMTPY_CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8) "k"))
     (begin (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS 7) (- k_acc 63))))
     (begin (matrix_set EMTPY_CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8) "q"))
     (begin (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS 8) (- k_acc 63))))
     (begin (matrix_set EMTPY_CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8) "r"))
     (begin (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS 9) (- k_acc 63))))
     (begin (matrix_set EMTPY_CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8) "b"))
     (begin (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS 10) (- k_acc 63))))
     (begin (matrix_set EMTPY_CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8) "n"))
     (begin (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS (add1 k_acc)))]
    [(equal? 1 (bitwise-and 1 (arithmetic-shift (vector-ref BITBOARDS 11) (- k_acc 63))))
     (begin (matrix_set EMTPY_CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8) "p"))
     (begin (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS (add1 k_acc)))]
    [else
     (begin (matrix_set EMTPY_CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8) " "))
     (begin (bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS (add1 k_acc)))]))

(matrixToBitBoards TEST_CHESSBOARD BITBOARDS 0)
(bitBoardsToMatrix EMTPY_CHESSBOARD BITBOARDS 0)


(define (printBitBoard2 BITBOARD k_acc)
  (cond
    [(equal? 8 k_acc) (void)]
    [else
     (begin (writeln (~r (bitwise-and (arithmetic-shift BITBOARD (* -8 (- 7 k_acc))) #b11111111) #:base 2 #:min-width 8 #:pad-string "0")))
     (begin (printBitBoard2 BITBOARD (add1 k_acc)))]))

(define (printBitBoard BITBOARD)
  (printBitBoard2 BITBOARD 0))


(define (printBitBoards2 BITBOARDS k_acc)
  (cond
    [(equal? (vector-length BITBOARDS) k_acc) (void)]
    [else
     (begin (writeln "        "))
     (begin  (printBitBoard (vector-ref BITBOARDS k_acc)))
     (begin (printBitBoards2 BITBOARDS (add1 k_acc)))]))

(define (printBitBoards BITBOARDS)
  (printBitBoards2 BITBOARDS 0))

;=============================================================================================

;DATA TYPE
;A piece-move is a bitwise operation where: 
;   - there are the initial coordinates
;   - the bitshift represents the direction of where the piece can go
;   - there are the new coordinates of the pieces after the bitshift

; Mossa Pietro
;;Pawn
(define FILE_P #b0000000100000001000000010000000100000001000000010000000100000001)
(define (PawnMoves matrixPosition)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 55 matrixPosition))))
    (bitwise-ior
      (arithmetic-shift (bitwise-and binaryPosition FILE_P) 7)
      (arithmetic-shift (bitwise-and binaryPosition FILE_P) 8)
      (arithmetic-shift (bitwise-and binaryPosition FILE_P) 9)
      (arithmetic-shift (bitwise-and binaryPosition FILE_P) 16))))
     

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


(define (DiagonalAntiDiagonalMoves matrixPosition)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 matrixPosition)))
    (define DiagonalMoves
      (bitwise-xor (- (bitwise-and OCCUPIED (vector-ref DIAGONALMASKS (+ (floor (/ matrixPosition 8)) (modulo matrixPosition 8)))) (* 2 binaryPosition))
                   (reverseBinary (- (reverseBinary (bitwise-and OCCUPIED (vector-ref DIAGONALMASKS (+ (floor (/ matrixPosition 8)) (modulo matrixPosition 8))))) (* 2 (reverseBinary binaryPosition))))))
    (define AntiDiagonalMoves
      (bitwise-xor (- (bitwise-and OCCUPIED (vector-ref ANTIDIAGONALMASKS (+ (floor (/ matrixPosition 8)) (- 7 (modulo matrixPosition 8))))) (* 2 binaryPosition))
                   (reverseBinary (- (reverseBinary (bitwise-and OCCUPIED (vector-ref ANTIDIAGONALMASKS (+ (floor (/ matrixPosition 8)) (- 7 (modulo matrixPosition 8)))))) (* 2 (reverseBinary binaryPosition)))))))
    (bitwise-ior (bitwise-and DiagonalMoves (vector-ref DIAGONALMASKS (+ (floor (/ matrixPosition 8)) (modulo matrixPosition 8)))) (bitwise-and AntiDiagonalMoves (vector-ref ANTIDIAGONALMASKS (+ (floor (/ matrixPosition 8)) (- 7 (modulo matrixPosition 8))))))))




(define FILE_A  #b0111111101111111011111110111111101111111011111110111111101111111)
(define FILE_AB #b0011111100111111001111110011111100111111001111110011111100111111)
(define FILE_GH #b1111110011111100111111001111110011111100111111001111110011111100)
(define FILE_H  #b1111111011111110111111101111111011111110111111101111111011111110)

(define (knightMoves matrixPosition)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 matrixPosition))))
    (bitwise-ior
     (arithmetic-shift (bitwise-and binaryPosition FILE_AB) 10)
     (arithmetic-shift (bitwise-and binaryPosition FILE_A) 17)
     (arithmetic-shift (bitwise-and binaryPosition FILE_H) 15)
     (arithmetic-shift (bitwise-and binaryPosition FILE_GH) 6)
     (arithmetic-shift (bitwise-and binaryPosition FILE_GH) -10)
     (arithmetic-shift (bitwise-and binaryPosition FILE_H) -17)
     (arithmetic-shift (bitwise-and binaryPosition FILE_A) -15)
     (arithmetic-shift (bitwise-and binaryPosition FILE_AB) -6))))

(define (kingMoves matrixPosition)
  (local
    ((define binaryPosition
      (arithmetic-shift 1 (- 63 matrixPosition))))
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
