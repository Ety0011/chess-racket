;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

(define (matrix_get matrix row col)
  (vector-ref (vector-ref matrix row) col))

(define (matrixToBitBoards CHESSBOARD WK WQ WR WB WN WP BK BQ BR BB BN BP k_acc)
  (cond
    [(equal? 64 k_acc) (vector WK WQ WR WB WN WP BK BQ BR BB BN BP)]
    [(equal? "K" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
                  (matrixToBitBoards CHESSBOARD (+ WK (arithmetic-shift 1 (- 63 k_acc))) WQ WR WB WN WP BK BQ BR BB BN BP (add1 k_acc))]
    [(equal? "Q" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
                  (matrixToBitBoards CHESSBOARD WK (+ WQ (arithmetic-shift 1 (- 63 k_acc))) WR WB WN WP BK BQ BR BB BN BP (add1 k_acc))]
    [(equal? "R" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
                  (matrixToBitBoards CHESSBOARD WK WQ (+ WR (arithmetic-shift 1 (- 63 k_acc))) WB WN WP BK BQ BR BB BN BP (add1 k_acc))]
    [(equal? "B" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
                  (matrixToBitBoards CHESSBOARD WK WQ WR (+ WB (arithmetic-shift 1 (- 63 k_acc))) WN WP BK BQ BR BB BN BP (add1 k_acc))]
    [(equal? "N" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
                  (matrixToBitBoards CHESSBOARD WK WQ WR WB (+ WN (arithmetic-shift 1 (- 63 k_acc))) WP BK BQ BR BB BN BP (add1 k_acc))]
    [(equal? "P" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
                  (matrixToBitBoards CHESSBOARD WK WQ WR WB WN (+ WP (arithmetic-shift 1 (- 63 k_acc))) BK BQ BR BB BN BP (add1 k_acc))]
    [(equal? "k" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
                  (matrixToBitBoards CHESSBOARD WK WQ WR WB WN WP (+ BK (arithmetic-shift 1 (- 63 k_acc))) BQ BR BB BN BP (add1 k_acc))]
    [(equal? "q" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
                  (matrixToBitBoards CHESSBOARD WK WQ WR WB WN WP BK (+ BQ (arithmetic-shift 1 (- 63 k_acc))) BR BB BN BP (add1 k_acc))]
    [(equal? "r" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
                  (matrixToBitBoards CHESSBOARD WK WQ WR WB WN WP BK BQ (+ BR (arithmetic-shift 1 (- 63 k_acc))) BB BN BP (add1 k_acc))]
    [(equal? "b" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
                  (matrixToBitBoards CHESSBOARD WK WQ WR WB WN WP BK BQ BR (+ BB (arithmetic-shift 1 (- 63 k_acc))) BN BP (add1 k_acc))]
    [(equal? "n" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
                  (matrixToBitBoards CHESSBOARD WK WQ WR WB WN WP BK BQ BR BB (+ BN (arithmetic-shift 1 (- 63 k_acc))) BP (add1 k_acc))]
    [(equal? "p" (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
                  (matrixToBitBoards CHESSBOARD WK WQ WR WB WN WP BK BQ BR BB BN (+ BP (arithmetic-shift 1 (- 63 k_acc))) (add1 k_acc))]
    [(equal? " " (matrix_get CHESSBOARD (floor (/ k_acc 8)) (modulo k_acc 8)))
                  (matrixToBitBoards CHESSBOARD WK WQ WR WB WN WP BK BQ BR BB BN BP (add1 k_acc))]))

(define (bitBoardsToMatrix EMTPY_CHESSBOARD WK WQ WR WB WN WP BK BQ BR BB BN BP k_acc)
  (cond
    [(equal? 64 k_acc) EMTPY_CHESSBOARD]
    [(equal? 1 (bitwise-and(arithmetic-shift 1 (- 63 k_acc)) 1))
     (bitBoardsToMatrix  WK WQ WR WB WN WP BK BQ BR BB BN BP k_acc)]))

    
(matrixToBitBoards CHESSBOARD WK WQ WR WB WN WP BK BQ BR BB BN BP 0)


;; prints line of a bitboard
(define (printBitBoardLine bb line)
  (writeln (~r (arithmetic-shift (bitwise-and (arithmetic-shift #x00000000000000FF (* 8 line)) bb) (* -8 line) ) #:base 2 #:min-width 8 #:pad-string "0")))

(define testBB #b0000000011111111000000000000000000000000000000000000000000000000)




;;;;;;; TEST

(define empty #b0000000000000000000000000000000000000000000000000000000000000000)
(define full  #b1111111111111111111111111111111111111111111111111111111111111111)

(define tWP   #b0000000000000000000000000000000000000000000000000111111000000000)
(define tWR   #b0000000000000000000000000000000000000000000000000000000010000001)
(define tWN   #b0000000000000000000000000000000000000000000000000000000001000010)

(define (get-whites wp wr wn)
  ( bitwise-ior (bitwise-ior wp wr) wn) )

(define whites (get-whites tWP tWR tWN))

(printBitBoardLine whites 0)
(printBitBoardLine whites 1)
(printBitBoardLine whites 2)
(printBitBoardLine whites 3)
(printBitBoardLine whites 4)
(printBitBoardLine whites 5)
(printBitBoardLine whites 6)
(printBitBoardLine whites 7)

;; where r is the bitmap of the rook
;; where rpos is the position of a specific rook
(define (rook-attacks r rpos)
  (bitwise-ior r (arithmetic-shift rpos 16)))







