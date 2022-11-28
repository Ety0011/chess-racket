;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define WHITE_KING      (text "WHITE_KING" 10 "white"))
(define WHITE_QUEEN     (text "WHITE_QUEEN" 10 "white"))
(define WHITE_ROOK      (text "WHITE_ROOK" 10 "white"))
(define WHITE_BISHOP    (text "WHITE_BISHOP" 10 "white"))
(define WHITE_KNIGHT    (text "WHITE_KNIGHT" 10 "white"))
(define WHITE_PAWN      (text "WHITE_PAWN" 10 "white"))

; Definition of the black pieces
(define BLACK_KING      (bitmap "BLACK_KING.png"))
(define BLACK_QUEEN     (bitmap "BLACK_QUEEN.png"))
(define BLACK_ROOK      (text "BLACK_ROOK" 10 "white"))
(define BLACK_BISHOP    (text "BLACK_BISHOP" 10 "white"))
(define BLACK_KNIGHT    (text "BLACK_KNIGHT" 10 "white"))
(define BLACK_PAWN      (text "BLACK_PAWN" 10 "white"))

(define EMPTY           (text "EMPTY" 10 "white"))
(define SQUARE1 (square 100 "solid" "grey"))
(define SQUARE2 (square 100 "solid" "grey"))

(define CHESSBOARD
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
  (above (beside SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2)
         (beside SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1)
         (beside SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2)
         (beside SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1)
         (beside SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2)
         (beside SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1)
         (beside SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2)
         (beside SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1 SQUARE2 SQUARE1)))

(define (place_pieces pieces chessboard row_acc col_acc)
  (cond
    [(> row_acc 7) chessboard]
    [(> col_acc 7) (place_pieces pieces chessboard (add1 row_acc) 0)]
    [else (place-image (vector-ref (vector-ref pieces row_acc) col_acc) (+ 50 (* 100 col_acc)) (+ 50 (* 100 row_acc)) (place_pieces pieces chessboard row_acc (add1 col_acc)))]))



(place_pieces CHESSBOARD EMPTY_CHESSBOARD 0 0)