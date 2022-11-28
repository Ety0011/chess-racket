;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname chess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require "board.rkt")
(require "moves.rkt")

;; INITIAL VALUES
; PAWN piece white
(define PAWNW (make-piece "pawn" PAWN-MOVES WHITE))
; ROOK piece white
(define ROOKW (make-piece "rook" ROOK-MOVES WHITE))
; BISHOP piece white
(define BISHOPW (make-piece "bishop" BISHOP-MOVES WHITE))
; KNIGHT piece white
(define KNIGHTW (make-piece "knight" KNIGHT-MOVES WHITE))
; KING piece white
(define KINGW (make-piece "king" KING-MOVES WHITE))
; QUEEN piece white
(define QUEENW (make-piece "queen" QUEEN-MOVES WHITE))

; PAWN piece black
(define PAWNB (make-piece "pawn" PAWN-MOVES BLACK))
; ROOK piece black
(define ROOKB (make-piece "rook" ROOK-MOVES BLACK))
; BISHOP piece black
(define BISHOPB (make-piece "bishop" BISHOP-MOVES BLACK))
; KNIGHT piece black
(define KNIGHTB (make-piece "knight" KNIGHT-MOVES BLACK))
; KING piece black
(define KINGB (make-piece "king" KING-MOVES BLACK))
; QUEEN piece black
(define QUEENB (make-piece "queen" QUEEN-MOVES BLACK))

; EMPTY piece
(define EMPTY (make-piece "empty" EMPTY-MOVES))

(define CELL1  (make-cell EMPTY))
(define CELL2  (make-cell EMPTY))
(define CELL3  (make-cell EMPTY))
(define CELL4  (make-cell EMPTY))
(define CELL5  (make-cell EMPTY))
(define CELL6  (make-cell EMPTY))
(define CELL7  (make-cell EMPTY))
(define CELL8  (make-cell EMPTY))
(define CELL9  (make-cell EMPTY))
(define CELL10 (make-cell EMPTY))
(define CELL11 (make-cell EMPTY))
(define CELL12 (make-cell EMPTY))
(define CELL13 (make-cell EMPTY))
(define CELL14 (make-cell EMPTY))
(define CELL15 (make-cell EMPTY))
(define CELL16 (make-cell EMPTY))
; ...

(define BOARD (vector A1  A2  A3  A4 A5  A6  A7  A8
                      CELL9  CELL10 CELL11 CELL12 CELL13 CELL14 CELL15 CELL16
                      CELL17  CELL18 CELL19 CELL20 CELL21 CELL22 CELL23 CELL24
                      CELL25  CELL26 CELL27 CELL28 CELL29 CELL30 CELL31 CELL32
                      CELL33  CELL34 CELL35 CELL36 CELL37 CELL38 CELL39 CELL40
                      CELL41  CELL42 CELL43 CELL44 CELL45 CELL46 CELL47 CELL48
                      CELL49  CELL50 CELL51 CELL52 CELL53 CELL54 CELL55 CELL56
                      CELL57  CELL58 CELL59 CELL60 CELL61 CELL62 CELL63 CELL64))

;;Main Function
(define (drawing-app initial-state)
  (big-bang initial-state
    [to-draw draw]
    [on-mouse handle-mouse]
    [on-key handle-key]
    [stop-when quit?]
    [close-on-stop #true]))