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


A Cell is one of:
- 
