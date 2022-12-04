# RacketChess

Chess game in Racket

by Leonardo Asaro, Pietro Quintavalle, Etienne Orio, Krit Nicol

# MILESTONE 1
- Defined a piece struct which holds a type (which can be pawn, rook, knight, bishop, queen, king or empty) and a color (which is a boolean #f for black and #t for white).

- Defined the board as a matrix (so a vector of 8 vectors, each representing a line of the chessboard) and a way to draw the pieces on the matrix.

# MILESTONE 2
- Redefined the chessboard as a bitboard that can be converted to a matrix and back.

- Implemented basic piece movement (not accounting for king safety)

- Herobrine remove