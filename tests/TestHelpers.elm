module TestHelpers (x, o) where

import GameModel exposing (Coordinates, Move, Player (X, O))


x : Int -> Int -> Move
x row col = Move (Coordinates row col) X


o : Int -> Int -> Move
o row col = Move (Coordinates row col) O
