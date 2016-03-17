module TestHelpers where

import Model exposing (Coordinates, Move, Player(X, O))


areUnorderedElementsEqual : List a -> List a -> Bool
areUnorderedElementsEqual list list' =
    (List.length list) == (List.length list')
        && List.all (\element -> (count element list) == (count element list')) list


count : a -> List a -> Int
count element list =
    List.foldl (\element' total -> if element == element' then total + 1 else total) 0 list


x : Int -> Int -> Move
x row col = Move (Coordinates row col) X


o : Int -> Int -> Move
o row col = Move (Coordinates row col) O
