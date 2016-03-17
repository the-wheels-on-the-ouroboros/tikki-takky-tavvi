module View.Utilities where

import Model exposing (Player (X, O))


playerToString : Maybe Player -> String
playerToString player =
    case player of
        Just X -> "X"
        Just O -> "O"
        Nothing -> " "
