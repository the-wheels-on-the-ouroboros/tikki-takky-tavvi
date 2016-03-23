module GameLogic.GameEnd (isGameOver, winningPlayer) where

import GameModel exposing (Coordinates, GameState, Move, Player(X, O))


winningPlayer : GameState -> Maybe Player
winningPlayer gameState =
    let
        winningLines = List.filter (isWinningLine gameState.boardSize) (getLines gameState)
    in
        case winningLines of
            (move::_)::_ -> Just move.player
            []::_ -> Nothing
            [] -> Nothing


isGameOver : GameState -> Bool
isGameOver gameState =
    let
        isGameWon = List.any (isWinningLine gameState.boardSize) (getLines gameState)
        isGameTied = (gameState.boardSize^2) == (List.length gameState.movesSoFar)
    in
        isGameWon || isGameTied


isWinningLine : Int -> List Move -> Bool
isWinningLine lineLength line =
    case line of
        firstMove::_ ->
            (List.length line) == lineLength &&
                List.all (\move -> move.player == firstMove.player) line
        _ ->
            False


getLines : GameState -> List (List Move)
getLines gameState =
    let
        maxIndex = gameState.boardSize - 1
        indices = [0..maxIndex]
        moves = gameState.movesSoFar
    in
        List.concat
            [ List.map (\row-> List.filter (\move -> move.coordinates.row == row) moves) indices
            , List.map (\col-> List.filter (\move -> move.coordinates.col == col) moves) indices
            , [ List.filter (\move -> move.coordinates.row == move.coordinates.col) moves ]
            , [ List.filter (\move -> move.coordinates.row + move.coordinates.col == maxIndex) moves ]
            ]
