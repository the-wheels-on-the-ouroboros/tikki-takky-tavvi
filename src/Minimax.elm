module Minimax where

import Model exposing (Coordinates, GameState, Status(InProgress, Tied, Won))
import Update


nextGameStates : GameState -> List GameState
nextGameStates gameState =
    let
        makeMove = \coordinates -> Update.makeMove coordinates gameState
    in
        case gameState.status of
            InProgress -> List.map makeMove (availableCoordinates gameState)
            _ -> []


availableCoordinates : GameState -> List Coordinates
availableCoordinates gameState =
    let
        isMoveTaken = \coordinates -> (Model.playerWhoMovedAt coordinates gameState) == Nothing
    in
        List.filter isMoveTaken (Model.boardCoordinates gameState)
