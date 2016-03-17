module GameLogic.UpdateState where

import GameLogic.GameEnd as GameEnd
import GameModel exposing (Coordinates, GameState, Move, Player(X, O), Status(InProgress, Tied, Won))


makeMove : Coordinates -> GameState -> GameState
makeMove coordinates gameState =
    case (gameState.status, GameModel.playerAt coordinates gameState) of
        (InProgress, Nothing) ->
            updateGameStatus
                { gameState |
                    currentPlayer = otherPlayer gameState.currentPlayer,
                    movesSoFar = Move coordinates gameState.currentPlayer :: gameState.movesSoFar
                }
        _ ->
            gameState


updateGameStatus : GameState -> GameState
updateGameStatus gameState =
    case GameEnd.winningPlayer gameState of
        Just player ->
            { gameState | status = Won player }
        Nothing ->
            if GameEnd.isGameOver gameState
                then { gameState | status = Tied }
                else { gameState | status = InProgress }


otherPlayer : Player -> Player
otherPlayer player =
    case player of
        X -> O
        O -> X
