module GameLogic.UpdateState (makeMove, nextPlayer) where

import GameLogic.GameEnd as GameEnd
import GameModel exposing
    ( Coordinates
    , GameState
    , Move
    , Player(X, O)
    , Status(InProgress, Tied, Won)
    )


makeMove : Coordinates -> GameState -> GameState
makeMove coordinates gameState =
    case (gameState.status, GameModel.playerAt coordinates gameState) of
        (InProgress, Nothing) ->
            updateGameStatus
                { gameState |
                    currentPlayer = nextPlayer gameState.currentPlayer,
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


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        X -> O
        O -> X
