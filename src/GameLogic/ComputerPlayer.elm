module GameLogic.ComputerPlayer (bestMove) where

import GameModel exposing (Coordinates, GameState, Status (InProgress, Tied, Won))
import GameLogic.HandleTurn as HandleTurn
import Utilities


bestMove : GameState -> Maybe Coordinates
bestMove gameState =
    let
        bestNextGameState = Utilities.maximumBy score (nextGameStates gameState)
    in
        case bestNextGameState of
            Just nextState -> Maybe.map .coordinates (List.head nextState.movesSoFar)
            Nothing -> Nothing


score : GameState -> Int
score gameState =
    -(scoreWithCutoff gameState -infinity infinity)


scoreWithCutoff : GameState -> Int -> Int -> Int
scoreWithCutoff gameState bestForCurrentPlayer bestForOpponent =
    case gameState.status of
        Tied -> 0
        Won player ->
            if player == gameState.currentPlayer
                then (numberOfAvailableMoves gameState) + 1
                else -((numberOfAvailableMoves gameState) + 1)
        InProgress ->
            maxScore
                (nextGameStates gameState)
                bestForCurrentPlayer
                bestForOpponent
                -infinity


maxScore : List GameState -> Int -> Int -> Int -> Int
maxScore gameStates bestForCurrentPlayer bestForOpponent bestSoFar =
    case gameStates of
        first :: rest ->
            let
                firstScore = -(scoreWithCutoff first -bestForOpponent -bestForCurrentPlayer)
                newBestForCurrentPlayer = max bestForCurrentPlayer firstScore
                newBestSoFar = max bestSoFar firstScore
            in
                if newBestForCurrentPlayer > bestForOpponent
                    then newBestSoFar
                    else maxScore rest newBestForCurrentPlayer bestForOpponent newBestSoFar
        [] ->
            bestSoFar


numberOfAvailableMoves : GameState -> Int
numberOfAvailableMoves gameState =
    gameState.boardSize^2 - (List.length gameState.movesSoFar)


infinity : Int
infinity = round (1/0)


nextGameStates : GameState -> List GameState
nextGameStates gameState =
    let
        makeMove = \coordinates -> HandleTurn.makeMove coordinates gameState
    in
        case gameState.status of
            InProgress -> List.map makeMove (availableCoordinates gameState)
            _ -> []


availableCoordinates : GameState -> List Coordinates
availableCoordinates gameState =
    let
        isMoveTaken = \coordinates -> (GameModel.playerAt coordinates gameState) == Nothing
    in
        List.filter isMoveTaken (GameModel.boardCoordinates gameState)
