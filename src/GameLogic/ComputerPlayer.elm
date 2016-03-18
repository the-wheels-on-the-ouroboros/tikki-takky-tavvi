module GameLogic.ComputerPlayer where

import GameModel exposing (Coordinates, GameState, Status(InProgress, Tied, Won))
import GameLogic.UpdateState as Update
import Utilities


makeMoveVsComputer : Coordinates -> GameState -> GameState
makeMoveVsComputer coordinates gameState =
    let
        nextGameState = Update.makeMove coordinates gameState
    in
        if (nextGameState.status /= InProgress) || (gameState == nextGameState)
            then nextGameState
            else
                case bestMove nextGameState of
                    Just computerMove -> Update.makeMove computerMove nextGameState
                    _ -> nextGameState


bestMove : GameState -> Maybe Coordinates
bestMove gameState =
    let
        bestNextGameState =
            Utilities.maximumBy (\state -> -(score state)) (nextGameStates gameState)
    in
        case bestNextGameState of
            Just nextState -> Maybe.map .coordinates (List.head nextState.movesSoFar)
            Nothing -> Nothing


score : GameState -> Int
score gameState = scoreWithCutoff gameState -infinity infinity


scoreWithCutoff : GameState -> Float -> Float -> Int
scoreWithCutoff gameState bestForCurrentPlayer bestForOpponent =
    let
        numberOfAvailableMoves = (gameState.boardSize^2) - (List.length gameState.movesSoFar)
    in
        case gameState.status of
            Tied -> 0
            Won player ->
                if player == gameState.currentPlayer
                    then numberOfAvailableMoves
                    else -numberOfAvailableMoves
            InProgress ->
                maxScore
                    (nextGameStates gameState)
                    bestForCurrentPlayer
                    bestForOpponent
                    (round -infinity)


maxScore : List GameState -> Float -> Float -> Int -> Int
maxScore gameStates bestForCurrentPlayer bestForOpponent bestSoFar =
    case gameStates of
        first :: rest ->
            let
                firstScore = -(scoreWithCutoff first -bestForOpponent -bestForCurrentPlayer)
                newBestForCurrentPlayer = max bestForCurrentPlayer (toFloat firstScore)
                newBestSoFar = max bestSoFar firstScore
            in
                if newBestForCurrentPlayer > bestForOpponent
                    then newBestSoFar
                    else maxScore rest newBestForCurrentPlayer bestForOpponent newBestSoFar
        [] ->
            bestSoFar


infinity : Float
infinity = (1/0)


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
        isMoveTaken = \coordinates -> (GameModel.playerAt coordinates gameState) == Nothing
    in
        List.filter isMoveTaken (GameModel.boardCoordinates gameState)
