module GameLogic.Update (Action (ComputerMove, MoveInput, Reset), update) where

import Effects exposing (Effects)
import Task

import GameLogic.ComputerPlayer as ComputerPlayer
import GameLogic.HandleTurn as HandleTurn
import GameModel exposing (Coordinates, GameState)


type Action = MoveInput Coordinates | ComputerMove (Maybe Coordinates) | Reset


update : Action -> GameState -> (GameState, Effects Action)
update action gameState =
    case action of
        MoveInput coordinates ->
            let
                nextGameState = HandleTurn.makeMove coordinates gameState
            in
                if gameState.isComputerTurn || nextGameState == gameState
                    then ( gameState, Effects.none )
                    else ( { nextGameState | isComputerTurn = True }, makeComputerMove nextGameState )
        ComputerMove (Just coordinates) ->
            let
                nextGameState = HandleTurn.makeMove coordinates gameState
            in
                ( { nextGameState | isComputerTurn = False }, Effects.none )
        ComputerMove Nothing ->
            ( { gameState | isComputerTurn = False }, Effects.none )
        Reset ->
            if gameState.isComputerTurn
                then ( gameState, Effects.none )
                else ( GameModel.initialGameState, Effects.none )


makeComputerMove : GameState -> Effects Action
makeComputerMove gameState =
    Effects.task (Task.map (ComputerMove << ComputerPlayer.bestMove) (Task.succeed gameState))
