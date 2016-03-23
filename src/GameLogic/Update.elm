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
                if nextGameState == gameState
                    then ( nextGameState, Effects.none )
                    else ( nextGameState, makeComputerMove nextGameState )
        ComputerMove (Just coordinates) ->
            ( HandleTurn.makeMove coordinates gameState, Effects.none )
        ComputerMove Nothing ->
            ( gameState, Effects.none )
        Reset ->
            ( GameModel.initialGameState, Effects.none )


makeComputerMove : GameState -> Effects Action
makeComputerMove gameState =
    Effects.task (Task.map (ComputerMove << ComputerPlayer.bestMove) (Task.succeed gameState))
