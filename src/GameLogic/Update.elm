module GameLogic.Update (Action (MoveInput, Reset), update) where

import GameLogic.ComputerPlayer as ComputerPlayer
import GameModel exposing (Coordinates, GameState)


type Action = MoveInput Coordinates | Reset


update : Action -> GameState -> GameState
update action gameState =
    case action of
        MoveInput coordinates ->
            ComputerPlayer.makeMoveVsComputer coordinates gameState
        Reset ->
            GameModel.initialGameState
