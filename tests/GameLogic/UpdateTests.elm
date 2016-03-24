module GameLogic.UpdateTests (all) where

import Effects exposing (Effects)
import ElmTest exposing (Test, assert, assertEqual, assertNotEqual, suite, test)
import Task

import GameLogic.HandleTurn as HandleTurn
import GameModel exposing (Coordinates, GameState, Move, Player (X, O), Status (InProgress))
import TestHelpers exposing (x, o)

import GameLogic.Update exposing (Action (ComputerMove, MoveInput, Reset), update)


getGameState : ( GameState, Effects Action ) -> GameState
getGameState ( gameState, _ ) = gameState


getEffects : ( GameState, Effects Action ) -> Effects Action
getEffects ( _, effects ) = effects


all : Test
all =
    suite "Updating the game state"

        [ suite "When a Reset action is triggered"

            [ test "Returns the initial game state with no effects" <|
                assertEqual
                    GameModel.initialGameState
                    (getGameState (update Reset (GameState 3 False O [ x 0 0 ] InProgress)))

            , test "Changes nothing if it is the computer's turn" <|
                let
                    gameState = GameState 3 True X [ x 1 1 ] InProgress
                in
                    assertEqual gameState (getGameState (update Reset gameState))

            , test "Triggers no effects" <|
                assertEqual
                    Effects.none
                    (getEffects (update Reset (GameState 3 False O [ x 0 0 ] InProgress)))
            ]

        , suite "When a MoveInput action is triggered"

            [ test "Makes a player move at the given coordinates" <|
                let
                    coordinates = Coordinates 1 1
                    gameState = GameState 3 False X [] InProgress
                in
                    assertEqual [ (x 1 1) ]
                        <| .movesSoFar
                        <| getGameState
                        <| update (MoveInput coordinates) gameState

            , test "Changes nothing if it is the computer's turn" <|
                let
                    gameState = GameState 3 True X [ x 1 1 ] InProgress
                in
                    assertEqual
                        gameState
                        (getGameState (update (MoveInput (Coordinates 0 0)) gameState))

            , test "Returns a game state where it is the computer's turn if move was valid" <|
                assertEqual True
                    <| .isComputerTurn
                    <| getGameState
                    <| update (MoveInput (Coordinates 0 0))
                    <| GameState 3 False X [] InProgress

            , test "Returns a game state where it is not the computer's turn if move was invalid" <|
                assertEqual False
                    <| .isComputerTurn
                    <| getGameState
                    <| update (MoveInput (Coordinates 0 0))
                    <| GameState 3 False X [ x 0 0 ] InProgress

            , test "Triggers a ComputerMove action if the move was valid" <|
                assertNotEqual Effects.none
                    <| getEffects
                    <| update (MoveInput (Coordinates 0 0))
                    <| GameState 3 False X [] InProgress

            , test "Triggers nothing if the move was invalid" <|
                assertEqual Effects.none
                    <| getEffects
                    <| update (MoveInput (Coordinates 0 0))
                    <| GameState 3 False O [ x 0 0 ] InProgress
            ]

        , suite "When a ComputerMove action is triggered"

            [ test "Makes the computer move if given coordinates" <|
                let
                    coordinates = Coordinates 1 1
                    gameState = GameState 3 False X [] InProgress
                in
                    assertEqual
                        (HandleTurn.makeMove coordinates gameState)
                        (getGameState (update (ComputerMove (Just coordinates)) gameState))

            , test "Returns a game state where it is no longer the computer's turn" <|
                assertEqual False
                    <| .isComputerTurn
                    <| getGameState
                    <| update (ComputerMove (Just (Coordinates 0 0)))
                    <| GameState 3 True X [] InProgress

            , test "Changes nothing if not given coordinates" <|
                let
                    gameState = GameState 3 False X [] InProgress
                in
                    assertEqual gameState (getGameState (update (ComputerMove Nothing) gameState))

            , test "Triggers no effects" <|
                assertEqual Effects.none
                    <| getEffects
                    <| update (ComputerMove (Just (Coordinates 0 0)))
                    <| GameState 3 False X [] InProgress
            ]
        ]
