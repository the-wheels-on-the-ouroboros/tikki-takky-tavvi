module GameLogic.UpdateTests (all) where

import ElmTest exposing (Test, assert, assertEqual, suite, test)

import GameLogic.Update exposing (Action (MoveInput, Reset), update)
import GameModel exposing (Coordinates, GameState, Player (X, O), Status (InProgress))
import TestHelpers exposing (x, o)


all : Test
all =
    suite "Updating the game state"

        [ test "Returns initial game state when the Reset action is triggered" <|
            assertEqual GameModel.initialGameState
                <| update Reset
                <| GameState 3 O [ x 0 0 ] InProgress

        , test "Makes a player move and computer move when the MoveInput action is triggered" <|
            let
                moves =
                    [ x 0 0, x 0 1, o 0 2
                    , o 1 0, o 1 1, x 1 2
                    , x 2 0
                    ]
            in
                assertEqual (x 2 2 :: o 2 1 :: moves)
                    <| .movesSoFar
                    <| update (MoveInput (Coordinates 2 1))
                    <| GameState 3 O moves InProgress
        ]
