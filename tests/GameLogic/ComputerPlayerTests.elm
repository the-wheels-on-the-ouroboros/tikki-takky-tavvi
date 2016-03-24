module GameLogic.ComputerPlayerTests (all) where

import ElmTest exposing (Test, assert, assertEqual, assertNotEqual, suite, test)

import GameModel exposing (Coordinates, GameState, Move, Player (X, O), Status (InProgress, Tied))
import TestHelpers exposing (x, o)
import Utilities

import GameLogic.ComputerPlayer exposing (bestMove)


all : Test
all =
    suite "Applying the minimax algorithm"

        [ suite "Getting the best next move"

            [ test "Returns nothing when no moves are possible" <|
                let
                    moves =
                        [ x 0 0, x 0 1, o 0 2
                        , o 1 0, o 1 1, x 1 2
                        , x 2 0, o 2 1, x 2 2
                        ]
                in
                    assertEqual Nothing (bestMove (GameState 3 X moves Tied))

            , test "Chooses a winning move" <|
                let
                    moves =
                        [ x 0 0, x 0 1
                        , o 1 0
                        , o 2 0
                        ]
                in
                    assertEqual (Just (Coordinates 0 2)) (bestMove (GameState 3 X moves InProgress))

            , test "Chooses a move that blocks the opponent from winning" <|
                let
                    moves =
                        [ x 0 0
                        , o 1 0, o 1 1
                        , x 2 0
                        ]
                in
                    assertEqual (Just (Coordinates 1 2)) (bestMove (GameState 3 X moves InProgress))

            , test "Chooses a move that creates a fork" <|
                let
                    moves =
                        [ x 0 0
                        , o 1 0
                        , x 2 0, o 2 1
                        ]
                in
                    assertEqual (Just (Coordinates 1 1)) (bestMove (GameState 3 X moves InProgress))

            , test "Chooses a winning move over a blocking move" <|
                let
                    moves =
                        [ x 0 0, x 0 1
                        , o 1 0, o 1 1
                        ]
                in
                    assertEqual (Just (Coordinates 0 2)) (bestMove (GameState 3 X moves InProgress))

            , test "Chooses a winning move over a fork" <|
                let
                    moves =
                        [ x 0 0, o 0 1, o 0 2
                        , x 1 0
                        ]
                in
                    assertEqual (Just (Coordinates 2 0)) (bestMove (GameState 3 X moves InProgress))

            , test "Chooses a blocking move over a fork" <|
                let
                    moves =
                        [ o 0 0, x 0 1, x 0 2
                        , o 1 0
                        ]
                in
                    assertEqual (Just (Coordinates 2 0)) (bestMove (GameState 3 X moves InProgress))

            , test "Avoids making a move that results in a sure loss - scenario 1" <|
                let
                    moves = [ x 0 1, o 2 1, x 1 0 ]
                in
                    assertNotEqual
                        (Just (Coordinates 2 2))
                        (bestMove (GameState 3 O [ x 0 1, o 2 1, x 1 0 ] InProgress))

            , test "Avoids making a move that results in a sure loss - scenario 2" <|
                let
                    moves = [ x 0 0, x 0 2, o 1 1, x 1 2, o 2 0, x 2 1, o 2 2 ]
                in
                    assertNotEqual
                        (Just (Coordinates 1 0))
                        (bestMove (GameState 3 O moves InProgress))

            , test "Avoids making a move that results in a sure loss - scenario 3" <|
                let
                    moves = [ x 0 1, x 0 2, x 1 0, o 1 1, o 1 2, o 2 0, x 2 1 ]
                in
                    assertNotEqual
                        (Just (Coordinates 2 2))
                        (bestMove (GameState 3 O moves InProgress))
            ]
        ]
