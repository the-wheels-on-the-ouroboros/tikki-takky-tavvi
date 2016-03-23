module GameLogic.ComputerPlayerTests (all) where

import ElmTest exposing (Test, assert, assertEqual, assertNotEqual, suite, test)

import GameModel exposing
    ( Coordinates
    , GameState
    , Move
    , Player(X, O)
    , Status (InProgress, Tied, Won)
    )
import TestHelpers exposing (x, o)
import Utilities

import GameLogic.ComputerPlayer exposing (bestMove, makeMoveVsComputer)


all : Test
all =
    suite "Applying the minimax algorithm"

        [ suite "Making a move versus a computer opponent"

            [ test "Makes given move" <|
                let
                    moves =
                        [ x 0 0, x 0 1, o 0 2
                        , o 1 0, o 1 1, x 1 2
                        , x 2 0
                        ]
                in
                    assert
                        <| List.any ((==) (Move (Coordinates 2 1) O))
                        <| .movesSoFar
                        <| makeMoveVsComputer (Coordinates 2 1) (GameState 3 O moves InProgress)

            , test "Makes a subsequent move for the computer" <|
                let
                    moves =
                        [ x 0 0, x 0 1, o 0 2
                        , o 1 0, o 1 1, x 1 2
                        , x 2 0
                        ]
                in
                    assertEqual (x 2 2 :: o 2 1 :: moves)
                        <| .movesSoFar
                        <| makeMoveVsComputer (Coordinates 2 1)
                        <| GameState 3 O moves InProgress

            , test "The original current player is the current player after both moves" <|
                let
                    player = O
                    moves =
                        [ x 0 0, x 0 1, o 0 2
                        , o 1 0, o 1 1, x 1 2
                        , x 2 0
                        ]
                in
                    assertEqual player
                        <| .currentPlayer
                        <| makeMoveVsComputer (Coordinates 2 1)
                        <| GameState 3 player moves InProgress

            , test "No second move is made if the first move wins the game" <|
                let
                    moves =
                        [ x 0 0, x 0 1
                        , o 1 0, o 1 1
                        ]
                in
                    assertEqual (x 0 2 :: moves)
                        <| .movesSoFar
                        <| makeMoveVsComputer (Coordinates 0 2)
                        <| GameState 3 X moves InProgress

            , test "No second move is made if the first move ties the game" <|
                let
                    moves =
                        [ x 0 0, x 0 1, o 0 2
                        , o 1 0, o 1 1, x 1 2
                        , x 2 0, o 2 1
                        ]
                in
                    assertEqual (x 2 2 :: moves)
                        <| .movesSoFar
                        <| makeMoveVsComputer (Coordinates 2 2)
                        <| GameState 3 X moves InProgress

            , test "Does not make any moves if a move already exists at coordinates" <|
                let
                    gameState = GameState 3 O [ Move (Coordinates 0 0) X ] InProgress
                in
                    assertEqual gameState (makeMoveVsComputer (Coordinates 0 0) gameState)

            , test "Does not make any moves if given game state status is Tied" <|
                let
                    gameState = GameState 3 X [] Tied
                in
                    assertEqual gameState (makeMoveVsComputer (Coordinates 0 0) gameState)

            , test "Does not make any moves if given game state status is Won" <|
                let
                    gameState = GameState 3 X [] (Won X)
                in
                    assertEqual gameState (makeMoveVsComputer (Coordinates 0 0) gameState)
            ]

        , suite "Getting the best next move"

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
