module MinimaxTests where

import ElmTest exposing (..)

import Minimax exposing (..)
import Model exposing (GameState, Player(X, O), Status(InProgress, Tied, Won))
import TestHelpers as Helpers exposing (x, o)


all : Test
all =
    suite "Applying the minimax algorithm"

        [ suite "Creating subsequent game states"

            [ test "Given an end game state, returns an empty list" <|
                let
                    moves =
                        [ x 0 0, x 0 1, o 0 2
                        , o 1 0, o 1 1, x 1 2
                        , x 2 0, o 2 1, x 2 2
                        ]
                in
                    assertEqual [] (nextGameStates (GameState 3 X moves Tied))

            , test "Returns a game state for every available move on the board" <|
                assertEqual 9 (List.length (nextGameStates (GameState 3 X [] InProgress)))

            , test "Returns all possible subsequent game states" <|
                let
                    moves =
                        [ x 0 0, x 0 1, o 0 2
                        , o 1 0, x 1 1, o 1 2
                        ]
                    potentialGameStates =
                        [ GameState 3 O (x 2 0 :: moves) InProgress
                        , GameState 3 O (x 2 1 :: moves) (Won X)
                        , GameState 3 O (x 2 2 :: moves) (Won X)
                        ]
                in
                    assert
                        <| Helpers.areUnorderedElementsEqual
                            potentialGameStates
                            (nextGameStates (GameState 3 X moves InProgress))
            ]
        , suite "Scoring a game state"

            [ suite "Scoring end game states"

                [ test "Returns zero if the game is tied" <|
                    let
                        moves =
                            [ x 0 0, x 0 1, o 0 2
                            , o 1 0, o 1 1, x 1 2
                            , x 2 0, o 2 1, x 2 2
                            ]
                    in
                        assertEqual 0 (score (GameState 3 X moves Tied))

                , test "The score is the number of available moves if the player has won" <|
                    let
                        moves =
                            [ x 0 0, x 0 1, x 0 2
                            , o 1 0, o 1 1
                            ]
                        numberOfAvailableMoves = 9 - (List.length moves)
                    in
                        assertEqual
                            numberOfAvailableMoves
                            (score (GameState 3 X moves (Won X)))

                , test "The score is negative the number of moves made if the player has lost" <|
                    let
                        moves =
                            [ x 0 0, x 0 1, x 0 2
                            , o 1 0, o 1 1
                            ]
                        numberOfAvailableMoves = 9 - (List.length moves)
                    in
                        assertEqual
                            (-1 * numberOfAvailableMoves)
                            (score (GameState 3 O moves (Won X)))
                ]
            , suite "Scoring in-progress game states"

                [ test "The score is the resulting tie game score if the players will tie" <|
                    let
                        moves =
                            [ x 0 0, x 0 1, o 0 2
                            , o 1 0, o 1 1, x 1 2
                            , x 2 0
                            ]
                    in
                        assertEqual 0 (score (GameState 3 O moves InProgress))

                , test "The score is the resulting won game score if the player will win" <|
                    let
                        moves =
                            [ x 0 0, x 0 1
                            , o 1 0, o 1 1
                            ]
                        numberOfAvailableMovesAtGameEnd = (9 - (List.length moves)) - 1
                    in
                        assertEqual
                            numberOfAvailableMovesAtGameEnd
                            (score (GameState 3 X moves InProgress))

                , test "The score is the resulting lost game score if the player will lose" <|
                    let
                        moves =
                            [ x 0 0, x 0 1
                            , o 1 0, x 1 1
                            , o 2 0
                            ]
                        numberOfAvailableMovesAtGameEnd = (9 - (List.length moves)) - 2
                    in
                        assertEqual
                            (-1 * numberOfAvailableMovesAtGameEnd)
                            (score (GameState 3 O moves InProgress))
                ]
            ]
        ]
