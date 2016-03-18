module GameLogic.NegamaxTests where

import ElmTest exposing (Test, assert, assertEqual, suite, test)

import GameModel exposing (Coordinates, GameState, Move, Player(X, O), Status(InProgress, Tied, Won))
import TestHelpers exposing (x, o)
import Utilities

import GameLogic.Negamax exposing (bestMove, makeMoveVsComputer, nextGameStates, score)


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
            ]

        , suite "Creating subsequent game states"

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
                        <| Utilities.areElementsEqual
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
