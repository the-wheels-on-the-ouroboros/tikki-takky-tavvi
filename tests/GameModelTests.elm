module GameModelTests (all) where

import ElmTest exposing (Test, assertEqual, suite, test)

import GameModel exposing
    ( Coordinates
    , GameState
    , Move
    , Player (X, O)
    , Status (InProgress, Tied, Won)
    , boardCoordinates
    , playerAt
    )

all : Test
all =
    suite "Using the game model"

        [ suite "Getting the board coordinates"

            [ test "A 3x3 board has 9 coordinates" <|
                assertEqual 9 (List.length (boardCoordinates (GameState 3 X [] InProgress)))

            , test "A 3x3 board has coordinates from [0,0] to [2,2]" <|
                let
                    coordinates =
                        [ (Coordinates 0 0), (Coordinates 0 1), (Coordinates 0 2)
                        , (Coordinates 1 0), (Coordinates 1 1), (Coordinates 1 2)
                        , (Coordinates 2 0), (Coordinates 2 1), (Coordinates 2 2)
                        ]
                in
                    assertEqual coordinates (boardCoordinates (GameState 3 X [] InProgress))
            ]
        , suite "Getting the player who made a move" <|

            [ test "Returns Nothing if the is no move at the given coordinates" <|
                assertEqual Nothing
                    <| playerAt (Coordinates 0 0)
                    <| GameState 3 X [] InProgress

            , test "Returns the player who moved at the given coordinates" <|
                let
                    gameState = GameState 3 X [ Move (Coordinates 0 0) X ] InProgress
                in
                    assertEqual (Just X)
                        <| playerAt (Coordinates 0 0)
                        <| GameState 3 X [ Move (Coordinates 0 0) X ] InProgress
            ]
        ]
