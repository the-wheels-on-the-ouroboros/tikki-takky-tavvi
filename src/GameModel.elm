module GameModel (
        Coordinates,
        GameState,
        Move,
        Player (X, O),
        Status (InProgress, Tied, Won),
        boardCoordinates,
        initialGameState,
        playerAt
    ) where


type Player = X | O


type Status = InProgress | Tied | Won Player


type alias Coordinates =
    { row : Int
    , col : Int
    }


type alias Move =
    { coordinates : Coordinates
    , player : Player
    }


type alias GameState =
    { boardSize : Int
    , currentPlayer : Player
    , movesSoFar : List Move
    , status : Status
    }


initialGameState : GameState
initialGameState =
    GameState 3 X [] InProgress


boardCoordinates : GameState -> List Coordinates
boardCoordinates gameState =
    let
        indices = [0..(gameState.boardSize - 1)]
    in
        List.concatMap (\row -> List.map (\col -> Coordinates row col) indices) indices


playerAt : Coordinates -> GameState -> Maybe Player
playerAt coordinates gameState =
    case List.filter (\move -> move.coordinates == coordinates) gameState.movesSoFar of
        move::_ -> Just move.player
        _ -> Nothing
