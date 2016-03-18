import StartApp.Simple exposing (start)

import GameModel as Model
import GameLogic.ComputerPlayer as Update
import View.View as View


main =
    start
        { model = Model.initialGameState
        , update = Update.makeMoveVsComputer
        , view = View.render
        }
