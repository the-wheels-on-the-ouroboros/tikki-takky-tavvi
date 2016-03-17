import StartApp.Simple exposing (start)

import GameModel as Model
import GameLogic.UpdateState as Update
import View.View as View


main =
    start
        { model = Model.initialGameState
        , update = Update.makeMove
        , view = View.render
        }
