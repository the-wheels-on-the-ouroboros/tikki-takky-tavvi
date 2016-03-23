import StartApp.Simple exposing (start)

import GameModel as Model
import GameLogic.Update as Update
import View.View as View


main =
    start
        { model = Model.initialGameState
        , update = Update.update
        , view = View.render
        }
