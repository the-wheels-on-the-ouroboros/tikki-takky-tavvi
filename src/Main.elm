import StartApp.Simple exposing (start)

import Model
import Update
import View.View as View


main =
    start
        { model = Model.initialGameState
        , update = Update.makeMove
        , view = View.render
        }
