import StartApp.Simple exposing (start)

import Model
import Update
import View


main =
  start
    { model = Model.initialGameState
    , update = Update.makeMove
    , view = View.render
    }
