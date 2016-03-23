import Effects exposing (Never)
import StartApp exposing (start)
import Task

import GameModel as Model
import GameLogic.Update as Update
import View.View as View


app =
    start
        { inputs = []
        , init = (Model.initialGameState, Effects.none)
        , update = Update.update
        , view = View.render
        }


main =
    app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
