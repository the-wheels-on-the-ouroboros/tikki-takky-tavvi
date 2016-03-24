module Main where

import Effects exposing (Never)
import StartApp exposing (start)
import Task exposing (Task)

import GameModel as Model
import GameLogic.Update as Update
import View.View as View


app =
    start
        { init = (Model.initialGameState, Effects.none)
        , inputs = []
        , update = Update.update
        , view = View.render
        }


main =
    app.html


port tasks : Signal (Task Never ())
port tasks =
  app.tasks
