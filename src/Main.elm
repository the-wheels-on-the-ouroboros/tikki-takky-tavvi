import StartApp.Simple exposing (start)

import Model
import Update
import View

initialGameState = Model.GameState Model.X []

main =
  start { model = initialGameState, update = Update.makeMove, view = View.render }
