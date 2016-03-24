module View.ResetButton (create) where

import Graphics.Collage as Collage
import Graphics.Element as Element exposing (Element)
import Graphics.Input as Input
import Text

import GameLogic.Update exposing (Action (Reset))
import View.Styles as Styles
import View.Utilities as ViewUtil


create : Signal.Address Action -> Element
create address =
    let
        text = Collage.toForm (ViewUtil.toText "Reset" Styles.buttonTextStyle)
        background =
            Collage.filled Styles.buttonColor
                <| Collage.rect (toFloat Styles.buttonWidth) (toFloat Styles.buttonHeight)
    in
        Input.clickable
            (Signal.message address Reset)
            (Collage.collage Styles.buttonWidth Styles.buttonHeight [ background, text ])
