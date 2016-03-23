module View.ResetButton (create) where

import Graphics.Collage as Collage
import Graphics.Element as Element exposing (Element)
import Graphics.Input as Input
import Text

import GameLogic.Update exposing (Action (Reset))
import View.Styles as Styles


create : Signal.Address Action -> Element
create address =
    let
        background =
            Collage.filled
                Styles.buttonColor
                (Collage.rect (toFloat Styles.buttonWidth) (toFloat Styles.buttonHeight))
        text =
            Collage.toForm
                <| Element.centered
                <| Text.style Styles.buttonTextStyle
                <| Text.fromString "Reset"
    in
        Input.clickable
            (Signal.message address Reset)
            (Collage.collage Styles.buttonWidth Styles.buttonHeight [ background, text ])
