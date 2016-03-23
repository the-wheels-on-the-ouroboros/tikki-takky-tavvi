module View.Title (create) where

import Graphics.Collage as Collage
import Graphics.Element as Element exposing (Element)
import Graphics.Input as Input
import Text

import GameLogic.Update exposing (Action (Reset))
import View.Styles as Styles


create : Element
create =
    let
        text =
            Element.centered
                <| Text.style Styles.titleTextStyle
                <| Text.fromString " Elm Tic Tac Toe"
        logoBackgroundSize = round (1.1 * (toFloat Styles.logoHeight))
        logo =
            Element.color Styles.logoBackgroundColor
                <| Element.container logoBackgroundSize logoBackgroundSize Element.middle
                <| Element.image Styles.logoHeight Styles.logoHeight Styles.logoUrl
    in
        Element.flow Element.right [ logo, text ]
