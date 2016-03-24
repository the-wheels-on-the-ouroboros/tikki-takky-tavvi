module View.Title (create) where

import Graphics.Collage as Collage
import Graphics.Element as Element exposing (Element)
import Graphics.Input as Input
import Text

import GameLogic.Update exposing (Action (Reset))
import View.Styles as Styles
import View.Utilities as ViewUtil


create : Element
create =
    let
        text = ViewUtil.toText " Elm Tic Tac Toe" Styles.titleTextStyle
        logoBackgroundSize = round (1.1 * (toFloat Styles.logoHeight))
        logo =
            Element.color Styles.logoBackgroundColor
                <| Element.container logoBackgroundSize logoBackgroundSize Element.middle
                <| Element.image Styles.logoHeight Styles.logoHeight Styles.logoUrl
    in
        Element.flow Element.right [ logo, text ]
