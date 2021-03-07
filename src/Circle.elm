module Circle exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Text exposing (..)
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as Attributes


main =
    view


w =
    500


h =
    500


view =
    let
        center =
            ( w / 2, h / 2 )

        radius =
            160
        
        dotRadius = 10
    in
    Canvas.toHtml
        ( w, h )
        []
        [ shapes [  fill Color.white ] [ rect ( 0, 0 ) w h ]
        , shapes [ stroke Color.black ] [ circle center radius ]
        , shapes [ fill Color.blue ] [ circle (Tuple.mapSecond (\y -> y - radius) center) dotRadius ]
        ]