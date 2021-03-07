module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Text exposing (..)
import Color exposing (Color)
import Debug
import Html exposing (Html, div, h1, img, text)
import Html.Attributes as Attributes exposing (src)
import Time exposing (Posix)



---- MODEL ----


type alias Model =
    { time : Float }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = AnimationFrame Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { time } =
    case msg of
        AnimationFrame t ->
            ( { time = t |> Time.posixToMillis |> toFloat }, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions { time } =
    onAnimationFrame AnimationFrame



---- VIEW ----


w =
    500


h =
    500


view : Model -> Html Msg
view { time } =
    let
        center =
            ( w / 2, h / 2 )

        radius =
            160

        dotRadius =
            10

        t =
            time / 1000

        sined =
            sin t

        cosed =
            cos t

        dotCenter =
            ( w / 2 + cosed * radius, h / 2 + sined * radius )

        canvas =
            Canvas.toHtml
                ( w, h )
                []
                [ shapes [ fill Color.white ] [ rect ( 0, 0 ) w h ]
                , shapes [ stroke Color.black ] [ circle center radius ]
                , shapes [ fill Color.blue ] [ circle dotCenter dotRadius ]
                ]
    in
    div []
        [ canvas ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
