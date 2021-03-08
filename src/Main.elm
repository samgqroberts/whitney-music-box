module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Text exposing (..)
import Color
import Html exposing (Html, div)
import Time exposing (Posix)



---- MODEL ----


type alias Model =
    { startTime : Maybe Float, currentTime : Float }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { startTime = Nothing, currentTime = 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = AnimationFrame Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame t ->
            let
                currentTime =
                    t |> Time.posixToMillis |> toFloat

                startTime =
                    case model.startTime of
                        Just s ->
                            Just s

                        Nothing ->
                            Just currentTime
            in
            ( { model | currentTime = currentTime, startTime = startTime }, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrame AnimationFrame



---- VIEW ----


w : number
w =
    500


h : number
h =
    500


view : Model -> Html Msg
view { startTime, currentTime } =
    let
        timeSinceStart =
            Maybe.withDefault 0 <| Maybe.map (\x -> currentTime - x) startTime

        center =
            ( w / 2, h / 2 )

        radius =
            160

        dotRadius =
            10

        t =
            timeSinceStart / 1000

        sined =
            sin (t - (pi / 2))

        cosed =
            cos (t - (pi / 2))

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
