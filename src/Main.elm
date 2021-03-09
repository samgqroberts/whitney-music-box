port module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Text exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes
import Html.Events exposing (onClick)
import Time exposing (Posix)



---- MODEL ----


type alias Model =
    { startTime : Maybe Float
    , currentTime : Float
    , played : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { startTime = Nothing, currentTime = 0, played = False }, Cmd.none )



---- UPDATE ----


type Msg
    = AnimationFrame Posix
    | PlaySound String
    | Start


getTimeSinceStart : { a | currentTime : number, startTime : Maybe number } -> number
getTimeSinceStart { currentTime, startTime } =
    Maybe.withDefault 0 <| Maybe.map (\x -> currentTime - x) startTime


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | startTime = Just model.currentTime, played = False }, Cmd.none )

        AnimationFrame t ->
            let
                currentTime =
                    t |> Time.posixToMillis |> toFloat

                timeSinceStart =
                    getTimeSinceStart <| { currentTime = currentTime, startTime = model.startTime }

                cmdM =
                    if timeSinceStart > 1000 && not model.played then
                        Just <| playSound "xkcd"

                    else
                        Nothing
            in
            ( { model
                | currentTime = currentTime
                , played = Maybe.withDefault model.played <| Maybe.map (\_ -> True) cmdM
              }
            , Maybe.withDefault Cmd.none cmdM
            )

        PlaySound s ->
            ( { model | played = True }, playSound s )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrame AnimationFrame



---- PORTS ----


port playSound : String -> Cmd msg



---- VIEW ----


w : number
w =
    500


h : number
h =
    500


view : Model -> Html Msg
view model =
    let
        timeSinceStart =
            getTimeSinceStart model

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
        [ canvas, Html.button [ onClick Start, Html.Attributes.id "startButton" ] [ Html.text "start" ] ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
