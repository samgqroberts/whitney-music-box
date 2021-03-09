port module Main exposing (..)

import Basics.Extra exposing (fractionalModBy)
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
import PlayState exposing (PlayAction(..), PlayState, getTimeSinceStart)
import Time exposing (Posix)



---- MODEL ----


type alias Model =
    { playState : PlayState
    , currentTime : Float
    , played : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { playState = { current = Stopped, history = [] }, currentTime = 0, played = False }, Cmd.none )


{-| Convert a timeSinceStart value to the position of the dot along the circumference
of the circle (in radians, modded by 2pi).

period: the time it takes the dot to complete a revolution, in ms
timeSinceStart: the time since the program was started, in ms

-}
dotPosition : Float -> Float -> Float
dotPosition period timeSinceStart =
    if period == 0 then
        0

    else
        (fractionalModBy period timeSinceStart / period) * (2 * pi)



---- UPDATE ----


type Msg
    = AnimationFrame Posix
    | PlaySound String
    | Start
    | Stop
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | playState = PlayState.push (Playing model.currentTime) model.playState, played = False }, Cmd.none )

        Stop ->
            ( { model | playState = PlayState.push Stopped model.playState, played = False }, Cmd.none )

        Pause ->
            ( { model | playState = PlayState.push (Paused model.currentTime) model.playState }, Cmd.none )

        AnimationFrame currentPosix ->
            let
                currentTime =
                    currentPosix |> Time.posixToMillis |> toFloat
            in
            case model.playState.current of
                Stopped ->
                    ( { model | currentTime = currentTime }, Cmd.none )

                Paused _ ->
                    ( { model | currentTime = currentTime }, Cmd.none )

                Playing _ ->
                    let
                        timeSinceStart =
                            getTimeSinceStart currentTime model.playState

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


dotCenter : Float -> Float -> ( Float, Float ) -> ( Float, Float )
dotCenter dp radius center =
    let
        sined =
            sin (dp - (pi / 2))

        cosed =
            cos (dp - (pi / 2))

        ( centerX, centerY ) =
            center
    in
    ( centerX + cosed * radius, centerY + sined * radius )


view : Model -> Html Msg
view model =
    let
        center =
            ( w / 2, h / 2 )

        radius =
            160

        dotRadius =
            10

        dotPeriod =
            3000

        timeSinceStart =
            getTimeSinceStart model.currentTime model.playState

        dp =
            dotPosition dotPeriod timeSinceStart

        dc =
            dotCenter dp radius center

        canvas =
            Canvas.toHtml
                ( w, h )
                []
                [ shapes [ fill Color.white ] [ rect ( 0, 0 ) w h ]
                , shapes [ stroke Color.black ] [ circle center radius ]
                , shapes [ fill Color.blue ] [ circle dc dotRadius ]
                ]
    in
    div []
        [ canvas
        , Html.button [ onClick Start, Html.Attributes.id "startButton" ] [ Html.text "start" ]
        , Html.button [ onClick Stop ] [ Html.text "stop" ]
        , Html.button [ onClick Pause ] [ Html.text "pause" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
