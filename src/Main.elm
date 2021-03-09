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


c5Freq =
    523.2511306011972


d5Freq =
    587.3295358348151


e5Freq =
    659.2551138257398


g5Freq =
    783.9908719634985


c6Freq =
    1046.5022612023945


type alias Dot =
    { frequency : Float
    , periodMultiple : Int
    }


type alias Config =
    { dots : List Dot
    , period : Float
    }


simpleConfig : Config
simpleConfig =
    { dots =
        [ { frequency = c5Freq, periodMultiple = 1 }
        , { frequency = e5Freq, periodMultiple = 2 }
        , { frequency = g5Freq, periodMultiple = 3 }
        , { frequency = c6Freq, periodMultiple = 4 }
        ]
    , period = 3000
    }


type alias Model =
    { playState : PlayState
    , currentTime : Float
    , config : Config
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { playState = { current = Stopped, history = [] }, currentTime = 0, config = simpleConfig }, Cmd.none )


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
    | Start
    | Stop
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | playState = PlayState.push (Playing model.currentTime) model.playState }, Cmd.none )

        Stop ->
            ( { model | playState = PlayState.push Stopped model.playState }, Cmd.none )

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
                        previousTimeSinceStart =
                            getTimeSinceStart model.currentTime model.playState

                        timeSinceStart =
                            getTimeSinceStart currentTime model.playState

                        maybePlaySound =
                            \dot ->
                                let
                                    period =
                                        model.config.period / toFloat dot.periodMultiple

                                    prevPosition =
                                        dotPosition period previousTimeSinceStart

                                    curPosition =
                                        dotPosition period timeSinceStart
                                in
                                if (prevPosition == 0 && curPosition > 0) || (prevPosition > curPosition) then
                                    Just <| playSound (String.fromFloat dot.frequency)

                                else
                                    Nothing

                        cmdMs =
                            List.map maybePlaySound model.config.dots

                        cmd =
                            Cmd.batch <| List.filterMap identity cmdMs
                    in
                    ( { model | currentTime = currentTime }, cmd )



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


dotPeriod =
    3000


renderDot : ( Float, Float ) -> Float -> Float -> Float -> Dot -> Shape
renderDot baseCenter radius timeSinceStart basePeriod dot =
    let
        period =
            basePeriod / toFloat dot.periodMultiple

        position =
            dotPosition period timeSinceStart

        center =
            dotCenter position radius baseCenter
    in
    circle center 10


view : Model -> Html Msg
view model =
    let
        center =
            ( w / 2, h / 2 )

        radius =
            160

        timeSinceStart =
            getTimeSinceStart model.currentTime model.playState

        dp =
            dotPosition dotPeriod timeSinceStart

        dc =
            dotCenter dp radius center

        dotList =
            List.map (renderDot center radius timeSinceStart model.config.period) model.config.dots

        canvas =
            Canvas.toHtml
                ( w, h )
                []
                [ shapes [ fill Color.white ] [ rect ( 0, 0 ) w h ]
                , shapes [ stroke Color.black ] [ circle center radius ]
                , shapes [ fill Color.blue ] dotList
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
