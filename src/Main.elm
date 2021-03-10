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
import Json.Encode
import Notes exposing (Note(..), Octave(..), freq)
import PlayState exposing (PlayAction(..), PlayState, getTimeSinceStart)
import Time exposing (Posix)



---- MODEL ----


type alias Dot =
    { frequency : Float
    , ordinal : Int
    }


type alias Config =
    { dots : List Dot
    , period : Float
    , sineTerms : List Float
    }


linearOrdinals : List Float -> List Dot
linearOrdinals frequencies =
    List.indexedMap (\ordinal frequency -> { frequency = frequency, ordinal = ordinal }) frequencies


simpleConfig : Config
simpleConfig =
    { dots =
        linearOrdinals
            [ freq C Oct2
            , freq D Oct2
            , freq E Oct2
            , freq F Oct2
            , freq G Oct2
            , freq A Oct2
            , freq B Oct2
            , freq C Oct3
            , freq D Oct3
            , freq E Oct3
            , freq F Oct3
            , freq G Oct3
            , freq A Oct3
            , freq B Oct3
            , freq C Oct4
            , freq D Oct4
            , freq E Oct4
            , freq F Oct4
            , freq G Oct4
            , freq A Oct4
            , freq B Oct4
            , freq C Oct5
            ]
    , period = 16000
    , sineTerms = [ 0, 0, 1, 0, 1 ]
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
                                        model.config.period / toFloat dot.ordinal

                                    prevPosition =
                                        dotPosition period previousTimeSinceStart

                                    curPosition =
                                        dotPosition period timeSinceStart
                                in
                                if (prevPosition == 0 && curPosition > 0) || (prevPosition > curPosition) then
                                    Just <| playSoundF model.config.sineTerms dot.frequency

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


playSoundF : List Float -> Float -> Cmd msg
playSoundF sineTerms frequency =
    [ ( "frequency", Json.Encode.float frequency )
    , ( "sineTerms", Json.Encode.list Json.Encode.float sineTerms )
    ]
        |> Json.Encode.object
        |> Json.Encode.encode 0
        |> playSound



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
            sin dp

        cosed =
            cos dp

        ( centerX, centerY ) =
            center
    in
    ( centerX + cosed * radius, centerY + sined * radius )


dotPeriod : number
dotPeriod =
    3000


renderDot : ( Float, Float ) -> Float -> Float -> Int -> Float -> Dot -> Shape
renderDot baseCenter baseRadius basePeriod largestOrdinal timeSinceStart dot =
    let
        period =
            basePeriod / toFloat dot.ordinal

        position =
            dotPosition period timeSinceStart

        largestSizeRadius =
            toFloat 10

        smallestSizeRadius =
            toFloat 3

        ordinalRatio =
            toFloat (dot.ordinal - 1) / toFloat (largestOrdinal - 1)

        dotSizeRadius =
            largestSizeRadius - ordinalRatio * (largestSizeRadius - smallestSizeRadius)

        smallestPositionRadius =
            toFloat 5

        dotPositionRadius =
            baseRadius - ordinalRatio * (baseRadius - smallestPositionRadius)

        center =
            dotCenter position dotPositionRadius baseCenter
    in
    circle center dotSizeRadius


view : Model -> Html Msg
view model =
    let
        center =
            ( w / 2, h / 2 )

        radius =
            240

        timeSinceStart =
            getTimeSinceStart model.currentTime model.playState

        largestOrdinal =
            Maybe.withDefault 1 <| List.maximum (List.map (\x -> x.ordinal) model.config.dots)

        dotList =
            List.map (renderDot center radius model.config.period largestOrdinal timeSinceStart) model.config.dots

        canvas =
            Canvas.toHtml
                ( w, h )
                []
                [ shapes [ fill Color.white ] [ rect ( 0, 0 ) w h ]
                , shapes
                    [ stroke Color.black ]
                    [ Canvas.path center [ lineTo (Tuple.mapFirst (\x -> x + radius) center) ]
                    ]
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
