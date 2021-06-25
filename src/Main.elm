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
import Html exposing (Html, a, button, div, li, p, ul)
import Html.Attributes
import Html.Events exposing (onClick)
import Json.Encode
import List
import Notes exposing (Note(..), Octave(..), freq)
import PlayState exposing (PlayAction(..), PlayState, getTimeSinceStart)
import Time exposing (Posix)



---- MODEL ----


type alias AppConfig =
    { baseRadius : Float
    , padding : Float
    , smallestSizeRadius : Float
    , largestSizeRadius : Float
    }


type alias Dot =
    { frequency : Float
    , ordinal : Int
    }


type ConfigMetadata
    = ConfigMetadata String


configName : ScenarioConfig -> String
configName c =
    case c.metadata of
        ConfigMetadata name ->
            name


type alias ScenarioConfig =
    { metadata : ConfigMetadata
    , dots : List Dot
    , period : Float
    , sineTerms : List Float
    }


linearOrdinals : List Float -> List Dot
linearOrdinals frequencies =
    List.indexedMap (\ordinal frequency -> { frequency = frequency, ordinal = ordinal }) frequencies


cMajor3OctConfig : ScenarioConfig
cMajor3OctConfig =
    { metadata = ConfigMetadata "C Major over 3 octaves"
    , dots =
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
    , period = 24000
    , sineTerms = [ 0, 0, 1, 0, 1 ]
    }


chromatic4OctConfig : ScenarioConfig
chromatic4OctConfig =
    { metadata = ConfigMetadata "Chromatic scale over 4 octaves"
    , dots =
        linearOrdinals
            [ freq A Oct2
            , freq Ashp Oct2
            , freq B Oct2
            , freq C Oct2
            , freq D Oct2
            , freq Dshp Oct2
            , freq E Oct2
            , freq F Oct2
            , freq Fshp Oct2
            , freq G Oct2
            , freq Gshp Oct2
            , freq A Oct3
            , freq Ashp Oct3
            , freq B Oct3
            , freq C Oct3
            , freq D Oct3
            , freq Dshp Oct3
            , freq E Oct3
            , freq F Oct3
            , freq Fshp Oct3
            , freq G Oct3
            , freq Gshp Oct3
            , freq A Oct4
            , freq Ashp Oct4
            , freq B Oct4
            , freq C Oct4
            , freq D Oct4
            , freq Dshp Oct4
            , freq E Oct4
            , freq F Oct4
            , freq Fshp Oct4
            , freq G Oct4
            , freq Gshp Oct4
            , freq A Oct5
            , freq Ashp Oct5
            , freq B Oct5
            , freq C Oct5
            , freq D Oct5
            , freq Dshp Oct5
            , freq E Oct5
            , freq F Oct5
            , freq Fshp Oct5
            , freq G Oct5
            , freq Gshp Oct5
            ]
    , period = 32000
    , sineTerms = [ 0, 0, 1, 0, 1 ]
    }


presets : List ScenarioConfig
presets =
    [ cMajor3OctConfig, chromatic4OctConfig ]


type alias Model =
    { playState : PlayState
    , currentTime : Float
    , config : ScenarioConfig
    , appConfig : AppConfig
    }


emptyPlayState : PlayState
emptyPlayState =
    { current = Stopped, history = [] }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { playState = emptyPlayState
      , currentTime = 0
      , config = cMajor3OctConfig
      , appConfig =
            { baseRadius = 250
            , padding = 10
            , largestSizeRadius = 10
            , smallestSizeRadius = 3
            }
      }
    , Cmd.none
    )


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
    | SetConfig ScenarioConfig


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | playState = PlayState.push (Playing model.currentTime) model.playState }, Cmd.none )

        Stop ->
            ( { model | playState = PlayState.push Stopped model.playState }, Cmd.none )

        Pause ->
            ( { model | playState = PlayState.push (Paused model.currentTime) model.playState }, Cmd.none )

        SetConfig c ->
            ( { model | config = c, playState = emptyPlayState }, Cmd.none )

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


getDotSizeRadius : Float -> Float -> Int -> Int -> Float
getDotSizeRadius largestSizeRadius smallestSizeRadius largestOrdinal ordinal =
    let
        ordinalRatio =
            toFloat (ordinal - 1) / toFloat (largestOrdinal - 1)
    in
    largestSizeRadius - ordinalRatio * (largestSizeRadius - smallestSizeRadius)


getSpaceBetweenDots : Float -> Float -> Float -> Int -> Float
getSpaceBetweenDots baseRadius largestSizeRadius smallestSizeRadius largestOrdinal =
    let
        summed =
            List.range 1 largestOrdinal
                |> List.map (getDotSizeRadius largestSizeRadius smallestSizeRadius largestOrdinal)
                |> List.map (\x -> x * 2)
                |> List.sum
    in
    (baseRadius - summed) / (toFloat largestOrdinal - 1)


getDotPositionRadius : Float -> Float -> Float -> Float -> Int -> Int -> Float
getDotPositionRadius baseRadius spaceBetweenDots largestSizeRadius smallestSizeRadius largestOrdinal ordinal =
    let
        summed =
            List.range 1 ordinal
                |> List.map (getDotSizeRadius largestSizeRadius smallestSizeRadius largestOrdinal)
                |> List.map (\x -> x * 2)
                |> List.sum

        thisSizeRadius =
            getDotSizeRadius largestSizeRadius smallestSizeRadius largestOrdinal ordinal
    in
    (baseRadius - (spaceBetweenDots * toFloat ordinal)) - summed + thisSizeRadius


renderDot : ( Float, Float ) -> Float -> Float -> Float -> Float -> Int -> Float -> Dot -> Shape
renderDot baseCenter baseRadius basePeriod largestSizeRadius smallestSizeRadius largestOrdinal timeSinceStart dot =
    let
        period =
            basePeriod / toFloat dot.ordinal

        position =
            dotPosition period timeSinceStart

        dotSizeRadius =
            getDotSizeRadius largestSizeRadius smallestSizeRadius largestOrdinal dot.ordinal

        spaceBetweenDots =
            getSpaceBetweenDots baseRadius largestSizeRadius smallestSizeRadius largestOrdinal

        dotPositionRadius =
            getDotPositionRadius baseRadius spaceBetweenDots largestSizeRadius smallestSizeRadius largestOrdinal dot.ordinal

        center =
            dotCenter position dotPositionRadius baseCenter
    in
    circle center dotSizeRadius


renderCanvas : Float -> Float -> Float -> PlayState -> List Dot -> Float -> Float -> Float -> Html Msg
renderCanvas baseRadius padding currentTime playState dots period largestSizeRadius smallestSizeRadius =
    let
        center =
            ( baseRadius + padding / 2, baseRadius + padding / 2 )

        width =
            baseRadius * 2 + padding * 2

        height =
            baseRadius * 2 + padding * 2

        timeSinceStart =
            getTimeSinceStart currentTime playState

        largestOrdinal =
            Maybe.withDefault 1 <| List.maximum (List.map (\x -> x.ordinal) dots)

        dotList =
            List.map (renderDot center baseRadius period largestSizeRadius smallestSizeRadius largestOrdinal timeSinceStart) dots
    in
    Canvas.toHtml
        ( round width, round height )
        []
        [ shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]
        , shapes
            [ stroke Color.black ]
            [ Canvas.path center [ lineTo (Tuple.mapFirst (\x -> x + baseRadius) center) ]
            ]
        , shapes [ fill Color.blue ] dotList
        ]


view : Model -> Html Msg
view model =
    div []
        [ description
        , div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "justify-content" "space-around"
            ]
            [ configControls
            , renderCanvas model.appConfig.baseRadius model.appConfig.padding model.currentTime model.playState model.config.dots model.config.period model.appConfig.largestSizeRadius model.appConfig.smallestSizeRadius
            , information model.config
            ]
        , buttonToolbar
        ]


renderConfigControl : ScenarioConfig -> Html Msg
renderConfigControl c =
    li [ Html.Attributes.style "margin" "10px 0" ] [ button [ onClick <| SetConfig c ] [ Html.text <| configName c ] ]


configControls : Html Msg
configControls =
    List.map renderConfigControl presets
        |> ul [ Html.Attributes.style "list-style" "none" ]


information : ScenarioConfig -> Html Msg
information c =
    ul []
        [ li [] [ Html.text <| "Base Period: " ++ String.fromFloat (c.period / 1000) ++ "s" ]
        , li [] [ Html.text <| "Num Dots: " ++ String.fromInt (List.length c.dots) ]
        ]


description : Html Msg
description =
    div []
        [ p [] [ Html.text "The Whitney Music Box was originally devised and implemented by Jim Bumgardner." ]
        , p [] [ Html.text "Each dot revolves at some whole-number multiple of the speed of the outermost dot. Eg. The outermost dot may complete a revolution in 32 seconds, the next will complete a revolution in 16 seconds, the next 8, etc." ]
        ]


buttonToolbar : Html Msg
buttonToolbar =
    div []
        [ Html.button [ onClick Start, Html.Attributes.id "startButton" ] [ Html.text "start" ]
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
