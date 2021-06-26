port module Main exposing (..)

import Basics.Extra exposing (fractionalModBy)
import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Text exposing (..)
import Color exposing (rgba)
import Html exposing (Html, a, div, li, p, ul)
import Html.Attributes
import Html.Events exposing (onClick)
import Json.Encode
import List
import Material.Icons as Filled
import Material.Icons.Types
import Notes exposing (Note(..), Octave(..), freq)
import PlayState exposing (PlayAction(..), PlayState, getTimeSinceStart)
import Time exposing (Posix)



---- MODEL ----


type alias AppConfig =
    { baseRadius : Float
    , padding : Float
    , innerPadding : Float
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
    List.indexedMap (\ordinal frequency -> { frequency = frequency, ordinal = ordinal + 1 }) frequencies


cMajor3OctConfig : ScenarioConfig
cMajor3OctConfig =
    { metadata = ConfigMetadata "C Major scale over 3 octaves"
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


cMajorTriad5OctConfig : ScenarioConfig
cMajorTriad5OctConfig =
    { metadata = ConfigMetadata "C Major triad over 5 octaves"
    , dots =
        linearOrdinals
            [ freq C Oct1
            , freq E Oct1
            , freq G Oct1
            , freq C Oct2
            , freq E Oct2
            , freq G Oct2
            , freq C Oct3
            , freq E Oct3
            , freq G Oct3
            , freq C Oct4
            , freq E Oct4
            , freq G Oct4
            , freq C Oct5
            , freq E Oct5
            , freq G Oct5
            , freq C Oct6
            ]
    , period = 24000
    , sineTerms = [ 0, 0, 1, 0, 1 ]
    }


gDom5OctConfig : ScenarioConfig
gDom5OctConfig =
    { metadata = ConfigMetadata "G dominant chord over 5 octaves"
    , dots =
        linearOrdinals
            [ freq G Oct1
            , freq B Oct1
            , freq D Oct2
            , freq F Oct2
            , freq G Oct2
            , freq B Oct2
            , freq D Oct3
            , freq F Oct3
            , freq G Oct3
            , freq B Oct3
            , freq D Oct4
            , freq F Oct4
            , freq G Oct4
            , freq B Oct4
            , freq D Oct5
            , freq F Oct5
            , freq G Oct5
            , freq B Oct5
            , freq D Oct6
            , freq F Oct6
            , freq G Oct6
            ]
    , period = 24000
    , sineTerms = [ 0, 0, 1, 0, 1 ]
    }


aMinor75OctConfig : ScenarioConfig
aMinor75OctConfig =
    { metadata = ConfigMetadata "A minor 7 chord over 5 octaves"
    , dots =
        linearOrdinals
            [ freq A Oct1
            , freq C Oct2
            , freq E Oct2
            , freq G Oct2
            , freq A Oct2
            , freq C Oct3
            , freq E Oct3
            , freq G Oct3
            , freq A Oct3
            , freq C Oct4
            , freq E Oct4
            , freq G Oct4
            , freq A Oct4
            , freq C Oct5
            , freq E Oct5
            , freq G Oct5
            , freq A Oct5
            , freq C Oct6
            , freq E Oct6
            , freq G Oct6
            , freq A Oct6
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
    , period = 42000
    , sineTerms = [ 0, 0, 1, 0, 1 ]
    }


wholeTone4OctConfig : ScenarioConfig
wholeTone4OctConfig =
    { metadata = ConfigMetadata "Whole tone scale over 4 octaves"
    , dots =
        linearOrdinals
            [ freq C Oct2
            , freq D Oct2
            , freq E Oct2
            , freq Fshp Oct2
            , freq Gshp Oct2
            , freq Ashp Oct2
            , freq C Oct3
            , freq D Oct3
            , freq E Oct3
            , freq Fshp Oct3
            , freq Gshp Oct3
            , freq Ashp Oct3
            , freq C Oct4
            , freq D Oct4
            , freq E Oct4
            , freq Fshp Oct4
            , freq Gshp Oct4
            , freq Ashp Oct4
            , freq C Oct5
            , freq D Oct5
            , freq E Oct5
            , freq Fshp Oct5
            , freq Gshp Oct5
            , freq Ashp Oct5
            , freq C Oct6
            ]
    , period = 38000
    , sineTerms = [ 0, 0, 1, 0, 1 ]
    }


presets : List ScenarioConfig
presets =
    [ cMajor3OctConfig
    , cMajorTriad5OctConfig
    , gDom5OctConfig
    , aMinor75OctConfig
    , chromatic4OctConfig
    , wholeTone4OctConfig
    ]


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
            , padding = 5
            , innerPadding = 5
            , largestSizeRadius = 9
            , smallestSizeRadius = 2
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
            (1 - (toFloat (ordinal - 1) / toFloat (largestOrdinal - 1))) ^ 2
    in
    smallestSizeRadius
        + (largestSizeRadius - smallestSizeRadius)
        * ordinalRatio


getSpaceBetweenDots : Float -> Float -> Float -> Float -> Int -> Float
getSpaceBetweenDots baseRadius innerPadding largestSizeRadius smallestSizeRadius largestOrdinal =
    let
        summed =
            List.range 1 largestOrdinal
                |> List.map (getDotSizeRadius largestSizeRadius smallestSizeRadius largestOrdinal)
                |> List.map (\x -> x * 2)
                |> List.sum

        spaceBetweenDots =
            (baseRadius - innerPadding - summed) / (toFloat largestOrdinal - 1)
    in
    spaceBetweenDots


getDotPositionRadius : Float -> Float -> Float -> Float -> Int -> Int -> Float
getDotPositionRadius baseRadius innerPadding largestSizeRadius smallestSizeRadius largestOrdinal ordinal =
    let
        spaceBetweenDots =
            getSpaceBetweenDots baseRadius innerPadding largestSizeRadius smallestSizeRadius largestOrdinal

        largerDotDiametersSummed =
            List.range 1 (ordinal - 1)
                |> List.map (getDotSizeRadius largestSizeRadius smallestSizeRadius largestOrdinal)
                |> List.map (\x -> x * 2)
                |> List.sum

        thisSizeRadius =
            getDotSizeRadius largestSizeRadius smallestSizeRadius largestOrdinal ordinal

        positionRadius =
            (baseRadius - (spaceBetweenDots * (toFloat ordinal - 1))) - largerDotDiametersSummed - thisSizeRadius
    in
    positionRadius


{-| TODO there is a bug here. if there are too many dots, and the space between dots becomes
larger than the smallest dot's diameter, the inner dots' radiuses will be broken.
-}
renderDot : ( Float, Float ) -> Float -> Float -> Float -> Float -> Float -> Int -> Float -> Dot -> Renderable
renderDot baseCenter baseRadius innerPadding basePeriod largestSizeRadius smallestSizeRadius largestOrdinal timeSinceStart dot =
    let
        period =
            basePeriod / toFloat dot.ordinal

        position =
            dotPosition period timeSinceStart

        dotSizeRadius =
            getDotSizeRadius largestSizeRadius smallestSizeRadius largestOrdinal dot.ordinal

        dotPositionRadius =
            getDotPositionRadius baseRadius innerPadding largestSizeRadius smallestSizeRadius largestOrdinal dot.ordinal

        center =
            dotCenter position dotPositionRadius baseCenter

        colorCompGradient =
            \current max c1 c2 -> c1 + ((c2 - c1) * current) / max

        colorGradient =
            \current max ( r1, g1, b1 ) ( r2, g2, b2 ) -> ( colorCompGradient current max r1 r2, colorCompGradient current max g1 g2, colorCompGradient current max b1 b2 )

        blue =
            ( 62 / 255, 111 / 255, 240 / 255 )

        red =
            ( 204 / 255, 0, 0 )

        white =
            ( 1, 1, 1 )

        baseColor =
            colorGradient (toFloat dot.ordinal) (toFloat largestOrdinal) blue red

        proportionIntoPeriod =
            fractionalModBy period timeSinceStart / period

        amountToWhiten =
            if proportionIntoPeriod > 0.2 || proportionIntoPeriod < 0.00001 then
                0

            else
                1 - (proportionIntoPeriod * 5)

        whitenedColor =
            colorGradient amountToWhiten 1 baseColor white

        getColor =
            \( r, g, b ) -> rgba r g b 1.0

        color =
            getColor whitenedColor
    in
    shapes [ fill color ] [ circle center dotSizeRadius ]


renderCanvas : Float -> Float -> Float -> Float -> PlayState -> List Dot -> Float -> Float -> Float -> Html Msg
renderCanvas baseRadius padding innerPadding currentTime playState dots period largestSizeRadius smallestSizeRadius =
    let
        center =
            ( baseRadius + padding, baseRadius + padding )

        width =
            baseRadius * 2 + padding * 2

        height =
            baseRadius * 2 + padding * 2

        timeSinceStart =
            getTimeSinceStart currentTime playState

        largestOrdinal =
            Maybe.withDefault 1 <| List.maximum (List.map (\x -> x.ordinal) dots)

        dotRenderables =
            List.map (renderDot center baseRadius innerPadding period largestSizeRadius smallestSizeRadius largestOrdinal timeSinceStart) dots

        bg =
            canvasBackground width height

        line =
            shapes [ stroke Color.gray ] [ Canvas.path center [ lineTo (Tuple.mapFirst (\x -> x + baseRadius + padding) center) ] ]

        renderables =
            List.concat [ bg, [ line ], dotRenderables ]
    in
    Canvas.toHtml
        ( round width, round height )
        []
        renderables


canvasBackground : Float -> Float -> List Renderable
canvasBackground width height =
    let
        smallerF =
            min width height

        smaller =
            round smallerF

        steps =
            List.map toFloat <| List.range 0 smaller

        minAlpha =
            0.9

        maxAlpha =
            0.5

        renderStep =
            \step ->
                let
                    boxWidth =
                        width * step / smallerF

                    boxHeight =
                        height * step / smallerF
                in
                shapes [ stroke (rgba 0 0 0 (minAlpha + ((maxAlpha - minAlpha) * step / smallerF))) ] [ rect ( (width - boxWidth) / 2, (height - boxHeight) / 2 ) boxWidth boxHeight ]

        gradient =
            List.map renderStep steps
    in
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ] :: gradient


view : Model -> Html Msg
view model =
    div []
        [ description
        , div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "justify-content" "space-around"
            ]
            [ div [ Html.Attributes.style "flex" "1", Html.Attributes.style "padding" "10px" ] [ configControls ]
            , div [ Html.Attributes.style "flex" "0" ] <|
                [ renderCanvas model.appConfig.baseRadius model.appConfig.padding model.appConfig.innerPadding model.currentTime model.playState model.config.dots model.config.period model.appConfig.largestSizeRadius model.appConfig.smallestSizeRadius
                , buttonToolbar
                    (case model.playState.current of
                        Playing _ ->
                            True

                        Paused _ ->
                            False

                        Stopped ->
                            False
                    )
                ]
            , div [ Html.Attributes.style "flex" "1", Html.Attributes.style "padding" "10px" ] [ information model.config ]
            ]
        ]


renderConfigControl : ScenarioConfig -> Html Msg
renderConfigControl c =
    li
        [ Html.Attributes.style "margin" "10px 0"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "justify-content" "flex-end"
        ]
        [ Html.button [ onClick <| SetConfig c, Html.Attributes.style "cursor" "pointer" ] [ Html.text <| configName c ] ]


configControls : Html Msg
configControls =
    List.map renderConfigControl presets
        |> ul
            [ Html.Attributes.style "list-style" "none"
            , Html.Attributes.style "padding" "0"
            ]


information : ScenarioConfig -> Html Msg
information c =
    ul [ Html.Attributes.style "padding" "0", Html.Attributes.style "list-style" "none" ] <|
        List.map
            (\text -> li [ Html.Attributes.style "display" "flex" ] [ Html.text text ])
            [ "Base Period: " ++ String.fromFloat (c.period / 1000) ++ "s", "Num Dots: " ++ String.fromInt (List.length c.dots) ]


description : Html Msg
description =
    div []
        [ p [] [ Html.text "The Whitney Music Box was originally devised and implemented by Jim Bumgardner." ]
        , p [] [ Html.text "Each dot revolves at some whole-number multiple of the speed of the outermost dot. Eg. The outermost dot may complete a revolution in 32 seconds, the next will complete a revolution in 16 seconds, the next 8, etc." ]
        ]


controlButton : List (Html.Attribute msg) -> List (Html msg) -> Html msg
controlButton attributes children =
    Html.button
        (List.append attributes
            [ Html.Attributes.style "padding" "0"
            , Html.Attributes.style "line-height" "0"
            , Html.Attributes.style "border" "0"
            , Html.Attributes.style "background" "transparent"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "margin" "5px"
            ]
        )
        children


buttonToolbar : Bool -> Html Msg
buttonToolbar isPlaying =
    div []
        [ controlButton
            [ onClick
                (if isPlaying then
                    Pause

                 else
                    Start
                )
            , Html.Attributes.id "startButton"
            ]
            [ (if isPlaying then
                Filled.pause

               else
                Filled.play_arrow
              )
                28
                (Material.Icons.Types.Color <| rgba 0 0 0 0.9)
            ]
        , controlButton [ onClick Stop ] [ Filled.stop 28 (Material.Icons.Types.Color <| rgba 0 0 0 0.9) ]
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
